// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "cpp-lang.hpp"
#include "cpp-scanner.hpp"
#include "nodeclass.hpp"
#include "nodes.hpp"
#include "nodeutils.hpp"
#include "utils.hpp"
#include "cpp-comments.hpp"
#include "textbook-parser.hpp"
#include "textbook-model.hpp"

#include "clang-c/Index.h"

#include <boost/algorithm/string.hpp>
#include <boost/filesystem.hpp>
#include <boost/range/adaptor/transformed.hpp>

#include <algorithm>
#include <cassert>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <tuple>
#include <vector>


namespace eyestep {

namespace fs = boost::filesystem;
namespace po = boost::program_options;


namespace {

  using ParameterTuple = std::tuple<std::string, Type>;
  using TypeRefTuple = std::tuple<std::string, std::string>;

  struct FunctionDetails {
    std::vector<ParameterTuple> _parameters;
    std::vector<std::string> _namespaces;
    bool _override_anno = false;
    TypeRefTuple _typeref;
  };

  struct ParseContext {
    ParseContext(Grove& grove) : _grove(grove) {}

    Grove& _grove;
    Node* _document_node;
  };


  FunctionDetails scan_function_children(Cursor ecursor);
  std::string encode_args_for_id(const std::vector<ParameterTuple>& params);
  std::string method_const_anno(Cursor ecursor);
  void attach_out_of_line_desc(ParseContext* ctx, Cursor ecursor);


  enum Kind {
    k_function = 0,
    k_ns,
    k_var,
    k_struct,
    k_class,
    k_field,
    k_method,
    k_alias,
    k_last
  };

  std::string join_ns(const std::string& namespaces, const std::string& nm)
  {
    std::stringstream ss;
    if (!namespaces.empty()) {
      ss << namespaces;
      if (!nm.empty()) {
        ss << "::";
      }
    }
    ss << nm;
    return ss.str();
  }

  std::string make_id(const std::string& dialect, Kind kind,
                      const std::string& namespaces, const std::string& nm,
                      const std::string& args = "",
                      const std::string& annotation = "")
  {
    static const auto kind2nm =
      std::vector<std::string>{"function", "ns",    "var",    "struct",
                               "class",    "field", "method", "alias"};
    assert(kind2nm.size() == k_last);

    std::stringstream ss;
    ss << dialect << "/" << kind2nm[kind] << "/" << join_ns(namespaces, nm);

    if (kind == k_function || kind == k_method) {
      ss << "(" << args << ")";
    }
    if (!annotation.empty()) {
      ss << "/" << annotation;
    }
    return ss.str();
  }

  std::string get_decl_namespace(Cursor ecursor)
  {
    std::vector<std::string> nss;
    Cursor p = ecursor.semantic_parent();
    while (p.is_set()) {
      switch (p.kind()) {
      case CXCursor_Namespace:
      case CXCursor_StructDecl:
      case CXCursor_ClassDecl:
        nss.emplace_back(p.spelling());
        break;
      default:
        ;
      }

      p = p.semantic_parent();
    }

    std::reverse(nss.begin(), nss.end());
    return utils::join(nss, "::");
  }

  std::string create_node_id(Cursor ecursor)
  {
    auto nm = ecursor.spelling();
    std::string nss;

    switch (ecursor.kind()) {
    case CXCursor_StructDecl:
      return make_id("cpp", k_struct, get_decl_namespace(ecursor),
                     ecursor.spelling());
    case CXCursor_ClassDecl:
      return make_id("cpp", k_class, get_decl_namespace(ecursor),
                     ecursor.spelling());
    case CXCursor_Namespace:
      return make_id("cpp", k_ns, get_decl_namespace(ecursor),
                     ecursor.spelling());
    case CXCursor_VarDecl:
      return make_id("cpp", k_var, get_decl_namespace(ecursor),
                     ecursor.spelling());
    case CXCursor_CXXMethod:
    case CXCursor_Destructor:
    case CXCursor_Constructor:
      return make_id("cpp", k_method, get_decl_namespace(ecursor),
                     ecursor.spelling(),
                     encode_args_for_id(
                       scan_function_children(ecursor)._parameters),
                     method_const_anno(ecursor));
    case CXCursor_TypeAliasDecl:
    case CXCursor_TypedefDecl:
      return make_id("cpp", k_alias, get_decl_namespace(ecursor),
                     ecursor.spelling());
    default:
      assert(false);
    }

    return "";
  }


  void set_namespaces_attribute(Node* nd, Cursor ecursor)
  {
    auto nss = get_decl_namespace(ecursor);
    if (!nss.empty()) {
      nd->add_attribute("namespaces", nss);
    }
  }

  std::string path_rel_to_cwd(const SourceLocation& loc)
  {
    std::stringstream ss;
    ss << utils::make_relative(fs::current_path(), loc.filename()).string()
       << ":" << loc.line() << ":" << loc.column();
    return ss.str();
  }


  Node* make_type_node(Grove* grove, const std::string& gi, Type type)
  {
    Node* nd = grove->make_elt_node(gi);

    nd->add_attribute("name", type.spelling());
    nd->add_attribute("const?", type.is_const());

    return nd;
  }


  Node* make_desc_node(ParseContext* ctx, Grove* grove, const Cursor& ecursor)
  {
    Node* desc_node = nullptr;

    auto comment = ecursor.raw_comment();
    auto norm_comment = normalize_comment(comment);
    if (boost::starts_with(norm_comment, "@doc")) {
      auto comment2 = norm_comment + "\n@end doc";

      Node* doc_node = grove->make_elt_node("doc");
      textbook::GroveBuilder grove_builder(doc_node);
      textbook::VariableEnv vars;
      textbook::Catalog catalog;
      textbook::Parser parser(*grove, grove_builder, vars, catalog,
                              nullptr, // docspec
                              {fs::path("share/sairy/textbook/spec")},
                              false, // mixed content
                              false  // verbose
                              );

      parser.parse_string(comment2);

      auto nodes = doc_node->property<Nodes>(CommonProps::k_children);
      if (!nodes.empty()) {
        auto grand_children =
          nodes[0]->property<Nodes>(CommonProps::k_children);
        unparent_nodes(grand_children);

        desc_node = grove->make_elt_node("desc");
        for (auto* docnd : grand_children) {
          desc_node->add_child_node(docnd);
        }
      }

      grove->remove_node(doc_node);
    }

    return desc_node;
  }

  FunctionDetails scan_function_children(Cursor ecursor)
  {
    FunctionDetails result;
    std::vector<ParameterTuple> parameters;
    std::vector<std::string> namespaces;

    visit_children(ecursor, [&result](Cursor ec, Cursor ep) {
      if (ec.kind() == CXCursor_ParmDecl) {
        result._parameters.push_back(std::make_tuple(ec.spelling(), ec.type()));
      }
      else if (ec.kind() == CXCursor_NamespaceRef) {
        result._namespaces.push_back(ec.spelling());
      }
      else if (ec.kind() == CXCursor_CXXOverrideAttr) {
        result._override_anno = true;
      }
      else if (ec.kind() == CXCursor_TypeRef) {
        auto type_cursor = ec.referenced_cursor();
        if (type_cursor.is_set()) {
          result._typeref = std::make_tuple(type_cursor.spelling(),
                                            create_node_id(type_cursor));
        }
        else {
          result._typeref = std::make_tuple(ec.spelling(), std::string());
        }
      }
      else {
        std::cout << "!{function}" << kind2str(ec.kind()) << std::endl;
      }
      return CXChildVisit_Continue;
    });

    return result;
  };

  Node* encode_function_params(Grove* grove, const Type& type,
                               const std::vector<ParameterTuple>& params)
  {
    auto* parameters = grove->make_elt_node("parameters");

    for (int i = 0; i < type.num_arg_types(); i++) {
      Node* param = grove->make_elt_node("parameter");
      param->add_attribute("name", std::get<0>(params[i]));
      param->add_child_node(
        make_type_node(grove, "type", std::get<1>(params[i])));
      parameters->add_child_node(param);
    }

    return parameters;
  }

  void encode_function(Node* func_nd, ParseContext* ctx, Grove* grove,
                       Cursor ecursor,
                       const std::vector<ParameterTuple>& params)
  {
    Type type = ecursor.type();
    assert(params.size() == type.num_arg_types());

    if (ecursor.kind() != CXCursor_Constructor &&
        ecursor.kind() != CXCursor_Destructor) {
      func_nd->add_child_node(
        make_type_node(grove, "return-type", type.result_type()));
    }

    if (auto* params_nd = encode_function_params(grove, type, params)) {
      func_nd->add_child_node(params_nd);
    }
  }

  std::string encode_args_for_id(const std::vector<ParameterTuple>& params)
  {
    return utils::join(boost::copy_range<std::vector<std::string>>(
                         params | boost::adaptors::transformed(
                                    [](const ParameterTuple& tup) {
                                      return std::get<1>(tup).spelling();
                                    })),
                       ",");
  }


  /*! results in:
   *
   * @tag{function}    {name}          {ANY}
   * @tag{parameters}                  {ANY}
   * @tag{parameter}   {name, const?}  {ANY}
   * @tag{return-type} {name, const?}  {ANY}
   * @tag{type}        {name, const?}  {ANY}
   * @tag{desc}        {}              {TEXT|ANY}
   */
  Node* make_function_node(ParseContext* ctx, Grove* grove, Cursor ecursor)
  {
    // @todo clang_Cursor_isVariadic
    Node* nd = grove->make_elt_node("function");
    nd->set_property(CommonProps::k_source,
                     path_rel_to_cwd(ecursor.location()));

    std::string nm = ecursor.spelling();
    nd->add_attribute("name", nm);
    nd->add_attribute("dialect", "cpp");

    auto details = scan_function_children(ecursor);

    std::string nss;
    if (!details._namespaces.empty()) {
      nss = utils::join(details._namespaces, "::");
    }
    else {
      nss = get_decl_namespace(ecursor);
    }
    if (!nss.empty()) {
      nd->add_attribute("namespaces", nss);
    }

    nd->set_property(CommonProps::k_id,
                     make_id("cpp", k_function, nss, nm,
                             encode_args_for_id(details._parameters), ""));

    encode_function(nd, ctx, grove, ecursor, details._parameters);

    auto* desc_node = make_desc_node(ctx, grove, ecursor);
    if (desc_node) {
      nd->add_child_node(desc_node);
    }

    return nd;
  }


  CXChildVisitResult scan_hierarchy_visitor(ParseContext* ctx, Cursor ecursor,
                                            Cursor eparent);

  void scan_namespace(ParseContext* ctx, Cursor ecursor, Cursor eparent)
  {
    Grove* grove = ctx->_document_node->grove();
    auto* desc_node = make_desc_node(ctx, grove, ecursor);

    if (desc_node) {
      auto nm = ecursor.spelling();
      Node* nd = grove->make_elt_node("namespace");
      nd->set_property(CommonProps::k_source,
                       path_rel_to_cwd(ecursor.location()));
      nd->add_attribute("name", nm);

      set_namespaces_attribute(nd, ecursor);
      nd->set_property(CommonProps::k_id, create_node_id(ecursor));
      nd->add_child_node(desc_node);
      ctx->_document_node->add_child_node(nd);
    }

    visit_children(ecursor, [&ctx](Cursor ec, Cursor ep) {
      return scan_hierarchy_visitor(ctx, ec, ep);
    });
  }


  Node* scan_var_def(ParseContext* ctx, Cursor ecursor, Cursor eparent,
                     bool only_if_documented, const std::string& node_nm)
  {
    Grove* grove = ctx->_document_node->grove();

    bool is_out_of_line = false;
    visit_children(ecursor, [&is_out_of_line](Cursor ec, Cursor ep) {
      if (ec.kind() == CXCursor_TypeRef) {
        // This is a out-of-line definition of a (static?) member variable
        is_out_of_line = true;
      }
      // else ignore.
      return CXChildVisit_Continue;
    });

    if (is_out_of_line) {
      attach_out_of_line_desc(ctx, ecursor);
    }
    else {
      auto* desc_node = make_desc_node(ctx, grove, ecursor);

      if (desc_node || !only_if_documented) {
        auto nm = ecursor.spelling();
        Node* nd = grove->make_elt_node(node_nm);
        nd->set_property(CommonProps::k_source,
                         path_rel_to_cwd(ecursor.location()));

        nd->add_attribute("name", nm);

        nd->add_child_node(make_type_node(grove, "type", ecursor.type()));
        if (desc_node) {
          nd->add_child_node(desc_node);
        }

        return nd;
      }
    }

    return nullptr;
  }

  Node* scan_var(ParseContext* ctx, Cursor ecursor, Cursor eparent)
  {
    if (auto* nd = scan_var_def(ctx, ecursor, eparent, true, "variable")) {
      set_namespaces_attribute(nd, ecursor);
      nd->set_property(CommonProps::k_id, create_node_id(ecursor));
      return nd;
    }
    return nullptr;
  }


  //----------------------------------------------------------------------------

  Node* scan_field(ParseContext* ctx, Cursor ecursor, Cursor eparent)
  {
    Grove* grove = ctx->_document_node->grove();
    auto* desc_node = make_desc_node(ctx, grove, ecursor);

    // document all
    if (ecursor.access_specifier() == CX_CXXPublic ||
        ecursor.access_specifier() == CX_CXXProtected) {
      auto nm = ecursor.spelling();
      Node* nd = grove->make_elt_node("field");
      nd->add_attribute("name", nm);
      nd->set_property(CommonProps::k_source,
                       path_rel_to_cwd(ecursor.location()));
      nd->set_property(CommonProps::k_id,
                       make_id("cpp", k_field, get_decl_namespace(ecursor),
                               nm));
      nd->add_attribute("access",
                        access_specifier_to_string(ecursor.access_specifier()));
      nd->add_attribute("linkage", "member");

      nd->add_child_node(make_type_node(grove, "type", ecursor.type()));

      if (desc_node) {
        nd->add_child_node(desc_node);
      }

      return nd;
    }

    return nullptr;
  }

  Node* scan_static_field(ParseContext* ctx, Cursor ecursor, Cursor eparent)
  {
    if (ecursor.access_specifier() == CX_CXXPublic ||
        ecursor.access_specifier() == CX_CXXProtected) {
      if (auto* nd = scan_var_def(ctx, ecursor, eparent, false, "field")) {
        nd->set_property(CommonProps::k_id,
                         make_id("cpp", k_field, get_decl_namespace(ecursor),
                                 ecursor.spelling()));
        nd->add_attribute("access", access_specifier_to_string(
                                      ecursor.access_specifier()));
        nd->add_attribute("linkage", "static");

        return nd;
      }
    }
    return nullptr;
  }

  std::string method_linkage(Cursor ecursor)
  {
    if (ecursor.is_static_method()) {
      return "static";
    }
    else if (ecursor.is_pure_virtual_method()) {
      return "pure-virtual";
    }
    else if (ecursor.is_virtual_method()) {
      return "virtual";
    }
    else {
      return "member";
    }
  }

  std::string method_const_anno(Cursor ecursor)
  {
    return ecursor.is_const_method() ? "const" : "";
  }

  Node* scan_method(ParseContext* ctx, Cursor ecursor, Cursor eparent,
                    const std::string& eltnm)
  {
    if (ecursor.access_specifier() == CX_CXXPublic ||
        ecursor.access_specifier() == CX_CXXProtected) {
      Grove* grove = ctx->_document_node->grove();
      auto* desc_node = make_desc_node(ctx, grove, ecursor);

      auto nm = ecursor.spelling();
      Node* nd = grove->make_elt_node(eltnm);
      nd->set_property(CommonProps::k_source,
                       path_rel_to_cwd(ecursor.location()));

      nd->add_attribute("name", nm);
      nd->add_attribute("access",
                        access_specifier_to_string(ecursor.access_specifier()));
      nd->add_attribute("linkage", method_linkage(ecursor));
      nd->add_attribute("const?", ecursor.is_const_method());

      // @todo: use clang_getOverriddenCursors to find override methods and
      //      produce a idref list as an attribute on this node.
      // @todo: clang_Type_getCXXRefQualifier

      auto details = scan_function_children(ecursor);

      nd->set_property(CommonProps::k_id, create_node_id(ecursor));

      encode_function(nd, ctx, grove, ecursor, details._parameters);

      if (desc_node) {
        nd->add_child_node(desc_node);
      }

      return nd;
    }

    return nullptr;
  }

  void attach_out_of_line_desc(ParseContext* ctx, Cursor ecursor)
  {
    if (ecursor.access_specifier() == CX_CXXPublic ||
        ecursor.access_specifier() == CX_CXXProtected) {
      Grove* grove = ctx->_document_node->grove();
      auto* desc_node = make_desc_node(ctx, grove, ecursor);

      if (desc_node) {
        auto id = create_node_id(ecursor);

        auto nodes = elements_with_id(grove, id);
        if (!nodes.empty()) {
          auto* decl_node = const_cast<Node*>(nodes[0]);
          auto* inline_desc_node = desc_element(decl_node);
          if (!inline_desc_node) {
            decl_node->add_child_node(desc_node);
            return;
          }
        }

        grove->remove_node(desc_node);
      }
    }
  }

  Node* scan_base_specifier(ParseContext* ctx, Cursor ecursor, Cursor eparent)
  {
    Grove* grove = ctx->_document_node->grove();
    auto* nd = grove->make_elt_node("inherit");

    auto base_cursor = ecursor.referenced_cursor();
    if (base_cursor.is_set()) {
      nd->add_attribute("base-ref", create_node_id(base_cursor));
    }

    nd->add_attribute("name", ecursor.type().spelling());
    nd->add_attribute("access",
                      access_specifier_to_string(ecursor.access_specifier()));
    nd->add_attribute("virtual?", int(ecursor.is_class_virtual_base()));

    return nd;
  }

  void add_wrapped_child_nodes(Node* parent_nd, const std::string& container_nm,
                               const std::vector<Node*>& nodes)
  {
    if (!nodes.empty()) {
      Node* wrap_nd = parent_nd->grove()->make_elt_node(container_nm);
      for (auto* nd : nodes) {
        wrap_nd->add_child_node(nd);
      }
      parent_nd->add_child_node(wrap_nd);
    }
  }

  Node* scan_typealias(ParseContext* ctx, Cursor ecursor, Cursor eparent,
                       bool inner)
  {
    if (inner && ecursor.access_specifier() != CX_CXXPublic &&
        ecursor.access_specifier() != CX_CXXProtected) {
      return nullptr;
    }

    Grove* grove = ctx->_document_node->grove();
    auto* desc_node = make_desc_node(ctx, grove, ecursor);

    if (desc_node) {
      auto* nd = grove->make_elt_node("type-alias");
      nd->set_property(CommonProps::k_source,
                       path_rel_to_cwd(ecursor.location()));

      nd->add_attribute("name", ecursor.spelling());
      if (inner) {
        nd->add_attribute("access", access_specifier_to_string(
                                      ecursor.access_specifier()));
      }

      set_namespaces_attribute(nd, ecursor);
      nd->set_property(CommonProps::k_id, create_node_id(ecursor));

      nd->add_attribute("referenced-type-name",
                        ecursor.typedef_underlying_type().spelling());

      auto decl_cursor = ecursor.typedef_underlying_type().declaration();
      if (decl_cursor.kind() != CXCursor_NoDeclFound) {
        auto decl_ns = get_decl_namespace(decl_cursor);
        auto type_id = create_node_id(decl_cursor);

        auto nodes = elements_with_id(grove, type_id);
        if (!nodes.empty()) {
          nd->add_attribute("type-ref", type_id);
        }
      }

      nd->add_child_node(desc_node);
      return nd;
    }

    return nullptr;
  }


  Node* scan_struct(ParseContext* ctx, Cursor ecursor, Cursor eparent,
                    const std::string& eltnm, bool inner)
  {
    if (inner && ecursor.access_specifier() != CX_CXXPublic &&
        ecursor.access_specifier() != CX_CXXProtected) {
      return nullptr;
    }

    Grove* grove = ctx->_document_node->grove();
    auto* desc_node = make_desc_node(ctx, grove, ecursor);

    if (desc_node) {
      auto nm = ecursor.spelling();
      Node* nd = grove->make_elt_node(eltnm);
      nd->set_property(CommonProps::k_source,
                       path_rel_to_cwd(ecursor.location()));

      nd->add_attribute("name", nm);
      if (inner) {
        nd->add_attribute("access", access_specifier_to_string(
                                      ecursor.access_specifier()));
      }

      set_namespaces_attribute(nd, ecursor);
      nd->set_property(CommonProps::k_id, create_node_id(ecursor));

      struct StructDefs {
        std::vector<Node*> _fields;
        std::vector<Node*> _meths;
        std::vector<Node*> _ctors;
        std::vector<Node*> _dtors;
        std::vector<Node*> _bases;
        std::vector<Node*> _types;
      } defs;

      visit_children(ecursor, [&ctx, &nd, &defs](Cursor ec, Cursor ep) {
        if (ec.kind() == CXCursor_FieldDecl) {
          if (auto* field_nd = scan_field(ctx, ec, ep)) {
            defs._fields.push_back(field_nd);
          }
        }
        else if (ec.kind() == CXCursor_VarDecl) {
          // static data members are reported as variables
          if (Node* var_nd = scan_static_field(ctx, ec, ep)) {
            defs._fields.push_back(var_nd);
          }
        }
        else if (ec.kind() == CXCursor_Constructor) {
          if (auto* meth_nd = scan_method(ctx, ec, ep, "constructor")) {
            defs._ctors.push_back(meth_nd);
          }
        }
        else if (ec.kind() == CXCursor_Destructor) {
          if (auto* meth_nd = scan_method(ctx, ec, ep, "destructor")) {
            defs._dtors.push_back(meth_nd);
          }
        }
        else if (ec.kind() == CXCursor_CXXMethod) {
          if (auto* meth_nd = scan_method(ctx, ec, ep, "method")) {
            defs._meths.push_back(meth_nd);
          }
        }
        else if (ec.kind() == CXCursor_CXXBaseSpecifier) {
          if (auto* base_nd = scan_base_specifier(ctx, ec, ep)) {
            defs._bases.push_back(base_nd);
          }
        }
        else if (ec.kind() == CXCursor_StructDecl) {
          if (auto* type_nd = scan_struct(ctx, ec, ep, "struct", true)) {
            defs._types.emplace_back(type_nd);
          }
        }
        else if (ec.kind() == CXCursor_ClassDecl) {
          if (auto* type_nd = scan_struct(ctx, ec, ep, "class", true)) {
            defs._types.emplace_back(type_nd);
          }
        }
        else if (ec.kind() == CXCursor_TypeAliasDecl ||
                 ec.kind() == CXCursor_TypedefDecl) {
          if (auto* alias_nd = scan_typealias(ctx, ec, ep, true)) {
            defs._types.emplace_back(alias_nd);
          }
        }
        else if (ec.kind() == CXCursor_CXXAccessSpecifier) {
          // nop, handled inside of the other scan handlers
        }
        else {
          std::cout << "!{struct}" << kind2str(ec.kind()) << std::endl;
        }

        return CXChildVisit_Continue;
      });

      add_wrapped_child_nodes(nd, "inherits", defs._bases);
      add_wrapped_child_nodes(nd, "types", defs._types);
      add_wrapped_child_nodes(nd, "fields", defs._fields);
      add_wrapped_child_nodes(nd, "constructors", defs._ctors);
      add_wrapped_child_nodes(nd, "destructors", defs._dtors);
      add_wrapped_child_nodes(nd, "methods", defs._meths);

      nd->add_child_node(desc_node);

      return nd;
    }

    return nullptr;
  }


  CXChildVisitResult scan_hierarchy_visitor(ParseContext* ctx, Cursor ecursor,
                                            Cursor eparent)
  {
    CXChildVisitResult retval = CXChildVisit_Continue;

    CXCursorKind kind = ecursor.kind();
    std::string nm = ecursor.spelling();

    SourceLocation loc = ecursor.location();
    if (loc.is_from_main_file()) {
      if (clang_isDeclaration(kind)) {
        if (kind == CXCursor_FunctionDecl) {
          Node* nd =
            make_function_node(ctx, ctx->_document_node->grove(), ecursor);
          ctx->_document_node->add_child_node(nd);

          retval = CXChildVisit_Continue;
        }
        else if (kind == CXCursor_Namespace) {
          scan_namespace(ctx, ecursor, eparent);
          retval = CXChildVisit_Continue;
        }
        else if (kind == CXCursor_VarDecl) {
          if (Node* var_nd = scan_var(ctx, ecursor, eparent)) {
            ctx->_document_node->add_child_node(var_nd);
          }
          retval = CXChildVisit_Continue;
        }
        else if (kind == CXCursor_StructDecl) {
          if (auto* struct_nd =
                scan_struct(ctx, ecursor, eparent, "struct", false)) {
            ctx->_document_node->add_child_node(struct_nd);
          }
          retval = CXChildVisit_Continue;
        }
        else if (kind == CXCursor_ClassDecl) {
          if (auto* class_nd =
                scan_struct(ctx, ecursor, eparent, "class", false)) {
            ctx->_document_node->add_child_node(class_nd);
          }
          retval = CXChildVisit_Continue;
        }
        else if (kind == CXCursor_CXXMethod || kind == CXCursor_Destructor ||
                 kind == CXCursor_Constructor) {
          attach_out_of_line_desc(ctx, ecursor);
        }
        else if (kind == CXCursor_TypeAliasDecl ||
                 kind == CXCursor_TypedefDecl) {
          if (auto* alias_nd = scan_typealias(ctx, ecursor, eparent, false)) {
            ctx->_document_node->add_child_node(alias_nd);
          }
        }
        else if (kind == CXCursor_UnexposedDecl) {
          // this is triggered e.g. by a orphaned ; on top level
          // nop
        }
        else if (kind == CXCursor_UsingDirective) {
          // e.g. using namespace foo;
          // nop
        }
        else {
          printf("UNHANDLED DECL TYPE %s %s (%s)\n", kind2str(kind), nm.c_str(),
                 loc.format().c_str());
        }
      }
      else if (kind == CXCursor_InclusionDirective) {
        // nop
      }
      else {
        printf("SOME OTHER KIND: %s \n", kind2str(kind));
      }
    }
    return retval;
  }


  CXChildVisitResult scan_hierarchy_visitor(CXCursor cursor, CXCursor parent,
                                            CXClientData client_data)
  {
    ParseContext* ctx = static_cast<ParseContext*>(client_data);
    return scan_hierarchy_visitor(ctx, Cursor(cursor), Cursor(parent));
  }

} // end anon namespace


//----------------------------------------------------------------------------------------

namespace {

  int compute_data_size(const std::vector<std::string>& values, const char* opt,
                        bool is_sep)
  {
    int totalsize = 0;
    int optlen = strlen(opt);

    for (const auto& str : values) {
      if (!is_sep) {
        totalsize += optlen;
      }

      totalsize += str.length() + 1;
    }

    return totalsize;
  }


  int append_options(int in_argc, const std::vector<std::string>& values,
                     const char* opt, bool is_sep,
                     std::vector<const char*>& args,
                     std::vector<char>& argsdata)
  {
    int argc = in_argc;
    for (const auto& str : values) {
      std::string arg;
      int ofs = argsdata.size();

      if (is_sep) {
        args.push_back(opt);
        argc++;

        arg = str;
      }
      else {
        arg = std::string() + opt + str;
      }

      argsdata.reserve(arg.length() + 1);
      argsdata.insert(argsdata.end(), arg.begin(), arg.end());
      argsdata.push_back(0);

      args.push_back(&argsdata[ofs]);
      argc++;
    }

    return argc;
  }

} // ns anon


//------------------------------------------------------------------------------

CppScanner::CppScanner() : _verbose(false)
{
}

CppScanner::CppScanner(const po::variables_map& args) : _verbose(false)
{
  if (!args.empty()) {

    std::vector<std::string> incl_paths;
    if (args.count("include-path")) {
      incl_paths = args["include-path"].as<std::vector<std::string>>();
    }

    if (args.count("isystem")) {
      auto v = args["isystem"].as<std::vector<std::string>>();
      incl_paths.insert(incl_paths.end(), v.begin(), v.end());
    }

    std::vector<std::string> defs;
    if (args.count("defs")) {
      defs = args["defs"].as<std::vector<std::string>>();
    }

    _incl_paths.swap(incl_paths);
    _defs.swap(defs);

    _verbose = args["verbose"].as<bool>();
    if (_verbose) {
      std::cout << "Cpp scanner:" << std::endl
                << "incl paths  : " << eyestep::utils::join(incl_paths, " ")
                << std::endl
                << "defs        : " << eyestep::utils::join(defs, " ")
                << std::endl;
    }
  }
}

std::string CppScanner::scanner_id() const
{
  return "cpp";
}

std::unordered_set<std::string> CppScanner::supported_extensions() const
{
  return {".c",   ".cpp", ".cxx", ".h", ".hh",
          ".hpp", ".hxx", ".ipp", ".m", ".mm"};
}


po::options_description CppScanner::program_options() const
{
  std::string opts_title =
    std::string("C++ parser [selector: '") + scanner_id() + "']";
  po::options_description desc(opts_title);

  // clang-format off
  desc.add_options()
    ("include-path,I", po::value<std::vector<std::string>>()->composing(),
                       "add include path to C parser")
    ("defs,D",         po::value<std::vector<std::string>>()->composing(),
                       "add preprocessor defines")
    ("isysroot",       po::value<std::vector<std::string>>()->composing(),
                       "ignored")
    ("isystem",        po::value<std::vector<std::string>>()->composing(),
                       "-isystem arg, like -I")
    ;
  // clang-format on

  return desc;
}

Node* CppScanner::scan_file(eyestep::Grove& grove, const fs::path& srcfile)
{
  CXIndex idx;
  CXTranslationUnit tu;

  ParseContext ctx(grove);

  ctx._document_node = grove.make_node(document_class_definition());

  ctx._document_node->set_property(CommonProps::k_source,
                                   utils::make_relative(fs::current_path(),
                                                        srcfile).string());
  ctx._document_node->set_property("app-info", "cpp");

  // excludeDeclsFromPCH = 1, displayDiagnostics=1
  idx = clang_createIndex(1, 1);

  std::vector<const char*> args;
  std::vector<char> argsdata;

  int totalsize = compute_data_size(_incl_paths, "-I", true) +
                  compute_data_size(_defs, "-D", false);
  argsdata.resize(totalsize);

  int argc = 0;
  argc = append_options(argc, _incl_paths, "-I", true, args, argsdata);
  argc = append_options(argc, _defs, "-D", false, args, argsdata);

  args.push_back("-x");
  argc++;
  args.push_back("c++");
  argc++;

  unsigned int options = CXTranslationUnit_SkipFunctionBodies |
                         CXTranslationUnit_DetailedPreprocessingRecord |
                         CXTranslationUnit_Incomplete |
                         CXTranslationUnit_IncludeBriefCommentsInCodeCompletion;


  tu = clang_parseTranslationUnit(idx, srcfile.string().c_str(),
                                  (const char* const*)&args[0], argc,
                                  (struct CXUnsavedFile*)NULL,
                                  0, // num_unsaved_files,
                                  options);

  clang_visitChildren(clang_getTranslationUnitCursor(tu),
                      (CXCursorVisitor)scan_hierarchy_visitor, &ctx);
  clang_disposeTranslationUnit(tu);

  return ctx._document_node;
}

} // ns eyestep
