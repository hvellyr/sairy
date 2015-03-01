// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "scm-context.hpp"
#include "estd/memory.hpp"
#include "nodes.hpp"
#include "nodelist.hpp"

#include "chibi/eval.h"
#include "chibi/sexp.h"

#include <boost/variant/apply_visitor.hpp>
#include <boost/variant/static_visitor.hpp>

#include <iostream>
#include <memory>
#include <string>
#include <vector>


namespace eyestep {

namespace fs = boost::filesystem;

namespace {
  static const Node* sRootNode;

  const Node* rootNode() { return sRootNode; }


#define NODE_TAG "<node>"
#define NODE_TAG_SIZE 6

#define NODELIST_TAG "<node-list>"
#define NODELIST_TAG_SIZE 11


  //----------------------------------------------------------------------------

  int node_tag_p(sexp ctx)
  {
    int retv = 0;
    sexp_gc_var2(ty, nm);
    sexp_gc_preserve2(ctx, ty, nm);

    ty =
        sexp_env_ref(ctx, sexp_context_env(ctx),
                     nm = sexp_intern(ctx, NODE_TAG, NODE_TAG_SIZE), SEXP_VOID);
    if (sexp_typep(ty)) {
      retv = sexp_type_tag(ty);
    }

    sexp_gc_release2(ctx);

    return retv;
  }


  int nodelist_tag_p(sexp ctx)
  {
    int retv = 0;
    sexp_gc_var2(ty, nm);
    sexp_gc_preserve2(ctx, ty, nm);

    ty = sexp_env_ref(ctx, sexp_context_env(ctx),
                      nm = sexp_intern(ctx, NODELIST_TAG, NODELIST_TAG_SIZE),
                      SEXP_VOID);
    if (sexp_typep(ty)) {
      retv = sexp_type_tag(ty);
    }

    sexp_gc_release2(ctx);

    return retv;
  }


  const Node* node_from_arg(sexp ctx, sexp arg)
  {
    if (sexp_check_tag(arg, node_tag_p(ctx))) {
      return (const Node*)(sexp_cpointer_value(arg));
    }
    else if (sexp_check_tag(arg, nodelist_tag_p(ctx))) {
      const NodeList* nl = (const NodeList*)(sexp_cpointer_value(arg));
      if (nl->length() == 1) {
        return nl->head();
      }
    }

    return nullptr;
  }


  //------------------------------------------------------------------------------

  sexp make_node(sexp ctx, const Node* obj)
  {
    sexp_gc_var4(ty, tmp, result, nm);
    sexp_gc_preserve4(ctx, ty, tmp, result, nm);

    ty =
        sexp_env_ref(ctx, sexp_context_env(ctx),
                     nm = sexp_intern(ctx, NODE_TAG, NODE_TAG_SIZE), SEXP_VOID);

    if (sexp_typep(ty)) {
      result = sexp_alloc_type(ctx, cpointer, sexp_type_tag(ty));
      sexp_cpointer_freep(result) = 0;
      sexp_cpointer_length(result) = 0;
      sexp_cpointer_value(result) = (void*)obj;
    }
    else {
      result = SEXP_VOID;
    }

    sexp_gc_release4(ctx);

    return result;
  }


  sexp func_gi(sexp ctx, sexp self, sexp n, sexp nodeArg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_VOID;

    if (const Node* node = node_from_arg(ctx, nodeArg)) {
      result = sexp_c_string(ctx, node->gi().c_str(), -1);
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node/singleton node-list",
                                   nodeArg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_node_property(sexp ctx, sexp self, sexp_sint_t n, sexp propNameArg,
                          sexp nodeArg)
  {
    class SexpPropVisitor : public boost::static_visitor<sexp> {
      sexp mCtx;
      sexp mSelf;
      sexp mPropName;

    public:
      SexpPropVisitor(sexp ctx, sexp self, sexp propName)
          : mCtx(ctx), mSelf(self), mPropName(propName)
      {
      }

      sexp operator()(const Undefined&)
      {
        return sexp_user_exception(mCtx, mSelf, "Undefined property",
                                   mPropName);
      }

      sexp operator()(const int& val) { return sexp_make_integer(mCtx, val); }

      sexp operator()(const std::string& val)
      {
        return sexp_c_string(mCtx, val.c_str(), -1);
      }

      sexp operator()(Node* nd)
      {
        // TODO
        return SEXP_VOID;
      }

      sexp operator()(const Nodes& nl)
      {
        // TODO
        return SEXP_VOID;
      }
    };

    sexp_gc_var2(result, str);
    sexp_gc_preserve2(ctx, result, str);

    result = SEXP_VOID;

    if (const Node* node = node_from_arg(ctx, nodeArg)) {
      std::string propName;
      if (sexp_isymbolp(propNameArg)) {
        str = sexp_symbol_to_string(ctx, propNameArg);
        propName = sexp_string_data(str);
      }
      else if (sexp_lsymbolp(propNameArg)) {
        propName = std::string(sexp_lsymbol_data(propNameArg),
                               sexp_lsymbol_length(propNameArg));
      }
      else {
        result = sexp_user_exception(ctx, self, "not a symbol", propNameArg);
      }

      SexpPropVisitor visitor(ctx, self, propNameArg);
      PropertyValue value = (*node)[propName];
      result = boost::apply_visitor(visitor, value);
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node/singleton node-list",
                                   nodeArg);
    }

    sexp_gc_release2(ctx);

    return result;
  }


  sexp func_grove_root(sexp ctx, sexp self, sexp_sint_t n)
  {
    return make_node(ctx, rootNode());
  }


  sexp finalize_node(sexp ctx, sexp self, sexp_sint_t n, sexp nodeArg)
  {
    sexp_cpointer_value(nodeArg) = nullptr;
    return SEXP_VOID;
  }


  void init_node_functions(sexp ctx)
  {
    sexp_gc_var3(nm, ty, op);
    sexp_gc_preserve3(ctx, nm, ty, op);

    // register qobject type
    ty = sexp_register_c_type(ctx, nm = sexp_c_string(ctx, "node", -1),
                              &finalize_node);

    sexp_env_cell_define(ctx, sexp_context_env(ctx),
                         nm = sexp_intern(ctx, NODE_TAG, NODE_TAG_SIZE), ty,
                         NULL);
    op =
        sexp_make_type_predicate(ctx, nm = sexp_c_string(ctx, "node?", -1), ty);
    sexp_env_define(ctx, sexp_context_env(ctx),
                    nm = sexp_intern(ctx, "node?", -1), op);

    // register functions
    sexp_define_foreign(ctx, sexp_context_env(ctx), "gi", 1, &func_gi);

    sexp_define_foreign(ctx, sexp_context_env(ctx), "node-property", 2,
                        &func_node_property);

    sexp_define_foreign(ctx, sexp_context_env(ctx), "grove-root", 0,
                        &func_grove_root);

    sexp_gc_release3(ctx);
  }


  //----------------------------------------------------------------------------

  sexp make_nodelist(sexp ctx, const NodeList* obj)
  {
    sexp_gc_var4(ty, tmp, result, nm);
    sexp_gc_preserve4(ctx, ty, tmp, result, nm);

    ty = sexp_env_ref(ctx, sexp_context_env(ctx),
                      nm = sexp_intern(ctx, NODELIST_TAG, NODELIST_TAG_SIZE),
                      SEXP_VOID);

    if (sexp_typep(ty)) {
      result = sexp_alloc_type(ctx, cpointer, sexp_type_tag(ty));
      sexp_cpointer_freep(result) = 0;
      sexp_cpointer_length(result) = 0;
      sexp_cpointer_value(result) = (void*)obj;
    }
    else {
      result = SEXP_VOID;
    }

    sexp_gc_release4(ctx);

    return result;
  }


  sexp free_nodelist(sexp ctx, sexp self, sexp_sint_t n, sexp nlArg)
  {
    const NodeList* nl = (const NodeList*)(sexp_cpointer_value(nlArg));
    delete nl;

    sexp_cpointer_value(nlArg) = nullptr;
    return SEXP_VOID;
  }


  sexp func_empty_node_list(sexp ctx, sexp self, sexp_sint_t n)
  {
    return make_nodelist(ctx, new NodeList());
  }


  sexp func_node_list_empty_p(sexp ctx, sexp self, sexp_sint_t n, sexp nlArg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (sexp_check_tag(nlArg, nodelist_tag_p(ctx))) {
      const NodeList* nl = (const NodeList*)(sexp_cpointer_value(nlArg));
      result = sexp_make_boolean(nl->empty());
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node-list", nlArg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_node_list_length(sexp ctx, sexp self, sexp_sint_t n, sexp nlArg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (sexp_check_tag(nlArg, nodelist_tag_p(ctx))) {
      const NodeList* nl = (const NodeList*)(sexp_cpointer_value(nlArg));
      result = sexp_make_fixnum(nl->length());
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node-list", nlArg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_node_list_head(sexp ctx, sexp self, sexp_sint_t n, sexp nlArg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (sexp_check_tag(nlArg, nodelist_tag_p(ctx))) {
      const NodeList* nl = (const NodeList*)(sexp_cpointer_value(nlArg));
      const Node* node = nl->head();
      result = node ? make_node(ctx, node) : SEXP_NULL;
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node-list", nlArg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_node_list_rest(sexp ctx, sexp self, sexp_sint_t n, sexp nlArg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (sexp_check_tag(nlArg, nodelist_tag_p(ctx))) {
      const NodeList* nl = (const NodeList*)(sexp_cpointer_value(nlArg));
      result = make_nodelist(ctx, new NodeList(std::move(nl->rest())));
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node-list", nlArg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp make_nodelist_on_axis(sexp ctx, sexp self, sexp_sint_t n, sexp nlArg,
                             NodeList::Kind kind)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (const Node* node = node_from_arg(ctx, nlArg)) {
      result = make_nodelist(ctx, new NodeList(node, kind));
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node/singleton node-list",
                                   nlArg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_children(sexp ctx, sexp self, sexp_sint_t n, sexp nlArg)
  {
    return make_nodelist_on_axis(ctx, self, n, nlArg, NodeList::kChildren);
  }


  sexp func_ancestors(sexp ctx, sexp self, sexp_sint_t n, sexp nlArg)
  {
    return make_nodelist_on_axis(ctx, self, n, nlArg, NodeList::kAncestors);
  }


  sexp func_descendants(sexp ctx, sexp self, sexp_sint_t n, sexp nlArg)
  {
    return make_nodelist_on_axis(ctx, self, n, nlArg, NodeList::kDescendants);
  }


  sexp func_siblings(sexp ctx, sexp self, sexp_sint_t n, sexp nlArg)
  {
    return make_nodelist_on_axis(ctx, self, n, nlArg, NodeList::kSiblings);
  }


  sexp func_follow(sexp ctx, sexp self, sexp_sint_t n, sexp nlArg)
  {
    return make_nodelist_on_axis(ctx, self, n, nlArg, NodeList::kFollow);
  }


  sexp func_preced(sexp ctx, sexp self, sexp_sint_t n, sexp nlArg)
  {
    return make_nodelist_on_axis(ctx, self, n, nlArg, NodeList::kPreced);
  }


  void init_nodelist_functions(sexp ctx)
  {
    sexp_gc_var3(nm, ty, op);
    sexp_gc_preserve3(ctx, nm, ty, op);

    // register qobject type
    ty = sexp_register_c_type(ctx, nm = sexp_c_string(ctx, "node-list", -1),
                              &free_nodelist);
    sexp_env_cell_define(ctx, sexp_context_env(ctx),
                         nm = sexp_intern(ctx, NODELIST_TAG, NODELIST_TAG_SIZE),
                         ty, NULL);

    op =
        sexp_make_type_predicate(ctx, nm = sexp_c_string(ctx, "node-list?", -1),
                                 ty);
    sexp_env_define(ctx, sexp_context_env(ctx),
                    nm = sexp_intern(ctx, "node-list?", -1), op);

    sexp_define_foreign(ctx, sexp_context_env(ctx), "empty-node-list", 0,
                        &func_empty_node_list);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "node-list-length", 1,
                        &func_node_list_length);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "node-list-empty?", 1,
                        &func_node_list_empty_p);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "node-list-head", 1,
                        &func_node_list_head);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "node-list-rest", 1,
                        &func_node_list_rest);

    sexp_define_foreign(ctx, sexp_context_env(ctx), "children", 1,
                        &func_children);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "siblings", 1,
                        &func_siblings);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "follow", 1, &func_follow);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "preced", 1, &func_preced);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "ancestors", 1,
                        &func_ancestors);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "descendants", 1,
                        &func_descendants);

    sexp_gc_release3(ctx);
  }


  void init_builtins(sexp ctx)
  {
    init_node_functions(ctx);
    init_nodelist_functions(ctx);
  }


  //----------------------------------------------------------------------------

  bool check_exception_p(sexp ctx, sexp res)
  {
    sexp_gc_var3(err, sym, tmp);

    if (res && sexp_exceptionp(res)) {
      sexp_gc_preserve3(ctx, err, sym, tmp);
      tmp = res;
      err = sexp_current_error_port(ctx);
      if (!sexp_oportp(err)) {
        err = sexp_make_output_port(ctx, stderr, SEXP_FALSE);
      }

      sexp_print_exception(ctx, res, err);
      sexp_stack_trace(ctx, err);

      sexp_gc_release3(ctx);

      return false;
    }
    return true;
  }


  class SchemeContext : public ISchemeContext {
    sexp mCtx;

  public:
    SchemeContext() : mCtx(nullptr) {}


    ~SchemeContext()
    {
      if (mCtx != nullptr) {
        sexp_destroy_context(mCtx);
      }
    }


    void initialize(const std::vector<fs::path>& modulePaths) override
    {
      mCtx = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);

      sexp_gc_var1(tmp);
      sexp_gc_preserve1(mCtx, tmp);

      init_builtins(mCtx);

      for (const auto& path : modulePaths) {
        std::string libPath = path.string();

        sexp_add_module_directory(mCtx,
                                  tmp =
                                      sexp_c_string(mCtx, libPath.c_str(), -1),
                                  SEXP_FALSE);
      }

      sexp_load_standard_env(mCtx, NULL, SEXP_SEVEN);
      sexp_load_standard_ports(mCtx, NULL, stdin, stdout, stderr, 1);

      sexp_gc_release1(mCtx);
    }


    bool loadModuleFile(const fs::path& scriptFile) override
    {
      sexp_gc_var1(res);
      sexp_gc_preserve1(mCtx, res);

      res = sexp_load_module_file(mCtx, scriptFile.string().c_str(), nullptr);
      bool retv = check_exception_p(mCtx, res);

      sexp_gc_release1(mCtx);

      return retv;
    }


    bool loadScript(const fs::path& scriptFile) override
    {
      sexp_gc_var2(obj1, res);
      sexp_gc_preserve2(mCtx, obj1, res);

      obj1 = sexp_c_string(mCtx, scriptFile.string().c_str(), -1);
      bool retv = check_exception_p(mCtx, res = sexp_load(mCtx, obj1, NULL));

      sexp_gc_release2(mCtx);

      return retv;
    }


    void setupTemplateFunctions(const Node* rootNode) override
    {
      sRootNode = rootNode;
    }
  };

} // ns anon


std::unique_ptr<ISchemeContext> createSchemeContext()
{
  return estd::make_unique<SchemeContext>();
}

} // ns eyestep
