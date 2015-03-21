// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "estd/memory.hpp"
#include "fo.hpp"
#include "nodelist.hpp"
#include "nodes.hpp"
#include "scm-context.hpp"
#include "sosofo.hpp"

#include "chibi/eval.h"
#include "chibi/sexp.h"

#include <boost/variant/apply_visitor.hpp>
#include <boost/variant/static_visitor.hpp>
#include <boost/optional/optional.hpp>

#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <cassert>


namespace eyestep {

namespace fs = boost::filesystem;

namespace {
  static const Node* sRootNode;

  const Node* rootNode() { return sRootNode; }


#define NODE_TAG "<node>"
#define NODE_TAG_SIZE 6

#define NODELIST_TAG "<node-list>"
#define NODELIST_TAG_SIZE 11

#define SOSOFO_TAG "<sosofo>"
#define SOSOFO_TAG_SIZE 8


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


  int sosofo_tag_p(sexp ctx)
  {
    int retv = 0;
    sexp_gc_var2(ty, nm);
    sexp_gc_preserve2(ctx, ty, nm);

    ty = sexp_env_ref(ctx, sexp_context_env(ctx),
                      nm = sexp_intern(ctx, SOSOFO_TAG, SOSOFO_TAG_SIZE),
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


  boost::optional<std::string> string_from_symbol_sexp_or_none(sexp ctx,
                                                               sexp obj)
  {
    sexp_gc_var1(str);
    sexp_gc_preserve1(ctx, str);

    boost::optional<std::string> result;
    if (sexp_isymbolp(obj)) {
      str = sexp_symbol_to_string(ctx, obj);
      result = sexp_string_data(str);
    }
    else if (sexp_lsymbolp(obj)) {
      result = std::string(sexp_lsymbol_data(obj), sexp_lsymbol_length(obj));
    }

    sexp_gc_release1(ctx);

    return result;
  }


  boost::optional<std::string> string_from_keyword_or_none(sexp ctx, sexp obj)
  {
    sexp_gc_var1(str);
    sexp_gc_preserve1(ctx, str);

    boost::optional<std::string> result;
    if (sexp_keywordp(obj)) {
      str = sexp_keyword_to_string(ctx, obj);
      result = sexp_string_data(str);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  fo::Unit map_name_to_unit(const std::string& nm)
  {
    if (nm == "pt") {
      return fo::k_pt;
    }
    else if (nm == "cm") {
      return fo::k_cm;
    }
    else if (nm == "em") {
      return fo::k_em;
    }
    else if (nm == "m") {
      return fo::k_m;
    }
    else if (nm == "mm") {
      return fo::k_mm;
    }

    return fo::k_pt;
  }

  boost::optional<fo::Dimen> dimen_from_sexp_or_none(sexp ctx, sexp obj)
  {
    boost::optional<fo::Dimen> result;

    if (sexp_pairp(obj)) {
      auto unitnm = string_from_symbol_sexp_or_none(ctx, sexp_cdr(obj));

      if (unitnm) {
        fo::Unit unit = map_name_to_unit(*unitnm);

        if (sexp_flonump(sexp_car(obj))) {
          result = fo::Dimen(sexp_flonum_value(sexp_car(obj)), unit);
        }
        else if (sexp_ratiop(sexp_car(obj))) {
          result = fo::Dimen(sexp_ratio_to_double(sexp_car(obj)), unit);
        }
        else if (sexp_fixnump(sexp_car(obj))) {
          result = fo::Dimen(int(sexp_unbox_fixnum(sexp_car(obj))), unit);
        }
      }
    }

    return result;
  }


  //------------------------------------------------------------------------------

  sexp make_node(sexp ctx, const Node* obj)
  {
    assert(obj);
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
    sexp_gc_var2(result, str);
    sexp_gc_preserve2(ctx, result, str);

    result = SEXP_VOID;

    if (const Node* node = node_from_arg(ctx, nodeArg)) {
      result =
          sexp_string_to_symbol(ctx,
                                str =
                                    sexp_c_string(ctx, node->gi().c_str(), -1));
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node/singleton node-list",
                                   nodeArg);
    }

    sexp_gc_release2(ctx);

    return result;
  }


  sexp func_parent(sexp ctx, sexp self, sexp n, sexp nodeArg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_NULL;

    if (const Node* node = node_from_arg(ctx, nodeArg)) {
      if (const Node* p = node->parent()) {
        result = make_node(ctx, p);
      }
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node/singleton node-list",
                                   nodeArg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_class(sexp ctx, sexp self, sexp n, sexp nodeArg)
  {
    sexp_gc_var2(result, str);
    sexp_gc_preserve2(ctx, result, str);

    result = SEXP_VOID;

    if (const Node* node = node_from_arg(ctx, nodeArg)) {
      result =
          sexp_string_to_symbol(ctx,
                                str = sexp_c_string(ctx,
                                                    node->className().c_str(),
                                                    -1));
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node/singleton node-list",
                                   nodeArg);
    }

    sexp_gc_release2(ctx);

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
      auto propName = string_from_symbol_sexp_or_none(ctx, propNameArg);
      if (!propName) {
        result = sexp_user_exception(ctx, self, "not a symbol", propNameArg);
      }
      else {
        SexpPropVisitor visitor(ctx, self, propNameArg);
        PropertyValue value = (*node)[*propName];
        result = boost::apply_visitor(visitor, value);
      }
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
    sexp_define_foreign(ctx, sexp_context_env(ctx), "parent", 1, &func_parent);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "class", 1, &func_class);

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


  sexp func_node_list_first(sexp ctx, sexp self, sexp_sint_t n, sexp nlArg)
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


  sexp func_node_list_ctor(sexp ctx, sexp self, sexp_sint_t n, sexp argsArg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (sexp_vectorp(argsArg)) {
      int len = sexp_vector_length(argsArg);

      std::vector<NodeList> nodelist;
      for (int i = 0; i < len; ++i) {
        sexp ref = sexp_vector_ref(argsArg, sexp_make_fixnum(i));

        if (sexp_check_tag(ref, node_tag_p(ctx))) {
          const Node* nd = (const Node*)(sexp_cpointer_value(ref));
          nodelist.emplace_back(NodeList(std::vector<const Node*>{nd}));
        }
        else if (sexp_check_tag(ref, nodelist_tag_p(ctx))) {
          const NodeList* nl = (const NodeList*)(sexp_cpointer_value(ref));
          nodelist.emplace_back(nl->clone());
        }
      }

      result = make_nodelist(ctx, new NodeList(nodelist));
    }
    else {
      result = sexp_user_exception(ctx, self, "not a vector", argsArg);
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


  sexp func_abs_first_sibling_p(sexp ctx, sexp self, sexp_sint_t n, sexp nlArg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (const Node* node = node_from_arg(ctx, nlArg)) {
      const Node* parent = node->parent();
      if (parent) {
        auto siblings = parent->property<Nodes>(CommonProps::kChildren);
        if (!siblings.empty()) {
          result = siblings.front() == node ? SEXP_TRUE : SEXP_FALSE;
        }
      }
    }
    else {
      result =
        sexp_user_exception(ctx, self, "not a node/singleton node-list", nlArg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_abs_last_sibling_p(sexp ctx, sexp self, sexp_sint_t n, sexp nlArg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (const Node* node = node_from_arg(ctx, nlArg)) {
      const Node* parent = node->parent();
      if (parent) {
        auto siblings = parent->property<Nodes>(CommonProps::kChildren);
        if (!siblings.empty()) {
          result = siblings.back() == node ? SEXP_TRUE : SEXP_FALSE;
        }
      }
    }
    else {
      result =
        sexp_user_exception(ctx, self, "not a node/singleton node-list", nlArg);
    }

    sexp_gc_release1(ctx);

    return result;
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
    sexp_define_foreign(ctx, sexp_context_env(ctx), "node-list-first", 1,
                        &func_node_list_first);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "node-list-rest", 1,
                        &func_node_list_rest);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "%node-list", 1,
                        &func_node_list_ctor);

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

    sexp_define_foreign(ctx, sexp_context_env(ctx), "absolute-first-sibling?", 1,
                        &func_abs_first_sibling_p);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "absolute-last-sibling?", 1,
                        &func_abs_last_sibling_p);

    sexp_gc_release3(ctx);
  }


  //----------------------------------------------------------------------------

  sexp make_sosofo(sexp ctx, const Sosofo* obj)
  {
    sexp_gc_var4(ty, tmp, result, nm);
    sexp_gc_preserve4(ctx, ty, tmp, result, nm);

    ty = sexp_env_ref(ctx, sexp_context_env(ctx),
                      nm = sexp_intern(ctx, SOSOFO_TAG, SOSOFO_TAG_SIZE),
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


  sexp free_sosofo(sexp ctx, sexp self, sexp_sint_t n, sexp sosofoArg)
  {
    const Sosofo* sosofo = (const Sosofo*)(sexp_cpointer_value(sosofoArg));
    delete sosofo;

    sexp_cpointer_value(sosofoArg) = nullptr;
    return SEXP_VOID;
  }


  sexp func_empty_sosofo(sexp ctx, sexp self, sexp_sint_t n)
  {
    return make_sosofo(ctx, new Sosofo());
  }


  sexp func_sosofo_append(sexp ctx, sexp self, sexp_sint_t n, sexp sosofoArg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_VOID;

    if (sexp_vectorp(sosofoArg)) {
      int len = sexp_vector_length(sosofoArg);

      std::vector<Sosofo> sosofos;

      for (int i = 0; i < len; ++i) {
        sexp ref = sexp_vector_ref(sosofoArg, sexp_make_fixnum(i));

        if (sexp_check_tag(ref, sosofo_tag_p(ctx))) {
          const Sosofo* sosofo = (const Sosofo*)(sexp_cpointer_value(ref));

          sosofos.emplace_back(*sosofo);
        }
      }

      result = make_sosofo(ctx, new Sosofo(sosofos));
    }
    else {
      result = sexp_user_exception(ctx, self, "not a sosofo", sosofoArg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  void init_sosofo_functions(sexp ctx)
  {
    sexp_gc_var3(nm, ty, op);
    sexp_gc_preserve3(ctx, nm, ty, op);

    // register qobject type
    ty = sexp_register_c_type(ctx, nm = sexp_c_string(ctx, "sosofo", -1),
                              &free_sosofo);
    sexp_env_cell_define(ctx, sexp_context_env(ctx),
                         nm = sexp_intern(ctx, SOSOFO_TAG, SOSOFO_TAG_SIZE), ty,
                         NULL);

    op = sexp_make_type_predicate(ctx, nm = sexp_c_string(ctx, "sosofo?", -1),
                                  ty);
    sexp_env_define(ctx, sexp_context_env(ctx),
                    nm = sexp_intern(ctx, "sosofo?", -1), op);

    sexp_define_foreign(ctx, sexp_context_env(ctx), "empty-sosofo", 0,
                        &func_empty_sosofo);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "%sosofo-append", 1,
                        &func_sosofo_append);

    sexp_gc_release3(ctx);
  }


  //----------------------------------------------------------------------------

  boost::optional<fo::PropertySpec>
  evaluate_keyword_parameter(sexp ctx, sexp self, const std::string& key,
                             sexp expr)
  {
    boost::optional<fo::PropertySpec> result;

    sexp_gc_var2(obj, excep);
    sexp_gc_preserve2(ctx, obj, excep);

    obj = sexp_eval(ctx, expr, sexp_context_env(ctx));
    if (obj && sexp_exceptionp(obj)) {
      excep = sexp_user_exception(ctx, self, "bad property expression", expr);
      check_exception_p(ctx, excep);
    }
    else {
      auto sym_value = string_from_symbol_sexp_or_none(ctx, obj);
      if (sym_value) {
        result = fo::PropertySpec(key, *sym_value);
      }
      else if (sexp_check_tag(obj, sosofo_tag_p(ctx))) {
        const Sosofo* sosofo = (const Sosofo*)(sexp_cpointer_value(obj));
        result = fo::PropertySpec(key, std::make_shared<Sosofo>(*sosofo));
      }
      else if (sexp_booleanp(obj)) {
        result = fo::PropertySpec(key, bool(sexp_unbox_boolean(obj)));
      }
      else if (sexp_fixnump(obj)) {
        result = fo::PropertySpec(key, int(sexp_unbox_fixnum(obj)));
      }
      else if (sexp_pairp(obj)) {
        auto dimen = dimen_from_sexp_or_none(ctx, obj);
        if (dimen) {
          result = fo::PropertySpec(key, *dimen);
        }
      }
      else if (sexp_stringp(obj)) {
        result = fo::PropertySpec(key, std::string(sexp_string_data(obj)));
      }
      else {
        excep = sexp_user_exception(ctx, self, "Bad property type: ", expr);
        check_exception_p(ctx, excep);
      }
    }

    sexp_gc_release2(ctx);

    return result;
  }


  std::unique_ptr<IFormattingObject> allocate_fo(sexp ctx,
                                                 const std::string& foClass,
                                                 const fo::PropertySpecs& props,
                                                 sexp principalPort)
  {
    if (sexp_check_tag(principalPort, sosofo_tag_p(ctx))) {
      const Sosofo* sosofo =
          (const Sosofo*)(sexp_cpointer_value(principalPort));

      return std::move(
          fo::createFoByClassName(std::string("#") + foClass, props, *sosofo));
    }

    Sosofo sosofo;
    return std::move(
        fo::createFoByClassName(std::string("#") + foClass, props, sosofo));
  }


  sexp func_make_fo(sexp ctx, sexp self, sexp_sint_t n, sexp foClassArg,
                    sexp argsArg)
  {
    sexp_gc_var4(result, str, obj, err);
    sexp_gc_preserve4(ctx, result, str, obj, err);

    result = SEXP_NULL;

    auto foClass = string_from_symbol_sexp_or_none(ctx, foClassArg);
    if (!foClass) {
      result = sexp_user_exception(ctx, self, "not a symbol", foClassArg);
    }

    fo::PropertySpecs props;

    if (sexp_vectorp(argsArg)) {
      int len = sexp_vector_length(argsArg);

      for (int i = 0; i < len; ++i) {
        sexp ref = sexp_vector_ref(argsArg, sexp_make_fixnum(i));

        auto key = string_from_keyword_or_none(ctx, ref);

        // is the parameter a keyword
        if (key) {
          if (i + 1 < len) {
            ref = sexp_vector_ref(argsArg, sexp_make_fixnum(i + 1));
            auto prop =
                evaluate_keyword_parameter(ctx, self,
                                           key->substr(0, key->size() - 1),
                                           ref);
            if (prop) {
              props.push_back(*prop);
            }

            i = i + 1;
          }
          else {
            result = sexp_user_exception(ctx, self, "value missing for keyword",
                                         ref);
            break;
          }
        }
        else {
          obj = sexp_eval(ctx, ref, sexp_context_env(ctx));
          if (obj && sexp_exceptionp(obj)) {
            result = obj;
            break;
          }
          else {
            result = obj;
          }
        }
      }
    }
    else {
      result = sexp_user_exception(ctx, self, "not a vector", argsArg);
    }

    if (foClass) {
      std::shared_ptr<IFormattingObject> fo(
          allocate_fo(ctx, *foClass, props, result));

      if (!fo) {
        result =
            sexp_user_exception(ctx, self, "Unknown fo-class: ", foClassArg);
      }
      else {
        result = make_sosofo(ctx, new Sosofo(fo));
      }
    }

    sexp_gc_release4(ctx);

    return result;
  }


  void init_make_functions(sexp ctx)
  {
    sexp_gc_var3(nm, ty, op);
    sexp_gc_preserve3(ctx, nm, ty, op);

    sexp_define_foreign(ctx, sexp_context_env(ctx), "%make-fo", 2,
                        &func_make_fo);

    sexp_gc_release3(ctx);
  }


  //----------------------------------------------------------------------------

  void init_builtins(sexp ctx)
  {
    init_node_functions(ctx);
    init_nodelist_functions(ctx);
    init_sosofo_functions(ctx);
    init_make_functions(ctx);
  }


  //----------------------------------------------------------------------------

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


    std::unique_ptr<Sosofo> processRootNode(const Node* rootNode) override
    {
      std::unique_ptr<Sosofo> result;

      sexp_gc_var1(res);
      sexp_gc_preserve1(mCtx, res);

      sRootNode = rootNode;
      res =
          sexp_eval_string(mCtx, "(process-node-list (children (grove-root)))",
                           -1, NULL);
      // res = sexp_eval_string(mCtx, "(foo (grove-root))", -1, NULL);

      check_exception_p(mCtx, res);

      if (sexp_check_tag(res, sosofo_tag_p(mCtx))) {
        const Sosofo* sosofo = (const Sosofo*)(sexp_cpointer_value(res));

        if (sosofo) {
          result.reset(new Sosofo(*sosofo));
        }
      }

      sexp_gc_release1(mCtx);

      return std::move(result);
    }
  };

} // ns anon


std::unique_ptr<ISchemeContext> createSchemeContext()
{
  return estd::make_unique<SchemeContext>();
}

} // ns eyestep
