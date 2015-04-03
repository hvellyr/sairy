// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "estd/memory.hpp"
#include "fo.hpp"
#include "nodeclass.hpp"
#include "nodelist.hpp"
#include "nodes.hpp"
#include "scm-context.hpp"
#include "sosofo.hpp"

#include "chibi/eval.h"
#include "chibi/sexp.h"

#include <boost/optional/optional.hpp>
#include <boost/range/algorithm/find_if.hpp>
#include <boost/variant/apply_visitor.hpp>
#include <boost/variant/static_visitor.hpp>

#include <iostream>
#include <memory>
#include <string>
#include <vector>
#include <cassert>
#include <stdarg.h>


namespace eyestep {

namespace fs = boost::filesystem;

namespace {
  static const Node* s_root_node;

  const Node* root_node() { return s_root_node; }


#define NODELIST_TAG "<node-list>"
#define NODELIST_TAG_SIZE 11

#define SOSOFO_TAG "<sosofo>"
#define SOSOFO_TAG_SIZE 8

#define DIMEN_TAG "<dimen>"
#define DIMEN_TAG_SIZE 7


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


  int dimen_tag_p(sexp ctx)
  {
    int retv = 0;
    sexp_gc_var2(ty, nm);
    sexp_gc_preserve2(ctx, ty, nm);

    ty =
      sexp_env_ref(ctx, sexp_context_env(ctx),
                   nm = sexp_intern(ctx, DIMEN_TAG, DIMEN_TAG_SIZE), SEXP_VOID);
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


  const Node* singleton_node_from_list(sexp ctx, sexp arg)
  {
    if (sexp_check_tag(arg, nodelist_tag_p(ctx))) {
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
    else if (nm == "px") {
      return fo::k_px;
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


  sexp make_nodelist(sexp ctx, const NodeList* obj);


  //------------------------------------------------------------------------------

  sexp make_dimen(sexp ctx, fo::Dimen dim)
  {
    sexp_gc_var4(ty, tmp, result, nm);
    sexp_gc_preserve4(ctx, ty, tmp, result, nm);

    ty =
      sexp_env_ref(ctx, sexp_context_env(ctx),
                   nm = sexp_intern(ctx, DIMEN_TAG, DIMEN_TAG_SIZE), SEXP_VOID);

    if (sexp_typep(ty)) {
      result = sexp_alloc_type(ctx, cpointer, sexp_type_tag(ty));
      sexp_cpointer_freep(result) = 0;
      sexp_cpointer_length(result) = 0;
      sexp_cpointer_value(result) = (void*)new fo::Dimen(dim);
    }
    else {
      result = SEXP_VOID;
    }

    sexp_gc_release4(ctx);

    return result;
  }


  sexp free_dimen(sexp ctx, sexp self, sexp_sint_t n, sexp dim_arg)
  {
    const fo::Dimen* dimen = (const fo::Dimen*)(sexp_cpointer_value(dim_arg));
    delete dimen;

    sexp_cpointer_value(dim_arg) = nullptr;
    return SEXP_VOID;
  }


  sexp func_make_dimen(sexp ctx, sexp self, sexp n, sexp val_arg, sexp unit_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    auto unitnm = string_from_symbol_sexp_or_none(ctx, unit_arg);

    result = SEXP_VOID;

    if (unitnm) {
      fo::Unit unit = map_name_to_unit(*unitnm);

      if (sexp_flonump(val_arg)) {
        result = make_dimen(ctx, fo::Dimen(sexp_flonum_value(val_arg), unit));
      }
      else if (sexp_ratiop(val_arg)) {
        result =
          make_dimen(ctx, fo::Dimen(sexp_ratio_to_double(val_arg), unit));
      }
      else if (sexp_fixnump(val_arg)) {
        result =
          make_dimen(ctx, fo::Dimen(int(sexp_unbox_fixnum(val_arg)), unit));
      }
      else {
        result = sexp_user_exception(ctx, self, "invalid number", val_arg);
      }
    }
    else {
      result = sexp_user_exception(ctx, self, "invalid unit", unit_arg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  void init_dimen_functions(sexp ctx)
  {
    sexp_gc_var3(nm, ty, op);
    sexp_gc_preserve3(ctx, nm, ty, op);

    // register qobject type
    ty = sexp_register_c_type(ctx, nm = sexp_c_string(ctx, "dimen", -1),
                              &free_dimen);

    sexp_env_cell_define(ctx, sexp_context_env(ctx),
                         nm = sexp_intern(ctx, DIMEN_TAG, DIMEN_TAG_SIZE), ty,
                         NULL);
    op =
      sexp_make_type_predicate(ctx, nm = sexp_c_string(ctx, "dimen?", -1), ty);
    sexp_env_define(ctx, sexp_context_env(ctx),
                    nm = sexp_intern(ctx, "dimen?", -1), op);

    // register functions
    sexp_define_foreign(ctx, sexp_context_env(ctx), "%make-dimen", 2,
                        &func_make_dimen);

    sexp_gc_release3(ctx);
  }


  //------------------------------------------------------------------------------

  sexp func_gi(sexp ctx, sexp self, sexp n, sexp node_arg)
  {
    sexp_gc_var2(result, str);
    sexp_gc_preserve2(ctx, result, str);

    result = SEXP_VOID;

    if (const Node* node = singleton_node_from_list(ctx, node_arg)) {
      result =
        sexp_string_to_symbol(ctx,
                              str = sexp_c_string(ctx, node->gi().c_str(), -1));
    }
    else {
      result =
        sexp_user_exception(ctx, self, "not a singleton node-list", node_arg);
    }

    sexp_gc_release2(ctx);

    return result;
  }


  sexp func_parent(sexp ctx, sexp self, sexp n, sexp node_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_NULL;

    if (const Node* node = singleton_node_from_list(ctx, node_arg)) {
      if (const Node* p = node->parent()) {
        result = make_nodelist(ctx, new NodeList(ConstNodes{p}));
      }
    }
    else {
      result =
        sexp_user_exception(ctx, self, "not a singleton node-list", node_arg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_class(sexp ctx, sexp self, sexp n, sexp node_arg)
  {
    sexp_gc_var2(result, str);
    sexp_gc_preserve2(ctx, result, str);

    result = SEXP_VOID;

    if (const Node* node = singleton_node_from_list(ctx, node_arg)) {
      result =
        sexp_string_to_symbol(ctx,
                              str =
                                sexp_c_string(ctx, node->classname().c_str(),
                                              -1));
    }
    else {
      result =
        sexp_user_exception(ctx, self, "not a singleton node-list", node_arg);
    }

    sexp_gc_release2(ctx);

    return result;
  }


  sexp func_node_property(sexp ctx, sexp self, sexp_sint_t n, sexp propname_arg,
                          sexp node_arg)
  {
    class SexpPropVisitor : public boost::static_visitor<sexp> {
      sexp _ctx;
      sexp _self;
      sexp _propname;
      sexp _default_value;

    public:
      SexpPropVisitor(sexp ctx, sexp self, sexp propname, sexp default_value)
        : _ctx(ctx), _self(self), _propname(propname),
          _default_value(default_value)
      {
      }

      sexp operator()(const Undefined&)
      {
        if (_default_value != SEXP_VOID) {
          return _default_value;
        }
        else {
          return sexp_user_exception(_ctx, _self, "Undefined property",
                                     _propname);
        }
      }

      sexp operator()(const int& val) { return sexp_make_integer(_ctx, val); }

      sexp operator()(const std::string& val)
      {
        return sexp_c_string(_ctx, val.c_str(), -1);
      }

      sexp operator()(Node* nd)
      {
        return make_nodelist(_ctx, new NodeList(Nodes{nd}));
      }

      sexp operator()(const Nodes& nl)
      {
        return make_nodelist(_ctx, new NodeList(nl));
      }
    };

    sexp_gc_var2(result, default_value);
    sexp_gc_preserve2(ctx, result, default_value);

    result = SEXP_VOID;
    default_value = SEXP_VOID;

    sexp* stack = sexp_stack_data(sexp_context_stack(ctx));
    sexp_sint_t top = sexp_context_top(ctx);
    sexp_sint_t stack_bot = sexp_context_last_fp(ctx) + 3;

    for (int i = top - 1 - n; i > stack_bot; --i) {
      sexp ref = stack[i];
      auto key = string_from_keyword_or_none(ctx, ref);

      if (key) {
        --i;

        if (i > stack_bot) {
          if (*key == "default:") {
            default_value = stack[i];
          }
          else {
            result = sexp_user_exception(ctx, self, "unexpected value", ref);
            break;
          }
        }
      }
    }

    if (result == SEXP_VOID) {
      if (const Node* node = singleton_node_from_list(ctx, node_arg)) {
        auto propname = string_from_symbol_sexp_or_none(ctx, propname_arg);
        if (!propname) {
          result = sexp_user_exception(ctx, self, "not a symbol", propname_arg);
        }
        else {
          SexpPropVisitor visitor(ctx, self, propname_arg, default_value);
          PropertyValue value = (*node)[*propname];
          result = boost::apply_visitor(visitor, value);
        }
      }
      else {
        result =
          sexp_user_exception(ctx, self, "not a singleton node-list", node_arg);
      }
    }

    sexp_gc_release2(ctx);

    return result;
  }


  sexp func_grove_root(sexp ctx, sexp self, sexp_sint_t n)
  {
    return make_nodelist(ctx, new NodeList(ConstNodes{root_node()}));
  }


  sexp func_named_node(sexp ctx, sexp self, sexp_sint_t n, sexp string_arg,
                       sexp nnl_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_VOID;

    std::string key;
    if (sexp_stringp(string_arg)) {
      key = std::string(sexp_string_data(string_arg));
    }
    else {
      result = sexp_user_exception(ctx, self, "not a string", string_arg);
    }

    if (result == SEXP_VOID) {
      if (sexp_check_tag(nnl_arg, nodelist_tag_p(ctx))) {
        const NodeList* nl = (const NodeList*)(sexp_cpointer_value(nnl_arg));

        ConstNodes nodes;

        NodeList p(nl->clone());
        while (!p.empty()) {
          auto* nd = p.head();
          if (nd->has_property(CommonProps::k_attr_name)) {
            if (nd->property<std::string>(CommonProps::k_attr_name) == key) {
              nodes.push_back(nd);
            }
          }
          p = p.rest();
        }

        if (nodes.empty()) {
          result = make_nodelist(ctx, new NodeList);
        }
        else {
          result = make_nodelist(ctx, new NodeList(nodes));
        }
      }
      else {
        result = sexp_user_exception(ctx, self, "not a node-list", nnl_arg);
      }
    }

    sexp_gc_release1(ctx);

    return result;
  }


  void init_node_functions(sexp ctx)
  {
    sexp_define_foreign(ctx, sexp_context_env(ctx), "gi", 1, &func_gi);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "parent", 1, &func_parent);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "class", 1, &func_class);

    sexp_define_foreign_aux(ctx, sexp_context_env(ctx), "node-property", 2,
                            SEXP_PROC_VARIADIC, (sexp_proc1)&func_node_property,
                            SEXP_TRUE);

    sexp_define_foreign(ctx, sexp_context_env(ctx), "grove-root", 0,
                        &func_grove_root);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "named-node", 2,
                        &func_named_node);
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


  sexp free_nodelist(sexp ctx, sexp self, sexp_sint_t n, sexp nl_arg)
  {
    const NodeList* nl = (const NodeList*)(sexp_cpointer_value(nl_arg));
    delete nl;

    sexp_cpointer_value(nl_arg) = nullptr;
    return SEXP_VOID;
  }


  sexp func_empty_node_list(sexp ctx, sexp self, sexp_sint_t n)
  {
    return make_nodelist(ctx, new NodeList());
  }


  sexp func_node_list_empty_p(sexp ctx, sexp self, sexp_sint_t n, sexp nl_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_VOID;

    if (sexp_check_tag(nl_arg, nodelist_tag_p(ctx))) {
      const NodeList* nl = (const NodeList*)(sexp_cpointer_value(nl_arg));
      result = sexp_make_boolean(nl->empty());
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node-list", nl_arg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_node_list_length(sexp ctx, sexp self, sexp_sint_t n, sexp nl_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (sexp_check_tag(nl_arg, nodelist_tag_p(ctx))) {
      const NodeList* nl = (const NodeList*)(sexp_cpointer_value(nl_arg));
      result = sexp_make_fixnum(nl->length());
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node-list", nl_arg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_node_list_first(sexp ctx, sexp self, sexp_sint_t n, sexp nl_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (sexp_check_tag(nl_arg, nodelist_tag_p(ctx))) {
      const NodeList* nl = (const NodeList*)(sexp_cpointer_value(nl_arg));
      const Node* node = nl->head();
      result = make_nodelist(ctx, node ? new NodeList({node}) : new NodeList());
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node-list", nl_arg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_node_list_rest(sexp ctx, sexp self, sexp_sint_t n, sexp nl_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (sexp_check_tag(nl_arg, nodelist_tag_p(ctx))) {
      const NodeList* nl = (const NodeList*)(sexp_cpointer_value(nl_arg));
      result = make_nodelist(ctx, new NodeList(std::move(nl->rest())));
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node-list", nl_arg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_node_list_ctor(sexp ctx, sexp self, sexp_sint_t n, sexp args_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_VOID;

    if (sexp_pairp(args_arg)) {
      std::vector<NodeList> nodelist;

      for (sexp ls = args_arg; sexp_pairp(ls); ls = sexp_cdr(ls)) {
        sexp ref = sexp_car(ls);

        if (sexp_check_tag(ref, nodelist_tag_p(ctx))) {
          const NodeList* nl = (const NodeList*)(sexp_cpointer_value(ref));
          nodelist.emplace_back(nl->clone());
        }
      }

      result = make_nodelist(ctx, new NodeList(nodelist));
    }
    else {
      result = sexp_user_exception(ctx, self, "not a list", args_arg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp make_nodelist_on_axis(sexp ctx, sexp self, sexp_sint_t n, sexp nl_arg,
                             NodeList::Kind kind)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (const Node* node = singleton_node_from_list(ctx, nl_arg)) {
      result = make_nodelist(ctx, new NodeList(node, kind));
    }
    else {
      result =
        sexp_user_exception(ctx, self, "not a singleton node-list", nl_arg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_children(sexp ctx, sexp self, sexp_sint_t n, sexp nl_arg)
  {
    return make_nodelist_on_axis(ctx, self, n, nl_arg, NodeList::k_children);
  }


  sexp func_ancestors(sexp ctx, sexp self, sexp_sint_t n, sexp nl_arg)
  {
    return make_nodelist_on_axis(ctx, self, n, nl_arg, NodeList::k_ancestors);
  }


  sexp func_descendants(sexp ctx, sexp self, sexp_sint_t n, sexp nl_arg)
  {
    return make_nodelist_on_axis(ctx, self, n, nl_arg, NodeList::k_descendants);
  }


  sexp func_siblings(sexp ctx, sexp self, sexp_sint_t n, sexp nl_arg)
  {
    return make_nodelist_on_axis(ctx, self, n, nl_arg, NodeList::k_siblings);
  }


  sexp func_follow(sexp ctx, sexp self, sexp_sint_t n, sexp nl_arg)
  {
    return make_nodelist_on_axis(ctx, self, n, nl_arg, NodeList::k_follow);
  }


  sexp func_preced(sexp ctx, sexp self, sexp_sint_t n, sexp nl_arg)
  {
    return make_nodelist_on_axis(ctx, self, n, nl_arg, NodeList::k_preced);
  }


  sexp func_abs_first_sibling_p(sexp ctx, sexp self, sexp_sint_t n, sexp nl_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_FALSE;

    if (const Node* node = singleton_node_from_list(ctx, nl_arg)) {
      const Node* parent = node->parent();
      if (parent) {
        auto siblings = parent->property<Nodes>(CommonProps::k_children);
        if (!siblings.empty()) {
          result = siblings[0] == node ? SEXP_TRUE : SEXP_FALSE;
        }
      }
    }
    else {
      result =
        sexp_user_exception(ctx, self, "not a singleton node-list", nl_arg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_abs_first_element_sibling_p(sexp ctx, sexp self, sexp_sint_t n,
                                        sexp nl_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_FALSE;

    if (const Node* node = singleton_node_from_list(ctx, nl_arg)) {
      const Node* parent = node->parent();
      if (parent) {
        auto siblings = parent->property<Nodes>(CommonProps::k_children);
        auto i_find = boost::find_if(siblings, [&node](const Node* lnd) {
          return lnd->node_class() == element_class_definition();
        });
        if (i_find != siblings.end()) {
          result = *i_find == node ? SEXP_TRUE : SEXP_FALSE;
        }
      }
    }
    else {
      result =
        sexp_user_exception(ctx, self, "not a singleton node-list", nl_arg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_abs_last_sibling_p(sexp ctx, sexp self, sexp_sint_t n, sexp nl_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_FALSE;

    if (const Node* node = singleton_node_from_list(ctx, nl_arg)) {
      const Node* parent = node->parent();
      if (parent) {
        auto siblings = parent->property<Nodes>(CommonProps::k_children);
        std::reverse(siblings.begin(), siblings.end());

        if (!siblings.empty()) {
          result = siblings[0] == node ? SEXP_TRUE : SEXP_FALSE;
        }
      }
    }
    else {
      result =
        sexp_user_exception(ctx, self, "not a singleton node-list", nl_arg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_abs_last_element_sibling_p(sexp ctx, sexp self, sexp_sint_t n,
                                       sexp nl_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_FALSE;

    if (const Node* node = singleton_node_from_list(ctx, nl_arg)) {
      const Node* parent = node->parent();
      if (parent) {
        auto siblings = parent->property<Nodes>(CommonProps::k_children);
        std::reverse(siblings.begin(), siblings.end());

        auto i_find = boost::find_if(siblings, [&node](Node* lnd) {
          return lnd && lnd->node_class() == element_class_definition();
        });
        if (i_find != siblings.end()) {
          result = *i_find == node ? SEXP_TRUE : SEXP_FALSE;
        }
      }
    }
    else {
      result =
        sexp_user_exception(ctx, self, "not a singleton node-list", nl_arg);
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

    sexp_define_foreign(ctx, sexp_context_env(ctx), "absolute-first-sibling?",
                        1, &func_abs_first_sibling_p);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "absolute-last-sibling?", 1,
                        &func_abs_last_sibling_p);

    sexp_define_foreign(ctx, sexp_context_env(ctx),
                        "absolute-first-element-sibling?", 1,
                        &func_abs_first_element_sibling_p);
    sexp_define_foreign(ctx, sexp_context_env(ctx),
                        "absolute-last-element-sibling?", 1,
                        &func_abs_last_element_sibling_p);

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


  sexp free_sosofo(sexp ctx, sexp self, sexp_sint_t n, sexp sosofo_arg)
  {
    const Sosofo* sosofo = (const Sosofo*)(sexp_cpointer_value(sosofo_arg));
    delete sosofo;

    sexp_cpointer_value(sosofo_arg) = nullptr;
    return SEXP_VOID;
  }


  sexp func_empty_sosofo(sexp ctx, sexp self, sexp_sint_t n)
  {
    return make_sosofo(ctx, new Sosofo());
  }


  sexp func_sosofo_append(sexp ctx, sexp self, sexp_sint_t n, sexp sosofo_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_VOID;

    if (sexp_pairp(sosofo_arg)) {
      std::vector<Sosofo> sosofos;

      for (sexp ls = sosofo_arg; sexp_pairp(ls); ls = sexp_cdr(ls)) {
        sexp ref = sexp_car(ls);
        if (sexp_check_tag(ref, sosofo_tag_p(ctx))) {
          const Sosofo* sosofo = (const Sosofo*)(sexp_cpointer_value(ref));

          sosofos.emplace_back(*sosofo);
        }
      }

      result = make_sosofo(ctx, new Sosofo(sosofos));
    }
    else {
      result = sexp_user_exception(ctx, self, "not a sosofo", sosofo_arg);
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

    op =
      sexp_make_type_predicate(ctx, nm = sexp_c_string(ctx, "sosofo?", -1), ty);
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

    sexp_gc_var1(excep);
    sexp_gc_preserve1(ctx, excep);

    auto sym_value = string_from_symbol_sexp_or_none(ctx, expr);
    if (sym_value) {
      result = fo::PropertySpec(key, *sym_value);
    }
    else if (sexp_check_tag(expr, sosofo_tag_p(ctx))) {
      const Sosofo* sosofo = (const Sosofo*)(sexp_cpointer_value(expr));
      result = fo::PropertySpec(key, std::make_shared<Sosofo>(*sosofo));
    }
    else if (sexp_booleanp(expr)) {
      result = fo::PropertySpec(key, bool(sexp_unbox_boolean(expr)));
    }
    else if (sexp_fixnump(expr)) {
      result = fo::PropertySpec(key, int(sexp_unbox_fixnum(expr)));
    }
    else if (sexp_check_tag(expr, dimen_tag_p(ctx))) {
      const fo::Dimen* dimen = (const fo::Dimen*)(sexp_cpointer_value(expr));
      result = fo::PropertySpec(key, *dimen);
    }
    else if (sexp_stringp(expr)) {
      result = fo::PropertySpec(key, std::string(sexp_string_data(expr)));
    }
    else {
      excep = sexp_user_exception(ctx, self, "Bad property type: ", expr);
      check_exception_p(ctx, excep);
    }

    sexp_gc_release2(ctx);

    return result;
  }


  std::unique_ptr<IFormattingObject> allocate_fo(sexp ctx,
                                                 const std::string& fo_class,
                                                 const fo::PropertySpecs& props,
                                                 sexp principal_port)
  {
    if (sexp_check_tag(principal_port, sosofo_tag_p(ctx))) {
      const Sosofo* sosofo =
        (const Sosofo*)(sexp_cpointer_value(principal_port));

      return std::move(fo::create_fo_by_classname(std::string("#") + fo_class,
                                                  props, *sosofo));
    }

    Sosofo sosofo;
    return std::move(
      fo::create_fo_by_classname(std::string("#") + fo_class, props, sosofo));
  }


  sexp func_make_fo(sexp ctx, sexp self, sexp_sint_t n, sexp fo_class_arg,
                    sexp args_arg)
  {
    sexp_gc_var2(result, obj);
    sexp_gc_preserve2(ctx, result, obj);

    result = SEXP_NULL;
    obj = SEXP_NULL;

    auto fo_class = string_from_symbol_sexp_or_none(ctx, fo_class_arg);
    if (!fo_class) {
      result = sexp_user_exception(ctx, self, "not a symbol", fo_class_arg);
    }

    fo::PropertySpecs props;
    if (sexp_pairp(args_arg)) {
      sexp ls = args_arg;

      for (; sexp_pairp(ls); ls = sexp_cdr(ls)) {
        sexp ref = sexp_car(ls);
        auto key = string_from_keyword_or_none(ctx, ref);

        if (key) {
          if (sexp_pairp(sexp_cdr(ls))) {
            ref = sexp_car(sexp_cdr(ls));
            auto prop =
              evaluate_keyword_parameter(ctx, self,
                                         key->substr(0, key->size() - 1), ref);
            if (prop) {
              props.push_back(*prop);
            }
          }
          else {
            result =
              sexp_user_exception(ctx, self, "value missing for keyword", ref);
            break;
          }
        }
        else {
          break;
        }

        ls = sexp_cdr(ls);
      }

      for (; sexp_pairp(ls); ls = sexp_cdr(ls)) {
        sexp ref = sexp_car(ls);
        auto key = string_from_keyword_or_none(ctx, ref);

        if (key) {
          result = sexp_user_exception(ctx, self,
                                       "unexpeced keyword in make body", ref);
          break;
        }
        obj = ref;
      }
    }
    else {
      result = sexp_user_exception(ctx, self, "not a list", args_arg);
    }

    if (result == SEXP_NULL && fo_class) {
      std::shared_ptr<IFormattingObject> fo(
        allocate_fo(ctx, *fo_class, props, obj));
      if (!fo) {
        result =
          sexp_user_exception(ctx, self, "Unknown fo-class: ", fo_class_arg);
      }
      else {
        result = make_sosofo(ctx, new Sosofo(fo));
      }
    }

    sexp_gc_release2(ctx);

    return result;
  }


  void init_make_functions(sexp ctx)
  {
    sexp_define_foreign(ctx, sexp_context_env(ctx), "%make-fo", 2,
                        &func_make_fo);
  }


  //----------------------------------------------------------------------------

  sexp func_use(sexp ctx, sexp self, sexp_sint_t n, sexp res_arg)
  {
    sexp_gc_var3(result, source, nm);
    sexp_gc_preserve3(ctx, result, source, nm);

    result = SEXP_VOID;

    auto resource = string_from_symbol_sexp_or_none(ctx, res_arg);
    if (resource) {
      sexp path = sexp_env_ref(ctx, sexp_context_env(ctx),
                               nm = sexp_intern(ctx, "%style-parent-path%", -1),
                               SEXP_VOID);
      if (sexp_stringp(path)) {
        auto parent_path = std::string(sexp_string_data(path));
        auto src_path = (fs::path(parent_path) / *resource)
                          .replace_extension(".tstyle")
                          .string();
        source = sexp_c_string(ctx, src_path.c_str(), src_path.size());
        result = sexp_load(ctx, source, sexp_context_env(ctx));
      }
      else {
        result =
          sexp_user_exception(ctx, self, "%style-parent-path% not set?", path);
      }
    }
    else {
      result = sexp_user_exception(ctx, self, "not a symbol", res_arg);
    }

    sexp_gc_release3(ctx);

    return result;
  }

  void init_registry_functions(sexp ctx)
  {
    sexp_define_foreign(ctx, sexp_context_env(ctx), "use", 1, &func_use);
  }


  //----------------------------------------------------------------------------

  void init_builtins(sexp ctx)
  {
    init_registry_functions(ctx);
    init_dimen_functions(ctx);
    init_nodelist_functions(ctx);
    init_node_functions(ctx);
    init_sosofo_functions(ctx);
    init_make_functions(ctx);
  }


  //----------------------------------------------------------------------------

  class SchemeContext : public ISchemeContext {
    sexp _ctx;

  public:
    SchemeContext() : _ctx(nullptr) {}


    ~SchemeContext()
    {
      if (_ctx != nullptr) {
        sexp_destroy_context(_ctx);
      }
    }


    void initialize(const std::vector<fs::path>& module_paths) override
    {
      _ctx = sexp_make_eval_context(NULL, NULL, NULL, 0, 0);

      sexp_gc_var1(tmp);
      sexp_gc_preserve1(_ctx, tmp);

      init_builtins(_ctx);

      for (const auto& path : module_paths) {
        std::string libpath = path.string();

        sexp_add_module_directory(_ctx,
                                  tmp =
                                    sexp_c_string(_ctx, libpath.c_str(), -1),
                                  SEXP_FALSE);
      }

      sexp_load_standard_env(_ctx, NULL, SEXP_SEVEN);
      sexp_load_standard_ports(_ctx, NULL, stdin, stdout, stderr, 1);

      sexp_gc_release1(_ctx);
    }


    bool load_module_file(const fs::path& script_file) override
    {
      sexp_gc_var1(res);
      sexp_gc_preserve1(_ctx, res);

      res = sexp_load_module_file(_ctx, script_file.string().c_str(), nullptr);
      bool retv = check_exception_p(_ctx, res);

      sexp_gc_release1(_ctx);

      return retv;
    }


    bool load_script(const fs::path& script_file) override
    {
      define_variable("%style-parent-path%",
                      script_file.parent_path().string());

      sexp_gc_var2(obj1, res);
      sexp_gc_preserve2(_ctx, obj1, res);

      obj1 = sexp_c_string(_ctx, script_file.string().c_str(), -1);
      bool retv = check_exception_p(_ctx, res = sexp_load(_ctx, obj1, NULL));

      sexp_gc_release2(_ctx);

      return retv;
    }


    void define_variable(const std::string& name,
                         const std::string& value) override
    {
      sexp_gc_var2(sym, val);
      sexp_gc_preserve2(_ctx, sym, val);

      sexp_env_define(_ctx, sexp_context_env(_ctx),
                      sym = sexp_intern(_ctx, name.c_str(), name.size()),
                      val = sexp_c_string(_ctx, value.c_str(), value.size()));

      sexp_gc_release2(_ctx);
    }


    std::unique_ptr<Sosofo> process_root_node(const Node* root_node) override
    {
      std::unique_ptr<Sosofo> result;

      sexp_gc_var1(res);
      sexp_gc_preserve1(_ctx, res);

      s_root_node = root_node;
      res =
        sexp_eval_string(_ctx, "(process-node-list (children (grove-root)))",
                         -1, NULL);
      // res = sexp_eval_string(_ctx, "(foo (grove-root))", -1, NULL);

      check_exception_p(_ctx, res);

      if (sexp_check_tag(res, sosofo_tag_p(_ctx))) {
        const Sosofo* sosofo = (const Sosofo*)(sexp_cpointer_value(res));

        if (sosofo) {
          result.reset(new Sosofo(*sosofo));
        }
      }

      sexp_gc_release1(_ctx);

      return std::move(result);
    }
  };

} // ns anon


std::unique_ptr<ISchemeContext> create_scheme_context()
{
  return estd::make_unique<SchemeContext>();
}

} // ns eyestep
