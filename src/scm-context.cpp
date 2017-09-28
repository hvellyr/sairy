// Copyright (c) 2015 Gregor Klinke
// All rights reserved.

#include "colors.hpp"
#include "estd/memory.hpp"
#include "fo.hpp"
#include "nodeclass.hpp"
#include "nodelist.hpp"
#include "nodes.hpp"
#include "nodeutils.hpp"
#include "scm-context.hpp"
#include "sosofo.hpp"
#include "utils.hpp"
#include "propstack.hpp"

#include "chibi/eval.h"
#include "chibi/sexp.h"

#include "fspp/estd/optional.hpp"

#include <boost/range/algorithm/find_if.hpp>
#include <boost/range/adaptor/transformed.hpp>

#include <cassert>
#include <iostream>
#include <memory>
#include <sstream>
#include <stdarg.h>
#include <string>
#include <vector>


namespace eyestep {

namespace fs = filesystem;

namespace {
  static const Node* s_root_node;

  const Node* root_node() { return s_root_node; }


#define NODELIST_TAG "<node-list>"
#define NODELIST_TAG_SIZE 11

#define SOSOFO_TAG "<sosofo>"
#define SOSOFO_TAG_SIZE 8

#define LENGTH_SPEC_TAG "<length-spec>"
#define LENGTH_SPEC_TAG_SIZE 13

#define COLOR_TAG "<color>"
#define COLOR_TAG_SIZE 7

#define STYLE_TAG "<style>"
#define STYLE_TAG_SIZE 7


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


  int length_spec_tag_p(sexp ctx)
  {
    int retv = 0;
    sexp_gc_var2(ty, nm);
    sexp_gc_preserve2(ctx, ty, nm);

    ty =
      sexp_env_ref(ctx, sexp_context_env(ctx),
                   nm = sexp_intern(ctx, LENGTH_SPEC_TAG, LENGTH_SPEC_TAG_SIZE), SEXP_VOID);
    if (sexp_typep(ty)) {
      retv = sexp_type_tag(ty);
    }

    sexp_gc_release2(ctx);

    return retv;
  }


  int color_tag_p(sexp ctx)
  {
    int retv = 0;
    sexp_gc_var2(ty, nm);
    sexp_gc_preserve2(ctx, ty, nm);

    ty =
      sexp_env_ref(ctx, sexp_context_env(ctx),
                   nm = sexp_intern(ctx, COLOR_TAG, COLOR_TAG_SIZE), SEXP_VOID);
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


  int style_tag_p(sexp ctx)
  {
    int retv = 0;
    sexp_gc_var2(ty, nm);
    sexp_gc_preserve2(ctx, ty, nm);

    ty = sexp_env_ref(ctx, sexp_context_env(ctx),
                      nm = sexp_intern(ctx, STYLE_TAG, STYLE_TAG_SIZE),
                      SEXP_VOID);
    if (sexp_typep(ty))
      retv = sexp_type_tag(ty);

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


  estd::optional<std::string> string_from_symbol_sexp_or_none(sexp ctx,
                                                              sexp obj)
  {
    sexp_gc_var1(str);
    sexp_gc_preserve1(ctx, str);

    estd::optional<std::string> result;
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


  estd::optional<std::string> string_from_keyword_or_none(sexp ctx, sexp obj)
  {
    sexp_gc_var1(str);
    sexp_gc_preserve1(ctx, str);

    estd::optional<std::string> result;
    if (sexp_keywordp(obj)) {
      str = sexp_keyword_to_string(ctx, obj);
      result = sexp_string_data(str);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp make_nodelist(sexp ctx, const NodeList* obj);


  sexp make_textbook_exception(sexp ctx, sexp self, const char *ms, sexp ir,
                               sexp source)
  {
    sexp res;
    sexp_gc_var3(sym, str, irr);
    sexp_gc_preserve3(ctx, sym, str, irr);
    res = sexp_make_exception(ctx,
                              sym = sexp_intern(ctx, "read", -1), // kind
                              str = sexp_c_string(ctx, ms, -1), // msg
                              ((sexp_pairp(ir) || sexp_nullp(ir))
                               ? ir : (irr = sexp_list1(ctx, ir))), // irritants
                              self, // procedure
                              source); // source
    sexp_gc_release3(ctx);
    return res;
  }


  //------------------------------------------------------------------------------

  sexp make_length_spec(sexp ctx, fo::LengthSpec ls)
  {
    sexp_gc_var4(ty, tmp, result, nm);
    sexp_gc_preserve4(ctx, ty, tmp, result, nm);

    ty =
      sexp_env_ref(ctx, sexp_context_env(ctx),
                   nm = sexp_intern(ctx, LENGTH_SPEC_TAG, LENGTH_SPEC_TAG_SIZE),
                   SEXP_VOID);

    if (sexp_typep(ty)) {
      result = sexp_alloc_type(ctx, cpointer, sexp_type_tag(ty));
      sexp_cpointer_freep(result) = 0;
      sexp_cpointer_length(result) = 0;
      sexp_cpointer_value(result) = (void*)new fo::LengthSpec(ls);
    }
    else {
      result = SEXP_VOID;
    }

    sexp_gc_release4(ctx);

    return result;
  }


  sexp free_length_spec(sexp ctx, sexp self, sexp_sint_t n, sexp ls_arg)
  {
    const fo::LengthSpec* ls = (const fo::LengthSpec*)(sexp_cpointer_value(ls_arg));
    delete ls;

    sexp_cpointer_value(ls_arg) = nullptr;
    return SEXP_VOID;
  }


  sexp func_displace_space_p(sexp ctx, sexp self, sexp_sint_t n, sexp q)
  {
    if (sexp_check_tag(q, length_spec_tag_p(ctx))) {
      const fo::LengthSpec* ls = (const fo::LengthSpec*)(sexp_cpointer_value(q));
      if (ls->_spec_type == fo::kDisplay)
        return sexp_make_boolean(1);
    }

    return sexp_make_boolean(0);
  }


  sexp func_inline_space_p(sexp ctx, sexp self, sexp_sint_t n, sexp q)
  {
    if (sexp_check_tag(q, length_spec_tag_p(ctx))) {
      const fo::LengthSpec* ls = (const fo::LengthSpec*)(sexp_cpointer_value(q));
      if (ls->_spec_type == fo::kInline)
        return sexp_make_boolean(1);
    }

    return sexp_make_boolean(0);
  }


  sexp func_make_length_spec(sexp ctx, sexp self, sexp_sint_t n,
                             sexp type_arg,
                             sexp val_arg, sexp min_arg, sexp max_arg,
                             sexp conditionalp_arg, sexp priority_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_VOID;

    if (!sexp_quantityp(val_arg)) {
      result =
        sexp_user_exception(ctx, self, "val: not a quantity", val_arg);
    }
    else if (!sexp_quantityp(min_arg)) {
      result =
        sexp_user_exception(ctx, self, "min: not a quantity", min_arg);
    }
    else if (!sexp_booleanp(conditionalp_arg)) {
      result =
        sexp_user_exception(ctx, self, "conditional?: not a bool", conditionalp_arg);
    }
    else if (!sexp_fixnump(priority_arg)) {
      result =
        sexp_user_exception(ctx, self, "priority: not a integer", priority_arg);
    }
    else {
      auto type = fo::kDimen;

      if (auto typev = string_from_symbol_sexp_or_none(ctx, type_arg)) {
        if (*typev == "inline")
          type = fo::kInline;
        else if (*typev == "display")
          type = fo::kDisplay;
        else
          result =
            sexp_user_exception(ctx, self, "type: not supported space type", type_arg);
      }
      else {
        result =
          sexp_user_exception(ctx, self, "type: not a symbol", type_arg);
      }

      // the number is normalized to 'm' unit; rebase it to 'pt'
      double val = sexp_quantity_normalize_to_double(ctx, val_arg) / 0.0003527778;
      double minv = sexp_quantity_normalize_to_double(ctx, min_arg) / 0.0003527778;
      double maxv = val;

      if (sexp_quantityp(max_arg)) {
        maxv = sexp_quantity_normalize_to_double(ctx, max_arg) / 0.0003527778;
      }
      else if (auto symbv = string_from_symbol_sexp_or_none(ctx, max_arg)) {
        if (*symbv == "inf")
          maxv = std::numeric_limits<double>::infinity();
        else
          result =
            sexp_user_exception(ctx, self, "max: symbol must be 'inf", max_arg);
      }
      else {
        result =
          sexp_user_exception(ctx, self, "max: not a quantity or real", max_arg);
      }

      if (result == SEXP_VOID) {
        result = make_length_spec(ctx,
                                  fo::LengthSpec(type,
                                                 val,
                                                 fo::k_pt,
                                                 minv,
                                                 maxv,
                                                 bool(sexp_unbox_boolean(conditionalp_arg)),
                                                 sexp_unbox_fixnum(priority_arg)));
      }
    }

    sexp_gc_release1(ctx);

    return result;
  }


  void init_length_spec_functions(sexp ctx)
  {
    sexp_gc_var3(nm, ty, op);
    sexp_gc_preserve3(ctx, nm, ty, op);

    // register qobject type
    ty = sexp_register_c_type(ctx, nm = sexp_c_string(ctx, "length-spec", -1),
                              &free_length_spec);

    sexp_env_cell_define(ctx, sexp_context_env(ctx),
                         nm = sexp_intern(ctx, LENGTH_SPEC_TAG, LENGTH_SPEC_TAG_SIZE), ty,
                         NULL);
    op =
      sexp_make_type_predicate(ctx, nm = sexp_c_string(ctx, "length-spec?", -1), ty);
    sexp_env_define(ctx, sexp_context_env(ctx),
                    nm = sexp_intern(ctx, "length-spec?", -1), op);

    // register functions
    sexp_define_foreign(ctx, sexp_context_env(ctx), "%make-length-spec", 6,
                        &func_make_length_spec);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "display-space?", 1,
                        &func_displace_space_p);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "inline-space?", 1,
                        &func_inline_space_p);

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
    struct SexpPropVisitor {
      using return_type = sexp;

      sexp _ctx;
      sexp _self;
      sexp _propname;
      sexp _default_value;

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
          if (*key == "default") {
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
        else if (propname.value() == CommonProps::k_id) {
          auto visitor = SexpPropVisitor(ctx, self, propname_arg, default_value);

          if (node->has_property(CommonProps::k_id)) {
            PropertyValue value = (*node)[CommonProps::k_id];
            result = apply(visitor, value);
          }
          else if (node->has_property(CommonProps::k_auto_id)) {
            PropertyValue value = (*node)[CommonProps::k_auto_id];
            result = apply(visitor, value);
          }
          else
            result = default_value;
        }
        else {
          auto visitor = SexpPropVisitor(ctx, self, propname_arg, default_value);
          PropertyValue value = (*node)[*propname];
          result = apply(visitor, value);
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
    return make_nodelist(ctx, new NodeList);
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
      result = make_nodelist(ctx, new NodeList(nl->rest()));
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node-list", nl_arg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_node_list_equal(sexp ctx, sexp self, sexp_sint_t n, sexp nl0_arg, sexp nl1_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (!sexp_check_tag(nl0_arg, nodelist_tag_p(ctx))) {
      result = sexp_user_exception(ctx, self, "not a node-list", nl0_arg);
    }
    else if (!sexp_check_tag(nl1_arg, nodelist_tag_p(ctx))) {
      result = sexp_user_exception(ctx, self, "not a node-list", nl1_arg);
    }
    else {
      result = SEXP_FALSE;

      const NodeList* nl0 = (const NodeList*)(sexp_cpointer_value(nl0_arg));
      const NodeList* nl1 = (const NodeList*)(sexp_cpointer_value(nl1_arg));

      if (nl0 != nl1) {
        NodeList p0 = nl0->clone();
        NodeList p1 = nl1->clone();
        while (!p0.empty() && !p1.empty()) {
          if (p0.head() != p1.head())
            break;
          p0 = p0.rest();
          p1 = p1.rest();
        }
        if (p0.empty() && p1.empty())
          result = SEXP_TRUE;
      }
      else
        result = SEXP_TRUE;
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


  sexp func_elements_with_id(sexp ctx, sexp self, sexp_sint_t n, sexp id_arg,
                             sexp nl_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_VOID;

    std::string key;
    if (sexp_stringp(id_arg)) {
      auto id = std::string(sexp_string_data(id_arg));

      if (const Node* node = singleton_node_from_list(ctx, nl_arg)) {
        ConstNodes nl_result = elements_with_id(node->grove(), id);

        if (!nl_result.empty()) {
          result = make_nodelist(ctx, new NodeList(nl_result));
        }
        else {
          result = make_nodelist(ctx, new NodeList);
        }
      }
      else {
        result =
          sexp_user_exception(ctx, self, "not a singleton node-list", nl_arg);
      }
    }
    else {
      result = sexp_user_exception(ctx, self, "not a string", id_arg);
    }

    if (result == SEXP_VOID) {
      result = make_nodelist(ctx, new NodeList);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_data(sexp ctx, sexp self, sexp_sint_t n, sexp nl_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_VOID;

    if (sexp_check_tag(nl_arg, nodelist_tag_p(ctx))) {
      const NodeList* nl = (const NodeList*)(sexp_cpointer_value(nl_arg));

      std::stringstream ss;
      NodeList p = nl->clone();
      while (!p.empty()) {
        ss << node_data(p.head());
        p = p.rest();
      }

      auto data = ss.str();
      result = sexp_c_string(ctx, data.c_str(), data.size());
    }
    else {
      result = sexp_user_exception(ctx, self, "not a node-list", nl_arg);
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
    sexp_define_foreign(ctx, sexp_context_env(ctx), "node-list=?", 2,
                        &func_node_list_equal);

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

    sexp_define_foreign(ctx, sexp_context_env(ctx), "elements-with-id", 2,
                        &func_elements_with_id);

    sexp_define_foreign(ctx, sexp_context_env(ctx), "data", 1, &func_data);

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

  sexp make_style(sexp ctx, const fo::PropertySpecs* obj)
  {
    sexp_gc_var3(ty, result, nm);
    sexp_gc_preserve3(ctx, ty, result, nm);

    ty = sexp_env_ref(ctx, sexp_context_env(ctx),
                      nm = sexp_intern(ctx, STYLE_TAG, STYLE_TAG_SIZE),
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

    sexp_gc_release3(ctx);

    return result;
  }


  sexp free_style(sexp ctx, sexp self, sexp_sint_t n, sexp style_arg)
  {
    const fo::PropertySpecs* style = (const fo::PropertySpecs*)(sexp_cpointer_value(style_arg));
    delete style;

    sexp_cpointer_value(style_arg) = nullptr;
    return SEXP_VOID;
  }


  estd::optional<fo::PropertySpec>
  evaluate_keyword_parameter(sexp ctx, sexp self, const std::string& key,
                             sexp expr)
  {
    estd::optional<fo::PropertySpec> result;

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
    else if (sexp_quantityp(expr)) {
      // the number is normalized to 'm' unit; rebase it to 'pt'
      double val = sexp_quantity_normalize_to_double(ctx, expr) / 0.0003527778;
      result = fo::PropertySpec(key, fo::LengthSpec(fo::kDimen, val, fo::k_pt));
    }
    else if (sexp_check_tag(expr, length_spec_tag_p(ctx))) {
      const fo::LengthSpec* ls = (const fo::LengthSpec*)(sexp_cpointer_value(expr));
      result = fo::PropertySpec(key, *ls);
    }
    else if (sexp_stringp(expr)) {
      result = fo::PropertySpec(key, std::string(sexp_string_data(expr)));
    }
    else if (sexp_check_tag(expr, color_tag_p(ctx))) {
      const fo::Color* co = (const fo::Color*)(sexp_cpointer_value(expr));
      result = fo::PropertySpec(key, *co);
    }
    else {
      excep = sexp_user_exception(ctx, self, "Bad property type: ", expr);
      check_exception_p(ctx, excep);
    }

    sexp_gc_release2(ctx);

    return result;
  }


  sexp make_fo(sexp ctx, sexp self, const std::string& fo_class,
               sexp fo_class_arg,
               const fo::PropertySpecs& props, sexp principal_port,
               sexp source)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    result = SEXP_NULL;

    if (sexp_check_tag(principal_port, sosofo_tag_p(ctx))) {
      const Sosofo* sosofo =
        (const Sosofo*)(sexp_cpointer_value(principal_port));

      std::shared_ptr<IFormattingObject> fo(
        fo::create_fo_by_classname(std::string("#") + fo_class, props, *sosofo));

      if (!fo) {
        result =
          make_textbook_exception(ctx, self, "Unknown fo-class: ", fo_class_arg,
                                  source);
      }
      else if (!fo->accepts_fo(*sosofo)) {
        result =
          make_textbook_exception(ctx, self, "bad FO nesting", fo_class_arg,
                                  source);
      }
      else
        result = make_sosofo(ctx, new Sosofo(fo));
    }
    else {
      std::shared_ptr<IFormattingObject> fo(
        fo::create_fo_by_classname(std::string("#") + fo_class, props, Sosofo()));
      result = make_sosofo(ctx, new Sosofo(fo));
    }

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_make_fo(sexp ctx, sexp self, sexp_sint_t n, sexp fo_class_arg,
                    sexp args_arg, sexp source)
  {
    sexp_gc_var2(result, obj);
    sexp_gc_preserve2(ctx, result, obj);

    result = SEXP_NULL;
    obj = SEXP_NULL;

    auto fo_class = string_from_symbol_sexp_or_none(ctx, fo_class_arg);
    if (!fo_class) {
      result = make_textbook_exception(ctx, self, "not a symbol", fo_class_arg,
                                       source);
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

            if (*key == "use") {
              if (sexp_check_tag(ref, style_tag_p(ctx))) {
                const fo::PropertySpecs* style =
                  (const fo::PropertySpecs*)(sexp_cpointer_value(ref));
                props = merge_property_specs(props, *style);
              }
              else {
                result =
                  make_textbook_exception(ctx, self, "use: requires style argument", ref,
                                          source);
                break;
              }
            }
            else {
              auto prop =
                evaluate_keyword_parameter(ctx, self, *key, ref);
              if (prop) {
                props.set(*prop);
              }
            }
          }
          else {
            result =
              make_textbook_exception(ctx, self, "value missing for keyword", ref,
                                      source);
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
          result = make_textbook_exception(ctx, self,
                                           "unexpeced keyword in make body", ref,
                                           source);
          break;
        }
        obj = ref;
      }
    }
    else {
      result = make_textbook_exception(ctx, self, "not a list", args_arg, source);
    }

    if (result == SEXP_NULL && fo_class) {
      result = make_fo(ctx, self, *fo_class, fo_class_arg, props, obj, source);
    }

    sexp_gc_release2(ctx);

    return result;
  }


  sexp func_make_style(sexp ctx, sexp self, sexp_sint_t n, sexp args_arg)
  {
    sexp_gc_var2(result, obj);
    sexp_gc_preserve2(ctx, result, obj);

    result = SEXP_NULL;
    obj = SEXP_NULL;

    fo::PropertySpecs props;
    if (sexp_pairp(args_arg)) {
      sexp ls = args_arg;

      for (; sexp_pairp(ls); ls = sexp_cdr(ls)) {
        sexp ref = sexp_car(ls);
        auto key = string_from_keyword_or_none(ctx, ref);

        if (key) {
          if (sexp_pairp(sexp_cdr(ls))) {
            ref = sexp_car(sexp_cdr(ls));
            if (*key == "use") {
              if (sexp_check_tag(ref, style_tag_p(ctx))) {
                const fo::PropertySpecs* style =
                  (const fo::PropertySpecs*)(sexp_cpointer_value(ref));
                props = merge_property_specs(props, *style);
              }
              else {
                result =
                  sexp_user_exception(ctx, self, "use: requires style argument", ref);
                break;
              }
            }
            else {
              auto prop = evaluate_keyword_parameter(ctx, self, *key, ref);
              if (prop) {
                props.set(*prop);
              }
            }
          }
          else {
            result =
              sexp_user_exception(ctx, self, "value missing for keyword", ref);
            break;
          }
        }
        else {
          result = sexp_user_exception(ctx, self,
                                       "unexpeced keyword in style list", ref);
          break;
        }

        ls = sexp_cdr(ls);
      }
    }
    else {
      result = sexp_user_exception(ctx, self, "not a list", args_arg);
    }

    if (result == SEXP_NULL) {
      result = make_style(ctx, new fo::PropertySpecs(props));
    }

    sexp_gc_release2(ctx);

    return result;
  }


  void init_make_functions(sexp ctx)
  {
    sexp_gc_var3(nm, ty, op);
    sexp_gc_preserve3(ctx, nm, ty, op);

    // register qobject type
    ty = sexp_register_c_type(ctx, nm = sexp_c_string(ctx, "style", -1),
                              &free_style);

    sexp_env_cell_define(ctx, sexp_context_env(ctx),
                         nm = sexp_intern(ctx, STYLE_TAG, STYLE_TAG_SIZE), ty,
                         NULL);
    op = sexp_make_type_predicate(ctx, nm = sexp_c_string(ctx, "style?", -1), ty);
    sexp_env_define(ctx, sexp_context_env(ctx),
                    nm = sexp_intern(ctx, "style?", -1), op);

    sexp_define_foreign(ctx, sexp_context_env(ctx), "%make-fo", 3,
                        &func_make_fo);
    sexp_define_foreign(ctx, sexp_context_env(ctx), "make-style", 1,
                        &func_make_style);
  }


  //----------------------------------------------------------------------------

  sexp free_color(sexp ctx, sexp self, sexp_sint_t n, sexp co_arg)
  {
    const fo::Color* co = (const fo::Color*)(sexp_cpointer_value(co_arg));
    delete co;

    sexp_cpointer_value(co_arg) = nullptr;
    return SEXP_VOID;
  }


  sexp make_color(sexp ctx, fo::Color co) {
    sexp_gc_var3(ty, result, nm);
    sexp_gc_preserve3(ctx, ty, result, nm);

    ty = sexp_env_ref(ctx, sexp_context_env(ctx),
                      nm = sexp_intern(ctx, COLOR_TAG, COLOR_TAG_SIZE),
                      SEXP_VOID);

    if (sexp_typep(ty)) {
      result = sexp_alloc_type(ctx, cpointer, sexp_type_tag(ty));
      sexp_cpointer_freep(result) = 0;
      sexp_cpointer_length(result) = 0;
      sexp_cpointer_value(result) = (void*)new fo::Color(co);
    }
    else {
      result = SEXP_VOID;
    }

    sexp_gc_release3(ctx);

    return result;
  }


  sexp make_rgb_color(sexp ctx, sexp self, sexp args_arg) {
    sexp_gc_var2(len, result);
    sexp_gc_preserve2(ctx, len, result);

    len = sexp_length_op(ctx, self, 1, args_arg);
    if (sexp_fixnump(len) && sexp_unbox_fixnum(len) == 3) {
      sexp red = sexp_car(args_arg);
      sexp green = sexp_cadr(args_arg);
      sexp blue = sexp_caddr(args_arg);

      float redv = sexp_flonump(red) ? sexp_flonum_value(red) : 0.0;
      float greenv = sexp_flonump(green) ? sexp_flonum_value(green) : 0.0;
      float bluev = sexp_flonump(blue) ? sexp_flonum_value(blue) : 0.0;

      result = make_color(ctx, fo::make_rgb_color(redv, greenv, bluev));
    }
    else
      result = sexp_user_exception(ctx, self,
                                   "3 reals expected for 'rgb' color space", args_arg);

    sexp_gc_release2(ctx);

    return result;
  }


  sexp make_cmyk_color(sexp ctx, sexp self, sexp args_arg) {
    sexp_gc_var2(len, result);
    sexp_gc_preserve2(ctx, len, result);

    len = sexp_length_op(ctx, self, 1, args_arg);
    if (sexp_fixnump(len) && sexp_unbox_fixnum(len) == 4) {
      sexp cyan = sexp_car(args_arg);
      sexp magenta = sexp_cadr(args_arg);
      sexp yellow = sexp_caddr(args_arg);
      sexp black = sexp_cadddr(args_arg);

      float cyanv = sexp_flonump(cyan) ? sexp_flonum_value(cyan) : 0.0;
      float magentav = sexp_flonump(magenta) ? sexp_flonum_value(magenta) : 0.0;
      float yellowv = sexp_flonump(yellow) ? sexp_flonum_value(yellow) : 0.0;
      float blackv = sexp_flonump(black) ? sexp_flonum_value(black) : 0.0;

      result = make_color(ctx, fo::make_cmyk_color(cyanv, magentav, yellowv, blackv));
    }
    else
      result = sexp_user_exception(ctx, self,
                                   "4 reals expected for 'cmyk' color space", args_arg);

    sexp_gc_release2(ctx);

    return result;
  }


  sexp make_gray_color(sexp ctx, sexp self, sexp args_arg) {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (sexp_numberp(args_arg)) {
      float val = (sexp_fixnump(args_arg)
                   ? sexp_unbox_fixnum(args_arg)
                   : (sexp_flonump(args_arg)
                      ? sexp_flonum_value(args_arg)
                       : 0.0));
      result = make_color(ctx, fo::make_gray_color(val));
    }
    else
      result = sexp_user_exception(ctx, self, "not a real", args_arg);

    sexp_gc_release1(ctx);

    return result;
  }


  sexp make_x11_color(sexp ctx, sexp self, sexp args_arg) {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    if (auto color = string_from_symbol_sexp_or_none(ctx, args_arg)) {
      result = make_color(ctx, fo::color_by_x11name(*color));
    }
    else
      result = sexp_user_exception(ctx, self, "not a real", args_arg);

    sexp_gc_release1(ctx);

    return result;
  }


  sexp func_color(sexp ctx, sexp self, sexp_sint_t n, sexp space_arg, sexp args_arg)
  {
    sexp_gc_var1(result);
    sexp_gc_preserve1(ctx, result);

    auto space = string_from_symbol_sexp_or_none(ctx, space_arg);
    if (!space) {
      result = sexp_user_exception(ctx, self, "not a symbol", space_arg);
    }
    else {
      if (*space == "rgb") {
        result = make_rgb_color(ctx, self, args_arg);
      }
      else if (*space == "cmyk") {
        result = make_cmyk_color(ctx, self, args_arg);
      }
      else if (*space == "gray") {
        result = make_gray_color(ctx, self, args_arg);
      }
      else if (*space == "x11") {
        result = make_x11_color(ctx, self, args_arg);
      }
      else
        result = sexp_user_exception(ctx, self, "unknown color space", space_arg);
    }

    sexp_gc_release1(ctx);

    return result;
  }


  void init_color_functions(sexp ctx)
  {
    sexp_gc_var3(nm, ty, op);
    sexp_gc_preserve3(ctx, nm, ty, op);

    // register qobject type
    ty = sexp_register_c_type(ctx, nm = sexp_c_string(ctx, "color", -1),
                              &free_color);
    sexp_env_cell_define(ctx, sexp_context_env(ctx),
                         nm = sexp_intern(ctx, COLOR_TAG, COLOR_TAG_SIZE), ty,
                         NULL);

    op =
      sexp_make_type_predicate(ctx, nm = sexp_c_string(ctx, "color?", -1), ty);
    sexp_env_define(ctx, sexp_context_env(ctx),
                    nm = sexp_intern(ctx, "color?", -1), op);

    sexp_define_foreign(ctx, sexp_context_env(ctx), "color", 2,
                        &func_color);

    sexp_gc_release3(ctx);
  }


  //----------------------------------------------------------------------------

  std::vector<fs::path>
  prepare_tstyle_search_path(const std::string& prefix_path)
  {
    return boost::copy_range<std::vector<fs::path>>(
      eyestep::utils::split_paths(prefix_path) |
      boost::adaptors::transformed(
        [](const fs::path& path) { return path / "tstyle"; }));
  }

  estd::optional<fs::path> search_in_path(const std::string& resource,
                                          const fs::path& parent_path,
                                          const std::string& prefix_path)
  {
    std::vector<fs::path> paths(prepare_tstyle_search_path(prefix_path));
    paths.insert(paths.begin(), parent_path);

    for (const auto& p : paths) {
      auto src_path = (fs::path(p) / resource).replace_extension(".tstyle");
      if (fs::exists(src_path)) {
        return src_path;
      }
    }

    return {};
  }

  sexp func_use(sexp ctx, sexp self, sexp_sint_t n, sexp res_arg)
  {
    sexp_gc_var4(result, source, nm, nm2);
    sexp_gc_preserve4(ctx, result, source, nm, nm2);

    result = SEXP_VOID;

    auto resource = string_from_symbol_sexp_or_none(ctx, res_arg);
    if (resource) {
      sexp path = sexp_env_ref(ctx, sexp_context_env(ctx),
                               nm = sexp_intern(ctx, "%style-parent-path%", -1),
                               SEXP_VOID);
      if (sexp_stringp(path)) {
        sexp tstyle_paths =
          sexp_env_ref(ctx, sexp_context_env(ctx),
                       nm2 = sexp_intern(ctx, "%textbook-prefix-paths%", -1),
                       SEXP_VOID);
        auto tstyle_paths_str = sexp_stringp(tstyle_paths)
                                  ? std::string(sexp_string_data(tstyle_paths))
                                  : std::string();

        auto src_path =
          search_in_path(*resource, std::string(sexp_string_data(path)),
                         tstyle_paths_str);
        if (src_path) {
          auto src_str = src_path->string();
          source = sexp_c_string(ctx, src_str.c_str(), src_str.size());
          result = sexp_load(ctx, source, sexp_context_env(ctx));
        }
        else {
          result = sexp_user_exception(ctx, self, "no such file", path);
        }
      }
      else {
        result =
          sexp_user_exception(ctx, self, "%style-parent-path% not set?", path);
      }
    }
    else {
      result = sexp_user_exception(ctx, self, "not a symbol", res_arg);
    }

    sexp_gc_release4(ctx);

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
    init_length_spec_functions(ctx);
    init_nodelist_functions(ctx);
    init_node_functions(ctx);
    init_sosofo_functions(ctx);
    init_make_functions(ctx);
    init_color_functions(ctx);
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

      return result;
    }
  };

} // ns anon


std::unique_ptr<ISchemeContext> create_scheme_context()
{
  return ::estd::make_unique<SchemeContext>();
}

} // ns eyestep
