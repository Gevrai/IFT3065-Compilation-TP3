(*
 *      Typer Compiler
 *
 * ---------------------------------------------------------------------------
 *
 *      Copyright (C) 2011-2016  Free Software Foundation, Inc.
 *
 *   Author: Pierre Delaunay <pierre.delaunay@hec.ca>
 *   Keywords: languages, lisp, dependent types.
 *
 *   This file is part of Typer.
 *
 *   Typer is free software; you can redistribute it and/or modify it under the
 *   terms of the GNU General Public License as published by the Free Software
 *   Foundation, either version 3 of the License, or (at your option) any
 *   later version.
 *
 *   Typer is distributed in the hope that it will be useful, but WITHOUT ANY
 *   WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
 *   FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
 *   more details.
 *
 *   You should have received a copy of the GNU General Public License along
 *   with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * --------------------------------------------------------------------------- *)

open Util
open Utest_lib

open Sexp
open Lexp

open Lparse     (* eval string      *)
open Eval       (* reset_eval_trace *)

open Builtin
open Env


(* default environment *)
let lctx = default_lctx ()
let rctx = default_rctx ()


let _ = (add_test "MACROS" "macros base" (fun () ->
    reset_eval_trace ();

    (* define 'lambda x -> x * x' using macros *)
    let dcode = "
    sqr_fun = node_ (symbol_ \"lambda_->_\") (symbol_ \"x\")
    (node_ (symbol_ \"_*_\") (symbol_ \"x\") (symbol_ \"x\"));

    sqr_type = node_ (symbol_ \"_->_\") (symbol_ \"Int\") (symbol_ \"Int\");

    has_type = lambda (expr : Sexp) ->
        lambda (tp : Sexp) ->
            node_ (symbol_ \"_:_\") expr tp;

    sqr = Macro_ (has_type sqr_fun sqr_type);" in

    let rctx, lctx = eval_decl_str dcode lctx rctx in

    let ecode = "(sqr 3);" in

    let ret = eval_expr_str ecode lctx rctx in

        match ret with
            | [Vint(r)] -> expect_equal_int r (3 * 3)
            | _ -> failure ())
);;


(* run all tests *)
run_all ()
;;