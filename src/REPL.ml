(* REPL.ml --- Read Eval Print Loop (REPL)

Copyright (C) 2016-2017  Free Software Foundation, Inc.

Author: Pierre Delaunay <pierre.delaunay@hec.ca>

This file is part of Typer.

Typer is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation, either version 3 of the License, or (at your option) any
later version.

Typer is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.  *)

(*
 *      Description:
 *          Read Eval Print Loop (REPL). It allows you to eval typer code
 *          interactively line by line.
 *
 *      Example:
 *
 *          $./_build/typer [files]
 *            In[ 1] >> sqr = lambda x -> x * x;
 *            In[ 2] >> (sqr 4);
 *              Out[ 2] >> 16
 *            In[ 3] >> let a = 3; b = 3; in
 *                    .     a + b;
 *              Out[ 3] >> 6
 *
 * -------------------------------------------------------------------------- *)

open Util
open Fmt
open Debug

open Prelexer
open Lexer
open Sexp
open Pexp
open Lexp

open Lparse
open Eval

open Grammar
open Builtin

open Env
open Debruijn
module OL = Opslexp
module EL = Elexp
module C = Cexp

(* how to handle arrow keys ? *)
let _history = ref []

let arg_batch = ref false
let arg_debug = ref false

let print_input_line i =
    print_string "  In[";
    ralign_print_int i 2;
    print_string "] >> "

let ieval_error loc msg =
    msg_error "IEVAL" loc msg

(*  Read stdin for input. Returns only when the last char is ';'
 *  We can use '%' to prevent parsing
 *  If return is pressed and the last char is not ';' then
 *  indentation will be added                                       *)
let rec read_input i =
    print_input_line i;
    flush stdout;

    let rec loop str i =
        flush stdout;
        let line = input_line stdin in
        let s = String.length str in
        let n = String.length line in
            if s = 0 && n = 0 then read_input i else
            (if n = 0 then (
                print_string "          . ";
                print_string (make_line ' ' (i * 4));
                loop str (i + 1))
            else
        let str = if s > 0 then  String.concat "\n" [str; line] else line in
            if line.[n - 1] = ';' || line.[0] = '%' then
                str
            else (
                print_string "          . ";
                print_string (make_line ' ' (i * 4));
                loop str (i + 1))) in

    loop "" i

(* Interactive mode is not usual typer
 It makes things easier to test out code *)
type ldecl = vname * lexp * ltype
type lexpr = lexp

(* Grouping declaration together will enable us to support mutually recursive
 * declarations while bringing us closer to normal typer *)
let ipexp_parse (sxps: sexp list): (pdecl list * pexp list) =
    let rec _pxp_parse sxps dacc pacc =
        match sxps with
            | [] -> (List.rev dacc), (List.rev pacc)
            | sxp::tl -> match sxp with
                (* Declaration *)
                | Node (Symbol (_, ("_=_" | "_:_")), [Symbol s; t]) ->
                    _pxp_parse tl (List.append (pexp_p_decls sxp) dacc) pacc

                (* f arg1 arg2 = function body;  *)
                | Node (Symbol (_, "_=_"), [Node (Symbol s, args); t]) ->
                    _pxp_parse tl (List.append (pexp_p_decls sxp) dacc) pacc

                (* Expression *)
                | _ -> _pxp_parse tl dacc ((pexp_parse sxp)::pacc) in
        _pxp_parse sxps [] []


let ierase_type (lexps: (ldecl list list * lexpr list)) =
    let (ldecls, lexprs) = lexps in
    (List.map OL.clean_decls ldecls),
    (List.map OL.erase_type  lexprs)

let ilexp_parse pexps lctx: ((ldecl list list * lexpr list) * elab_context) =
    let pdecls, pexprs = pexps in
    let ldecls, lctx = lexp_p_decls pdecls lctx in
    let lexprs = lexp_parse_all pexprs lctx in
    let meta_ctx, _ = !global_substitution in
    List.iter (fun lxp -> ignore (OL.check meta_ctx (ectx_to_lctx lctx) lxp))
              lexprs;
    (ldecls, lexprs), lctx

let ieval lexps rctx =
    let (ldecls, lexprs) = lexps in
    let rctx = eval_decls_toplevel ldecls rctx in
    let vals = eval_all lexprs rctx false in
        vals, rctx

let _ieval f str  lctx rctx =
    let pres = (f str) in
    let sxps = lex default_stt pres in
    (* FIXME: This is too eager: it prevents one declaration from changing
     * the grammar used in subsequent declarations.  *)
    let nods = sexp_parse_all_to_list default_grammar sxps (Some ";") in

    (*  Different from usual typer *)
    let pxps = ipexp_parse nods in
    let lxps, lctx = ilexp_parse pxps lctx in
    let elxps = ierase_type lxps in
    let v, rctx = ieval elxps rctx in
        v, lctx, rctx

let _raw_eval f str lctx rctx =
    let pres = (f str) in
    let sxps = lex default_stt pres in
    let nods = sexp_parse_all_to_list default_grammar sxps (Some ";") in
    let pxps = pexp_decls_all nods in
    let lxps, lctx = lexp_p_decls pxps lctx in
    let elxps = List.map OL.clean_decls lxps in
    (* At this point, `elxps` is a `(vname * elexp) list list`, where:
     * - each `(vname * elexp)` is a definition
     * - each `(vname * elexp) list` is a list of definitions which can
     *   refer to each other (i.e. they can be mutually recursive).
     * - hence the overall "list of lists" is a sequence of such
     *   blocs of mutually-recursive definitions.  *)
    let _ = if !arg_debug then
              List.iter (List.iter (fun ((_, name), e) ->
                             print_string ("ELEXP: "^ name ^ " = ");
                             EL.elexp_print e;
                             print_string "\n"))
                        elxps; flush stdout in
    let rctx = eval_decls_toplevel elxps rctx in
        (* This is for consistency with ieval *)
        [], lctx, rctx

let ieval_string = _ieval prelex_string
let ieval_file = _ieval prelex_file

let eval_string = _raw_eval prelex_string
let eval_file = _raw_eval prelex_file


let _welcome_msg =
"      Typer 0.0.0 - Interpreter - (c) 2016

      %quit         (%q) : leave REPL
      %help         (%h) : print help
"

let _help_msg =
"      %quit         (%q) : leave REPL
      %who          (%w) : print runtime environment
      %info         (%i) : print elaboration environment
      %calltrace    (%ct): print eval trace of last call
      %elabtrace    (%et): print elaboration trace
      %typertrace   (%tt): print typer trace
      %readfile          : read a Typer file
      %help         (%h) : print help
"


let readfiles files (i, lctx, rctx) prt =
    (* Read specified files *)
    List.fold_left (fun (i, lctx, rctx) file ->

        (if prt then (
        print_string "  In["; ralign_print_int i 2;  print_string "] >> ";
        print_string ("%readfile " ^ file); print_string "\n";));

        try let (ret, lctx, rctx) = eval_file file lctx rctx in
            (List.iter (print_eval_result i) ret; (i + 1, lctx, rctx))
        with
            Sys_error _ -> (
                 ieval_error dloc ("file \"" ^ file ^ "\" does not exist.");
                (i, lctx, rctx))
        )
        (i, lctx, rctx)  files


(*  Specials commands %[command-name] [args] *)
let rec repl i clxp rctx =
    let repl = repl (i + 1) in
    let ipt = try read_input i with End_of_file -> "%quit" in
        _history := ipt::!_history;
        match ipt with
            (*  Check special keywords *)
            | "%quit" | "%q" -> ()
            | "%help" | "%h" -> (print_string _help_msg;  repl clxp rctx)
            | "%calltrace"  | "%ct" -> (print_eval_trace None; repl clxp rctx)
            | "%typertrace" | "%tt" -> (print_typer_trace None; repl clxp rctx)

            (* command with arguments *)
            | _ when (ipt.[0] = '%' && ipt.[1] != ' ') -> (
                match (str_split ipt ' ') with
                    | "%readfile"::args ->
                        let (i, clxp, rctx) = readfiles args (i, clxp, rctx) false in
                            repl clxp rctx;
                    | "%who"::args | "%w"::args -> (
                      let _ = match args with
                        | ["all"] -> dump_rte_ctx rctx
                        | _       -> print_rte_ctx rctx in
                          repl clxp rctx)
                    | "%info"::args | "%i"::args -> (
                      let _ = match args with
                        | ["all"] -> dump_lexp_ctx (ectx_to_lctx clxp)
                        | _       -> print_lexp_ctx (ectx_to_lctx clxp) in
                          repl clxp rctx)

                    | cmd::_ ->
                        ieval_error dloc (" \"" ^ cmd ^ "\" is not a correct repl command");
                        repl clxp rctx
                    | _ -> repl clxp rctx)

            (* eval input *)
            | _ -> (
                try let (ret, clxp, rctx) = (ieval_string ipt clxp rctx) in
                    List.iter (print_eval_result i) ret;
                    repl clxp rctx
                with e ->
                    repl clxp rctx)

let arg_files = ref []


(* ./typer [options] files *)
let arg_defs = [
    ("--batch", Arg.Set arg_batch, "Don't run the interactive loop");
    ("--debug", Arg.Set arg_debug, "Print the Elexp representation")
    (*"-I",
        Arg.String (fun f -> searchpath := f::!searchpath),
        "Append a directory to the search path"*)
]

let parse_args () =
  Arg.parse arg_defs (fun s -> arg_files:= s::!arg_files) ""

let main () =
    parse_args ();

    let ectx = default_ectx in
    let rctx = default_rctx in

    if not !arg_batch then
      (print_string (make_title " TYPER REPL ");
       print_string _welcome_msg;
       print_string (make_sep '-');
       flush stdout);

    let (i, ectx, rctx) = readfiles (List.rev !arg_files) (1, ectx, rctx)
                                    (not !arg_batch) in

    flush stdout;

    if not !arg_batch then
      (* Initiate REPL. This will allow us to inspect interpreted code *)
      repl i ectx rctx


let _ = main ()
