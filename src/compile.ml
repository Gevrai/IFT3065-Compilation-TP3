(* compile.ml A compiler for a .typer program to C

   Copyright (C) 2016-2017  Free Software Foundation, Inc.

   Author: Pierre Delaunay <pierre.delaunay@hec.ca>
   Author: Gevrai Jodoin-Tremblay <gevrai@gmail.com>
   Author: Nicolas Lafond <>

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
 *          Compile a type program to C
 * -------------------------------------------------------------------------- *)

module U = Util
module OL = Opslexp
module EL = Elexp

let arg_debug = ref false
let arg_output_filename = ref "a.c"

(* Simple exception for output in stderr *)
exception File_not_found of string

let compile_error loc msg =
  U.msg_error "COMPILE" loc msg

(* Almost a carbon copy of _raw_eval from REPL.ml *)
let typerfile_to_elexpss f str lctx =
  let pres = (f str) in
  let sxps = Lexer.lex Grammar.default_stt pres in
  let nods = Sexp.sexp_parse_all_to_list Grammar.default_grammar sxps (Some ";") in
  let pxps = Pexp.pexp_decls_all nods in
  let lxps, lctx = Lparse.lexp_p_decls pxps lctx in
  (* At this point, `elxps` is a `(vname * elexp) list list`, where:
   * - each `(vname * elexp)` is a definition
   * - each `(vname * elexp) list` is a list of definitions which can
   *   refer to each other (i.e. they can be mutually recursive).
   * - hence the overall "list of lists" is a sequence of such
   *   blocs of mutually-recursive definitions.  *)
  let elxpss = List.map OL.clean_decls lxps in
  (* We keep the debug option because why not? *)
  let _ = if !arg_debug then
      List.iter (List.iter (fun ((_, name), e) ->
          print_string ("ELEXP: "^ name ^ " = ");
          EL.elexp_print e;
          print_string "\n"))
        elxpss; flush stdout in
  (elxpss, lctx)

(* Compiles a single file (from file's name) to a (vname * elexp) list list *)
let compile_file file_name lctx rctx =
  try
    let (elxpss, lctx) = typerfile_to_elexpss Prelexer.prelex_file file_name lctx in
    let (cfile, rctx) = Cexp.compile_decls_toplevel elxpss rctx in
    (cfile, lctx, rctx)
  with Sys_error _ ->
    raise (File_not_found file_name)

(* FIXME No idea if this works... Possible to read many files at once? *)
let rec filenames_to_elexps files_names lctx rctx = match files_names with
  | str::strs
    -> let (cfile, lctx, rctx) = compile_file str lctx rctx in
    let (_cfile, _lctx, _rctx) = filenames_to_elexps strs lctx rctx in
    (cfile @ _cfile , _lctx, _rctx)
  | [] -> [], lctx, rctx

(* function for tests *)
let print_whole_rctx rctx =
  let print n =
    let v = Env.get_rte_variable None n rctx in
    Env.value_print v in
  for i = 1 to (Env.get_rte_size rctx) - 1 do
    print_int i;
    print i;
    print_string "\n"
  done

(* Compile a list of typer files to a .c file whose name is declared in arg_output_filename *)
let rec compile_files files_names lctx rctx =
  let (elexps, lctx, rctx) = filenames_to_elexps files_names lctx rctx in
  print_whole_rctx rctx;
  ()
  (* let cfiles = Cexp.compile_decls_toplevel elexps lctx rctx in *)
  (* let code_str = Cexp.cfile_to_c_code cfiles in *)
  (* let out_chnl = open_out !arg_output_filename in *)
  (* Printf.fprintf out_chnl "%s" code_str; *)
  (* close_out out_chnl *)

let arg_files = ref []

(* ./typer [options] files *)
let arg_defs = [
  ("--debug", Arg.Set arg_debug, "Print the Elexp representation");
  ("--output", Arg.Set_string arg_output_filename, "Name of the C file to be outputted, \
                                                    defaults to a.c")
]

let parse_args () =
  Arg.parse arg_defs (fun s -> arg_files:= s::!arg_files) ""

let main () =
  parse_args ();

  let lctx = Lparse.default_ectx in
  let rctx = Lparse.default_rctx in

  try compile_files (List.rev !arg_files) lctx rctx
  with File_not_found filename ->(
      Printf.eprintf "File \"%s\" does not exist." filename;
      flush stderr)

let _ = main ()
