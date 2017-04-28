(* codify.ml Writes a cfile to a file.c

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
 *          From a cfile, get a real .C file !
 * -------------------------------------------------------------------------- *)

open Cexp
open Printf

(* Builtin map for conversion to c functions *)

(* Utility functions for coherence... *)
let ctx_select_string n = Cexp.ctx_select_string n

(* Entry point to this file, only starts the printing on a file with a predefined
   print function *)
let output_cfile output_file_name cfile =
  let out_chnl = open_out output_file_name in

  (* Print every ctexp one after the other *)
  let rec print_cfile cfile = match cfile with
    | ((loc, name), ctexp)::next ->
      (* Debug string before each ctexp print *)
      fprintf out_chnl "/* %s: %s */\n" name (Util.loc_string loc);
      print_ctexp ctexp name;
      print_cfile next
    | [] -> ()
  and print_ctexp ct name = match ct with
    (* TODO  add type to arguments *)
    | Lambda (arg, body) ->
      fprintf out_chnl "%s(%s){\n" name (arg_string arg);
      print_cexp body;
      fprintf out_chnl "\n}"
    | Cexp c -> print_cexp c
  and arg_string (_, name) = name
  (* and args_string args = match args with *)
  (*   | (_, arg_name) :: [] -> arg_name *)
  (*   | (_, arg_name) :: next -> arg_name ^ (args_string next) *)
  (*   | [] -> "" *)
  and print_cexp c = match c with
    | Imm (Sexp.String (_, s))  -> fprintf out_chnl "%s" s
    | Imm (Sexp.Integer (_, i)) -> fprintf out_chnl "%d" i
    | Imm (Sexp.Float (_, f))   -> fprintf out_chnl "%f" f
    | Builtin (loc, name) -> print_builtin name
    | Context_Select i -> fprintf out_chnl "%s" (ctx_select_string i)
    | Select (record, ind)
      -> print_cexp record; fprintf out_chnl "[%i]" ind
    (* FIXME FIXME FIXME take care of every cases ! *)
    | _ -> ()
  and print_builtin blt = fprintf out_chnl "TODO print_builtin %s" blt
  (* Print comma separated args *)
  (* and typeof_ctexp ctexp = *)
  (*   (\* TODO *\) *)
  (*   "u_type" *)

  (* Start the process ! *)
  in print_cfile cfile
