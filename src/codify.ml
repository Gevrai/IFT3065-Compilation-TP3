(* codify.ml Writes a cfile to a file.c

   Copyright (C) 2016-2017  Free Software Foundation, Inc.

   Author: Pierre Delaunay <pierre.delaunay@hec.ca>
   Author: Gevrai Jodoin-Tremblay <gevrai@gmail.com>
   Author: Nicolas Lafond <lafondni89@gmail.com>

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

let header_string = ref "#include \"../src/runtime_support.c\""
let main_string = ref "int main(){"

let gentype = "prim_type"
let blt_prefix = "builtin_"

let assoc_builtins_to_c_function = [
  ("Int.+",           "int_add");
  ("Int.-",           "int_sub");
  ("Int.*",           "int_mul");
  ("Int./",           "int_div");
  ("Float.+",         "float_add");
  ("Float.-",         "float_sub");
  ("Float.*",         "float_mul");
  ("Float./",         "float_div");
  ("Float.to_string", "float_tostring");
  ("Int.<",           "int_lt");
  ("Int.>",           "int_gt");
  ("Int.=",           "int_eq");
  ("Int.<=",          "int_leq");
  ("Int.>=",          "int_geq");
  ("String.=",        "str_eq");
  ("Sexp.=",          "sexp_eq");
  ("Sexp.symbol",     "sexp_symbol");
  ("Sexp.string",     "sexp_string");
  ("Sexp.node",       "sexp_node");
  ("Sexp.integer",    "sexp_int");
  ("Sexp.float",      "sexp_float");
  ("Sexp.dispatch",   "sexp_dispatch");
  ("IO.bind",         "io_bind");
  ("IO.return",       "io_return");
  ("IO.run",          "io_run");
  ("File.open",       "file_open");
  ("File.stdout",     "file_stdout");
  ("File.write",      "file_write");
  ("File.read",       "file_read");
  ("Sys.cpu_time",    "sys_cputime");
  ("Sys.exit",        "sys_exit");
  ("Eq.refl",         "eq_refl");
  ("Eq.cast",        "eq_cast")
  (* ("Y",               "Ycombinator"); *)
]

let builtin_c_str name =
  try blt_prefix ^ (List.assoc name assoc_builtins_to_c_function)
  with Not_found -> (printf "Unsuported builtin: %s\n" name ; exit 1)

(* Entry point to this file *)
let output_cfile output_file_name cfile =
  let outc = open_out output_file_name in
  (* Print declarations only, meaning the file is wholy mutally recursive... *)
  let rec print_globals cfile = match cfile with
    | [] -> ()
    | ((_,funname), Lambda((loc,argname),body))::next
       -> fprintf outc "%s %s(%s %s, %s *%s);\n" gentype funname gentype argname gentype ctxstring;
       print_globals next;
       (* FIXME beware of redefining the same variable name *)
    | ((_,varname), _)::next
       -> fprintf outc "%s %s;\n" gentype varname;
       print_globals next
  (* Print every lambdas one after the other, returns the rest of cfile *)
  and print_lambdas cfile = match cfile with
    | ((funloc, funname), Lambda((argloc,argname),body))::next ->
      (* fprintf outc "\n/* %s: %s */\n" funname (Util.loc_string funloc); *)
      fprintf outc "%s(%s %s, %s *%s){\n" funname gentype argname gentype ctxstring;
      print_cexp body;
      fprintf outc "\n}\n";
      print_lambdas next
    | _ :: next -> print_lambdas next
    | [] -> ()
  and print_main cfile = match cfile with
    | ((exprloc, exprname), Cexp(cexp))::next ->
      (* fprintf outc "\n/* %s: %s */\n" exprname (Util.loc_string exprloc); *)
      fprintf outc "%s = " exprname;
      print_cexp cexp;
      fprintf outc ";\n";
       print_main next
    | _ :: next -> print_main next
    | [] -> ()
  and print_cexp c = match c with
    | Imm (Sexp.String (_, s))  -> fprintf outc "mkString(%s)" s
    | Imm (Sexp.Integer (_, i)) -> fprintf outc "mkInt(%d)" i
    | Imm (Sexp.Float (_, f))   -> fprintf outc "mkFloat(%f)" f
    | Builtin (loc, name) -> fprintf outc "%s" (builtin_c_str name)
    | Context_Select i -> fprintf outc "%s" (ctx_select_string i)
    | Select (record, ind)
      -> print_cexp record; fprintf outc "[%i]" ind
    (* FIXME FIXME FIXME take care of every cases ! *)
    | Closure (name, args)
        (* not sure if OK *)
        -> fprintf outc "mkClosure( %s, %s)" name (environement_string args)
    | _ -> ()
and environement_string args = 
  let rec aux args str = match args with
    | [] -> ""
    | arg :: []-> arg
    | arg :: others -> aux others (arg ^ ",")
  in "(prim_type[]){" ^ (aux args "") ^ "}"

  in
  (* Start the process ! *)
  fprintf outc "%s\n\n" !header_string;
  print_globals cfile;
  fprintf outc "\n\n";
  print_lambdas cfile;
  fprintf outc "\n%s\n" !main_string;
  print_main cfile;
  fprintf outc "\n}\n";
