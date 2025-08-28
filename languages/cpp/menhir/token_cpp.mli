(*
   Copyright (c) 2022-2024 Semgrep Inc.

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public License
   version 2.1 as published by the Free Software Foundation.

   This library is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the file
   LICENSE for more details.
*)
type cppcommentkind =
  | CppDirective
  | CppAttr
  | CppMacro
  | CppMacroExpanded
  | CppPassingNormal (* ifdef 0, cplusplus, etc *)
  | CppPassingCosWouldGetError (* expr passsing *)
  | CppOther

type cpluspluscommentkind = CplusplusTemplate | CplusplusQualifier
