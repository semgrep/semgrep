(*****************************************************************************)
(* Prelude *)
(*****************************************************************************)
(*
 * update: see ast_php_simple.ml for now, our best shot at a PHP
 * Intermediate Language.
 * 
 * The Ast_php module is nice when you want to do some simple analysis
 * or refactoring as it directly matches the source code; error reporting
 * is precise and you can easily unparse a modified AST to a file that is
 * close to the original coding style. Unfortunately this AST, which
 * is in fact more a CST for Concrete Syntax Tree, is not very convenient
 * for heavier static analysis such as data flow tracking. 
 * 
 * Indeed, PHP is a large language and as a consequence Ast_php contains 
 * lots of constructors with many many cases to handle for the analysis 
 * writer. For instance incrementing a variable can be written as
 *  $i = $i + 1; or $i++; or ++$i;  which in our AST is represented by
 * 3 different constructs.  Moreover some important constructs
 * such as function calls or assignements can be very
 * deeply nested inside an expression. One can obviously write in PHP
 * x = foo() + y = bar();  and this would be naively represented
 * as a single node in a CFG; matching over function calls
 * or assignements would require then to write visitors over those nodes.
 * Even include/require constructs are represented at the expression level
 * whereas they should be really more at the toplevel or at least
 * statement level.
 * 
 * Enter PIL for PHP Intermediate Language, a better representation
 * for a PHP program. We just follow the tradition started by
 * CIL[1] and RIL[2] and propose here an AST which makes it
 * easier to write some static analysis.
 * 
 * Maybe we should focus on Ast_php because if we want to transform code
 * we have to work on that. But working on Pil can also help
 * clarify the semantic of PHP :) It could also maybe make it
 * easier to write the type inferer later.
 * 
 * 
 * We try, just like in Ast_php, to keep position information for the
 * elements in PIL, but we do it less. For instance comma_list or paren
 * stuff are removed here.
 * 
 * TODO: does the linearization respect the semantic of PHP ? Is there
 * cases where spliting a complex expression into multiple parts
 * with intermediate variables leads to a different semantic?
 * For instance apparently $i++ is not equivalent to $i = $i + 1 when
 * $i is a string. Wonderful PHP. 
 * But at the same time our goal here is not to write a compiler. Our goal
 * is to find bugs so we maybe it's ok to relax some of the requirements.
 * 
 * References:
 *  [1] CIL, C Intermediate Language, Necula et al, CC'00
 *  [2] The Ruby Intermediate Language, Furr et al, DLS'09
 *)
