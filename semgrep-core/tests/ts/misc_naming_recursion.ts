// the number:number used to create a cycle in the AST which
// made -dump_ast to loop forever and some semgrep queries to
// use all memory

export function foobar(
  number: number,
  params?: Record<string, any>
) {

  //ERROR: match
m  return Array(number)
    //ERROR: match
    .fill(true)
    //ERROR: match
    .map(() => {
      //ERROR: match
      return bar(params);
    });
}
