// The second semicolon seperates an empty statement, currently the tree-sitter
// parser attempt to parse it, and give back partial parsing error on the ";"
// (at least on semgrep-cpp/tree/eddec4922e67e757c546cdec7e640a6e7b0ecba4)
namespace grumbler = aaa;;


std::string grr() {
  return "yippie";
// semicolons at the end of top level declerations also result in a partial
// parsing error.
};
