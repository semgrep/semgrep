# https://github.com/returntocorp/semgrep/issues/3560
# For interpolated strings we generate some IdSpecials where we used to
# attach fake tokens, and if these ids got matched by e.g. a $X pattern
# then we had a fatal error when calling Visitor_AST.range_of_any from
# Match_patterns.check.
class Foo
  #ERROR:
  route :post, '/api/foo' do
    #ERROR:
    File.join(File.dirname(path), "foo_#{File.basename(path)}")
    #ERROR:
    api_ok {}
  end
end
