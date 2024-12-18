# On this target file, we get some parse errors on the "bar" field
# and tree-sitter error recovery insert some special tokens at the end
# that have a line number above the actual number of lines in the file
# (probably because it gets the line of the EOF token). This in turn
# used to create some crash of semgrep and out-of-bound array access
# for the nosemgrep analysis and the lines: report in the core_output.
# This was fixed by having better error management around
# the UFile.lines_of_file() function (hence the name of this test)
# ruleid: aws-dynamodb-table-unencrypted
resource "aws_dynamodb_table" "grants" {
  tags {
    "bar" = "${var.environment}"
    "baz"=  "${var.bar_tag}"
  }
}
