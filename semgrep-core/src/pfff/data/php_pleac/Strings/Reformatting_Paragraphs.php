# **********************************************************************
# Reformatting Paragraphs
# **********************************************************************
<?php
function pleac_Reformatting_Paragraphs() {
#-----------------------------
$output = wordwrap($str, $width, $break, $cut);
#-----------------------------
# @@INCLUDE@@ include/php/wrapdemo.php
#-----------------------------
// merge multiple lines into one, then wrap one long line
print wordwrap(str_replace("\n", " ", file_get_contents('php://stdin')));
#-----------------------------
while(!feof(STDIN)) {
    print wordwrap(str_replace("\n", " ", stream_get_line(STDIN, 0, "\n\n")));
    print "\n\n";
}
#-----------------------------

}
?>
