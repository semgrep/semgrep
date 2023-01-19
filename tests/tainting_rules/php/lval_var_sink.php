<?php
// https://github.com/returntocorp/semgrep/issues/4320
$doc = new DOMDocument();
// We did not catch the error when the sink was the variable in an l-value,
// because we assumed that only expressions or instructions could be sinks.
//ERROR:
$doc->load('file.xml');
//ERROR:
$doc->load($doc);
//OK:
$something->load($doc);
