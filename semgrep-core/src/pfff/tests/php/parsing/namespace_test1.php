#!/usr/bin/env php
<?php
/**
 */

namespace de\weltraumschaf\ebnf;

use \Exception   as Exception;

require_once dirname(__DIR__) . "/src/Command.php";

exit(Command::main(getopt("s:f:o:hd"), basename(__FILE__)));
