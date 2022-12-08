<?php

//src: semgrep-rules/php/lang/security/use-backsticks.php

//ERROR: 
echo `ping -n 3 {$user_input}`;
