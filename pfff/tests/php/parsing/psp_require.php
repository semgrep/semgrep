<?php

  psp_register_postsend_function('unknown_func', 1);
  psp_register_postsend_function(array('UnknownClass', 'foo'), 1);
