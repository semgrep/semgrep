<?php


echo ($this->tokPtr <= $this->stringLen)
        ? $this->string{$this->tokPtr - 1}
        : null;
