<?php

foreach ($blacklist as $cannot_send_to) {
  if (strpos($to, $cannot_send_to) !== false) {
    return;
  }
}
