<?php

Route::get('/foobar/{input}', function ($input) {
  $s_url = "https://{$input}/fooo";
  // Since the same sink spec matches the entire call as well as the
  // argument $s_url, when computing "best sinks" we will pick the
  // larger match, which is the entire call.
  // This is kind of messy, we should fix it somehow.
  // ruleid: test
  Http::withDigestAuth()->post($s_url);
});
