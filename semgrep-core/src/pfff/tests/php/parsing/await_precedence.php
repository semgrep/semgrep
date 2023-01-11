<?php

async function foo() {
  await true ? cached_result(1) : cached_result(2);
}
