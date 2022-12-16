<?php

interface I {
  // HipHop Fatal error: Access type for interface method
  private function foo();
}
