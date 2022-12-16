<?php

// dead
function nocaller() {
  calledbynocaller();
}

// dead by transitivity of dead nocaller
function calledbynocaller() {
}
