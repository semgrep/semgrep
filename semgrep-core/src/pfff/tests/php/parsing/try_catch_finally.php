<?php

function bar() {
  try {
  } catch (Exception $e) {
  }

  try {
    echo "A";
  } catch (Exception $e) {
    echo "B";
  }

  try {
  } finally {
  }

  try {
    echo "C";
  } finally {
    echo "D";
  }

  try {
  } catch (Exception $e) {
  } finally {
  }

  try {
    echo "E";
  } catch (Exception $e) {
    echo "F";
  } finally {
    echo "G";
  }

  try {
    try {
    } finally {
    }
  } finally {
  }

  try {
    try {
      echo "H";
    } finally {
      echo "I";
    }
  } finally {
    echo "J";
  }

  try {
  } finally {
    try {
    } finally {
    }
  }

  try {
    echo "K";
  } finally {
    try {
      echo "L";
    } finally {
      echo "M";
    }
  }
}
