<?php

// people sometimes test the PHP runtime where they stubout functions
// to unknown function. This causes pfff static analysis to fail. So
// here we whitelist a few of this "functions used only in test context"
function some_fake_function_name() {
}

class SomeRandomClass {
}
