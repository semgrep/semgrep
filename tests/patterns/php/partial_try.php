<?

// MATCH:
try {
    $result = 10 / 0; // division by zero
} catch (Exception $e) {
    echo "handled"; // code to handle the exception
}
