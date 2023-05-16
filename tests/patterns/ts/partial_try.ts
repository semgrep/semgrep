// MATCH:
try {
    let result = 10 / 0; // division by zero
} catch (error) {
    console.log("handled"); // code to handle the exception
}