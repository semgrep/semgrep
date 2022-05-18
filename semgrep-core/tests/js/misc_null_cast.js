try {
    nodeFS=(null)("fs"); //code from https://github.com/mbebenita/Broadway/blob/d32f9deb2c39f15ba3f9ec5105f5cd31e244bd2d/Player/Decoder.js
}
catch (error) {
    console.error("An exception was thrown.");
    //ERROR: match
    2 == 2; //This wasn't flagged before due to syntax error above.
}
