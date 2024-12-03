const express = require("express");
const app = express();

async function test2(req, res, next) {
  const input = req.query.input;

  setTimeout(() => {
    // ok: eval-express
    console.log("Delayed for 1 second." + input);
  }, 1000);

  // ruleid: eval-express
  setTimeout("console.log(" + input + ")", 1000);
}

app.get("/", test2);
