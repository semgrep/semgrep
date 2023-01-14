// https://github.com/returntocorp/semgrep/issues/2787
import React, { Component } from "react";
class BadPractice extends Component {
  render() {
    // Testing DOM XSS source --> sink taint tracking in `mode: taint`

    const a = location.href;
    //ERROR:
    React.createElement("a", { href: a });

    //ERROR:
    React.createElement("a", { href: location.hash });

    const c = location.href + "bbb";
    //ERROR:
    React.createElement("a", { href: c });

    //ERROR:
    React.createElement("a", { href: "aa" + location.href });

  }
}
export default BadPractice;

