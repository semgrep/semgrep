// https://github.com/returntocorp/semgrep/issues/2787
import React, { Component } from "react";
class BadPractice extends Component {
  dangerous() {
    return location.href;
  }

  render() {
    // Testing DOM XSS source --> sink taint tracking in `mode: taint`

    //ERROR:
    React.createElement("a", { href: this.dangerous() });
  }
}
export default BadPractice;

