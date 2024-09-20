// https://github.com/returntocorp/semgrep/issues/2787
import React, { Component } from "react";
class BadPractice extends Component {
  render() {
    // Testing DOM XSS source --> sink taint tracking in `mode: taint`

    const a = location.href;
    //ruleid: test
    React.createElement("a", { href: a });

    //ruleid: test
    React.createElement("a", { href: location.hash });

    const c = location.href + "bbb";
    //ruleid: test
    React.createElement("a", { href: c });

    //ruleid: test
    React.createElement("a", { href: "aa" + location.href });

  }
}
export default BadPractice;

