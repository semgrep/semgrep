import React, { Component } from "react";
class BadPractice extends Component {
    render() {
        //ERROR:
        return React.createElement("a", { href: "aa" + location.href });
    }
}
