import dompurify from 'isomorphic-dompurify';
import dompurify1 from 'dompurify';



export var Comment = function (props) {
    const { i18n, text, attachments } = this.props;
    return (
      React.createElement("div", { className: "comment-text", 
      // ruleid: react-props-injection-without-elements
      dangerouslySetInnerHTML: {__html: text} })
    );
};


export var Comment = function (props) {
    return (
      React.createElement("div", { className: "comment-text", 
      // ok: react-props-injection-without-elements
      dangerouslySetInnerHTML: {__html: 'a'} })
    );
};



export var Comment = function (props) {
    var comment = props.data
    return (
      React.createElement("div", { className: "comment-text", 
      // ruleid: react-props-injection-without-elements
      dangerouslySetInnerHTML: {__html: comment} })
    );
};

export var Comment = function (props) {
    return (
      // ruleid: react-props-injection-without-elements
      React.createElement("div", {...props},'a')
    );
};

export var Comment = function (props) {
    var comment = props.data
    return (
      React.createElement("div", { className: "comment-text", dangerouslySetInnerHTML: {__html: dompurify.sanitize(comment)} })
    );
};

export var Comment = function (props) {
    var comment = props.data
    return (
      React.createElement("div", { className: "comment-text", dangerouslySetInnerHTML: {__html: dompurify1.sanitize(comment)} })
    );
};
