import DOMPurify from "dompurify"

function ok(props) {
  <span
  dangerouslySetInnerHTML = {
      // ok: test
      {__html: props ? DOMPurify.sanitize(props.text) : ''}
    }
/>
}
