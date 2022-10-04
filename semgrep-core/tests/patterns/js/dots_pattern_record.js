//ERROR: match
const BlogText = ({ data }) => {
  const bodyText = useRef();
    bodyText.current.innerHTML = marked(DOMPurify.sanitize(data.body_text));
};
