// https://gist.github.com/lukehoban/9303054#template-strings
var x =`In JavaScript '\n' is a line-feed.`;

var x = `<script>"in quotes without escape"</script>`;

var name = "Bob", time = "today";
// perl/ruby/python interpolated strings
var out = `Hello ${name}, how are you ${time}?`;

var out = sanitize`Hello ${name}, how are you ${time}?`;

const getTermLinkMarkdownBlock = termTitle => {
  let anchor = util.getMarkDownAnchor(termTitle);
  return `* [\`${termTitle}\`](#${anchor})` + '\n';
};
