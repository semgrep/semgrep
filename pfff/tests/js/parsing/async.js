function getCode(rawSnippetString) {
  return rawSnippetString.match(CODE_RE)[1].replace('\n', '');
}

async function doRollup() {
  // Plugins
  const es5 = babel({ presets: ['@babel/preset-env'] });
  const min = minify({ comments: false });

}

const foo = (x) => async () => {
  console.log("yup");
}
