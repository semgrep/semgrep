
function qux(x="hello",...y):string { foo(x); return y[0]; }
qux(0,...["",42]);

const JSONtoCSV = (arr, columns, delimiter = ',') =>
  [
    columns.join(delimiter),
    ...arr.map(obj =>
      columns.reduce(
        (acc, key) => `${acc}${!acc.length ? '' : delimiter}"${!obj[key] ? '' : obj[key]}"`,
        ''
      )
    )
  ].join('\n');

  config.devServer = {
    ...config.devServer,
    hot: false,
    inline: false
  };
