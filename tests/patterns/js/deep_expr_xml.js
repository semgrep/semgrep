export default function TestComponent4({assets, children, title}) {
    //ERROR: match
    return (
      <html lang="en">
        <head>
          <meta charSet="utf-8" />
          <meta name="viewport" content="width=device-width, initial-scale=1" />
          <link rel="shortcut icon" href="favicon.ico" />
          <link rel="stylesheet" href={assets['main.css']} />
          <title>{title}</title>
        </head>
        //ERROR: match
        <body>
          //ERROR: match
          <noscript
            //ERROR: match
            dangerouslySetInnerHTML={{
              __html: `<b>Enable JavaScript to run this app.</b>`,
            }}
          />
          {children}
          //ERROR: match
          <script
            //ERROR: match
            dangerouslySetInnerHTML={{
              __html: `assetManifest = ${JSON.stringify(assets)};`,
            }}
          />
        </body>
      </html>
    );
  }
