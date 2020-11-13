const config = require('./config')
const {JWT} = require('jose')

function example(user) {
// ruleid: jose-exposed-data
    const token = JWT.sign(user, secret)
    return token;
}

function example2(user) {
// ok
    const token = JWT.sign({name: user.name}, secret)
    return token;
}

function example3(user) {
// ok
    const obj = {
        name: user.name
    }
    const token = JWT.sign(obj, secret)
    return token;
}

module.exports = {
    siteMetadata: {
      title: "blah",
      titleTemplate: "%s",
      tagline: "blah",
      author: "blah",
      imageUrl: "https://stuff-n-things.com/static/",
      description:
        "blah",
      keywords: `blah`,
    },
    plugins: [
      "gatsby-plugin-react-helmet",
      "gatsby-plugin-sitemap",
      {
        resolve: `gatsby-source-filesystem`,
        options: {
          name: `images`,
          path: `${__dirname}/src/images`,
        },
      },
      {
        resolve: `gatsby-source-filesystem`,
        options: {
          name: "pages",
          path: `${__dirname}/src/pages`,
        },
      },
      {
        resolve: `gatsby-source-filesystem`,
        options: {
          name: `blog`,
          path: `${__dirname}/src/pages/blog`,
        },
      },
      {
        resolve: `gatsby-plugin-manifest`,
        options: {
          name: `blah`,
          short_name: `blah`,
          start_url: `/`,
          background_color: `#ffffff`,
          theme_color: `#ffffff`,
          display: `standalone`,
          icon: "src/images/favicon.png",
        },
      },
      "gatsby-plugin-offline",
      {
        resolve: "gatsby-plugin-google-tagmanager",
        options: {
          id: "GTM-XXXXXXX",
          includeInDevelopment: true,
          defaultDataLayer: { platform: "gatsby" },
        },
      },
      {
        resolve: `gatsby-transformer-remark`,
        options: {
          plugins: [
            {
              resolve: `gatsby-remark-social-cards`,
            },
            {
              resolve: `gatsby-remark-embedder`,
            },
            {
              resolve: `gatsby-remark-images`,
              options: {
                maxWidth: 1200,
              },
            },
            `gatsby-remark-responsive-iframe`,
            {
              resolve: "gatsby-remark-embed-youtube",
              options: {
                width: 640,
                height: 360,
                related: false,
                noIframeBorder: true,
              },
            },
            {
              resolve: `gatsby-remark-prismjs`,
              options: {
                classPrefix: "language-",
                inlineCodeMarker: null,
                aliases: {},
                showLineNumbers: false,
                noInlineHighlight: false,
              },
            },
            {
              resolve: "gatsby-remark-external-links",
              options: {
                target: "_blank",
                rel: "noopener",
              },
            },
            {
              resolve: `gatsby-remark-copy-linked-files`,
              options: {
                ignoreFileExtensions: [
                  `png`,
                  `jpg`,
                  `jpeg`,
                  `bmp`,
                  `tiff`,
                  `pdf`,
                ],
              },
            },
          ],
        },
      },
      "gatsby-plugin-twitter",
      "gatsby-transformer-sharp",
      "gatsby-plugin-sharp",
      `gatsby-plugin-sass`,
      {
        resolve: `gatsby-plugin-google-analytics`,
        options: {
          trackingId: "UA-XXXXXXXXX",
          anonymize: true,
        },
      },
      {
        resolve: `gatsby-plugin-sitemap`,
        options: {
          output: `/sitemap.xml`,
          query: `
          {
            site {
              siteMetadata {
                siteUrl
              }
            }
            allSitePage(
              filter: {isCreatedByStatefulCreatePages: {eq: true}}
              ) {
              edges {
                node {
                  path
                }
              }
            }
            allMarkdownRemark(
              filter: {frontmatter: {unlisted: {ne: true}}}
            ) {
              edges {
                node {
                  fields {
                    slug
                  }
                }
              }
            }
          }`,
          serialize: ({ site, allSitePage, allMarkdownRemark }) => {
            let pages = [];
            allSitePage.edges.map((edge) => {
              pages.push({
                url: site.siteMetadata.siteUrl + edge.node.path,
                changefreq: `daily`,
                priority: 0.7,
              });
            });
            allMarkdownRemark.edges.map((edge) => {
              pages.push({
                url: site.siteMetadata.siteUrl + edge.node.fields.slug,
                changefreq: `daily`,
                priority: 0.7,
              });
            });
            return pages;
          },
        },
      },
    ],
  };
