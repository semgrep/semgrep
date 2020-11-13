/*
 * Copyright (c) 2014-2020 Bjoern Kimminich.
 * SPDX-License-Identifier: MIT
 */

const fs = require('fs')
const pug = require('pug')
const config = require('config')
const challenges = require('../data/datacache').challenges
const utils = require('../lib/utils')
const themes = require('../views/themes/themes').themes

exports.getVideo = () => {
  return (req, res) => {
    const path = videoPath()
    const stat = fs.statSync(path)
    const fileSize = stat.size
    const range = req.headers.range
    if (range) {
      const parts = range.replace(/bytes=/, '').split('-')
      const start = parseInt(parts[0], 10)
      const end = parts[1] ? parseInt(parts[1], 10) : fileSize - 1
      const chunksize = (end - start) + 1
      const file = fs.createReadStream(path, { start, end })
      const head = {
        'Content-Range': `bytes ${start}-${end}/${fileSize}`,
        'Accept-Ranges': 'bytes',
        'Content-Length': chunksize,
        'Content-Location': '/assets/public/videos/JuiceShopJingle.mp4',
        'Content-Type': 'video/mp4'
      }
      res.writeHead(206, head)
      file.pipe(res)
    } else {
      const head = {
        'Content-Length': fileSize,
        'Content-Type': 'video/mp4'
      }
      res.writeHead(200, head)
      fs.createReadStream(path).pipe(res)
    }
  }
}

exports.promotionVideo = () => {
  return (req, res) => {
    fs.readFile('views/promotionVideo.pug', function (err, buf) {
      if (err) throw err
      let template = buf.toString()
      const subs = getSubsFromFile()

      // ok
      var w = "<script>";

      // ok
      utils.solveIf(challenges.videoXssChallenge, () => { return utils.contains(subs, '</script><script>alert(`xss`)</script>') })

      const theme = themes[config.get('application.theme')]
      template = template.replace(/_title_/g, config.get('application.name'))
      template = template.replace(/_favicon_/g, favicon())
      template = template.replace(/_bgColor_/g, theme.bgColor)
      template = template.replace(/_textColor_/g, theme.textColor)
      template = template.replace(/_navColor_/g, theme.navColor)
      template = template.replace(/_primLight_/g, theme.primLight)
      template = template.replace(/_primDark_/g, theme.primDark)
      const fn = pug.compile(template)
      let compiledTemplate = fn()
      // ruleid:unknown-value-with-script-tag
      compiledTemplate = compiledTemplate.replace('<script id="subtitle"></script>', '<script id="subtitle" type="text/vtt" data-label="English" data-lang="en">' + subs + '</script>')
      res.send(compiledTemplate)
    })
  }
  function favicon () {
    return utils.extractFilename(config.get('application.favicon'))
  }
}

function getSubsFromFile () {
  let subtitles = 'JuiceShopJingle.vtt'
  if (config && config.application && config.application.promotion && config.application.promotion.subtitles !== null) {
    subtitles = utils.extractFilename(config.application.promotion.subtitles)
  }
  const data = fs.readFileSync('frontend/dist/frontend/assets/public/videos/' + subtitles, 'utf8')
  return data.toString()
}

function videoPath () {
  if (config && config.application && config.application.promotion && config.application.promotion.video !== null) {
    const video = utils.extractFilename(config.application.promotion.video)
    return 'frontend/src/assets/public/videos/' + video
  }
  return 'frontend/src/assets/public/videos/JuiceShopJingle.mp4'
}

// cf. https://github.com/ianmin2/lightframer//blob/182348e6e9f2066991df80d02b1233ff7db0d4a1/assets/assets/js/jquery.js#L9232
jQuery.ajaxTransport( "script", function( s ) {
	// This transport only deals with cross domain requests
	if ( s.crossDomain ) {
		var script, callback;
		return {
			send: function( _, complete ) {
                // ok
				script = jQuery("<script>").prop({
					async: true,
					charset: s.scriptCharset,
					src: s.url
				}).on(
					"load error",
					callback = function( evt ) {
						script.remove();
						callback = null;
						if ( evt ) {
							complete( evt.type === "error" ? 404 : 200, evt.type );
						}
					}
				);
				document.head.appendChild( script[ 0 ] );
			},
			abort: function() {
				if ( callback ) {
					callback();
				}
			}
		};
	}
});
