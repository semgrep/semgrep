//https://github.com/mybb/mybb/blob/897593d36d2db00ac09dd0c0379595354538b85a/jscripts/bbcodes_sceditor.js
$(function ($) {
    'use strict';

    $.sceditor.formats.bbcode
        .set('align', {
            html: function (element, attrs, content) {
                // ruleid: raw-html-concat
                var x = ['<div align="', (attrs.defaultattr || 'left'), '">', content, '</div>'].join();

                // ruleid: raw-html-concat
                return '<div align="' + (attrs.defaultattr || 'left') + '">' + content + '</div>';
            },
            isInline: false
        });

      $.sceditor.formats.bbcode.set('quote', {
          format: function (element, content) {
              var author = '',
                  $elm = $(element),
                  $cite = $elm.children('cite').first();
              $cite.html($cite.text());

              if ($cite.length === 1 || $elm.data('author')) {
                  author = $cite.text() || $elm.data('author');

                  $elm.data('author', author);
                  $cite.remove();

                  content = this.elementToBbcode(element);
                  author = '=' + author.replace(/(^\s+|\s+$)/g, '');

                  $elm.prepend($cite);
              }

              if ($elm.data('pid'))
                  author += " pid='" + $elm.data('pid') + "'";

              if ($elm.data('dateline'))
                  author += " dateline='" + $elm.data('dateline') + "'";

              return '[quote' + author + ']' + content + '[/quote]';
          },
          html: function (token, attrs, content) {
              var data = '';

              if (attrs.pid)
                  // ruleid: raw-html-concat
                  data = [data, ' data-pid="', attrs.pid,'"'].join(',');

                  // ruleid: raw-html-concat
                  data += ' data-pid="' + attrs.pid + '"';

              if (attrs.dateline)
                  // ruleid: raw-html-concat
                  data = [data, ' data-dateline="', attrs.dateline, '"'].join(',');

                  // ruleid: raw-html-concat
                  data += ' data-dateline="' + attrs.dateline + '"';

              if (typeof attrs.defaultattr !== "undefined")
                  // ruleid: raw-html-concat
                  data = [data, ' data-dateline="', attrs.dateline, '"'].join(',');

                  // ruleid: raw-html-concat
                  content = '<cite>' + attrs.defaultattr.replace(/ /g, '&nbsp;') + '</cite>' + content;

              return '<blockquote' + data + '>' + content + '</blockquote>';
          },
          quoteType: function (val, name) {
              var quoteChar = val.indexOf('"') !== -1 ? "'" : '"';

              return quoteChar + val + quoteChar;
          },
          breakStart: true,
          breakEnd: true
      });

});

//https://github.com/AmauriC/tarteaucitron.js/blob/92d0af3a93ed807f711862830bc4ead3d84a0752/tarteaucitron.js

var tarteaucitron = {
    "version": 20200730,
    "cdn": cdn,
    "user": {},
    "lang": {},
    "services": {},
    "added": [],
    "idprocessed": [],
    "state": [],
    "launch": [],
    "parameters": {},
    "isAjax": false,
    "reloadThePage": false,
    "events": {
        "init": function () {},
        "load": function () {},
    },
    "number": function () {
        "use strict";
        var cookies = document.cookie.split(';'),
            nb = (document.cookie !== '') ? cookies.length : 0,
            html = '';

        cookies = cookies.sort(function (a, b) {
            namea = a.split('=', 1).toString().replace(/ /g, '');
            nameb = b.split('=', 1).toString().replace(/ /g, '');
            c = (tarteaucitron.cookie.owner[namea] !== undefined) ? tarteaucitron.cookie.owner[namea] : '0';
            d = (tarteaucitron.cookie.owner[nameb] !== undefined) ? tarteaucitron.cookie.owner[nameb] : '0';
            if (c + a > d + b) { return 1; }
            if (c + a < d + b) { return -1; }
            return 0;
        });

        if (document.cookie !== '') {
            for (i = 0; i < nb; i += 1) {
                html += '<li class="tarteaucitronCookiesListMain">';
                // ruleid: raw-html-concat
                html =  [html, '    <div class="tarteaucitronCookiesListRight">', cookies[i].split('=').slice(1).join('='), '</div>'].join();

                // ruleid: raw-html-concat
                html += '    <div class="tarteaucitronCookiesListRight">' + cookies[i].split('=').slice(1).join('=') + '</div>';
                html += '</li>';
            }
        } else {
            html += '<div class="tarteaucitronCookiesListMain">';
            html += '    <div class="tarteaucitronCookiesListLeft"><strong>-</strong></div>';
            html += '    <div class="tarteaucitronCookiesListRight"></div>';
            html += '</div>';
        }
    }
};

//https://github.com/mbraak/jqTree/blob/d6b8d11c4ebd7aa4a60498786bc94724b6f6ffda/lib/dragAndDropHandler.js
var DragElement = /** @class */ (function () {
    function DragElement(nodeName, offsetX, offsetY, $tree) {
        this.offsetX = offsetX;
        this.offsetY = offsetY;
        // ruleid: raw-html-concat
        this.$element = jQuery(["<span class=\"jqtree-title jqtree-dragging\">",nodeName, "</span>"].join());

        // ruleid: raw-html-concat
        this.$element = jQuery("<span class=\"jqtree-title jqtree-dragging\">" + nodeName + "</span>");
        this.$element.css("position", "absolute");
        $tree.append(this.$element);
    }
    DragElement.prototype.move = function (pageX, pageY) {
        this.$element.offset({
            left: pageX - this.offsetX,
            top: pageY - this.offsetY
        });
    };
    DragElement.prototype.remove = function () {
        this.$element.remove();
    };
    return DragElement;
}());

// https://github.com/PrismJS/prism/blob/8403e4537b2fdc23435b7235ad082df1f6e6c6e4/plugins/previewers/prism-previewers.js
(function() {
    new Prism.plugins.Previewer('easing', function (value) {

        value = {
            'linear': '0,0,1,1',
            'ease': '.25,.1,.25,1',
            'ease-in': '.42,0,1,1',
            'ease-out': '0,0,.58,1',
            'ease-in-out':'.42,0,.58,1'
        }[value] || value;

        var p = value.match(/-?\d*\.?\d+/g);

        if(p.length === 4) {
            p = p.map(function(p, i) { return (i % 2? 1 - p : p) * 100; });

            this.querySelector('path').setAttribute('d', 'M0,100 C' + p[0] + ',' + p[1] + ', ' + p[2] + ',' + p[3] + ', 100,0');

            var lines = this.querySelectorAll('line');
            lines[0].setAttribute('x2', p[0]);
            lines[0].setAttribute('y2', p[1]);
            lines[1].setAttribute('x2', p[2]);
            lines[1].setAttribute('y2', p[3]);

            return true;
        }

        return false;
    }, '*', function () {
        // ruleid: raw-html-concat
        this._elt.innerHTML = '<svg viewBox="-20 -20 140 140" width="100" height="100">' +
            '<defs>' +
            '<marker id="prism-previewer-easing-marker" viewBox="0 0 4 4" refX="2" refY="2" markerUnits="strokeWidth">' +
            '<circle cx="2" cy="2" r="1.5" />' +
            '</marker>' +
            '</defs>' +
            '<path d="M0,100 C20,50, 40,30, 100,0" />' +
            '<line x1="0" y1="100" x2="20" y2="50" marker-start="url(' + location.href + '#prism-previewer-easing-marker)" marker-end="url(' + location.href + '#prism-previewer-easing-marker)" />' +
            '<line x1="100" y1="0" x2="40" y2="30" marker-start="url(' + location.href + '#prism-previewer-easing-marker)" marker-end="url(' + location.href + '#prism-previewer-easing-marker)" />' +
            '</svg>';
    });
}());

Object.keys(queries).forEach(function someName(key) {
    value = queries[key];
    if (angular.isDefined(value)) {
    // ok: raw-html-concat
    params.push(key + '=' + value.toString());
    }
});

function BytesFilter($translate) {
    return function(bytes, precision) {
        if (isNaN(parseFloat(bytes)) || !isFinite(bytes)) { return '-'; }
        if (typeof precision === 'undefined') { precision = 1; }
        var units = ['SIZE_BYTES', 'SIZE_KB', 'SIZE_MB', 'SIZE_GB', 'SIZE_TB', 'SIZE_TB'],
                number = Math.floor(Math.log(bytes) / Math.log(1024));

        units = units.map(function (unit) {
            // ok: raw-html-concat
            var x = $translate.instant(['FORM.LABELS.', unit].join());

            // ok: raw-html-concat
            return $translate.instant('FORM.LABELS.' + unit);
        });

        return (bytes / Math.pow(1024, Math.floor(number))).toFixed(precision) +  ' ' + units[number];
    };
}

function urldecode(str) {
    // ok: raw-html-concat
    return decodeURIComponent((str+'').replace(/\+/g, '%20'));
}

