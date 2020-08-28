  beforeEach(inject(function($filter) {
    items = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'];
    str = 'tuvwxyz';
    number = 100.045;
    arrayLike = {
      0: 'a',
      1: 'b',
      2: 'c',
      3: 'd',
      4: 'e',
      5: 'f',
      6: 'g',
      7: 'h',
      get length() {
        return Object.keys(this).length - 1;
      }
    };
    limitTo = $filter('limitTo');
  }));

  this.location = {
    get href() {
      return committedHref.href;
    },
    set href(value) {
      locationHref.href = value;
      mockWindow.history.state = null;
      historyEntriesLength++;
      if (!options.updateAsync) this.flushHref();
    },
    get hash() {
      return getHash(committedHref.href);
    },
    set hash(value) {
      locationHref.href = replaceHash(locationHref.href, value);
      if (!options.updateAsync) this.flushHref();
    },
    replace: function(url) {
      locationHref.href = url;
      mockWindow.history.state = null;
      if (!options.updateAsync) this.flushHref();
    },
    flushHref: function() {
      committedHref.href = locationHref.href;
    }
  };
