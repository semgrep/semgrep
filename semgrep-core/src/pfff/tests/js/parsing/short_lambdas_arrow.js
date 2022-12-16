//https://people.mozilla.org/~jorendorff/es6-draft.html#sec-arrow-function-definitions

// Expression bodies
var odds = evens.map(v => v + 1);

var nums = evens.map((v, i) => v + i);


// Statement bodies
nums.forEach(v => {
  if (v % 5 === 0)
    fives.push(v);
});


// Lexical this
var bob = {
  _name: "Bob",
  _friends: [],
  printFriends: function () {
    this._friends.forEach(f =>
      console.log(this._name + " knows " + f));
  }
};
