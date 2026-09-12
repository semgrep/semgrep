/*
  Optional chaining '?.'

  https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Optional_chaining

*/

export function ensureLowerCaseEmail(profile) {
  return typeof profile?.emails?.[0]?.value === 'string'
    ? profile.emails[0].value.toLowerCase()
    : '';
}
