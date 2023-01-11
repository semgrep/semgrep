//import { module } from 'qunit';
//import Ember from 'ember';
//import startApp from '../helpers/start-app';
//import destroyApp from '../helpers/destroy-app';

// PatNested
const { RSVP: { resolve } } = Ember;

const [ x, { a, b } ] = y;

const [ m, [ n, o ] ] = p;

//export default function(name, options = {}) {
//    module(name, {
//        beforeEach() {
//            this.application = startApp();
//
//            if (options.beforeEach) {
//                return options.beforeEach.apply(this, arguments);
//            }
//        },
//
//        afterEach() {
//            let afterEach = options.afterEach && options.afterEach.apply(this, arguments);
//            return resolve(afterEach).then(() => destroyApp(this.application));
//        },
//    });
//}
