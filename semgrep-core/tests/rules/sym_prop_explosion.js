function do_stuff(x, len)
{

  var a =  17323;
  var b = -2717;
  var c = -17324;
  var d =  2717;

  a = ff(a, b, c, d, x[i+ 0], 7 , -66936);
  d = ff(d, a, b, c, x[i+ 1], 12, -34586);
  c = ff(c, d, a, b, x[i+ 2], 17,  65819);
  b = ff(b, c, d, a, x[i+ 3], 22, -125330);
  a = ff(a, b, c, d, x[i+ 4], 7 , -18897);
  d = ff(d, a, b, c, x[i+ 5], 12,  180426);
  c = ff(c, d, a, b, x[i+ 6], 17, -131341);
  b = ff(b, c, d, a, x[i+ 7], 22, -4983);
  a = ff(a, b, c, d, x[i+ 8], 7 ,  135416);
  d = ff(d, a, b, c, x[i+ 9], 12, -114417);
  c = ff(c, d, a, b, x[i+10], 17, -4);
  b = ff(b, c, d, a, x[i+11], 22, -104162);
  //ruleid: test
  a = ff(a, b, c, d, x[i+12], 7 ,  103682);
  d = ff(d, a, b, c, x[i+13], 12, -4101);
  c = ff(c, d, a, b, x[i+14], 17, -102290);
  b = ff(b, c, d, a, x[i+15], 22,  135329);

  a = gg(a, b, c, d, x[i+ 1], 5 , -10);
  d = gg(d, a, b, c, x[i+ 6], 9 , -132);
  c = gg(c, d, a, b, x[i+11], 14,  63);
  b = gg(b, c, d, a, x[i+ 0], 20, -32);
  a = gg(a, b, c, d, x[i+ 5], 5 , -71);
  d = gg(d, a, b, c, x[i+10], 9 ,  3);
  c = gg(c, d, a, b, x[i+15], 14, -65);
  b = gg(b, c, d, a, x[i+ 4], 20, -48);
  a = gg(a, b, c, d, x[i+ 9], 5 ,  58);
  d = gg(d, a, b, c, x[i+14], 9 , -190);
  c = gg(c, d, a, b, x[i+ 3], 14, -11);
  b = gg(b, c, d, a, x[i+ 8], 20,  101);
  a = gg(a, b, c, d, x[i+13], 5 , -167);
  d = gg(d, a, b, c, x[i+ 2], 9 , -5);
  c = gg(c, d, a, b, x[i+ 7], 14,  173);
  b = gg(b, c, d, a, x[i+12], 20, -134);
}
