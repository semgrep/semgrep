while(true) {}
a = 1;
import 'foo';

//this one causes an extra lookahead, to check for an 'else'
//if(true) { }
// this does not
if(true) { } else { }


//this one causes an extra lookahead, to check for a 'finally'
//try { } catch(x) { }
try { } catch(x) { } finally { }

if(true) {} else { }
