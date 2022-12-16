/*
// EmptyStatment
;

// BlockStatement
{}

// IfStatement
if (a) b
if (a) b;
if (a) {b}
if (a) {b;}
if (a) {b} else c
if (a) {b;} else c
if (a) {b} else c;
if (a) {b;} else c;
if (a) {b} else {c}
if (a) {b;} else {c}
if (a) {b} else {c;}
if (a) {b;} else {c;}

// LabeledStatement
//foo: 1 // broken in pfff
foo: 1;

// WithStatement
with(a){}
with(a){b}
with(a){b;}

// SwitchStatement
switch(a){}
switch(a){case b:c}
switch(a){case b:c;}
switch(a){default: b}
switch(a){default: b;}

// BreakStatement
switch(a){case b: break}
switch(a){case b: break;}
foo: switch(a){case b: break foo}
foo: switch(a){case b: break foo;}

// WhileStatement
while(a){}
while(a){b}
while(a){b;}

// ContinueStatement
while(a){continue;}
while(a){continue}
foo: while(a){continue foo}
foo: while(a){continue foo;}

// ThrowStatement
//throw a // broken in pfff
throw a;

// TryStatement
try {a} catch(b) {}
try {a;} catch(b) {}
try {a} catch(b) {c}
try {a;} catch(b) {c}
try {a} catch(b) {c;}
try {a;} catch(b) {c;}

// DoWhileStatement
do{}while(a)
//do{}while(a); // broken in pfff
//do{a}while(b) // broken in pfff
//do{a}while(b); // broken in pfff
//do{a;}while(b) // broken in pfff
//do{a;}while(b); // broken in pfff

// ForStatement
for(;;){}
for(a;;){}
for(a;b;){}
for(a;b;c){}
for(a;b;c){d}
for(a;b;c){d;}
for(var a;;){}

// ForInStatement
for(a in b){}
for(a in b){c}
for(var a in b){}

// DebuggerStatement
debugger;

// FunctionDeclaration
function a(){}
function a(b){}
function a(b){c}
function a(b){c;}
function a(b,c){}
function a(b,c){d}
function a(b,c){d;}

// VariableDeclaration
var a
var a;
var a=b
var a=b;
var a,b
var a,b;
var a=b,c
var a=b,c;
var a,b=c
var a,b=c;
var a=b,c=d
var a=b,c=d;

// ThisExpression
//this // broken in pfff
this;

// ArrayExpression
//[] // broken in pfff
[];
//[a] // broken in pfff
[a];
//[a,b] // broken in pfff
[a,b];

// ObjectExpression
({a:b,c:d})
({a:b,c:d});

// FunctionExpression
(function(){})
(function a(){})
(function a(b){})
(function a(b,c){})
(function a(b,c){d})
(function a(b,c){d;})

// SequenceExpression
a,b
a,b;
a,b,c
a,b,c;

// UnaryExpression
!a;
-a;
+a;
~a;
typeof a;
void a;
delete a;

// BinaryExpression
a==b;
a!=b;
a===b;
a!==b;
a<b;
a<=b;
a>b;
a>=b;
a<<b;
a>>b;
a>>>b;
a+b;
a-b;
a*b;
a/b;
a%b;
a|b;
a^b;
a&b;
a in b;
a instanceof b;

// AssignmentExpression
a=b
a=b;

// UpdateExpression
++a;
--a;
a++;
a--;

// LogicalExpression
a||b;
a&&b;

// ConditionalExpression
//a?b:c // Broken in pfff
a?b:c;

// NewExpression
new a;
new a();
new a(b);
new a(b,c);

// CallExpression
a();
a(b);
a(b,c);

// MemberExpression
a.b;
a.b.c;

// Identifier
a;

// Literal
"a";
true;
false;
null;
1;
1.1;
/a/;

// ClassDeclaration
class Foo{}
class Foo{bar(){}}
class Foo{static bar(){}}
class Foo{bar(){} baz(){}}
class Foo extends Bar{}
class Foo extends Bar(){}
*/

// XJS
//<a/>; // Broken in pfff
a=<a/>; // Workaround until pfff supports jsx in an expression statement
a=<a b="1"/>;
a=<a b={c()}/>;
a=<a>text</a>;
a=<a>"text"</a>;
a=<a><b/></a>;
a=<a><b></b></a>;
a=<a>{b()}</a>;
//a=<a>begin{middle()}end</a>; // Broken in pfff
