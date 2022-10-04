//ERROR:
api("literal");

const LITERAL = "literal";
//ERROR:
api(LITERAL);

const LIT = "lit";
//ERROR:
api(LIT + "eral");

const LIT = "lit";
//ERROR:
api(`${LIT}eral`);
