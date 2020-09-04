async function execIf(c, s, f) {
    let result = 0;
    //ERROR: match
    try {
  	await exec(c, true);
  	result = 3;
    } catch {}
    
    return result ? s() : f();
}
