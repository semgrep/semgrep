function Vulnerable1(input) {
    return (
        <SuperDiv>
// ruleid: react-css-injection
            <div style={input}>
                    Hello world
            </div>
        </SuperDiv>
    );
}

function Vulnerable3() {
    const input = loadUserInput();
    return (
        <SuperDiv>
// ruleid: react-css-injection
            <div style={input}>
                    Hello world
            </div>
        </SuperDiv>
    );
}

function Vulnerable4(input) {
// ruleid: react-css-injection
    return React.createElement('div', {style: input}, `foobar`);
}

function OkTest({siteUrl, input}) {
    return (
        <SuperDiv>
// ok: react-css-injection
            <div style={{color: input}}>
                    Hello world
            </div>
        </SuperDiv>
    );
}

function OkTest2(input) {
    let styles = {color: input};
    return (
        <SuperDiv>
// ok: react-css-injection
            <div style={styles}>
                    Hello world
            </div>
        </SuperDiv>
    );
}

function OkTest3(input) {
// ok: react-css-injection
    return React.createElement('div', {style: {width: 100}}, `foobar`);
}
