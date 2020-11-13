function test1(body) {
    // ruleid: xml2json-xxe
    const xml2json = require('xml2json')
    const result = xml2json.toJson(body, { object: true, arrayNotation: true })
    return result
}

function okTest1() {
    // ok
    const xml2json = require('xml2json')
    const result = xml2json.toJson('<xml></xml>', { object: true, arrayNotation: true })
    return result
}

function okTest1() {
    // ok
    const xml2json = require('xml2json')
    let body = '<xml></xml>'
    const result = xml2json.toJson(body, { object: true, arrayNotation: true })
    return result
}
