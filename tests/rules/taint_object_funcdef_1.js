import * as http from 'http';

const httpOptions = urlToHttpOptions(reqUrl);
const host: string = httpOptions.hostname;

// no sanitizeIP call in beforeRedirect
// ruleid: js-req-without-checks
const req: http.ClientRequest = (reqUrl.protocol.startsWith('https') ? https: http).request({
    host, port, method, path, rejectUnauthorized: false, //ok with self signed certs
    timeout: timeoutMS,
    cacert: opts.cacert,
    lookup: getLookupFunction(logger, Boolean(opts.useRoundRobinDns),sanitize),
    beforeRedirect: (opts) => {
        // no sanitizeIP call
    },
})
