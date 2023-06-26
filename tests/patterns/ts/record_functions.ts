// ERROR: match
const x = foo({
    func: () => {
        return 1;
    },
})

// ERROR: match
const y = foo({
    func: function (opts) {
        return 2;
    },
})

// ERROR: match
const z = foo({
    obj: {
        f: () => {return 1;},
    },
})

// ok: 
const z = foo()