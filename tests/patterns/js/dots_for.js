app.post("/foo", (req, res) => {
    var obj = req.body;

    var ret = [];

    //ERROR: Potential DoS if obj.length is large.
    for (var i = 0; i < obj.length; i++) {
        ret.push(obj[i]);
    }
});
