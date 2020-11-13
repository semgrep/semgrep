const jwt = require('jsonwebtoken')

User.findOne({name: req.body.name}, function(err, user){
// ruleid: jwt-exposed-data
    var token = jwt.sign(user, key, {expiresIn: 60*60*10});
    res.json({
        success: true,
        message: 'Enjoy your token!',
        token: token
    });
});

User.findOne({name: req.body.name}, function(err, user){
// ok
    const {name, email} = user
    var token = jwt.sign({name, email}, key, {expiresIn: 60*60*10});
    return token;
});

User.findOne({name: req.body.name}, function(err, user){
// ok
    const {name, email} = user
    var token = jwt.sign({name, email}, key, {expiresIn: 60*60*10});
    return token;
});
