'use strict';
const config = require('./app.config');
const privateMethods = {
    initialize(USER) {
        // ruleid: hardcoded-jwt-secret
        const router = require('express').Router(),
        jwt = require('jsonwebtoken');
        if (config) {
            router.route('/register').post((req, res) => {
                USER.findOne({}).exec((error, user) => {
                    if (error)
                        return res.status(400).send({error: error});
                    user.save((error, user) => {
                        if (error) {
                            return res.status(400).send({error: error});
                        } else {
                            const token = jwt.sign({id: user._id}, 'hardcoded-secret');
                            return res.status(201).json({token: token});
                        }
                    });
                });
            });
        }
    }
};
module.exports = privateMethods;
