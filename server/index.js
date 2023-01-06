const loginRoute = require('./login-api');
const signupRoute = require('./signup-api');
const profileRoute = require('./profile-put-api');
const verifyRoute = require('./verify-put-api');
const socketRoute = require('./socket-api');

exports.routes = [loginRoute, signupRoute, profileRoute, verifyRoute];
