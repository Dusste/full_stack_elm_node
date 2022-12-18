const tournament = require('../tournament.json');

exports.handler = async function (req) {
    return {
        statusCode: 200,
        body: JSON.stringify(tournament),
    };
};
