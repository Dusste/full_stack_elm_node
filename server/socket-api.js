exports.handler = async function (req) {
    const clientId = req.queryStringParameters['clientId'] || 'NO_CLIENT_ID';

    return {
        statusCode: 200,
        headers: { 'content-type': 'application/json' },
        body: JSON.stringify('123'),
    };
};
