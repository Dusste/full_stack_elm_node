const { Client } = require('cassandra-driver');

const run = async () => {
    const client = new Client({
        cloud: {
            secureConnectBundle: '.secure-connect-elmappserverlessdb.zip',
        },
        credentials: {
            username: process.env.DB_CREDENTIALS_USERNAME,
            password: process.env.DB_CREDENTIALS_PASSWORD
                
        },
    });
    try {
        await client.connect();

        return client;
    } catch (ex) {
        console.log('Could not establish Client', ex.toString());
    }
};

const client = run();

exports.clientPromise = client;
