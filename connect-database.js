const { Client } = require('cassandra-driver');

const run = async () => {
    const client = new Client({
        cloud: {
            secureConnectBundle:
                process.env.NODE_ENV === 'development'
                    ? process.env.PATH_TO_DB
                    : process.env.PATH_TO_DB_PROD,
        },
        credentials: {
            username:
                process.env.NODE_ENV === 'development'
                    ? process.env.DEV_DB_CREDENTIALS_USERNAME
                    : process.env.DB_CREDENTIALS_USERNAME,
            password:
                process.env.NODE_ENV === 'development'
                    ? process.env.DEV_DB_CREDENTIALS_PASSWORD
                    : process.env.DB_CREDENTIALS_PASSWORD,
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
