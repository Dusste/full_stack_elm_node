// const { createClient } = require('@astrajs/collections');
const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');
const { v4: uuid } = require('uuid');
const { clientPromise } = require('../nodeJsProject/connect-database');

exports.handler = async function (req) {
    const { body } = req;
    const { email, password } = JSON.parse(body);
    const client = await clientPromise;

    // const astraClient = await createClient({
    //     astraDatabaseId: process.env.ASTRA_DB_ID,
    //     astraDatabaseRegion: process.env.ASTRA_DB_REGION,
    //     applicationToken: process.env.ASTRA_DB_APPLICATION_TOKEN,
    // });

    // const usersCollection = astraClient
    //     .namespace(process.env.ASTRA_DB_KEYSPACE)
    //     .collection('users');

    // const user = await usersCollection.findOne({ email: { $eq: email } });
    const findUser = async (parameters) => {
        const query = `SELECT * FROM ${process.env.ASTRA_DB_KEYSPACE}.users WHERE email = ? ALLOW FILTERING;`;
        try {
            const result = await client.execute(query, parameters, { prepare: true });

            return result;
        } catch (ex) {
            console.log('Error in finduser', ex.toString());
        }
    };
    const user = await findUser([email]);

    if (!user?.rows[0]) {
        return {
            statusCode: 401,
        };
    }

    const { id, isverified, passwordhash, salt, firstname } = user.rows[0];
    const pepper = process.env.PEPPER_STRING;

    const isCorrect = await bcrypt.compare(salt + password + pepper, passwordhash);

    if (isCorrect) {
        const token = jwt.sign({ id, isverified, email, firstname }, process.env.JWT_SECRET, {
            expiresIn: '2h',
        });

        return {
            statusCode: 200,
            body: JSON.stringify({ token }),
        };
    } else {
        return {
            statusCode: 401,
        };
    }
};
