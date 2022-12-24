// const { createClient } = require('@astrajs/collections');
const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');
const { v4: uuid } = require('uuid');
const { clientPromise } = require('../connect-database');

exports.handler = async function (req) {
    const { body } = req;
    const { authorization } = req.headers;
    const { email, isverified, firstname } = JSON.parse(body);
    const client = await clientPromise;

    if (!client)
        return {
            statusCode: 500,
        };

    // const astraClient = await createClient({
    //     astraDatabaseId: process.env.ASTRA_DB_ID,
    //     astraDatabaseRegion: process.env.ASTRA_DB_REGION,
    //     applicationToken: process.env.ASTRA_DB_APPLICATION_TOKEN,
    // });

    // const usersCollection = astraClient
    //     .namespace(process.env.ASTRA_DB_KEYSPACE)
    //     .collection('users');

    if (!authorization) {
        return {
            statusCode: 401,
            body: 'No authorization header sent',
        };
    }

    const token = authorization.split(' ')[1];
    // const user = await usersCollection.findOne({ email: { $eq: email } });

    let decodedToken;

    try {
        decodedToken = await jwt.verify(token, process.env.JWT_SECRET);
    } catch (err) {
        return {
            statusCode: 401,
            body: err.toString(),
        };
    }
    // async (err, decoded) => {
    //     if (err)
    //         return {
    //             statusCode: 401,
    //             body: 'Unable to verify token',
    //         };

    const { id } = decodedToken;
    // if (id !== userId) {
    //     return {
    //         statusCode: 403,
    //         body: 'Not allowed to update that users data',
    //     };
    // }

    // if (!isverified)
    //     return {
    //         statusCode: 403,
    //         body: 'You need to verify your email before you can update your data',
    //     };

    const updateUser = async (parameters) => {
        const query = `UPDATE ${
            process.env.NODE_ENV === 'development'
                ? process.env.ASTRA_DB_KEYSPACE
                : process.env.ASTRA_DB_KEYSPACE_PROD
        }.users SET email = ?, firstname = ?, isverified = ? WHERE id = ? IF EXISTS;`;
        try {
            const result = await client.execute(query, parameters, { prepare: true });
            //  result would be undefined but query would be executed and entery is wirtten in the DB
            return result;
        } catch (ex) {
            console.log('Error in createUser', ex.toString());
        }
    };

    const {
        rows: [applied],
    } = await updateUser([email, firstname, isverified, id]);

    if (!applied['[applied]']) {
        console.log('Unsuccessfull in updating user');
        return {
            statusCode: 404,
        };
    }

    const findUser = async (parameters) => {
        const query = `SELECT * FROM ${
            process.env.NODE_ENV === 'development'
                ? process.env.ASTRA_DB_KEYSPACE
                : process.env.ASTRA_DB_KEYSPACE_PROD
        }.users WHERE email = ? ALLOW FILTERING;`;
        try {
            const result = await client.execute(query, parameters, { prepare: true });

            return result;
        } catch (ex) {
            console.log('Error in finduser', ex.toString());
        }
    };
    const user = await findUser([email]);
    const {
        id: decodedId,
        isverified: isVerified,
        email: emailFromUser,
        firstname: firstName,
    } = user.rows[0];
    console.log('dusan', user.rows[0]);
    const newToken = jwt.sign(
        { id: decodedId, isverified: isVerified, email: emailFromUser, firstname: firstName },
        process.env.JWT_SECRET,
        { expiresIn: '2h' },
    );

    return {
        statusCode: 200,
        body: JSON.stringify({ token: newToken }),
    };
};
