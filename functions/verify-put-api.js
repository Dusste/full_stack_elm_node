const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');
const { v4: uuid } = require('uuid');
const { clientPromise } = require('../connect-database');

exports.handler = async function (req) {
    const { httpMethod } = req;
    const { authorization } = req.headers;
    const client = await clientPromise;

    if (httpMethod !== 'PUT')
        return {
            statusCode: 403,
        };

    if (!client)
        return {
            statusCode: 500,
        };

    if (!authorization) {
        return {
            statusCode: 401,
            body: 'No authorization header sent',
        };
    }

    const token = authorization.split(' ')[1];

    let decodedToken;

    try {
        decodedToken = await jwt.verify(token, process.env.JWT_SECRET);
    } catch (err) {
        return {
            statusCode: 401,
            body: err.toString(),
        };
    }

    const { id, email, isverified } = decodedToken;

    const updateUser = async (parameters) => {
        const query = `UPDATE ${
            process.env.NODE_ENV === 'development'
                ? process.env.ASTRA_DB_KEYSPACE
                : process.env.ASTRA_DB_KEYSPACE_PROD
        }.users SET isverified = True WHERE id = ? IF EXISTS;`;
        try {
            const result = await client.execute(query, parameters, { prepare: true });
            //  result would be undefined but query would be executed and entery is wirtten in the DB
            return result;
        } catch (ex) {
            console.log('Error in createUser', ex.toString());
        }
    };

    if (isverified) {
        return {
            statusCode: 200,
            body: JSON.stringify({ token }),
        };
    }

    const {
        rows: [applied],
    } = await updateUser([id]);

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
        verificationstring,
    } = user.rows[0];

    const newToken = jwt.sign(
        {
            id: decodedId,
            isverified: true,
            email: emailFromUser,
            firstname: '',
            verificationstring,
            profilepicurl: '',
        },
        process.env.JWT_SECRET,
        { expiresIn: '2h' },
    );

    if (!newToken)
        return {
            statusCode: 403,
        };

    return {
        statusCode: 200,
        body: JSON.stringify({ token: newToken }),
    };
};
