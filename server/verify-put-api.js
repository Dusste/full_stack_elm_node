const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');
const { v4: uuid } = require('uuid');
const { clientPromise } = require('../connect-database');

exports.module = {
    path: '/api/verify',
    method: 'put',
    handler: async (req, res) => {
        const { authorization } = req.headers;
        const client = await clientPromise;

        if (!client)
            return res.status(500).json(`{message: There is no client, payload: ${client}}`);

        if (!authorization) {
            return res.status(401).json('No authorization header sent');
        }

        const token = authorization.split(' ')[1];

        let decodedToken;

        try {
            decodedToken = await jwt.verify(token, process.env.JWT_SECRET);
        } catch (err) {
            return res.sendStatus(401);
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
            return res.status(200).json(token);
        }

        const {
            rows: [applied],
        } = await updateUser([id]);

        if (!applied['[applied]']) {
            console.log('Unsuccessfull in updating user');
            return res.sendStatus(404);
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

        return jwt.sign(
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
            (err, token) => {
                if (err) {
                    return res.status(200).json(err);
                }
                res.status(200).json({ token });
            },
        );
    },
};
