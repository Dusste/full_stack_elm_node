// const { createClient } = require('@astrajs/collections');
const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');
const { v4: uuid } = require('uuid');
const { clientPromise } = require('../connect-database');

exports.module = {
    path: '/api/login',
    method: 'post',
    handler: async (req, res) => {
        const { body } = req;
        const { email, password } = body;

        let client;

        try {
            client = await clientPromise;
        } catch (err) {
            console.log('Client: ', err);
            client = err.toString();
        }

        if (!client || typeof client?.execute !== 'function') {
            return res.status(500).json(`{message: There is no client, payload: ${client}}`);
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

        if (!user?.rows?.length && !user?.rows[0]) {
            return res.sendStatus(401);
        }

        const { id, isverified, passwordhash, salt, firstname, verificationstring, avatarurl } =
            user.rows[0];
        const pepper = process.env.PEPPER_STRING;

        const isCorrect = await bcrypt.compare(salt + password + pepper, passwordhash);

        if (isCorrect) {
            jwt.sign(
                {
                    id,
                    isverified,
                    email,
                    firstname,
                    verificationstring,
                    profilepicurl: avatarurl || '',
                },
                process.env.JWT_SECRET,
                {
                    expiresIn: '2h',
                },
                (err, token) => {
                    if (err) {
                        res.sendStatus(500);
                    }

                    res.status(200).json({ token });
                },
            );
        } else {
            return res.sendStatus(401);
        }
    },
};
