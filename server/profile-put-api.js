// const { createClient } = require('@astrajs/collections');
const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');
const { v4: uuid } = require('uuid');
const { clientPromise } = require('../connect-database');
const querystring = require('querystring').escape;
const initializeApp = require('firebase/app').initializeApp;
const {
    getStorage,
    ref,
    uploadBytes,
    getDownloadURL,
    getStream,
    uploadString,
} = require('firebase/storage');

exports.module = {
    path: '/api/profile',
    method: 'put',
    handler: async (req, res) => {
        const { body } = req;
        const { firstname, imagefile } = body;
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
        if (!isverified && !firstname.length) return res.sendStatus(403);

        const firebaseConfig = {
            apiKey: process.env.FIREBASE_API_KEY,
            authDomain: process.env.FIREBASE_AUTH_DOMAIN,
            projectId: process.env.FIREBASE_PROJECT_ID,
            storageBucket: process.env.FIREBASE_STORAGE_BUCKET,
            messagingSenderId: process.env.FIREBASE_MESSAGING_SENDER_ID,
            appId: process.env.FIREBASE_APP_ID,
            measurementId: process.env.FIREBASE_MEASUREMENT_ID,
        };

        const app = initializeApp(firebaseConfig);
        const storage = getStorage(app);

        let photoUrl = '';

        try {
            if (imagefile.length > 0) {
                await uploadString(
                    ref(storage, `/images/profile-pic-${id}.jpeg`),
                    imagefile,
                    'data_url',
                );
            }
            const downloadUrl = await getDownloadURL(
                ref(storage, `/images/profile-pic-${id}.jpeg`),
            );
            photoUrl =
                downloadUrl.length > 0
                    ? `https://firebasestorage.googleapis.com/v0/b/${
                          process.env.FIREBASE_STORAGE_BUCKET
                      }/o/${querystring(`images/profile-pic-${id}.jpeg`)}?alt=media`
                    : '';
        } catch (err) {
            console.log('DUSAN', err);

            return res.sendStatus(403);
        }

        const updateUser = async (parameters) => {
            const query = `UPDATE ${
                process.env.NODE_ENV === 'development'
                    ? process.env.ASTRA_DB_KEYSPACE
                    : process.env.ASTRA_DB_KEYSPACE_PROD
            }.users SET firstname = ?, avatarurl = ? WHERE id = ? IF EXISTS;`;
            try {
                const result = await client.execute(query, parameters, { prepare: true });
                //  result would be undefined but query would be executed and entery is written in the DB
                return result;
            } catch (ex) {
                console.log('Error in createUser', ex.toString());
            }
        };

        const {
            rows: [applied],
        } = await updateUser([firstname, photoUrl, id]);

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
            firstname: firstName,
            verificationstring,
            avatarurl,
        } = user.rows[0];

        return jwt.sign(
            {
                id: decodedId,
                isverified: isVerified,
                email: emailFromUser,
                firstname: firstName,
                verificationstring,
                profilepicurl: avatarurl || '',
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
