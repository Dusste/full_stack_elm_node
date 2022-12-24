// const { createClient } = require('@astrajs/collections');
const cassandra = require('cassandra-driver');
const bcrypt = require('bcrypt');
const jwt = require('jsonwebtoken');
const { v4: uuid } = require('uuid');
const { clientPromise } = require('../connect-database');
const sendgrid = require('@sendgrid/mail');

sendgrid.setApiKey(process.env.SENDGRID_API_KEY);

const sendEmail = ({ to, from, subject, text, html = '' }) => {
    const msg = { to, from, subject, text, html };
    return sendgrid.send(msg);
};

exports.handler = async function (req, context) {
    const { body } = req;
    const { email, password } = JSON.parse(body);
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

    if (user?.rows[0]) {
        // conflict error code
        return {
            statusCode: 409,
        };
    }
    const salt = uuid();
    const pepper = process.env.PEPPER_STRING;
    const passwordHash = await bcrypt.hash(salt + password + pepper, 10); // 2 arg is num of iterations to
    const verificationString = uuid();
    const createdId = uuid();

    const createUser = async (parameters) => {
        const query = `INSERT INTO ${process.env.ASTRA_DB_KEYSPACE}.users (id, email, firstname, isadmin, isverified, lastname, passwordhash, salt, verificationstring) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?);`;
        try {
            const result = await client.execute(query, parameters, { prepare: true });
            //  result would be undefined but query would be executed and entery is wirtten in the DB
            return result;
        } catch (ex) {
            console.log('Error in createUser', ex.toString());
        }
    };

    const createdUser = await createUser([
        createdId,
        email,
        '',
        false,
        false,
        '',
        passwordHash,
        salt,
        verificationString,
    ]);

    try {
        await sendEmail({
            to: email,
            from: 'dooshanstevanovic@gmail.com',
            subject: 'Please verify your email',
            text: `Thanks for signin up ! To verify your email click here: http://localhost:8888/verify-email/${verificationString}`,
            html: `<div>
            <h1>Hello !</h1>
            <div>
              <h2>Thanks for signin up ! </h2> 
              <p>To verify your email click here: <a href="http://localhost:8888/verify-email/${verificationString}">http://localhost:8888/verify-email/${verificationString}</a></p>
            </div>
          </div>`,
        });
    } catch (err) {
        console.log('Error in signUp', err.response.body.errors);
        return {
            statusCode: 500,
        };
    }

    // console.log('signup posle', { createdUser });
    // const { id } = createdUser;

    const token = jwt.sign(
        {
            id: createdId,
            isverified: false,
            email,
            firstname: '',
        },
        process.env.JWT_SECRET,
        { expiresIn: '2h' },
    );

    return {
        statusCode: 200,
        body: JSON.stringify({ token }),
    };

    // (err, token) => {
    //     if (err) {
    //         return {
    //             statusCode: 500,
    //             body: JSON.stringify({ err }),
    //         };
    //     }
    //     return {
    //         statusCode: 200,
    //         body: JSON.stringify({ token }),
    //     };
    // },
    // );
};
