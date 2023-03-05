const { clientPromise } = require('../connect-database');

exports.module = {
    path: '/api/socket',
    method: 'get',
    handler: async (req, res) => {
        const { roomId } = req.query;

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

        const findUserById = async (parameters) => {
            const query = `SELECT * FROM ${
                process.env.NODE_ENV === 'development'
                    ? process.env.ASTRA_DB_KEYSPACE
                    : process.env.ASTRA_DB_KEYSPACE_PROD
            }.users WHERE id = ? ALLOW FILTERING;`;
            try {
                const result = await client.execute(query, parameters, { prepare: true });

                return result;
            } catch (ex) {
                console.log('Error in finduser', ex.toString());
            }
        };

        const createNewMessageEntry = async (parameters) => {
            const query = `INSERT INTO ${
                process.env.NODE_ENV === 'development'
                    ? process.env.ASTRA_DB_KEYSPACE
                    : process.env.ASTRA_DB_KEYSPACE_PROD
            }.chat (userid, messages) VALUES (?, ?);`;
            try {
                const result = await client.execute(query, parameters, { prepare: true });
                //  result would be undefined but query would be executed and entery is wirtten in the DB
                return result;
            } catch (ex) {
                console.log('Error in createNewMessageEntry', ex.toString());
            }
        };

        const findChatUserById = async (parameters) => {
            const query = `SELECT * FROM ${
                process.env.NODE_ENV === 'development'
                    ? process.env.ASTRA_DB_KEYSPACE
                    : process.env.ASTRA_DB_KEYSPACE_PROD
            }.chat WHERE userid = ? ALLOW FILTERING;`;
            try {
                const result = await client.execute(query, parameters, { prepare: true });

                return result;
            } catch (ex) {
                console.log('Error in findChatUserById', ex.toString());
            }
        };

        const updateChatUser = async (parameters) => {
            const query = `UPDATE ${
                process.env.NODE_ENV === 'development'
                    ? process.env.ASTRA_DB_KEYSPACE
                    : process.env.ASTRA_DB_KEYSPACE_PROD
            }.chat SET messages = ? WHERE userid = ? IF EXISTS;`;
            try {
                const result = await client.execute(query, parameters, { prepare: true });
                //  result would be undefined but query would be executed and entery is written in the DB
                return result;
            } catch (ex) {
                console.log('Error in createUser', ex.toString());
            }
        };

        const io = req.app.get('socketio');

        io.once('connection', (socket) => {
            const tokenFragment = socket.handshake.auth.token;

            const users = [];
            socket.on('add user', async (userData) => {
                const { userName, userId } = userData;
                const user = await findUserById([userId]);

                if (!user?.rows?.length && !user?.rows[0]) {
                    return res.sendStatus(401);
                }

                const hasDuplicates = !!users.filter((user) => user.userId === userId).length;

                if (hasDuplicates) return;

                users.push(userData);
                // we store the username in the socket session for this client
                socket.data.user = userData;

                io.emit('user joined', {
                    username: socket.data.user.userName,
                    numUsers: users.length,
                });
            });
            socket.on('new message', async (msg) => {
                const chatUser = await findChatUserById([socket.data.user.userId]);
                const messageEntry = {
                    name: socket.data.user.userName,
                    message: msg,
                    time: Date.now(),
                };

                if (!chatUser?.rows?.length && !chatUser?.rows[0]) {
                    const stringifiedMsg = JSON.stringify(messageEntry);

                    await createNewMessageEntry([socket.data.user.userId, stringifiedMsg]);
                } else {
                    const { messages } = chatUser.rows[0];
                    const extendedMessages = messages + '$$' + JSON.stringify(messageEntry);

                    const {
                        rows: [applied],
                    } = await updateChatUser([extendedMessages, socket.data.user.userId]);

                    if (!applied['[applied]']) {
                        console.log('Unsuccessfull in updating chat user');
                    }
                }

                io.emit('new message', { message: msg, userName: socket.data.user.userName });
            });

            // when the client emits 'typing', we broadcast it to others
            socket.on('typing', () => {
                console.log('TYPEING');
                io.emit('typing', {
                    userName: socket.data.user.userName,
                });
            });

            // when the client emits 'stop typing', we broadcast it to others
            socket.on('stop typing', () => {
                io.emit('stop typing', {
                    userName: socket.data.user.userName,
                });
            });

            // when the user disconnects.. perform this
            socket.on('disconnect', (data) => {
                // if (addedUser) {
                console.log('DISCONNECT', data);

                // socket.data.user = {};
                console.log('USERS_IN_ROOM', users);
                console.log('USER', socket.data.user);

                // echo globally that this client has left
                io.emit('user left', {
                    username: socket.data.user.userName,
                    numUsers: users.lengt - 1,
                    // numUsers: users.filter(user => user.userId !== socket.data.user.userId).length,
                });
                // }
            });
        });

        return res.status(200).json({ roomId });
    },
};
