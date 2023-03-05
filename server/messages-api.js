const { clientPromise } = require('../connect-database');

exports.module = {
    path: '/api/messages',
    method: 'get',
    handler: async (req, res) => {
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

        const findAllMessages = async () => {
            const query = `SELECT * FROM ${
                process.env.NODE_ENV === 'development'
                    ? process.env.ASTRA_DB_KEYSPACE
                    : process.env.ASTRA_DB_KEYSPACE_PROD
            }.chat;`;
            try {
                const result = await client.execute(query, null, { prepare: true });

                return result;
            } catch (ex) {
                console.log('Error in findAllMessages', ex.toString());
            }
        };

        const messagesRow = await findAllMessages();

        if (!messagesRow?.rows?.length) {
            return res.status(200).json([]);
        }

        const modifiedMessagesArray = messagesRow.rows
            .map((row) => {
                const { userid: userId, messages } = row;

                const messagesArray = messages.split('$$');

                return messagesArray.map((m) => {
                    const { message, time, name } = JSON.parse(m);

                    return {
                        name,
                        id: userId,
                        clientId: '123',
                        connectionId: '123',
                        timestamp: time,
                        data: { message },
                    };
                });
            })
            .flat()
            .sort((a, b) => {
                if (a.timestamp > b.timestamp) {
                    return 1;
                } else if (a.timestamp < b.timestamp) {
                    return -1;
                } else {
                    return 0;
                }
            });

        return res.status(200).json(modifiedMessagesArray);
    },
};
