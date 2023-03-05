require('dotenv').config();
const express = require('express');
const path = require('path');
const { createServer } = require('http');
const app = express();
const { Server } = require('socket.io');
const bodyParser = require('body-parser');
const { routes } = require('./server');

const PORT = process.env.PORT || 7999;
const httpServer = createServer(app);
const io = new Server(httpServer, {
    cors: {
        origin: ['http://localhost:8000'],
    },
});

app.set('socketio', io);

app.use(express.json({ limit: '3mb' }));
app.use(bodyParser.json({ limit: '3mb', extended: true }));
app.use(bodyParser.urlencoded({ limit: '3mb', extended: true }));

routes.forEach((route) => {
    route = route.module;

    app[route.method](route.path, route.handler);
});

httpServer.listen(PORT, () => {
    console.log(`Server is listening on port ${PORT}`);
});
