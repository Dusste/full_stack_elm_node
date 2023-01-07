require('dotenv').config();
const express = require('express');
const path = require('path');
const bodyParser = require('body-parser');
const { routes } = require('./server');

const PORT = process.env.PORT || 8080;

const app = express();

app.use(express.json({ limit: '3mb' }));
app.use(bodyParser.json({ limit: '3mb', extended: true }));
app.use(bodyParser.urlencoded({ limit: '3mb', extended: true }));

routes.forEach((route) => {
    route = route.module;

    app[route.method](route.path, route.handler);
});

app.listen(PORT, () => {
    console.log(`Server is listening on port ${PORT}`);
});
