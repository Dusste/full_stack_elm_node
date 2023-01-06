require('dotenv').config();
const express = require('express');
const path = require('path');
const bodyParser = require('body-parser');
const { routes } = require('./server');

const PORT = process.env.PORT || 8080;

const app = express();

app.use(express.json());

routes.forEach((route) => {
    route = route.module;

    app[route.method](route.path, route.handler);
});

app.listen(PORT, () => {
    console.log(`Server is listening on port ${PORT}`);
});
