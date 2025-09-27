/**
 * JSON Code Base Generator
 * Generates boilerplate JSON configuration files
 */

function generateJSONCodeBase() {
    return `{
  "name": "TSI Header Application",
  "version": "1.0.0",
  "description": "Basic JSON template for configuration or data structure",
  "main": "index.js",
  "scripts": {
    "start": "node index.js",
    "test": "npm test"
  },
  "author": "TSI Student",
  "license": "MIT",
  "dependencies": {},
  "devDependencies": {},
  "keywords": [
    "tsi",
    "template",
    "configuration"
  ],
  "repository": {
    "type": "git",
    "url": "https://github.com/example/tsi-project"
  },
  "config": {
    "environment": "development",
    "debug": true,
    "settings": {
      "theme": "default",
      "language": "en"
    }
  },
  "data": {
    "message": "Hello, World!",
    "timestamp": "2024-01-01T00:00:00.000Z",
    "status": "active"
  }
}`;
}

module.exports = {
    generateJSONCodeBase
};