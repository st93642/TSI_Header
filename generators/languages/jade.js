/**
 * Jade Code Base Generator
 * Generates boilerplate Jade/Pug templates
 */

function generateJadeCodeBase() {
    return `\n//- Basic Jade template\ndoctype html\nhtml(lang="en")\n  head\n    meta(charset="UTF-8")\n    meta(name="viewport", content="width=device-width, initial-scale=1.0")\n    title TSI Header - Basic Jade Template\n    style.\n      body {\n        font-family: Arial, sans-serif;\n        margin: 40px;\n        line-height: 1.6;\n      }\n      .container {\n        max-width: 800px;\n        margin: 0 auto;\n      }\n  body\n    .container\n      header\n        h1 Hello, World!\n        p This is a basic Jade template.\n      main\n        section\n          h2 Welcome to TSI\n          p Transport and Telecommunication Institute\n          ul\n            li Modern education\n            li Innovative technologies\n            li Professional development\n      footer\n        p © 2025 TSI - All rights reserved\n`;
}

module.exports = {
    generateJadeCodeBase
};