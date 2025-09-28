/**
 * SAS Code Base Generator
 * Generates boilerplate SAS code
 */

function generateSasCodeBase() {
    return `\n/* Basic SAS program */\n/* Note: SAS is used for statistical analysis */\n\n/* Main data step */\ndata hello_world;\n    message = "Hello, World!";\n    description = "This is a basic SAS program.";\n    put message;\n    put description;\nrun;\n\n/* Example proc step */\nproc print data=hello_world;\nrun;\n\n/* Example with variables */\n/* data tsi_students; */\n/*     input name $ program $ year; */\n/*     datalines; */\n/* John CS 2024 */\n/* Jane EE 2023 */\n/* Bob IT 2025 */\n/* ; */\n/* run; */\n\n/* proc means data=tsi_students; */\n/*     var year; */\n/* run; */\n\n/* proc freq data=tsi_students; */\n/*     tables program; */\n/* run; */\n`;
}

module.exports = {
    generateSasCodeBase
};