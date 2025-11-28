const headerCommands = require('./headerCommands');
const codeGenerationCommands = require('./codeGenerationCommands');
const projectCommands = require('./projectCommands');
const learnCommands = require('./learnCommands');

function registerAll(context, deps) {
    headerCommands.register(context, deps);
    codeGenerationCommands.register(context, deps);
    projectCommands.register(context, deps);
    learnCommands.register(context, deps);
}

module.exports = {
    registerAll,
    headerCommands,
    codeGenerationCommands,
    projectCommands,
    learnCommands
};
