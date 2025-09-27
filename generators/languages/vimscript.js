/**
 * Vimscript Code Base Generator
 * Generates boilerplate Vimscript code
 */

function generateVimscriptCodeBase() {
    return `\n\" Basic Vimscript code\n\" Note: This is a Vim script for editor automation\n\n\" Main function - entry point\nfunction! HelloWorld()\n    echo \"Hello, World!\"\n    echo \"This is a basic Vimscript program.\"\nendfunction\n\n\" Call main function\ncall HelloWorld()\n\n\" Example function with parameters\nfunction! Greet(name)\n    if a:name ==# ''\n        let name = 'World'\n    else\n        let name = a:name\n    endif\n    return 'Hello, ' . name . '!'\nendfunction\n\n\" Example usage\n\" echo Greet('TSI Student')\n\n\" Example with variables and lists\n\" let languages = ['Vim', 'Python', 'Ruby']\n\" let config = {'version': '1.0', 'debug': 1}\n\n\" for lang in languages\n\"     echo 'Language: ' . lang\n\" endfor\n\n\" echo 'Version: ' . config.version\n`;
}

module.exports = {
    generateVimscriptCodeBase
};