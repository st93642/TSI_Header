/**
 * Git Ignore Generator
 * Creates .gitignore files optimized for programming languages
 */

let vscode;
try {
    vscode = require('vscode');
} catch (e) {
    // vscode not available (running outside VS Code)
    vscode = null;
}

/**
 * Create .gitignore file for the project
 */
async function createGitIgnoreFile(language, projectUri) {
    if (!vscode) {
        throw new Error('VS Code API not available');
    }

    const gitignoreContent = generateGitIgnoreContent(language);
    const gitignoreUri = vscode.Uri.joinPath(projectUri, '.gitignore');
    
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(gitignoreUri, encoder.encode(gitignoreContent));
}

/**
 * Generate .gitignore content based on language
 */
function generateGitIgnoreContent(language) {
    const commonPatterns = [
        '# TSI Header Extension Generated .gitignore',
        '# Transport and Telecommunication Institute',
        '',
        '# IDE and Editor files',
        '.vscode/settings.json',
        '.vscode/launch.json',
        '.idea/',
        '*.swp',
        '*.swo',
        '*.tmp',
        '*.temp',
        '',
        '# OS generated files',
        '.DS_Store',
        '.DS_Store?',
        '._*',
        '.Spotlight-V100',
        '.Trashes',
        'ehthumbs.db',
        'Thumbs.db',
        '',
        '# Debug files',
        'debug_*',
        '*.log',
        '*.out',
        '',
        '# Temporary files',
        '*.bak',
        '*.orig',
        '*.rej'
    ];

    if (language === 'c' || language === 'cpp') {
        return generateCGitIgnore(commonPatterns);
    }
    
    return commonPatterns.join('\n');
}

/**
 * Generate C/C++ specific .gitignore
 */
function generateCGitIgnore(commonPatterns) {
    const cPatterns = [
        '',
        '# C/C++ specific',
        '# Build artifacts',
        'build/',
        'bin/',
        'obj/',
        '*.o',
        '*.obj',
        '*.exe',
        '*.dll',
        '*.so',
        '*.dylib',
        '*.a',
        '*.lib',
        '',
        '# Compiler intermediate files',
        '*.i',
        '*.s',
        '*.bc',
        '*.ll',
        '',
        '# Debug symbols',
        '*.dSYM/',
        '*.pdb',
        '',
        '# IDE specific',
        '*.cbp',
        '*.depend',
        '*.layout',
        '',
        '# Makefiles',
        'Makefile.backup',
        '',
        '# Autotools',
        'autom4te.cache/',
        'config.log',
        'config.status',
        'configure',
        'Makefile.in',
        '',
        '# CMake',
        'CMakeCache.txt',
        'CMakeFiles/',
        'cmake_install.cmake',
        'install_manifest.txt',
        'compile_commands.json',
        'CTestTestfile.cmake',
        '_deps/',
        '',
        '# Qt',
        '*.pro.user',
        '*.pro.user.*',
        '*.qbs.user',
        '*.qbs.user.*',
        '*.moc',
        'moc_*.cpp',
        'moc_*.h',
        'qrc_*.cpp',
        'ui_*.h',
        'Makefile*',
        '*build-*',
        '',
        '# Visual Studio Code',
        '.vscode/c_cpp_properties.json',
        '.vscode/launch.json',
        '.vscode/tasks.json'
    ];

    return [...commonPatterns, ...cPatterns].join('\n');
}

module.exports = {
    createGitIgnoreFile
};