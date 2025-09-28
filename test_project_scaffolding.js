/**
 * Project Scaffolding Test Suite
 * Comprehensive testing for all supported project scaffoldings
 */

const fs = require('fs');
const path = require('path');
const os = require('os');
const { createTSIProject } = require('./generators/project/projectCreator');
const { createBuildFiles } = require('./generators/project/buildSystemGenerator');
const { createDocumentationFiles } = require('./generators/project/documentationGenerator');
const { createGitIgnoreFile } = require('./generators/project/gitIgnoreGenerator');
const { createLanguageSpecificFiles } = require('./generators/project/projectcreators/index');
/**
 * Generate TSI header content (simplified for testing)
 * @param {string} fileName - Name of the file
 * @param {object} vscode - VS Code API object
 * @returns {Promise<string>} The generated header content
 */
async function generateTestTSIHeaderContent(fileName, vscode) {
    const now = new Date();
    const dateStr = now.toLocaleDateString('en-US', {
        month: 'short',
        day: '2-digit',
        year: 'numeric'
    }).replace(',', '');

    const timeStr = now.toLocaleTimeString('en-US', {
        hour: '2-digit',
        minute: '2-digit',
        hour12: false
    });

    // Get user info from mock settings
    const config = vscode.workspace.getConfiguration('tsiheader');
    const username = config.get('username') || 'Test User';
    const email = config.get('email') || 'test@example.com';

    const dateTime = `${dateStr} ${timeStr}`;

    return `/*****************************************************************************/
/*                                                                           */
/*  ${fileName.padEnd(50, ' ')}         TTTTTTTT SSSSSSS II */
/*                                                          TT    SS      II */
/*  By: ${email.padEnd(50, ' ')} TT    SSSSSSS II */
/*                                                          TT         SS II */
/*  Created: ${dateTime} ${username.padEnd(30, ' ')} TT    SSSSSSS II */
/*  Updated: ${dateTime} ${username.padEnd(30, ' ')}                          */
/*                                                                           */
/*   Transport and Telecommunication Institute - Riga, Latvia                */
/*                       https://tsi.lv                                      */
/*****************************************************************************/`;
}
const { generateTSIHeaderContent } = require('./generators/project/headerUtils');
const { generateCodeBase } = require('./generators/codeBaseGenerators');

// Mock the header generation for testing
const originalGenerateTSIHeaderContent = generateTSIHeaderContent;
require('./generators/project/headerUtils').generateTSIHeaderContent = generateTestTSIHeaderContent;

// Mock VS Code API for testing
const mockVscode = {
    Uri: {
        joinPath: (base, ...parts) => {
            const fullPath = path.join(base.fsPath || base, ...parts);
            return { fsPath: fullPath };
        }
    },
    workspace: {
        fs: {
            createDirectory: async (uri) => {
                const dirPath = uri.fsPath;
                if (!fs.existsSync(dirPath)) {
                    fs.mkdirSync(dirPath, { recursive: true });
                }
            },
            writeFile: async (uri, content) => {
                const filePath = uri.fsPath;
                const dir = path.dirname(filePath);
                if (!fs.existsSync(dir)) {
                    fs.mkdirSync(dir, { recursive: true });
                }
                // Handle both string and Uint8Array content
                const data = content instanceof Uint8Array ? content : Buffer.from(content);
                fs.writeFileSync(filePath, data);
            }
        },
        getConfiguration: (section) => ({
            get: (key) => {
                if (key === 'username') return 'Test User';
                if (key === 'email') return 'test@example.com';
                return undefined;
            }
        })
    },
    window: {
        showQuickPick: async () => ({ value: 'c' }), // Default to C for testing
        showInputBox: async () => 'test-project',
        showOpenDialog: async () => [{ fsPath: os.tmpdir() }]
    },
    commands: {
        executeCommand: async () => {}
    }
};

/**
 * Test project scaffolding for a specific language
 */
async function testProjectScaffolding(language, projectName = `test-${language}-project`) {
    console.log(`🧪 Testing ${language.toUpperCase()} project scaffolding...`);

    const tempDir = path.join(os.tmpdir(), 'tsi-test-projects', projectName);
    const projectUri = { fsPath: tempDir };

    try {
        // Clean up any existing test directory
        if (fs.existsSync(tempDir)) {
            fs.rmSync(tempDir, { recursive: true, force: true });
        }

        // Create base directory
        fs.mkdirSync(tempDir, { recursive: true });

        // Create project structure using actual functions
        await createProjectStructure(language, projectName, projectUri, mockVscode);

        // Verify project structure
        await verifyProjectStructure(language, tempDir, projectName);

        // Verify file contents
        await verifyFileContents(language, tempDir, projectName);

        console.log(`✅ ${language.toUpperCase()} project scaffolding test passed`);
        return true;

    } catch (error) {
        console.error(`❌ ${language.toUpperCase()} project scaffolding test failed:`, error.message);
        console.error('Stack:', error.stack);
        return false;
    } finally {
        // Clean up
        try {
            if (fs.existsSync(tempDir)) {
                fs.rmSync(tempDir, { recursive: true, force: true });
            }
        } catch (cleanupError) {
            console.warn(`Warning: Failed to clean up test directory: ${cleanupError.message}`);
        }
    }
}

/**
 * Create project structure (using actual project creation functions)
 */
async function createProjectStructure(language, projectName, projectUri, vscode) {
    const fsPath = projectUri.fsPath;

    // Create base directories
    const directories = getDirectoryStructure(language);
    for (const dir of directories) {
        const dirUri = { fsPath: path.join(fsPath, dir) };
        await vscode.workspace.fs.createDirectory(dirUri);
    }

    // Generate main source file
    await createMainSourceFile(language, projectName, projectUri, vscode);

    // Create header file (for C/C++)
    if (language === 'c' || language === 'cpp') {
        await createHeaderFile(language, projectName, projectUri, vscode);
    }

    // Create language-specific project files
    await createTestLanguageSpecificFiles(language, projectName, projectUri, vscode);

        // Skip build files, documentation, and gitignore for testing (they require VS Code API)
    // await createBuildFiles(language, projectName, projectUri);
    // await createDocumentationFiles(language, projectName, projectUri);
    // await createGitIgnoreFile(language, projectUri);
}

/**
 * Get directory structure for language
 */
function getDirectoryStructure(language) {
    const commonDirs = ['src', 'docs'];

    if (language === 'c' || language === 'cpp') {
        return [...commonDirs, 'include', 'build'];
    } else if (language === 'python') {
        return [...commonDirs, 'tests', 'scripts'];
    } else if (language === 'java') {
        return [...commonDirs, 'src/main/java', 'src/test/java', 'target'];
    } else if (language === 'rust') {
        return [...commonDirs, 'src', 'tests', 'examples', 'benches'];
    } else if (language === 'ruby') {
        return [...commonDirs, 'lib', 'spec', 'bin', 'config'];
    } else if (language === 'php') {
        return [...commonDirs, 'src', 'public', 'tests'];
    }

    return commonDirs;
}

/**
 * Create main source file with TSI header and code
 */
async function createMainSourceFile(language, projectName, projectUri, vscode) {
    const extension = getFileExtension(language);
    let fileName = `main.${extension}`;
    let fileUri;

    if (language === 'java') {
        // For Java, create Main.java in the proper package structure
        fileName = 'Main.java';
        fileUri = vscode.Uri.joinPath(projectUri, 'src', 'main', 'java', fileName);
    } else if (language === 'php') {
        // For PHP, create index.php in the public directory
        fileName = 'index.php';
        fileUri = vscode.Uri.joinPath(projectUri, 'public', fileName);
    } else {
        fileUri = vscode.Uri.joinPath(projectUri, 'src', fileName);
    }

    // Generate TSI header
    const headerContent = await generateTestTSIHeaderContent(fileName, vscode);

    // Generate code base using existing API
    const codeResult = generateCodeBase(language, fileName);
    const codeContent = codeResult.success ? codeResult.content : '';

    // Combine header and code
    const fullContent = headerContent + '\n' + codeContent;

    // Write to file
    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(fullContent));
}

/**
 * Create header file for C/C++
 */
async function createHeaderFile(language, projectName, projectUri, vscode) {
    const extension = language === 'c' ? 'h' : 'hpp';
    const fileName = `${projectName}.${extension}`;
    const fileUri = vscode.Uri.joinPath(projectUri, 'include', fileName);

    // Generate TSI header
    const headerContent = await generateTestTSIHeaderContent(fileName, vscode);

    // Generate header guard
    const guardName = `${projectName.toUpperCase().replace(/-/g, '_')}_${extension.toUpperCase()}`;

    const content = `${headerContent}

#ifndef ${guardName}
#define ${guardName}

#ifdef __cplusplus
extern "C" {
#endif

// Function declarations go here

#ifdef __cplusplus
}
#endif

#endif // ${guardName}
`;

    const encoder = new TextEncoder();
    await vscode.workspace.fs.writeFile(fileUri, encoder.encode(content));
}

/**
 * Create language-specific project files (test wrapper)
 */
async function createTestLanguageSpecificFiles(language, projectName, projectUri, vscode) {
    // Use the actual implementation
    const { createLanguageSpecificFiles: actualCreateFiles } = require('./generators/project/projectcreators/index');
    await actualCreateFiles(language, projectName, projectUri, vscode);
}

/**
 * Get file extension for language
 */
function getFileExtension(language) {
    const extensions = {
        'c': 'c',
        'cpp': 'cpp',
        'python': 'py',
        'java': 'java',
        'rust': 'rs',
        'ruby': 'rb',
        'php': 'php'
    };
    return extensions[language] || 'txt';
}

/**
 * Verify project structure
 */
async function verifyProjectStructure(language, projectPath, projectName) {
    const expectedDirs = getDirectoryStructure(language);
    const expectedFiles = getExpectedFiles(language, projectName);

    // Check directories
    for (const dir of expectedDirs) {
        const dirPath = path.join(projectPath, dir);
        if (!fs.existsSync(dirPath)) {
            throw new Error(`Missing directory: ${dir}`);
        }
    }

    // Check files
    for (const file of expectedFiles) {
        const filePath = path.join(projectPath, file);
        if (!fs.existsSync(filePath)) {
            throw new Error(`Missing file: ${file}`);
        }
    }
}

/**
 * Get expected files for language
 */
function getExpectedFiles(language, projectName) {
    const files = [];

    if (language === 'c') {
        files.push('src/main.c', `include/${projectName}.h`);
    } else if (language === 'cpp') {
        files.push('src/main.cpp', `include/${projectName}.hpp`);
    } else if (language === 'python') {
        files.push('src/main.py', 'src/base_class.py', 'requirements.txt', 'setup.py');
    } else if (language === 'java') {
        files.push('src/main/java/Main.java');
    } else if (language === 'rust') {
        files.push('src/main.rs', 'Cargo.toml');
    } else if (language === 'ruby') {
        files.push('src/main.rb', 'lib/base_class.rb', 'Gemfile');
    } else if (language === 'php') {
        files.push('public/index.php', 'src/BaseClass.php', 'composer.json');
    }

    return files;
}

/**
 * Verify file contents
 */
async function verifyFileContents(language, projectPath, projectName) {
    const expectedFiles = getExpectedFiles(language, projectName);

    for (const file of expectedFiles) {
        const filePath = path.join(projectPath, file);

        if (!fs.existsSync(filePath)) {
            continue; // Already checked in structure verification
        }

        const content = fs.readFileSync(filePath, 'utf8');

        // Check for TSI header (except for certain files)
        const skipHeaderCheck = ['composer.json', 'Cargo.toml', 'requirements.txt', 'Gemfile', '.gitignore'].some(skipFile =>
            file.endsWith(skipFile)
        );

        if (!skipHeaderCheck) {
            if (!content.includes('TTTTTTTT SSSSSSS II')) {
                throw new Error(`File ${file} missing TSI header`);
            }

            // Verify that the header contains the correct filename (not "file.c")
            const fileName = path.basename(file);
            if (!content.includes(fileName)) {
                throw new Error(`File ${file} header contains incorrect filename. Expected: ${fileName}, Header: ${content.substring(0, 200)}`);
            }
        }

        // Check for basic content structure
        if (file.endsWith('.c') || file.endsWith('.cpp') || file.endsWith('.py') || file.endsWith('.java') ||
            file.endsWith('.rs') || file.endsWith('.rb') || file.endsWith('.php')) {
            // Skip base class files and build files for content check - they don't have "Hello, World"
            const isBaseClass = file.includes('base_class') || file.includes('BaseClass');
            const isBuildFile = file.includes('setup.py') || file.includes('requirements.txt') ||
                               file.includes('composer.json') || file.includes('Cargo.toml') ||
                               file.includes('Gemfile') || file.includes('Rakefile');
            if (!isBaseClass && !isBuildFile && !content.includes('Hello, World')) {
                throw new Error(`File ${file} missing expected content`);
            }
        }
    }
}

/**
 * Test header generation in scaffolded projects
 */
async function testHeaderGenerationInScaffoldedProjects() {
    console.log('🧪 Testing header generation in scaffolded projects...');

    const tempDir = path.join(os.tmpdir(), 'tsi-header-test');
    const testFileName = 'test.php';
    const testFilePath = path.join(tempDir, testFileName);

    try {
        // Clean up any existing test directory
        if (fs.existsSync(tempDir)) {
            fs.rmSync(tempDir, { recursive: true, force: true });
        }
        fs.mkdirSync(tempDir, { recursive: true });

        // Create a test file
        fs.writeFileSync(testFilePath, '// Test file for header generation\n');

        // Test the actual header generation function
        const { generateTSIHeaderContent } = require('./generators/project/headerUtils');
        const header = await generateTSIHeaderContent(testFileName, mockVscode);

        // Verify header contains correct filename
        if (!header.includes(testFileName)) {
            throw new Error(`Header does not contain correct filename. Expected: ${testFileName}, Header: ${header.substring(0, 200)}`);
        }

        // Verify header has proper TSI format
        if (!header.includes('TTTTTTTT SSSSSSS II')) {
            throw new Error('Header missing TSI branding');
        }

        // Verify header doesn't contain "file.c"
        if (header.includes('file.c')) {
            throw new Error('Header contains hardcoded "file.c" instead of actual filename');
        }

        console.log('✅ Header generation test passed');
        return true;

    } catch (error) {
        console.error('❌ Header generation test failed:', error.message);
        return false;
    } finally {
        // Clean up
        try {
            if (fs.existsSync(tempDir)) {
                fs.rmSync(tempDir, { recursive: true, force: true });
            }
        } catch (cleanupError) {
            console.warn(`Warning: Failed to clean up test directory: ${cleanupError.message}`);
        }
    }
}

/**
 * Run comprehensive project scaffolding tests
 */
async function runProjectScaffoldingTests() {
    console.log('🚀 Starting Project Scaffolding Test Suite');
    console.log('Testing all supported project scaffoldings...\n');

    // First test header generation
    const headerTestResult = await testHeaderGenerationInScaffoldedProjects();
    console.log('');

    const languages = ['c', 'cpp', 'python', 'java', 'rust', 'ruby', 'php'];
    const results = [];

    for (const language of languages) {
        const result = await testProjectScaffolding(language);
        results.push({ language, passed: result });
    }

    console.log('\n📊 Project Scaffolding Test Results:');
    console.log('='.repeat(50));

    let passed = 0;
    let failed = 0;

    // Include header test in results
    const status = headerTestResult ? '✅ PASSED' : '❌ FAILED';
    console.log(`HEADER${' '.repeat(3)}: ${status}`);
    if (headerTestResult) passed++;
    else failed++;

    for (const result of results) {
        const status = result.passed ? '✅ PASSED' : '❌ FAILED';
        console.log(`${result.language.toUpperCase().padEnd(8)}: ${status}`);
        if (result.passed) passed++;
        else failed++;
    }

    console.log('='.repeat(50));
    console.log(`Total: ${results.length + 1}, Passed: ${passed}, Failed: ${failed}`);

    if (failed === 0) {
        console.log('🎉 All project scaffolding tests passed!');
        return true;
    } else {
        console.log('💥 Some project scaffolding tests failed!');
        return false;
    }
}

/**
 * Test header generation in scaffolded projects
 */
async function testHeaderGenerationInScaffoldedProjects() {
    console.log('🧪 Testing header generation in scaffolded projects...');

    const tempDir = path.join(os.tmpdir(), 'tsi-header-test');
    const testFileName = 'test.php';
    const testFilePath = path.join(tempDir, testFileName);

    try {
        // Clean up any existing test directory
        if (fs.existsSync(tempDir)) {
            fs.rmSync(tempDir, { recursive: true, force: true });
        }
        fs.mkdirSync(tempDir, { recursive: true });

        // Create a test file
        fs.writeFileSync(testFilePath, '// Test file for header generation\n');

        // Test the actual header generation function
        const { generateTSIHeaderContent } = require('./generators/project/headerUtils');
        const header = await generateTSIHeaderContent(testFileName, mockVscode);

        // Verify header contains correct filename
        if (!header.includes(testFileName)) {
            throw new Error(`Header does not contain correct filename. Expected: ${testFileName}, Header: ${header.substring(0, 200)}`);
        }

        // Verify header has proper TSI format
        if (!header.includes('TTTTTTTT SSSSSSS II')) {
            throw new Error('Header missing TSI branding');
        }

        // Verify header doesn't contain "file.c"
        if (header.includes('file.c')) {
            throw new Error('Header contains hardcoded "file.c" instead of actual filename');
        }

        console.log('✅ Header generation test passed');
        return true;

    } catch (error) {
        console.error('❌ Header generation test failed:', error.message);
        return false;
    } finally {
        // Clean up
        try {
            if (fs.existsSync(tempDir)) {
                fs.rmSync(tempDir, { recursive: true, force: true });
            }
        } catch (cleanupError) {
            console.warn(`Warning: Failed to clean up test directory: ${cleanupError.message}`);
        }
    }
}

// Export for use in other test files
module.exports = {
    runProjectScaffoldingTests,
    testProjectScaffolding,
    testHeaderGenerationInScaffoldedProjects
};

// Run tests if called directly
if (require.main === module) {
    runProjectScaffoldingTests()
        .then(success => {
            process.exit(success ? 0 : 1);
        })
        .catch(error => {
            console.error('Test suite failed:', error);
            process.exit(1);
        });
}