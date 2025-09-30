const test = require('node:test');
const assert = require('node:assert');
const fs = require('fs');
const path = require('path');
const os = require('os');
const { createHtmlFiles } = require('./htmlProjectCreator');

// Mock VS Code API
const mockVSCode = {
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
                const dirPath = path.dirname(filePath);
                if (!fs.existsSync(dirPath)) {
                    fs.mkdirSync(dirPath, { recursive: true });
                }
                fs.writeFileSync(filePath, content);
            }
        },
        getConfiguration: (section) => ({
            get: (key, defaultValue) => {
                const config = {
                    'tsiheader.username': 'Test User',
                    'tsiheader.email': 'test@example.com'
                };
                return config[`${section}.${key}`] || defaultValue;
            },
            update: (key, value, global) => Promise.resolve()
        })
    },
    Uri: {
        joinPath: (base, ...paths) => {
            const basePath = base.fsPath || base;
            const fullPath = path.join(basePath, ...paths);
            return { fsPath: fullPath };
        }
    }
};

test('HTML Project Creator', async (t) => {
    const tempDir = path.join(os.tmpdir(), 'tsi-html-test-' + Date.now());
    const projectUri = { fsPath: tempDir };
    const projectName = 'test-html-project';

    t.before(async () => {
        // Clean up any existing test directory
        if (fs.existsSync(tempDir)) {
            fs.rmSync(tempDir, { recursive: true, force: true });
        }
        fs.mkdirSync(tempDir, { recursive: true });
    });

    t.after(async () => {
        // Clean up
        if (fs.existsSync(tempDir)) {
            fs.rmSync(tempDir, { recursive: true, force: true });
        }
    });

    await t.test('should create HTML project structure', async () => {
        // Create HTML project files
        await createHtmlFiles(mockVSCode, projectName, projectUri);

        // Check that all expected files are created
        const expectedFiles = [
            'index.html',
            'css/styles.css',
            'js/script.js',
            'package.json',
            'README.md',
            '.gitignore',
            'webpack.config.js',
            '.eslintrc.js',
            '.prettierrc'
        ];

        for (const file of expectedFiles) {
            const filePath = path.join(tempDir, file);
            assert(fs.existsSync(filePath), `File ${file} should be created`);
        }

        // Check that directories are created
        const expectedDirs = ['css', 'js'];
        for (const dir of expectedDirs) {
            const dirPath = path.join(tempDir, dir);
            assert(fs.existsSync(dirPath), `Directory ${dir} should be created`);
            assert(fs.statSync(dirPath).isDirectory(), `${dir} should be a directory`);
        }
    });

    await t.test('should create files with proper TSI headers', async () => {
        // Create HTML project files
        await createHtmlFiles(mockVSCode, projectName, projectUri);

        // Check that files contain TSI headers
        const filesWithHeaders = [
            'index.html',
            'css/styles.css',
            'js/script.js'
        ];

        for (const file of filesWithHeaders) {
            const filePath = path.join(tempDir, file);
            const content = fs.readFileSync(filePath, 'utf8');
            assert(content.includes('TTTTTTTT SSSSSSS II'), `File ${file} should contain TSI header`);
            const fileName = path.basename(file);
            assert(content.includes(fileName), `File ${file} header should contain correct filename`);
        }
    });

    await t.test('should create valid HTML structure', async () => {
        // Create HTML project files
        await createHtmlFiles(mockVSCode, projectName, projectUri);

        const indexPath = path.join(tempDir, 'index.html');
        const content = fs.readFileSync(indexPath, 'utf8');

        // Check for basic HTML structure
        assert(content.includes('<!DOCTYPE html>'), 'Should contain DOCTYPE');
        assert(content.includes('<html lang="en">'), 'Should contain html tag with lang');
        assert(content.includes('<head>'), 'Should contain head tag');
        assert(content.includes('<meta charset="UTF-8">'), 'Should contain charset meta');
        assert(content.includes('<title>'), 'Should contain title tag');
        assert(content.includes('<body>'), 'Should contain body tag');
        assert(content.includes('<div id="app">'), 'Should contain app div');
        assert(content.includes('<link rel="stylesheet"'), 'Should contain stylesheet link');
        assert(content.includes('<script src="js/script.js">'), 'Should contain script tag');
    });

    await t.test('should create valid CSS structure', async () => {
        // Create HTML project files
        await createHtmlFiles(mockVSCode, projectName, projectUri);

        const cssPath = path.join(tempDir, 'css', 'styles.css');
        const content = fs.readFileSync(cssPath, 'utf8');

        // Check for CSS variables and basic structure
        assert(content.includes(':root'), 'Should contain CSS variables');
        assert(content.includes('--primary-color'), 'Should contain primary color variable');
        assert(content.includes('#app'), 'Should contain app selector');
        assert(content.includes('font-family'), 'Should contain font-family');
        assert(content.includes('box-sizing'), 'Should contain box-sizing');
    });

    await t.test('should create valid JavaScript structure', async () => {
        // Create HTML project files
        await createHtmlFiles(mockVSCode, projectName, projectUri);

        const jsPath = path.join(tempDir, 'js', 'script.js');
        const content = fs.readFileSync(jsPath, 'utf8');

        // Check for JavaScript structure
        assert(content.includes('DOMContentLoaded'), 'Should contain DOMContentLoaded event');
        assert(content.includes('initializeApp'), 'Should contain initializeApp function');
        assert(content.includes('setupEventListeners'), 'Should contain setupEventListeners function');
        assert(content.includes('console.log'), 'Should contain console.log');
        assert(content.includes('addEventListener'), 'Should contain addEventListener');
    });

    await t.test('should create valid package.json', async () => {
        // Create HTML project files
        await createHtmlFiles(mockVSCode, projectName, projectUri);

        const packagePath = path.join(tempDir, 'package.json');
        const content = fs.readFileSync(packagePath, 'utf8');

        // Parse JSON and verify structure
        const packageJson = JSON.parse(content);

        assert.equal(packageJson.name, `${projectName.toLowerCase().replace(/[^a-z0-9-]/g, '-')}`);
        assert.equal(packageJson.version, '1.0.0');
        assert(packageJson.description.includes(projectName));
        assert(packageJson.scripts.start, 'Should have start script');
        assert(packageJson.scripts.build, 'Should have build script');
        assert(packageJson.devDependencies['live-server'], 'Should have live-server dependency');
        assert(packageJson.devDependencies.eslint, 'Should have eslint dependency');
    });

    await t.test('should create valid webpack config', async () => {
        // Create HTML project files
        await createHtmlFiles(mockVSCode, projectName, projectUri);

        const webpackPath = path.join(tempDir, 'webpack.config.js');
        const content = fs.readFileSync(webpackPath, 'utf8');

        // Check for webpack configuration
        assert(content.includes('module.exports'), 'Should be a valid webpack config');
        assert(content.includes('entry'), 'Should have entry configuration');
        assert(content.includes('output'), 'Should have output configuration');
        assert(content.includes('mode'), 'Should have mode configuration');
        assert(content.includes('HtmlWebpackPlugin'), 'Should include HTML plugin');
    });

    await t.test('should create valid eslint config', async () => {
        // Create HTML project files
        await createHtmlFiles(mockVSCode, projectName, projectUri);

        const eslintPath = path.join(tempDir, '.eslintrc.js');
        const content = fs.readFileSync(eslintPath, 'utf8');

        // Check for eslint configuration
        assert(content.includes('module.exports'), 'Should be a valid eslint config');
        assert(content.includes('extends'), 'Should have extends configuration');
        assert(content.includes('rules'), 'Should have rules configuration');
        assert(content.includes('env'), 'Should have env configuration');
    });

    await t.test('should create valid prettier config', async () => {
        // Create HTML project files
        await createHtmlFiles(mockVSCode, projectName, projectUri);

        const prettierPath = path.join(tempDir, '.prettierrc');
        const content = fs.readFileSync(prettierPath, 'utf8');

        // Parse JSON and verify structure
        const prettierConfig = JSON.parse(content);

        assert.equal(prettierConfig.semi, true, 'Should have semi set to true');
        assert.equal(prettierConfig.singleQuote, true, 'Should have singleQuote set to true');
        assert.equal(prettierConfig.tabWidth, 4, 'Should have tabWidth set to 4');
        assert.equal(prettierConfig.useTabs, false, 'Should have useTabs set to false');
    });

    await t.test('should create valid gitignore', async () => {
        // Create HTML project files
        await createHtmlFiles(mockVSCode, projectName, projectUri);

        const gitignorePath = path.join(tempDir, '.gitignore');
        const content = fs.readFileSync(gitignorePath, 'utf8');

        // Check for gitignore content
        assert(content.includes('node_modules/'), 'Should ignore node_modules');
        assert(content.includes('.env'), 'Should ignore env files');
        assert(content.includes('dist/'), 'Should ignore dist directory');
        assert(content.includes('.DS_Store'), 'Should ignore macOS files');
        assert(content.includes('*.log'), 'Should ignore log files');
    });

    await t.test('should create comprehensive README', async () => {
        // Create HTML project files
        await createHtmlFiles(mockVSCode, projectName, projectUri);

        const readmePath = path.join(tempDir, 'README.md');
        const content = fs.readFileSync(readmePath, 'utf8');

        // Check for README content
        assert(content.includes(projectName), 'Should include project name');
        assert(content.includes('Installation'), 'Should have installation section');
        assert(content.includes('Usage'), 'Should have usage section');
        assert(content.includes('Development'), 'Should have development section');
        assert(content.includes('Build'), 'Should have build section');
        assert(content.includes('Contributing'), 'Should have contributing section');
        assert(content.includes('License'), 'Should have license section');
    });
});