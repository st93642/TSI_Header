#!/usr/bin/env node

const fs = require('fs');
const path = require('path');
const { spawn } = require('child_process');

const projectRoot = path.resolve(__dirname, '..');
const ignoreDirs = new Set(['node_modules', '.git', '.github', '.vscode']);
const testFiles = [];

const discoverTests = (dir) => {
    const entries = fs.readdirSync(dir, { withFileTypes: true });

    for (const entry of entries) {
        if (entry.isDirectory()) {
            if (ignoreDirs.has(entry.name)) {
                continue;
            }
            discoverTests(path.join(dir, entry.name));
            continue;
        }

        if (entry.isFile() && entry.name.endsWith('.test.js')) {
            const fullPath = path.join(dir, entry.name);
            testFiles.push(path.relative(projectRoot, fullPath));
        }
    }
};

const runTestFile = (relativePath) => {
    return new Promise((resolve, reject) => {
        const fullPath = path.join(projectRoot, relativePath);
        const child = spawn(process.execPath, [fullPath], {
            stdio: 'inherit',
            cwd: projectRoot
        });

        child.on('exit', (code) => {
            resolve({ file: relativePath, code });
        });

        child.on('error', (error) => {
            reject(error);
        });
    });
};

(async () => {
    discoverTests(projectRoot);
    testFiles.sort();

    if (testFiles.length === 0) {
        console.log('No Node-based test files found.');
        process.exit(0);
    }

    const priorityFiles = [
        path.join('core', 'src', 'storage', 'globalStateStore.test.js')
    ];
    const orderedTests = [
        ...priorityFiles.filter((file) => testFiles.includes(file)),
        ...testFiles.filter((file) => !priorityFiles.includes(file))
    ];

    console.log('═'.repeat(60));
    console.log('  🚀 Running Node-based test suites');
    console.log('═'.repeat(60));
    console.log(`Discovered ${testFiles.length} test file(s). Running sequentially...`);

    let failures = 0;

    for (const file of orderedTests) {
        console.log(`\n🧪 ${file}`);
        console.log('─'.repeat(60));
        const result = await runTestFile(file);
        if (result.code === 0) {
            console.log(`✅ ${file} passed`);
        } else {
            failures++;
            console.log(`❌ ${file} failed (exit code ${result.code})`);
        }
    }

    console.log('\n' + '═'.repeat(60));
    console.log('  📊 Test Summary');
    console.log('═'.repeat(60));
    console.log(`Total test files: ${testFiles.length}`);
    console.log(`Passed: ${testFiles.length - failures}`);
    console.log(`Failed: ${failures}`);
    console.log('═'.repeat(60));

    process.exit(failures === 0 ? 0 : 1);
})().catch((error) => {
    console.error('Fatal error running tests:', error);
    process.exit(1);
});
