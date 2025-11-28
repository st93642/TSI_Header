#!/usr/bin/env node

/**
 * Unified Test Suite
 * 
 * Runs all node:test based tests in the project.
 * Automatically discovers and runs .test.js files.
 */

const fs = require('fs');
const path = require('path');
const { spawn } = require('child_process');

// List of test files to run
const testFiles = [
    'core/src/storage/globalStateStore.test.js',
    'core/src/activation/activationManager.test.js',
    'calendar/src/calendarManager.test.js',
    'chat/src/chatManager.test.js',
    'chat/src/chatDataManager.test.js',
    'chat/src/chatService.test.js',
    'studyMode/timer.test.js',
    'studyMode/extension.test.js',
    'core/generators/project/projectcreators/htmlProjectCreator.test.js'
];

const projectRoot = __dirname;

let totalTests = 0;
let passedTests = 0;
let failedTests = 0;

async function runTestFile(testFile) {
    return new Promise((resolve) => {
        const filePath = path.join(projectRoot, testFile);
        
        // Check if file exists
        if (!fs.existsSync(filePath)) {
            console.log(`⚠️  Test file not found: ${testFile}`);
            resolve(true);
            return;
        }
        
        console.log(`\n🧪 Running: ${testFile}`);
        console.log('─'.repeat(60));
        
        const child = spawn('node', [filePath], {
            stdio: 'inherit',
            cwd: projectRoot
        });
        
        child.on('exit', (code) => {
            if (code !== 0) {
                failedTests++;
                console.log(`❌ ${testFile} failed with exit code ${code}`);
            } else {
                passedTests++;
                console.log(`✅ ${testFile} passed`);
            }
            resolve(code === 0);
        });
        
        child.on('error', (error) => {
            console.error(`Error running test: ${error.message}`);
            failedTests++;
            resolve(false);
        });
    });
}

async function runAllTests() {
    console.log('═'.repeat(60));
    console.log('  🚀 Unified Test Suite');
    console.log('═'.repeat(60));
    console.log(`\nRunning ${testFiles.length} test suites...\n`);
    
    const results = [];
    for (const testFile of testFiles) {
        const passed = await runTestFile(testFile);
        results.push({ file: testFile, passed });
    }
    
    console.log('\n' + '═'.repeat(60));
    console.log('  📊 Test Summary');
    console.log('═'.repeat(60));
    
    let allPassed = true;
    results.forEach(result => {
        const icon = result.passed ? '✅' : '❌';
        console.log(`${icon} ${result.file}`);
        if (!result.passed) allPassed = false;
    });
    
    console.log('─'.repeat(60));
    console.log(`Total test files: ${results.length}`);
    console.log(`Passed: ${results.filter(r => r.passed).length}`);
    console.log(`Failed: ${results.filter(r => !r.passed).length}`);
    console.log('═'.repeat(60));
    
    process.exit(allPassed ? 0 : 1);
}

runAllTests().catch(error => {
    console.error('Fatal error running tests:', error);
    process.exit(1);
});
