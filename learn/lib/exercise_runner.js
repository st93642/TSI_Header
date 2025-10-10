/**
 * Exercise Runner - Executes exercises and validates solutions
 */

const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs').promises;
const http = require('http');
const https = require('https');

class ExerciseRunner {
    constructor(vscode) {
        this.vscode = vscode;
    }

    /**
     * Run an exercise and validate the solution
     * @param {string} language - Programming language
     * @param {Object} exercise - Exercise object
     * @returns {Promise<Object>} Test results
     */
    async run(language, exercise) {
        try {
            // Get the active editor's document
            const editor = this.vscode.window.activeTextEditor;
            if (!editor) {
                throw new Error('No active editor found. Please open your exercise file.');
            }

            const document = editor.document;
            const code = document.getText();

            // If exercise points to external tests, try to fetch them first
            if (exercise.externalTestsUrl && (!exercise.tests || exercise.tests.length === 0)) {
                try {
                    const fetched = await this.fetchExternalTests(exercise.externalTestsUrl, 5000);
                    if (Array.isArray(fetched) && fetched.length > 0) {
                        exercise.tests = fetched;
                    }
                } catch (e) {
                    // Log but continue; if no tests available we'll treat as manual
                    console.error(`Failed to fetch external tests: ${e.message}`);
                }
            }

            // Check if this is a manual exercise (no tests)
            if (!exercise.tests || exercise.tests.length === 0) {
                return {
                    passed: true,
                    isManual: true,
                    score: 1,
                    total: 1,
                    message: 'This is a practice exercise. Complete the tasks and mark as done when ready.'
                };
            }

            // Run tests based on language
            const result = await this.runTests(language, code, exercise.tests);

            return result;
        } catch (error) {
            return {
                passed: false,
                error: error.message,
                score: 0,
                total: exercise.tests?.length || 0
            };
        }
    }

    /**
     * Run tests for the given code
     * @param {string} language - Programming language
     * @param {string} code - User's code
     * @param {Array} tests - Test cases
     * @returns {Promise<Object>} Test results
     */
    async runTests(language, code, tests = []) {
        switch (language.toLowerCase()) {
            case 'ruby':
                return await this.runRubyTests(code, tests);
            case 'python':
                return await this.runPythonTests(code, tests);
            case 'javascript':
                return await this.runJavaScriptTests(code, tests);
            case 'cpp':
            case 'c++':
                return await this.runCppTests(code, tests, { language });
            case 'c':
                return await this.runCppTests(code, tests, { language });
            case 'rust':
                return await this.runRustTests(code, tests);
            default:
                throw new Error(`Testing not yet implemented for ${language}`);
        }
    }

    /**
     * Run Rust tests by compiling with rustc and executing the binary
     * @param {string} code - User's Rust code
     * @param {Array} tests - Test cases
     * @returns {Promise<Object>} Test results
     */
    async runRustTests(code, tests) {
        const tempDir = path.join(__dirname, '..', '..', '.temp_tests');
        await fs.mkdir(tempDir, { recursive: true });

        const codeFile = path.join(tempDir, 'main.rs');
        const binFile = path.join(tempDir, 'main');

        // Check if rustc exists
        try {
            execSync('rustc --version', { stdio: 'ignore' });
        } catch (e) {
            return {
                passed: false,
                isManual: true,
                error: 'rustc not found in PATH. Install Rust toolchain or mark this exercise as manual.',
                score: 0,
                total: tests.length
            };
        }

        try {
            await fs.writeFile(codeFile, code);

            // Compile with rustc
            execSync(`rustc "${codeFile}" -o "${binFile}"`, {
                cwd: tempDir,
                encoding: 'utf8',
                timeout: 15000
            });

            // Execute binary and capture stdout
            const output = execSync(`"${binFile}"`, {
                cwd: tempDir,
                encoding: 'utf8',
                timeout: 10000
            });

            console.log('Rust runner: output =', JSON.stringify(output));
            console.log('Rust runner: tests =', tests);

            // For now support only 'output' tests that compare stdout
            const outputTrimmed = output;
            let allPassed = true;
            const failures = [];

            for (const test of tests) {
                console.log('Rust runner: processing test', test.name, 'type:', test.type);
                if (test.type === 'output') {
                    const expected = test.expected;
                    const actual = outputTrimmed;
                    console.log('Rust runner: expected =', JSON.stringify(expected));
                    console.log('Rust runner: actual =', JSON.stringify(actual));
                    if (actual !== expected) {
                        console.log('Rust runner: test failed');
                        allPassed = false;
                        failures.push({
                            name: test.name,
                            expected,
                            actual,
                            message: this.formatOutputDiff(expected, actual)
                        });
                    } else {
                        console.log('Rust runner: test passed');
                    }
                } else {
                    // unsupported test type for rust runner
                    console.log('Rust runner: unsupported test type');
                    allPassed = false;
                    failures.push({ name: test.name, message: 'Unsupported test type for Rust runner' });
                }
            }

            console.log('Rust runner: allPassed =', allPassed, 'failures.length =', failures.length);

            return {
                passed: allPassed,
                score: allPassed ? tests.length : (tests.length - failures.length),
                total: tests.length,
                failures,
                output
            };
        } catch (error) {
            console.log('Rust runner: error =', error.message);
            return {
                passed: false,
                error: error.stdout || error.stderr || error.message,
                score: 0,
                total: tests.length,
                failures: [{
                    name: 'Execution Error',
                    expected: '',
                    actual: '',
                    message: `Failed to execute: ${error.message}`
                }]
            };
        } finally {
            try {
                await fs.unlink(codeFile);
            } catch (e) {}
            try {
                await fs.unlink(binFile);
            } catch (e) {}
        }
    }

    /**
     * Produce a human-readable diff between expected and actual stdout.
     * Shows visible newline markers and a small line-level context around the first differing line.
     * @param {string} expected
     * @param {string} actual
     * @returns {string} formatted message
     */
    formatOutputDiff(expected, actual) {
        const visible = s => String(s === undefined ? '' : s).replace(/\r/g, '')
            .replace(/\n/g, '\\n\n');

        // Quick equality check handled earlier; build informative message
        const expLines = (expected || '').split(/\r?\n/);
        const actLines = (actual || '').split(/\r?\n/);

        // Find first differing line
        let idx = -1;
        const maxLines = Math.max(expLines.length, actLines.length);
        for (let i = 0; i < maxLines; i++) {
            if (expLines[i] !== actLines[i]) { idx = i; break; }
        }

        let diffSnippet = '';
        if (idx === -1) {
            // Rare case: lines same but strings differ (e.g., trailing whitespace)
            diffSnippet = `Expected (visible): ${visible(expected)}\nActual (visible): ${visible(actual)}`;
        } else {
            const contextBefore = 2;
            const start = Math.max(0, idx - contextBefore);
            const end = Math.min(maxLines - 1, idx + contextBefore);
            const parts = [];
            for (let i = start; i <= end; i++) {
                const e = expLines[i] === undefined ? '' : expLines[i];
                const a = actLines[i] === undefined ? '' : actLines[i];
                if (e === a) {
                    parts.push(` ${i + 1} |  ${e}`);
                } else {
                    parts.push(`-${i + 1} |  ${e}`);
                    parts.push(`+${i + 1} |  ${a}`);
                }
            }
            diffSnippet = parts.join('\n');
        }

        const message = `Expected:\n${visible(expected)}\n\nActual:\n${visible(actual)}\n\nDiff (context around first mismatch):\n${diffSnippet}`;
        return message;
    }

    /**
     * Fetch external tests over HTTP(S) with a timeout
     * @param {string} url - URL to fetch JSON test array from
     * @param {number} timeoutMs - request timeout in ms
     * @returns {Promise<Array>} Parsed JSON array of tests
     */
    fetchExternalTests(url, timeoutMs = 5000) {
        return new Promise((resolve, reject) => {
            try {
                const client = url.startsWith('https://') ? https : http;
                const req = client.get(url, { timeout: timeoutMs }, (res) => {
                    const { statusCode } = res;
                    if (statusCode !== 200) {
                        res.resume();
                        return reject(new Error(`Request Failed. Status Code: ${statusCode}`));
                    }
                    let raw = '';
                    res.setEncoding('utf8');
                    res.on('data', (chunk) => { raw += chunk; });
                    res.on('end', () => {
                        try {
                            const parsed = JSON.parse(raw);
                            resolve(parsed);
                        } catch (e) {
                            reject(new Error('Invalid JSON from external tests'));
                        }
                    });
                });

                req.on('error', (err) => reject(err));
                req.on('timeout', () => {
                    req.abort();
                    reject(new Error('Request timed out'));
                });
            } catch (e) {
                reject(e);
            }
        });
    }

    /**
     * Run Ruby tests
     * @param {string} code - User's Ruby code
     * @param {Array} tests - Test cases
     * @returns {Promise<Object>} Test results
     */
    async runRubyTests(code, tests) {
        const tempDir = path.join(__dirname, '..', '..', '.temp_tests');
        await fs.mkdir(tempDir, { recursive: true });

        const codeFile = path.join(tempDir, 'solution.rb');
        const testFile = path.join(tempDir, 'test_solution.rb');

        try {
            // Write user's code
            await fs.writeFile(codeFile, code);

            // Generate test file
            const testCode = this.generateRubyTestCode(tests);
            await fs.writeFile(testFile, testCode);

            // Run tests with detailed output
            const output = execSync(`ruby "${testFile}"`, {
                cwd: tempDir,
                encoding: 'utf8',
                timeout: 10000
            });

            // Parse results
            return this.parseRubyTestOutput(output, tests.length);

        } catch (error) {
            // Test failures throw errors - parse the output
            const output = error.stdout || error.stderr || error.message;
            return this.parseRubyTestOutput(output, tests.length);
        } finally {
            // Cleanup
            try {
                await fs.unlink(codeFile);
                await fs.unlink(testFile);
            } catch (e) {
                // Ignore cleanup errors
            }
        }
    }

    /**
     * Generate Ruby test code
     * @param {Array} tests - Test cases
     * @returns {string} Ruby test code
     */
    generateRubyTestCode(tests) {
        let testCode = `require 'minitest/autorun'\nrequire 'stringio'\nrequire_relative 'solution'\n\n`;
        testCode += `class TestSolution < Minitest::Test\n`;

        tests.forEach((test, index) => {
            const testName = test.name.replace(/\s+/g, '_').replace(/[^a-zA-Z0-9_]/g, '');
            testCode += `  def test_${index + 1}_${testName}\n`;
            
            // Check if this is an output test (tests printed output)
            if (test.type === 'output') {
                testCode += `    # Capture stdout\n`;
                testCode += `    captured_output = StringIO.new\n`;
                testCode += `    original_stdout = $stdout\n`;
                testCode += `    $stdout = captured_output\n`;
                testCode += `    \n`;
                testCode += `    ${test.call}\n`;
                testCode += `    \n`;
                testCode += `    $stdout = original_stdout\n`;
                testCode += `    output = captured_output.string\n`;
                testCode += `    assert_equal(${this.rubyValue(test.expected)}, output.chomp)\n`;
            } else if (test.type === 'exception') {
                // Exception test - verify that code raises expected exception
                testCode += `    # Test that code raises an exception\n`;
                testCode += `    assert_raises(${this.rubyValue(test.expected)}) do\n`;
                testCode += `      ${test.call}\n`;
                testCode += `    end\n`;
            } else if (test.type === 'side_effect') {
                // Side effect test - check file creation/modification or other side effects
                testCode += `    # Test side effects\n`;
                testCode += `    ${test.setup || ''}\n`; // Optional setup code
                testCode += `    ${test.call}\n`;
                testCode += `    ${test.assertion || `assert(${test.expected})`}\n`; // Custom assertion
            } else if (test.expected !== undefined) {
                // Regular return value test
                testCode += `    result = ${test.call}\n`;
                testCode += `    assert_equal(${this.rubyValue(test.expected)}, result)\n`;
            } else {
                testCode += `    assert(${test.call})\n`;
            }
            
            testCode += `  end\n\n`;
        });

        testCode += `end\n`;
        return testCode;
    }

    /**
     * Convert JavaScript value to Ruby syntax
     * @param {*} value - JavaScript value
     * @returns {string} Ruby representation
     */
    rubyValue(value) {
        if (value === null) return 'nil';
        if (typeof value === 'string') return JSON.stringify(value);
        if (typeof value === 'boolean') return value.toString();
        if (Array.isArray(value)) return `[${value.map(v => this.rubyValue(v)).join(', ')}]`;
        if (typeof value === 'object') {
            const pairs = Object.entries(value).map(([k, v]) => `${this.rubyValue(k)} => ${this.rubyValue(v)}`);
            return `{${pairs.join(', ')}}`;
        }
        return String(value);
    }

    /**
     * Parse Ruby test output with detailed failure information
     * @param {string} output - Test output from minitest
     * @param {number} totalTests - Total number of tests
     * @returns {Object} Parsed results
     */
    parseRubyTestOutput(output, totalTests) {
        const lines = output.split('\n');
        
        // Extract test summary
        const summaryMatch = output.match(/(\d+) runs?, (\d+) assertions?, (\d+) failures?, (\d+) errors?/);
        
        if (!summaryMatch) {
            // No summary found - likely a syntax error or crash
            return {
                passed: false,
                error: 'Test execution failed',
                output: output,
                score: 0,
                total: totalTests,
                failures: [{ message: output.substring(0, 500) }]
            };
        }

        const [, runs, assertions, failures, errors] = summaryMatch;
        const failureCount = parseInt(failures) + parseInt(errors);
        const passed = failureCount === 0;

        // Extract failure messages
        const failureMessages = [];
        let inFailure = false;
        let currentFailure = '';

        for (const line of lines) {
            // Minitest failure format: "  1) Failure:"
            if (line.match(/^\s+\d+\)\s+(Failure|Error):/)) {
                if (currentFailure) {
                    failureMessages.push(currentFailure.trim());
                }
                inFailure = true;
                currentFailure = '';
            } else if (inFailure) {
                currentFailure += line + '\n';
                
                // Stop at next test or summary
                if (line.match(/^\d+ runs?,/) || line.match(/^Finished in/)) {
                    inFailure = false;
                    if (currentFailure.trim()) {
                        failureMessages.push(currentFailure.trim());
                    }
                    currentFailure = '';
                }
            }
        }

        if (currentFailure.trim()) {
            failureMessages.push(currentFailure.trim());
        }

        return {
            passed: passed,
            score: totalTests - failureCount,
            total: totalTests,
            failedTests: failureCount,
            output: output,
            failures: failureMessages.map(msg => ({
                message: this.extractFailureMessage(msg)
            })),
            hint: failureCount > 0 ? 'Review the assertion failures above. The tests show what was expected vs what your code returned.' : undefined
        };
    }

    /**
     * Extract clean failure message with diff
     * @param {string} failureText - Raw failure text from minitest
     * @returns {string} Clean failure message
     */
    extractFailureMessage(failureText) {
        // Extract the test name
        const testMatch = failureText.match(/test_\d+_(\w+)/);
        const testName = testMatch ? testMatch[1].replace(/_/g, ' ') : 'Test';

        // Extract Expected vs Actual
        const expectedMatch = failureText.match(/Expected:\s*(.+)/);
        const actualMatch = failureText.match(/Actual:\s*(.+)/);

        if (expectedMatch && actualMatch) {
            return `${testName}:\n  Expected: ${expectedMatch[1].trim()}\n  Actual: ${actualMatch[1].trim()}`;
        }

        // For errors, extract the error message
        const errorMatch = failureText.match(/(Error|Exception):\s*(.+)/);
        if (errorMatch) {
            return `${testName}:\n  ${errorMatch[0]}`;
        }

        // Return first line as fallback
        return failureText.split('\n')[0] || 'Test failed';
    }

    /**
     * Run Python tests
     * @param {string} code - User's Python code
     * @param {Array} tests - Test cases
     * @returns {Promise<Object>} Test results
     */
    async runPythonTests(code, tests) {
        const tempDir = path.join(__dirname, '..', '..', '.temp_tests');
        await fs.mkdir(tempDir, { recursive: true });

        const codeFile = path.join(tempDir, 'solution.py');
        const testFile = path.join(tempDir, 'test_solution.py');

        try {
            await fs.writeFile(codeFile, code);

            const testCode = this.generatePythonTestCode(tests);
            await fs.writeFile(testFile, testCode);

            const output = execSync(`python3 -m pytest "${testFile}" -v --tb=short`, {
                cwd: tempDir,
                encoding: 'utf8',
                timeout: 10000
            });

            return this.parsePythonTestOutput(output, tests.length);

        } catch (error) {
            const output = error.stdout || error.stderr || error.message;
            return this.parsePythonTestOutput(output, tests.length);
        } finally {
            try {
                await fs.unlink(codeFile);
                await fs.unlink(testFile);
            } catch (e) {
                // Ignore
            }
        }
    }

    /**
     * Generate Python test code
     * @param {Array} tests - Test cases
     * @returns {string} Python test code
     */
    generatePythonTestCode(tests) {
        let testCode = `import pytest\nfrom solution import *\nimport sys\nfrom io import StringIO\n\n`;

        tests.forEach((test, index) => {
            const testName = test.name.replace(/\s+/g, '_').replace(/[^a-zA-Z0-9_]/g, '');
            testCode += `def test_${index + 1}_${testName}():\n`;
            
            if (test.type === 'output') {
                // Output test - capture stdout
                testCode += `    # Capture stdout\n`;
                testCode += `    old_stdout = sys.stdout\n`;
                testCode += `    sys.stdout = captured_output = StringIO()\n`;
                testCode += `    \n`;
                testCode += `    ${test.call}\n`;
                testCode += `    \n`;
                testCode += `    sys.stdout = old_stdout\n`;
                testCode += `    output = captured_output.getvalue()\n`;
                testCode += `    assert output.strip() == ${this.pythonValue(test.expected)}, f"Expected {${this.pythonValue(test.expected)}}, got {output.strip()}"\n\n`;
            } else if (test.type === 'exception') {
                // Exception test
                testCode += `    # Test that code raises an exception\n`;
                testCode += `    with pytest.raises(${this.pythonValue(test.expected)}):\n`;
                testCode += `        ${test.call}\n\n`;
            } else if (test.type === 'side_effect') {
                // Side effect test
                testCode += `    # Test side effects\n`;
                testCode += `    ${test.setup || ''}\n`;
                testCode += `    ${test.call}\n`;
                testCode += `    ${test.assertion || `assert ${test.expected}`}\n\n`;
            } else {
                // Regular return value test
                testCode += `    result = ${test.call}\n`;
                testCode += `    assert result == ${this.pythonValue(test.expected)}, f"Expected {${this.pythonValue(test.expected)}}, got {result}"\n\n`;
            }
        });

        return testCode;
    }

    /**
     * Convert JavaScript value to Python syntax
     * @param {*} value - JavaScript value
     * @returns {string} Python representation
     */
    pythonValue(value) {
        if (value === null) return 'None';
        if (typeof value === 'string') return JSON.stringify(value);
        if (typeof value === 'boolean') return value ? 'True' : 'False';
        if (Array.isArray(value)) return `[${value.map(v => this.pythonValue(v)).join(', ')}]`;
        if (typeof value === 'object') {
            const pairs = Object.entries(value).map(([k, v]) => `${this.pythonValue(k)}: ${this.pythonValue(v)}`);
            return `{${pairs.join(', ')}}`;
        }
        return String(value);
    }

    /**
     * Parse Python test output
     * @param {string} output - Test output from pytest
     * @param {number} totalTests - Total number of tests
     * @returns {Object} Parsed results
     */
    parsePythonTestOutput(output, totalTests) {
        // Extract pytest summary
        const passedMatch = output.match(/(\d+) passed/);
        const failedMatch = output.match(/(\d+) failed/);
        
        const passed = passedMatch ? parseInt(passedMatch[1]) : 0;
        const failed = failedMatch ? parseInt(failedMatch[1]) : totalTests;
        
        // Extract assertion errors
        const failures = [];
        const assertionMatches = output.matchAll(/AssertionError: Expected (.+?), got (.+)/g);
        
        for (const match of assertionMatches) {
            failures.push({
                message: `Expected: ${match[1]}\n  Actual: ${match[2]}`
            });
        }

        return {
            passed: failed === 0,
            score: passed,
            total: totalTests,
            failedTests: failed,
            output: output,
            failures: failures,
            hint: failed > 0 ? 'Check the assertion failures above for expected vs actual values.' : undefined
        };
    }

    /**
     * Run JavaScript tests
     * @param {string} code - User's JavaScript code
     * @param {Array} tests - Test cases
     * @returns {Promise<Object>} Test results
     */
    async runJavaScriptTests(code, tests) {
        // Simple in-process testing for JavaScript
        try {
            const passedTests = [];
            const failedTests = [];

            // Create a sandbox context for execution
            const createSandbox = () => ({
                console: {
                    log: (...args) => {
                        if (!sandbox._output) sandbox._output = [];
                        sandbox._output.push(args.join(' '));
                    },
                    output: [],
                    getOutput: () => sandbox._output ? sandbox._output.join('\n') : ''
                },
                process: {
                    stdout: {
                        write: (text) => {
                            if (!sandbox._stdout) sandbox._stdout = [];
                            sandbox._stdout.push(text);
                        }
                    }
                },
                require: (module) => {
                    if (module === 'fs') {
                        return {
                            writeFileSync: (path, content) => {
                                sandbox._files = sandbox._files || {};
                                sandbox._files[path] = content;
                            },
                            readFileSync: (path) => {
                                sandbox._files = sandbox._files || {};
                                return sandbox._files[path] || '';
                            },
                            existsSync: (path) => {
                                sandbox._files = sandbox._files || {};
                                return path in sandbox._files;
                            }
                        };
                    }
                    throw new Error(`Module ${module} not available in test environment`);
                }
            });

            const testsCopy = JSON.parse(JSON.stringify(tests));
            
            // Execute each test in isolation
            for (let i = 0; i < testsCopy.length; i++) {
                const test = testsCopy[i];
                const sandbox = createSandbox();
                
                try {
                    if (test.type === 'output') {
                        // Output test - capture console.log or stdout
                        const context = `
                            ${code}
                            (function() {
                                var result;
                                ${test.call}
                                return {
                                    output: (console.getOutput && console.getOutput()) || (_stdout ? _stdout.join('') : ''),
                                    result: result
                                };
                            })()
                        `;
                        
                        const func = new Function('console', 'process', 'require', context);
                        const testResult = func(sandbox.console, sandbox.process, sandbox.require);
                        
                        const actualOutput = testResult.output || '';
                        const passed = actualOutput.trim() === String(test.expected).trim();
                        
                        if (passed) {
                            passedTests.push(test.name);
                        } else {
                            failedTests.push({
                                name: test.name,
                                message: `Expected output: "${test.expected}"\n  Actual output: "${actualOutput}"`
                            });
                        }
                        
                    } else if (test.type === 'exception') {
                        // Exception test
                        const context = `
                            ${code}
                            (function() {
                                try {
                                    ${test.call}
                                    return { success: true };
                                } catch (e) {
                                    return { success: false, error: e.name || e.constructor.name };
                                }
                            })()
                        `;
                        
                        const func = new Function('console', 'process', 'require', context);
                        const testResult = func(sandbox.console, sandbox.process, sandbox.require);
                        
                        const expectedException = test.expected;
                        const actualException = testResult.error;
                        const passed = !testResult.success && actualException === expectedException;
                        
                        if (passed) {
                            passedTests.push(test.name);
                        } else {
                            failedTests.push({
                                name: test.name,
                                message: `Expected exception: ${expectedException}\n  Actual: ${testResult.success ? 'No exception thrown' : actualException}`
                            });
                        }
                        
                    } else if (test.type === 'side_effect') {
                        // Side effect test
                        const context = `
                            ${code}
                            (function() {
                                ${test.setup || ''}
                                ${test.call}
                                return {
                                    assertion: ${test.assertion || 'true'},
                                    files: _files || {}
                                };
                            })()
                        `;
                        
                        const func = new Function('console', 'process', 'require', context);
                        const testResult = func(sandbox.console, sandbox.process, sandbox.require);
                        
                        const passed = testResult.assertion;
                        
                        if (passed) {
                            passedTests.push(test.name);
                        } else {
                            failedTests.push({
                                name: test.name,
                                message: `Side effect test failed: ${test.assertion || 'assertion failed'}`
                            });
                        }
                        
                    } else {
                        // Regular return value test
                        const context = `
                            ${code}
                            (function() {
                                return ${test.call};
                            })()
                        `;
                        
                        const func = new Function('console', 'process', 'require', context);
                        const result = func(sandbox.console, sandbox.process, sandbox.require);
                        const passed = JSON.stringify(result) === JSON.stringify(test.expected);
                        
                        if (passed) {
                            passedTests.push(test.name);
                        } else {
                            failedTests.push({
                                name: test.name,
                                message: `Expected: ${JSON.stringify(test.expected)}\n  Actual: ${JSON.stringify(result)}`
                            });
                        }
                    }
                    
                } catch (e) {
                    failedTests.push({
                        name: test.name,
                        message: `Test execution error: ${e.message}`,
                        error: e.message
                    });
                }
            }

            return {
                passed: failedTests.length === 0,
                score: passedTests.length,
                total: tests.length,
                failedTests: failedTests.length,
                passedTests: passedTests,
                failures: failedTests,
                hint: failedTests.length > 0 ? 'Check the differences between expected and actual values.' : undefined
            };

        } catch (error) {
            return {
                passed: false,
                error: error.message,
                score: 0,
                total: tests.length,
                failures: [{ message: error.message }]
            };
        }
    }

    /**
     * Detect available C/C++ compiler
     * @param {boolean} isC - Whether compiling C (true) or C++ (false)
     * @returns {string|null} Compiler command or null if none found
     */
    detectCompiler(isC) {
        const compilers = isC ? ['gcc', 'clang'] : ['g++', 'clang++'];
        
        // On Windows, also try common paths
        if (process.platform === 'win32') {
            // Try MinGW paths
            const mingwPaths = [
                'C:\\MinGW\\bin\\gcc.exe',
                'C:\\MinGW\\bin\\g++.exe',
                'C:\\mingw64\\bin\\gcc.exe',
                'C:\\mingw64\\bin\\g++.exe',
                'C:\\msys64\\mingw64\\bin\\gcc.exe',
                'C:\\msys64\\mingw64\\bin\\g++.exe'
            ];
            
            for (const path of mingwPaths) {
                if (isC && path.includes('gcc')) compilers.unshift(path);
                if (!isC && path.includes('g++')) compilers.unshift(path);
            }
        }
        
        for (const compiler of compilers) {
            try {
                execSync(`"${compiler}" --version`, { stdio: 'pipe', timeout: 2000 });
                return compiler;
            } catch (error) {
                // Compiler not found or not working, try next
                continue;
            }
        }
        
        return null;
    }

    /**
     * Run C++ tests
     * @param {string} code - User's C++ code
     * @param {Array} tests - Test cases
     * @returns {Promise<Object>} Test results
     */
    async runCppTests(code, tests, options = {}) {
        const tempDir = path.join(__dirname, '..', '..', '.temp_tests');
        const totalTests = tests.length;
        const language = (options.language || 'cpp').toLowerCase();
        const isC = language === 'c';

        try {
            await fs.mkdir(tempDir, { recursive: true });

            const sourceExtension = isC ? 'c' : 'cpp';
            const sourceFile = path.join(tempDir, `exercise.${sourceExtension}`);
            await fs.writeFile(sourceFile, code);

            const executableFile = path.join(tempDir, 'exercise');
            try {
                const compiler = this.detectCompiler(isC);
                if (!compiler) {
                    const compilerName = isC ? 'C' : 'C++';
                    const errorMessage = `${compilerName} compiler not found. Please install a ${compilerName} compiler (GCC/MinGW recommended).\n\nFor Windows:\n- Install MinGW: https://www.mingw-w64.org/\n- Or MSYS2: https://www.msys2.org/\n- Add compiler to PATH\n\nFor Linux/Mac: Install build-essential or Xcode Command Line Tools.`;
                    
                    const failures = [{
                        message: `Compilation failed: ${errorMessage}`
                    }];

                    return {
                        passed: false,
                        success: false,
                        score: 0,
                        total: totalTests,
                        totalTests,
                        failedTests: totalTests,
                        results: tests.map(test => ({
                            name: test.name,
                            passed: false,
                            error: errorMessage
                        })),
                        failures,
                        hint: `No ${compilerName} compiler found. Please install and configure a compiler.`,
                        hints: [`Install a ${compilerName} compiler and ensure it's in your PATH`]
                    };
                }
                
                const standardFlag = isC ? '-std=c11' : '-std=c++17';
                execSync(`"${compiler}" ${standardFlag} -o "${executableFile}" "${sourceFile}"`, {
                    cwd: tempDir,
                    stdio: 'pipe'
                });
            } catch (compileError) {
                const errorMessage = compileError.stderr?.toString() || compileError.stdout?.toString() || compileError.message;
                const failures = [{
                    message: `Compilation failed:\n${errorMessage.trim()}`
                }];

                return {
                    passed: false,
                    success: false,
                    score: 0,
                    total: totalTests,
                    totalTests,
                    failedTests: totalTests,
                    results: tests.map(test => ({
                        name: test.name,
                        passed: false,
                        error: `Compilation failed: ${errorMessage.trim()}`
                    })),
                    failures,
                    hint: 'Compilation failed. Check the error details and ensure your code follows C++ syntax.',
                    hints: this.getCppHints([], language)
                };
            }

            const results = [];
            let passedTests = 0;

            for (const test of tests) {
                try {
                    if (test.type === 'output') {
                        const execOptions = {
                            cwd: tempDir,
                            encoding: 'utf8',
                            timeout: 5000
                        };
                        if (test.input !== undefined) {
                            execOptions.input = String(test.input);
                        }

                        const output = execSync(`"${executableFile}"`, execOptions).trim();

                        const passed = output === String(test.expected).trim();
                        if (passed) passedTests++;

                        results.push({
                            name: test.name,
                            passed,
                            expected: test.expected,
                            actual: output,
                            input: test.input,
                            error: passed ? null : `Expected "${test.expected}" but got "${output}"`
                        });
                    } else if (test.type === 'output_contains') {
                        const execOptions = {
                            cwd: tempDir,
                            encoding: 'utf8',
                            timeout: 5000
                        };
                        if (test.input !== undefined) {
                            execOptions.input = String(test.input);
                        }

                        const output = execSync(`"${executableFile}"`, execOptions).trim();
                        const expectedFragment = String(test.expected).trim();
                        const passed = output.includes(expectedFragment);
                        if (passed) passedTests++;

                        results.push({
                            name: test.name,
                            passed,
                            expected: `Output to include: ${expectedFragment}`,
                            actual: output,
                            input: test.input,
                            error: passed ? null : `Expected output to include "${expectedFragment}" but got "${output}"`
                        });
                    } else {
                        results.push({
                            name: test.name,
                            passed: false,
                            error: 'Only output-based tests are currently supported for C and C++ exercises.'
                        });
                    }
                } catch (runError) {
                    const errorMessage = runError.stderr?.toString() || runError.stdout?.toString() || runError.message;
                    results.push({
                        name: test.name,
                        passed: false,
                        error: `Runtime error: ${errorMessage.trim()}`,
                        input: test.input
                    });
                }
            }

            const failures = results
                .filter(result => !result.passed)
                .map(result => ({
                    message: result.error || `Test "${result.name}" failed.`
                }));

            const passedAll = passedTests === totalTests;

            return {
                passed: passedAll,
                success: passedAll,
                score: passedTests,
                total: totalTests,
                totalTests,
                failedTests: totalTests - passedTests,
                results,
                failures,
                hint: passedAll ? undefined : 'Review the failure details and ensure your output matches exactly.',
                hints: passedAll ? [] : this.getCppHints(results, language)
            };

        } catch (error) {
            const errorMessage = error.stderr?.toString() || error.stdout?.toString() || error.message;
            const failures = [{
                message: `Test execution failed: ${errorMessage.trim()}`
            }];

            return {
                passed: false,
                success: false,
                score: 0,
                total: totalTests,
                totalTests,
                failedTests: totalTests,
                results: tests.map(test => ({
                    name: test.name,
                    passed: false,
                    error: `Test execution failed: ${errorMessage.trim()}`
                })),
                failures,
                hint: 'There was an error running the tests. Make sure your code compiles and runs without errors.',
                hints: ['There was an error running the tests', 'Check that your code compiles correctly']
            };
        } finally {
            try {
                await fs.rm(tempDir, { recursive: true, force: true });
            } catch (cleanupError) {
                // Ignore cleanup errors
            }
        }
    }

    /**
     * Get C++ specific hints based on test results
     * @param {Array} results - Test results
     * @returns {Array} Hints
     */
    getCppHints(results, language = 'cpp') {
        const hints = [];
        const isC = (language || '').toLowerCase() === 'c';
        
        for (const result of results) {
            if (!result.passed && result.error) {
                if (result.error.includes('Compilation failed')) {
                    hints.push('Check your syntax - make sure all statements end with semicolons');
                    hints.push(
                        isC
                            ? 'Verify that you have included the correct headers like <stdio.h>'
                            : 'Verify that you have included all necessary headers like <iostream>'
                    );
                } else if (result.error.includes('Runtime error')) {
                    hints.push('Your program compiled but had a runtime error');
                    hints.push('Check for infinite loops or array bounds issues');
                } else if (result.error.includes('Expected')) {
                    hints.push('Your output doesn\'t match the expected result');
                    hints.push(
                        isC
                            ? 'Check your printf statements and make sure you\'re printing exactly what\'s expected'
                            : 'Check your cout statements and make sure you\'re printing exactly what\'s expected'
                    );
                }
            }
        }
        
        // Add general hints if no specific ones were found
        if (hints.length === 0) {
            hints.push('Review the lesson content for examples');
            hints.push('Make sure your output exactly matches the expected format');
            hints.push(
                isC
                    ? 'Check that you are using printf with the correct format specifiers'
                    : 'Check that you\'re using cout correctly with the << operator'
            );
        }
        
        return hints.slice(0, 3); // Limit to 3 hints
    }

    /**
     * Parse test output (deprecated - use language-specific parsers)
     * @param {string} output - Test output
     * @param {number} totalTests - Total number of tests
     * @returns {Object} Parsed results
     */
    parseTestOutput(output, totalTests) {
        // Simple parser - look for common patterns
        const passedMatch = output.match(/(\d+) tests?, (\d+) assertions?, 0 failures/);
        const failedMatch = output.match(/(\d+) failures?/);

        if (passedMatch) {
            return {
                passed: true,
                score: totalTests,
                total: totalTests,
                failedTests: 0,
                output: output
            };
        } else if (failedMatch) {
            const failures = parseInt(failedMatch[1]);
            return {
                passed: false,
                score: totalTests - failures,
                total: totalTests,
                failedTests: failures,
                output: output,
                hint: 'Review the test output for details on what failed.'
            };
        }

        return {
            passed: false,
            score: 0,
            total: totalTests,
            output: output
        };
    }
}

module.exports = ExerciseRunner;
