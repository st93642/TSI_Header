/**
 * Exercise Runner - Executes exercises and validates solutions
 */

const { execSync } = require('child_process');
const path = require('path');
const fs = require('fs').promises;

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
            default:
                throw new Error(`Testing not yet implemented for ${language}`);
        }
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
        let testCode = `import pytest\nfrom solution import *\n\n`;

        tests.forEach((test, index) => {
            const testName = test.name.replace(/\s+/g, '_').replace(/[^a-zA-Z0-9_]/g, '');
            testCode += `def test_${index + 1}_${testName}():\n`;
            testCode += `    result = ${test.call}\n`;
            testCode += `    assert result == ${this.pythonValue(test.expected)}, f"Expected {${this.pythonValue(test.expected)}}, got {result}"\n\n`;
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

            // Eval the user's code in isolated context
            const sandbox = {};
            const context = `
                ${code}
                ${tests.map((test, i) => `
                    try {
                        const result_${i} = ${test.call};
                        tests[${i}].result = result_${i};
                        tests[${i}].passed = JSON.stringify(result_${i}) === JSON.stringify(${JSON.stringify(test.expected)});
                    } catch (e) {
                        tests[${i}].error = e.message;
                        tests[${i}].passed = false;
                    }
                `).join('\n')}
            `;

            const testsCopy = JSON.parse(JSON.stringify(tests));
            eval(`(function() { const tests = ${JSON.stringify(testsCopy)}; ${context}; return tests; })()`).forEach((test, i) => {
                if (test.passed) {
                    passedTests.push(test.name);
                } else {
                    failedTests.push({
                        name: test.name,
                        message: `Expected: ${JSON.stringify(test.expected)}\n  Actual: ${JSON.stringify(test.result)}`,
                        error: test.error
                    });
                }
            });

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
