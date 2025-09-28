/**
 * Code Quality Standards Enforcement Module
 *
 * This module implements automated enforcement of institutional coding standards
 * beyond headers, providing custom rule sets for code style, naming conventions,
 * and structural patterns.
 */

const vscode = require('vscode');
const path = require('path');

class CodeQualityEnforcement {
    constructor(core) {
        this.core = core;
        this.rules = this.loadDefaultRules();
    }

    /**
     * Load default institutional coding standards rules
     */
    loadDefaultRules() {
        return {
            naming: {
                // Variable naming conventions
                variables: /^[a-z][a-zA-Z0-9]*$/,
                constants: /^[A-Z][A-Z0-9_]*$/,
                functions: /^[a-z][a-zA-Z0-9]*$/,
                classes: /^[A-Z][a-zA-Z0-9]*$/,
                // File naming
                files: /^[a-z][a-z0-9_-]*\.[a-z]+$/,
            },
            structure: {
                // Maximum line length
                maxLineLength: 100,
                // Indentation (spaces vs tabs)
                indentation: 'spaces',
                indentSize: 4,
                // Blank lines
                maxConsecutiveBlankLines: 2,
            },
            comments: {
                // Require comments for functions
                requireFunctionComments: true,
                // Comment style
                commentStyle: '/* */',
            }
        };
    }

    /**
     * Analyze code for compliance with standards
     * @param {string} code - Code content to analyze
     * @param {string} languageId - Language identifier
     * @returns {Array} - Array of violations found
     */
    analyzeCode(code, languageId) {
        const violations = [];

        // Split code into lines for analysis
        const lines = code.split('\n');

        // Check line length
        lines.forEach((line, index) => {
            if (line.length > this.rules.structure.maxLineLength) {
                violations.push({
                    type: 'structure',
                    rule: 'maxLineLength',
                    line: index + 1,
                    message: `Line exceeds maximum length of ${this.rules.structure.maxLineLength} characters`,
                    severity: 'warning'
                });
            }
        });

        // Check for excessive blank lines
        let consecutiveBlanks = 0;
        lines.forEach((line, index) => {
            if (line.trim() === '') {
                consecutiveBlanks++;
                if (consecutiveBlanks > this.rules.structure.maxConsecutiveBlankLines) {
                    violations.push({
                        type: 'structure',
                        rule: 'maxConsecutiveBlankLines',
                        line: index + 1,
                        message: `Too many consecutive blank lines (max ${this.rules.structure.maxConsecutiveBlankLines})`,
                        severity: 'info'
                    });
                }
            } else {
                consecutiveBlanks = 0;
            }
        });

        // Language-specific checks
        if (languageId === 'javascript' || languageId === 'typescript') {
            violations.push(...this.analyzeJavaScript(code, lines));
        } else if (languageId === 'python') {
            violations.push(...this.analyzePython(code, lines));
        } else if (languageId === 'java') {
            violations.push(...this.analyzeJava(code, lines));
        }

        return violations;
    }

    /**
     * Analyze JavaScript/TypeScript code
     */
    analyzeJavaScript(code, lines) {
        const violations = [];

        // Check for console.log statements (development code)
        const consoleLogRegex = /console\.log\(/g;
        let match;
        while ((match = consoleLogRegex.exec(code)) !== null) {
            const lineNumber = code.substring(0, match.index).split('\n').length;
            violations.push({
                type: 'best_practice',
                rule: 'noConsoleLog',
                line: lineNumber,
                message: 'Avoid using console.log in production code',
                severity: 'warning'
            });
        }

        return violations;
    }

    /**
     * Analyze Python code
     */
    analyzePython(code, lines) {
        const violations = [];

        // Check for proper import organization
        const importLines = lines.filter(line => line.trim().startsWith('import ') || line.trim().startsWith('from '));
        if (importLines.length > 0) {
            // Check if imports are at the top
            const firstNonImportLine = lines.findIndex(line =>
                !line.trim().startsWith('import ') &&
                !line.trim().startsWith('from ') &&
                !line.trim().startsWith('#') &&
                line.trim() !== ''
            );

            if (firstNonImportLine !== -1 && firstNonImportLine < importLines.length) {
                violations.push({
                    type: 'structure',
                    rule: 'importsAtTop',
                    line: firstNonImportLine + 1,
                    message: 'Imports should be organized at the top of the file',
                    severity: 'warning'
                });
            }
        }

        return violations;
    }

    /**
     * Analyze Java code
     */
    analyzeJava(code, lines) {
        const violations = [];

        // Check for proper class naming
        const classRegex = /class\s+([A-Za-z][A-Za-z0-9]*)/g;
        let match;
        while ((match = classRegex.exec(code)) !== null) {
            const className = match[1];
            if (!this.rules.naming.classes.test(className)) {
                const lineNumber = code.substring(0, match.index).split('\n').length;
                violations.push({
                    type: 'naming',
                    rule: 'classNaming',
                    line: lineNumber,
                    message: `Class name '${className}' does not follow naming convention`,
                    severity: 'error'
                });
            }
        }

        return violations;
    }

    /**
     * Apply automatic fixes for some violations
     * @param {Array} violations - Violations to fix
     * @param {string} code - Original code
     * @returns {string} - Fixed code
     */
    applyAutoFixes(violations, code) {
        let fixedCode = code;

        // Fix excessive blank lines
        const blankLineViolations = violations.filter(v => v.rule === 'maxConsecutiveBlankLines');
        if (blankLineViolations.length > 0) {
            const lines = fixedCode.split('\n');
            const maxBlanks = this.rules.structure.maxConsecutiveBlankLines;

            // Remove excessive blank lines
            const cleanedLines = [];
            let consecutiveBlanks = 0;

            for (const line of lines) {
                if (line.trim() === '') {
                    consecutiveBlanks++;
                    if (consecutiveBlanks <= maxBlanks) {
                        cleanedLines.push(line);
                    }
                } else {
                    consecutiveBlanks = 0;
                    cleanedLines.push(line);
                }
            }

            fixedCode = cleanedLines.join('\n');
        }

        return fixedCode;
    }

    /**
     * Register VS Code commands for this module
     * @param {vscode.ExtensionContext} context - Extension context
     */
    registerCommands(context) {
        // Command to analyze current file
        const analyzeFileCommand = vscode.commands.registerCommand('tsiheader.analyzeCodeQuality', async () => {
            const editor = vscode.window.activeTextEditor;
            if (!editor) {
                vscode.window.showErrorMessage('No active editor found');
                return;
            }

            const document = editor.document;
            const code = document.getText();
            const languageId = document.languageId;

            const violations = this.analyzeCode(code, languageId);

            if (violations.length === 0) {
                vscode.window.showInformationMessage('✅ Code quality check passed! No violations found.');
                return;
            }

            // Show violations in output channel
            const outputChannel = vscode.window.createOutputChannel('TSI Code Quality');
            outputChannel.clear();
            outputChannel.appendLine(`Code Quality Analysis Results (${violations.length} violations found):`);
            outputChannel.appendLine('='.repeat(50));

            violations.forEach(violation => {
                const severityIcon = violation.severity === 'error' ? '❌' :
                                   violation.severity === 'warning' ? '⚠️' : 'ℹ️';
                outputChannel.appendLine(`${severityIcon} Line ${violation.line}: ${violation.message}`);
            });

            outputChannel.show();

            // Ask if user wants to apply auto-fixes
            const autoFixable = violations.filter(v => v.rule === 'maxConsecutiveBlankLines').length;
            if (autoFixable > 0) {
                const applyFixes = await vscode.window.showInformationMessage(
                    `${autoFixable} violation(s) can be auto-fixed. Apply fixes?`,
                    'Apply Fixes', 'Cancel'
                );

                if (applyFixes === 'Apply Fixes') {
                    const fixedCode = this.applyAutoFixes(violations, code);
                    if (fixedCode !== code) {
                        await editor.edit(editBuilder => {
                            const range = new vscode.Range(
                                document.positionAt(0),
                                document.positionAt(code.length)
                            );
                            editBuilder.replace(range, fixedCode);
                        });
                        vscode.window.showInformationMessage('Auto-fixes applied successfully!');
                    }
                }
            }
        });

        // Command to configure quality rules
        const configureRulesCommand = vscode.commands.registerCommand('tsiheader.configureQualityRules', async () => {
            // Open settings for TSI quality rules
            await vscode.commands.executeCommand('workbench.action.openSettings', '@ext:st93642.tsi-header quality');
        });

        context.subscriptions.push(analyzeFileCommand);
        context.subscriptions.push(configureRulesCommand);
    }

    /**
     * Get diagnostic collection for VS Code problems panel
     * @returns {vscode.DiagnosticCollection}
     */
    getDiagnosticCollection() {
        return vscode.languages.createDiagnosticCollection('tsi-code-quality');
    }

    /**
     * Update diagnostics for a document
     * @param {vscode.TextDocument} document
     * @param {vscode.DiagnosticCollection} diagnosticCollection
     */
    updateDiagnostics(document, diagnosticCollection) {
        const code = document.getText();
        const languageId = document.languageId;
        const violations = this.analyzeCode(code, languageId);

        const diagnostics = violations.map(violation => {
            const range = new vscode.Range(
                new vscode.Position(violation.line - 1, 0),
                new vscode.Position(violation.line - 1, document.lineAt(violation.line - 1).text.length)
            );

            const severity = violation.severity === 'error' ? vscode.DiagnosticSeverity.Error :
                           violation.severity === 'warning' ? vscode.DiagnosticSeverity.Warning :
                           vscode.DiagnosticSeverity.Information;

            return new vscode.Diagnostic(range, violation.message, severity);
        });

        diagnosticCollection.set(document.uri, diagnostics);
    }
}

module.exports = {
    CodeQualityEnforcement
};