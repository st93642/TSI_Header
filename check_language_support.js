const fs = require('fs');
const path = require('path');

// Read the VS Code languages list
const vscodeLanguagesPath = path.join(__dirname, 'vscode_languages_list.txt');
const vscodeLanguagesContent = fs.readFileSync(vscodeLanguagesPath, 'utf8');
const vscodeLanguages = vscodeLanguagesContent.split('\n')
    .filter(line => line.trim())
    .map(line => {
        // Extract language identifier from format "1. Language Name (identifier)"
        const match = line.match(/^\d+\.\s+(.+?)\s*\(([^)]+)\)$/);
        if (match) {
            const name = match[1];
            const identifierString = match[2];
            
            // Handle complex identifiers like "perl and perl6", "jade, pug", etc.
            const identifiers = [];
            
            // Handle different formats
            if (identifierString.includes(' and ')) {
                // Format: "perl and perl6"
                identifiers.push(...identifierString.split(/\s+and\s+/));
            } else if (identifierString.includes(',')) {
                // Format: "jade, pug" or complex "scss - syntax..., sass - ..."
                if (identifierString.includes(' - ')) {
                    // For complex cases like SCSS, just take the first identifier before any dash
                    const firstPart = identifierString.split(' - ')[0].trim();
                    if (firstPart.includes(',')) {
                        // If there are commas in the first part, take just the first identifier
                        identifiers.push(firstPart.split(',')[0].trim());
                    } else {
                        identifiers.push(firstPart);
                    }
                } else {
                    // Simple comma-separated: "jade, pug"
                    identifiers.push(...identifierString.split(/\s*,\s*/));
                }
            } else if (identifierString.includes(' - ')) {
                // Format: "scss - syntax using curly brackets, sass - indented syntax"
                // Just take the first identifier before the dash
                identifiers.push(identifierString.split(' - ')[0].trim());
            } else {
                // Simple format: "javascript"
                identifiers.push(identifierString);
            }
            
            // Clean up identifiers
            const cleanIdentifiers = identifiers
                .map(id => id.trim())
                .filter(id => id.length > 0)
                .map(id => id.replace(/^["']|["']$/g, '')); // Remove quotes if present
            
            return {
                name: name,
                identifiers: identifiers,
                primaryIdentifier: identifiers[0] // Use first identifier as primary
            };
        }
        return null;
    })
    .filter(item => item !== null);

// Read the extension's supported languages from test suite
const testSuitePath = path.join(__dirname, 'unified_test_suite.js');
const testSuiteContent = fs.readFileSync(testSuitePath, 'utf8');

// Extract HEADER_LANGUAGES array - use regex to find all quoted strings
const headerLanguagesMatch = testSuiteContent.match(/const HEADER_LANGUAGES = \[([\s\S]*?)\];/);
if (!headerLanguagesMatch) {
    console.error('Could not find HEADER_LANGUAGES array in test suite');
    process.exit(1);
}
const headerLanguagesContent = headerLanguagesMatch[1];
const quotedStrings = headerLanguagesContent.match(/'([^']+)'/g) || [];
const extensionLanguages = quotedStrings
    .map(lang => lang.replace(/'/g, '').trim())
    .filter(lang => lang.length > 0);

// Create sets for easier comparison
const vscodeIdentifiers = new Set();
vscodeLanguages.forEach(lang => {
    lang.identifiers.forEach(id => vscodeIdentifiers.add(id));
});
const extensionLanguagesSet = new Set(extensionLanguages);

// Find missing languages
const missingLanguages = vscodeLanguages.filter(lang => {
    // Check if any of the language's identifiers are supported
    return !lang.identifiers.some(id => extensionLanguagesSet.has(id));
});

// Find supported languages
const supportedLanguages = vscodeLanguages.filter(lang => {
    // Check if any of the language's identifiers are supported
    return lang.identifiers.some(id => extensionLanguagesSet.has(id));
});

// Output results
console.log('🔍 VS Code Language Support Analysis for TSI Header Extension\n');
console.log(`📊 Total VS Code languages: ${vscodeLanguages.length}`);
console.log(`✅ Supported by extension: ${supportedLanguages.length}`);
console.log(`❌ Not supported by extension: ${missingLanguages.length}`);
console.log(`📈 Support coverage: ${((supportedLanguages.length / vscodeLanguages.length) * 100).toFixed(1)}%\n`);

if (supportedLanguages.length > 0) {
    console.log('✅ SUPPORTED LANGUAGES:');
    supportedLanguages.forEach(lang => {
        console.log(`   • ${lang.name} (${lang.identifiers.join(', ')})`);
    });
    console.log();
}

if (missingLanguages.length > 0) {
    console.log('❌ MISSING LANGUAGES:');
    missingLanguages.forEach(lang => {
        console.log(`   • ${lang.name} (${lang.identifiers.join(', ')})`);
    });
    console.log();
}

// Save detailed report
const report = {
    summary: {
        totalVscodeLanguages: vscodeLanguages.length,
        supportedByExtension: supportedLanguages.length,
        missingFromExtension: missingLanguages.length,
        coveragePercentage: ((supportedLanguages.length / vscodeLanguages.length) * 100).toFixed(1)
    },
    supported: supportedLanguages.map(lang => ({
        name: lang.name,
        identifiers: lang.identifiers
    })),
    missing: missingLanguages.map(lang => ({
        name: lang.name,
        identifiers: lang.identifiers
    })),
    timestamp: new Date().toISOString()
};

const reportPath = path.join(__dirname, 'language_support_analysis.json');
fs.writeFileSync(reportPath, JSON.stringify(report, null, 2));
console.log(`📄 Detailed report saved to: ${reportPath}`);