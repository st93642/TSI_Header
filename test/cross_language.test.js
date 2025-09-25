const assert = require('assert');
const fs = require('fs');
const path = require('path');
const { execSync } = require('child_process');

describe('Cross-language/project validation', function() {
  const languages = [
    { id: 'c', ext: 'c', className: 'TestC' },
    { id: 'cpp', ext: 'cpp', className: 'TestCPP' },
    { id: 'python', ext: 'py', className: 'TestPy' },
    { id: 'java', ext: 'java', className: 'TestJava' },
    { id: 'typescript', ext: 'ts', className: 'TestTS' }
  ];

  languages.forEach(lang => {
    it(`should generate header and class for ${lang.id}`, function() {
      // Simulate CLI header generation
      const cliPath = path.resolve(__dirname, '../lib/tsi_header_cli.rb');
      const fileName = `test_${lang.className}.${lang.ext}`;
      const command = `ruby "${cliPath}" insert "${lang.id}" "${fileName}"`;
      const result = execSync(command, { encoding: 'utf8' });
      const response = JSON.parse(result);
      assert.ok(response.success);
      assert.ok(response.header.includes(fileName));
      assert.ok(response.header.includes('TTTTTTTT SSSSSSS II'));
    });
  });
});
