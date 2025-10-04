const assert = require('assert');
const fs = require('fs');
const path = require('path');

(function run() {
  const lessonPath = path.join(
    __dirname,
    '..',
    'curriculum',
    'git',
    'lessons',
    'git_ci_cd_integration.md'
  );

  const content = fs.readFileSync(lessonPath, 'utf8');
  const lines = content.split(/\r?\n/);

  assert.ok(
    lines.length >= 500,
    `git_ci_cd_integration.md should be at least 500 lines, found ${lines.length}`
  );

  const requiredSections = [
    'Local Automation with Hooks',
    'Continuous Integration Pipelines',
    'Gates and Policies',
    'Progressive Delivery Playbooks',
    'Observability for Pipelines',
    'Security and Compliance Automation',
    'Disaster Recovery and Rollback',
    'Case Studies',
    'Hands-On Labs',
    'Practice Checklist'
  ];

  for (const heading of requiredSections) {
    assert.ok(
      content.includes(`# ${heading}`) || content.includes(`## ${heading}`) || content.includes(`### ${heading}`),
      `Expected to find section heading containing "${heading}"`
    );
  }

  console.log('✅ git_ci_cd_integration.md length and section coverage test passed.');
})();
