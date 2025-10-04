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
    'git_branching_workflows.md'
  );

  const content = fs.readFileSync(lessonPath, 'utf8');
  const lines = content.split(/\r?\n/);

  // Ensure we retain the expanded depth for the branching lesson.
  assert.ok(
    lines.length >= 500,
    `git_branching_workflows.md should be at least 500 lines, found ${lines.length}`
  );

  const requiredSections = [
    'Learning Objectives',
    'Feature Branching Deep Dive',
    'Trunk-Based Development Deep Dive',
    'Gitflow Deep Dive',
    'Hybrid and Contextual Models',
    'Governance Playbook',
    'Metrics and Observability',
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

  console.log('✅ git_branching_workflows.md length and section coverage test passed.');
})();
