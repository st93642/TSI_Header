const assert = require('assert');
const fs = require('fs');
const path = require('path');

const lessons = [
  {
    name: 'Branching Strategies',
    file: 'git_branching_workflows.md',
    minLines: 500,
    requiredHeadings: [
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
    ]
  },
  {
    name: 'CI/CD Automation',
    file: 'git_ci_cd_integration.md',
    minLines: 500,
    requiredHeadings: [
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
    ]
  },
  {
    name: 'Code Review Practices',
    file: 'git_code_review_practices.md',
    minLines: 500,
    requiredHeadings: [
      'Learning Objectives',
      'Roles and Responsibilities',
      'Author Preparation Checklist',
      'Review Channels Comparison',
      'Review SLAs and Cadence',
      'Tooling and Automation',
      'Review Analytics and Metrics',
      'Security and Compliance Reviews',
      'Hands-On Labs',
      'Practice Checklist'
    ]
  },
  {
    name: 'Core Concepts',
    file: 'git_core_concepts.md',
    minLines: 500,
    requiredHeadings: [
      'Git Objects Demystified',
      'Object Storage Layers',
      'Commit Graph Anatomy',
      'Exploring the Object Database',
      'The Three Trees Revisited',
      'Visualizing States with Status'
    ]
  },
  {
    name: 'History Rewrite',
    file: 'git_history_rewrite.md',
    minLines: 500,
    requiredHeadings: [
      'Why Rewrite History?',
      'Tools of Choice',
      'Safety Checklist',
      'Guardrails for History Rewrites',
      'Force Push Risk Matrix'
    ]
  },
  {
    name: 'Inspecting History',
    file: 'git_inspecting_history.md',
    minLines: 500,
    requiredHeadings: [
      'Navigating History',
      'Diff Inspection Layers',
      'Understanding Diffs',
      'Binary Search with Bisect',
      'Reflog Timeline'
    ]
  },
  {
    name: 'Installation and Configuration',
    file: 'git_installation_configuration.md',
    minLines: 500,
    requiredHeadings: [
      'Learning Objectives',
      'Pre-Installation Checklist',
      'Environment Matrix',
      'Foundational Configuration',
      'Extended `.gitconfig` Reference',
      'Credential Management Options',
      'Hands-On Labs',
      'Automation Blueprint for CI Containers',
      'Practice Checklist'
    ]
  },
  {
    name: 'Introduction Foundations',
    file: 'git_introduction_foundations.md',
    minLines: 500,
    requiredHeadings: [
      'Learning Objectives',
      'Why Version Control Matters',
      'Git Object Model Deep Dive',
      'Distributed Collaboration Topology',
      'Commit Craftsmanship',
      'Hooks and Automation',
      'Observability and Metrics',
      'Troubleshooting Playbook',
      'Summary',
      'Exercises, Labs, and Practice'
    ]
  },
  {
    name: 'Merges and Conflict Resolution',
    file: 'git_merges_resolving_conflicts.md',
    minLines: 500,
    requiredHeadings: [
      'Merge Mechanics',
      'Conflict Resolution Workflow',
      'Strategic Choices',
      'Conflict Resolution Matrix',
      'Merge Decision Tree'
    ]
  },
  {
    name: 'Monorepo Management',
    file: 'git_monorepo_management.md',
    minLines: 500,
    requiredHeadings: [
      'The Monorepo Landscape',
      'Techniques for Repository Composition',
      'Sparse Checkouts',
      'Dependency Mesh',
      'Trade-Offs',
      'CI Matrix Strategy'
    ]
  },
  {
    name: 'Performance Optimization',
    file: 'git_performance_optimization.md',
    minLines: 500,
    requiredHeadings: [
      'Performance Challenges',
      'Client Techniques',
      'Server-Side Optimizations',
      'Garbage Collection Lifecycle',
      'Distributed Team Considerations',
      'Parallel Clone Flow'
    ]
  },
  {
    name: 'Rebasing Strategies',
    file: 'git_rebasing_strategies.md',
    minLines: 500,
    requiredHeadings: [
      'Why Rebase?',
      'Common Rebase Flows',
      'Rebase vs. Merge Comparison',
      'Interactive Rebase Checklist',
      'Policies for Safe Rewrites'
    ]
  },
  {
    name: 'Release Management',
    file: 'git_release_management.md',
    minLines: 500,
    requiredHeadings: [
      'Release Tagging Strategy',
      'Managing Release Branches',
      'Hotfix Workflow',
      'Release Gate Checklist',
      'Hotfix Flow'
    ]
  },
  {
    name: 'Remote Collaboration',
    file: 'git_remote_collaboration.md',
    minLines: 500,
    requiredHeadings: [
      'Remote Fundamentals',
      'Fetch vs Pull',
      'Fetch and Push Cycle',
      'Keeping Remotes Clean',
      'Fork Synchronization Flow'
    ]
  },
  {
    name: 'Repository Management',
    file: 'git_repository_management.md',
    minLines: 500,
    requiredHeadings: [
      'Choosing the Right Start',
      'Repository Layout Best Practices',
      'Managing Remotes',
      'Remote Synchronization Cycle',
      'Maintenance Planner'
    ]
  },
  {
    name: 'Security and Signing',
    file: 'git_security_signing.md',
    minLines: 500,
    requiredHeadings: [
      'Why Signing Matters',
      'Generating Keys',
      'Verifying Signatures',
      'Signature Validation Flow',
      'Protecting the Supply Chain',
      'Policy Enforcement Matrix'
    ]
  },
  {
    name: 'Staging and Committing',
    file: 'git_staging_committing.md',
    minLines: 500,
    requiredHeadings: [
      'The Purpose of the Staging Area',
      'Common Staging Commands',
      'Commit Message Anatomy',
      'Hook Execution Flow',
      'Writing Excellent Commit Messages',
      'Client-Side Hooks'
    ]
  },
  {
    name: 'Troubleshooting and Recovery',
    file: 'git_troubleshooting_recovery.md',
    minLines: 500,
    requiredHeadings: [
      'Reflog – Your Safety Net',
      'Stashing Work in Progress',
      'Recovery Workflow',
      'Disaster Scenarios',
      'Safety Net Layers'
    ]
  }
];

(function run() {
  for (const lesson of lessons) {
    const lessonPath = path.join(
      __dirname,
      '..',
      'curriculum',
      'git',
      'lessons',
      lesson.file
    );

    const content = fs.readFileSync(lessonPath, 'utf8');
    const lines = content.split(/\r?\n/);

    assert.ok(
      lines.length >= lesson.minLines,
      `${lesson.file} should be at least ${lesson.minLines} lines, found ${lines.length}`
    );

    for (const heading of lesson.requiredHeadings) {
      const headingFound =
        content.includes(`# ${heading}`) ||
        content.includes(`## ${heading}`) ||
        content.includes(`### ${heading}`);

      assert.ok(
        headingFound,
        `Expected to find section heading containing "${heading}" in ${lesson.file}`
      );
    }

    console.log(`✅ ${lesson.name} lesson checks passed.`);
  }
})();
