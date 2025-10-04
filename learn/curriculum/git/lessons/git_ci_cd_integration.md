# Lesson 6.1: Automating Quality Gates with Git Hooks and CI

## Local Automation with Hooks

Git hooks trigger scripts at lifecycle events. Combine with linters, formatters, or security scanners to catch issues before pushing.

![CI/CD Pipeline](../../../../resources/git/git_ci_cd_pipeline.svg)

Common hooks:

- `pre-commit`: run linters, formatting, or unit tests.
- `pre-push`: run integration tests or block large binary pushes.
- `post-merge`: regenerate code or update dependencies automatically.

### CI Stage Breakdown

![CI Stage Breakdown](../../../../resources/git/git_ci_stage_breakdown.svg)

Structure pipelines into focused stages:

- Lint and test fast-moving codepaths first to catch failures early.
- Build artifacts and run integration suites in parallel to optimize runtimes.
- Publish coverage, SBOMs, and caches for downstream deploy workflows.

## Continuous Integration Pipelines

Connect Git to CI providers (GitHub Actions, GitLab CI, Jenkins, Azure DevOps) to automate builds and tests per push.

```yaml
name: CI
on: [push, pull_request]
jobs:
  test:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - uses: actions/setup-node@v4
        with:
          node-version: '20'
      - run: npm ci
      - run: npm test
```

## Gates and Policies

- Require green CI before merging.
- Use status checks with branch protection rules.
- Automate semantic release workflows via bots.

### CD Progressive Rings

![CD Release Rings](../../../../resources/git/git_cd_release_rings.svg)

Roll out changes safely:

- Ring 0 (internal) validates with canary environments or shadow traffic.
- Ring 1 (staging) exercises full integration with dependent services.
- Ring 2 (production) gradually increases traffic while monitoring KPIs.

### Practice

- Add Husky or Lefthook to run local hooks across languages.
- Configure a sample GitHub Actions workflow and observe status checks.
- Implement pre-push safeguards to block commits missing tests.
