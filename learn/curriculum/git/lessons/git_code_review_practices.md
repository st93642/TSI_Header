# Lesson 4.2: Pull Requests, Patches, and Collaborative Etiquette

## Code Review Channels

Distributed teams collaborate through pull requests, email patches, and code review platforms. Align on expectations for review scope, response time, and approval thresholds.

![Code Review Cycle](../../../../resources/git/git_code_review_cycle.svg)

### Preparing a Reviewable Change

- Keep branches focused and small (1–3 logical commits).
- Include context in the pull request description (problem, solution, validation).
- Link to issue trackers or design docs.

### Consuming Feedback

- Address comments with follow-up commits rather than force pushes when history must remain stable.
- Use suggestions features to apply reviewer-provided patches quickly.
- Share status updates when delays occur to maintain trust.

### Feedback Loop

![Review Feedback Loop](../../../../resources/git/git_review_feedback_loop.svg)

Accelerate reviews with explicit signals:

- Draft PRs communicate work-in-progress while still enabling early feedback.
- Reviewer assignment, mention workflows, and ownership mapping keep load balanced.
- Reaction emojis, labels, and automation mark when changes are ready for final sign-off.

## Email Patch Workflows

`git format-patch` and `git send-email` remain popular in kernel and embedded communities.

```bash
git format-patch origin/main --cover-letter
git send-email --to maintainer@example.com 000*.patch
```

### CI Status Matrix

![CI Status Matrix](../../../../resources/git/git_ci_status_matrix.svg)

Use status checks to protect mainline quality:

- Required checks block merges until passing; optional checks provide advisory signals.
- Combine unit, integration, security, and linting pipelines for comprehensive coverage.
- Automatically rerun flaky jobs and surface failure context directly in the review UI.

### Practice

- Submit a pull request and include CI status badges in the description.
- Review a teammate’s change focusing on architectural impact rather than nitpicks.
- Convert a pull request to an email patch series using `git format-patch`.
