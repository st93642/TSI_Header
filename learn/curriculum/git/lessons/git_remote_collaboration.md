# Lesson 4.1: Remotes, Fetching, Pulling, and Upstream Hygiene

## Remote Fundamentals

Remotes define where you exchange commits. Each remote is a shortcut to a URL plus tracked branches.

```bash
git remote -v
origin  git@github.com:org/app.git (fetch)
origin  git@github.com:org/app.git (push)
```

![Remote Topology](../../../../resources/git/git_remote_topology.svg)

Add upstream remotes to stay aligned with canonical repositories:

```bash
git remote add upstream git@github.com:parent/app.git
```

## Fetch vs Pull

- `git fetch` downloads new objects without touching your working tree.
- `git pull` combines fetch + merge (or rebase) into your current branch.

Always fetch before complex merges to inspect remote changes.

### Fetch and Push Cycle

![Fetch and Push Cycle](../../../../resources/git/git_fetch_push_cycle.svg)

Clarify how refs travel between clones:

- Fetch updates `refs/remotes/origin/*` without touching local branches.
- Push attempts to fast-forward remote refs; policies may reject force pushes.
- Mirror repositories replicate refs for read-only CI or analytics mirrors.

## Keeping Remotes Clean

- Use `git remote prune origin` to delete stale remote-tracking branches.
- Configure `fetch.prune` to true for automatic cleanup.
- Leverage `git remote set-url` when rotating credentials or migrating hosting providers.

### Fork Synchronization Flow

![Fork Synchronization Flow](../../../../resources/git/git_fork_sync_flow.svg)

Maintain forks without drift:

- Track both `origin` (your fork) and `upstream` (canonical repository).
- Use `git fetch upstream` and `git rebase upstream/main` to stay current.
- Push rebased branches to `origin` before opening pull requests.

### Practice

- Configure push URLs for read-only upstream fetch, write-only fork pushes.
- Compare `git fetch --all` vs targeted fetches to large repositories.
- Visualize remote tracking branches with `git branch -r` and `git branch -vv`.

## Remote Collaboration Appendix — Forks, Upstreams, and Policies

This appendix explains collaboration models (fork-based, shared repo), branch protection, and ownership policies, plus automation patterns for sync and CI.

### Collaboration Models

- Fork-based: Contributors push to personal forks and open PRs to upstream. Good for open-source.
- Shared-repo: Contributors push branches to the canonical repo with RBAC and protected branches.
- Hybrid: Core teams use shared branches while external contributors use forks.

---

### Code Ownership and PR Routing

<table>
  <thead>
    <tr>
      <th>Area</th>
      <th>Primary Owner</th>
      <th>Reviewers</th>
      <th>Policy</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Core Runtime</td>
      <td>@runtime-team</td>
      <td>@runtime-sigs</td>
      <td>Require 2 approvers, CI green</td>
    </tr>
    <tr>
      <td>CLI Tools</td>
      <td>@cli-team</td>
      <td>@cli-owners</td>
      <td>1 approver, optional sign-off</td>
    </tr>
    <tr>
      <td>Docs & Tutorials</td>
      <td>@docs-team</td>
      <td>@tech-writers</td>
      <td>Auto-merge if CI passes and spellcheck OK</td>
    </tr>
  </tbody>
</table>

---

### Sync Automation Recipes

- Keep a bot account that periodically fetches `upstream` and opens PRs with updates for long-running feature branches.
- Use GitHub's `pull_request` automation to backport approved changes to release branches.

---

### Protected Branch Rules (Examples)

- Require status checks (CI, lint, tests) before merging.
- Require linear history and disallow force pushes to `main`/`release/*`.
- Enforce commit signature verification on release branches.

---

### Exercises

1. Configure a simple GitHub Action that rebases a feature branch on `upstream/main` and opens a PR when conflicts are resolved.
2. Implement a small bot that creates sync PRs from `upstream` to `origin` every 24 hours for a long-running branch.
3. Draft branch protection rules for `main`, `develop`, and `release/*` in your repository's settings and test the enforcement.

---

End of Remote Collaboration Appendix.

## Scaling Contributor Workflows — Bots, Syncs, and Governance

As projects grow, manual sync and fork workflows don't scale. Use automation patterns to keep forks, branches, and release lines consistent.

### Bot Patterns for Fork Syncing

- Use a trusted bot account with a deploy key and least privilege.
- Bot periodically fetches `upstream` and opens a PR against `origin` feature branches with conflict-free merges or rebases.
- Implement rate limits and backoff to avoid CI overload.

Example: Minimal sync script for a bot (pseudo):

```bash
#!/usr/bin/env bash
set -euo pipefail
REPO_DIR=/tmp/repo
git clone git@github.com:fork-owner/repo.git $REPO_DIR
cd $REPO_DIR
git remote add upstream git@github.com:upstream/repo.git
git fetch upstream --prune
for branch in $(git for-each-ref --format='%(refname:short)' refs/heads/ | grep '^feature/'); do
  git checkout $branch
  git rebase upstream/main || (git rebase --abort; git checkout -)
  git push origin $branch --force-with-lease
done
```

---

### Forking Policy Checklist

- Clearly document when to use forks vs branches (external contributors vs internal teams).
- Enforce pull request templates to capture intent, breaking changes, and acceptance criteria.
- Use `CODEOWNERS` to automatically request reviews from the right teams.

---

### Sample GitHub Action: Auto-sync Upstream to Fork

```yaml
name: Auto-sync Upstream
on:
  schedule:
    - cron: '0 3 * * *' # daily at 03:00 UTC
  workflow_dispatch:

jobs:
  sync:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          repository: fork-owner/repo
          token: ${{ secrets.BOT_TOKEN }}
      - name: Add upstream
        run: |
          git remote add upstream https://github.com/upstream/repo.git
          git fetch upstream
      - name: Rebase feature branches
        run: |
          for branch in $(git for-each-ref --format='%(refname:short)' refs/heads/ | grep '^feature/'); do
            git checkout $branch
            git rebase upstream/main || (git rebase --abort; git checkout -)
            git push origin $branch --force-with-lease
          done
        env:
          GITHUB_TOKEN: ${{ secrets.BOT_TOKEN }}
```

---

### Governance and Onboarding

- Document repository branching strategy and branch naming rules in `CONTRIBUTING.md`.
- Provide onboarding scripts that configure `origin`/`upstream` remotes and helpful aliases.

---

### Exercises

1. Implement the bot script in a sandbox and test syncing a long-running feature branch.
2. Create a PR template that enforces a checklist for breaking changes.
3. Add a GitHub Action that enforces branch naming conventions on new branches.

---

End of Scaling Contributor Workflows.

## Collaboration workflows: fork vs shared repository

Choose fork-based workflows for public projects or where external contributors must be isolated. Choose shared-branch workflows (feature branches in the upstream repo) where contributor identity and access are controlled and onboarding is frequent.

### Fork-based contributor flow

- Contributor forks repo, creates branch, opens PR to upstream.
- Maintainers run CI and review; merges are performed by integrators with repository write access.
- Periodically encourage contributors to keep forks in sync (via documentation or bot assistance).

### Shared-branch (internal) flow

- Developers create branches in the main repo, open PRs, and rely on branch protection rules to enforce checks.
- Use CODEOWNERS to route reviews to the right teams and require approvals where needed.

## Automation: bot-based sync for forks

A small automation script can keep forks or mirrors in sync with upstream. This example is intentionally simple and suitable for a deployable bot.

```bash
#!/usr/bin/env bash
# simple sync bot: fetch upstream changes, fast-forward mirror
set -euo pipefail
UPSTREAM_REPO="https://github.com/org/repo.git"
MIRROR_DIR="/tmp/upstream-sync"
rm -rf "$MIRROR_DIR"
mkdir -p "$MIRROR_DIR"
cd "$MIRROR_DIR"

git clone --mirror "$UPSTREAM_REPO" .
# push mirror to backup or to a read-only mirror
# git push --mirror git@backup.example.com:mirror/repo.git

echo "Mirror updated"
```

### GitHub Actions example: auto-sync branch from upstream

```yaml
name: upstream-sync
on:
  schedule:
    - cron: '0 3 * * *' # daily at 03:00 UTC
jobs:
  sync:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          repository: org/repo
          token: ${{ secrets.PERSONAL_TOKEN }}
      - name: fetch upstream
        run: |
          git remote add upstream https://github.com/upstream/repo.git || true
          git fetch upstream
          git checkout main
          git merge --ff-only upstream/main || echo "No fast-forward available"
      - name: push
        run: git push origin main
```

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Collaborator tier</th><th>Typical permissions</th><th>Expectation</th></tr>
  </thead>
  <tbody>
    <tr><td>Contributors</td><td>Fork/PR</td><td>Follow contribution doc; open PRs</td></tr>
    <tr><td>Maintainers</td><td>Merge/Release</td><td>Review backlog and triage</td></tr>
    <tr><td>Release Engineers</td><td>Admin</td><td>Publish releases and manage tags</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Contributor onboarding and triage automation

Make it easy for new contributors: provide a clear `CONTRIBUTING.md`, label automation, and a triage workflow that routes PRs to the right owners.

### PR triage GitHub Action (label & assign)

```yaml
name: PR Triage
on: [pull_request]
jobs:
  triage:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Label and assign
        run: |
          if [[ "${{ github.event.pull_request.body }}" =~ "security" ]]; then
            gh pr edit ${{ github.event.number }} --add-label security
            gh pr review --request-reviewer @security-team
          else
            gh pr edit ${{ github.event.number }} --add-label needs-review
          fi
```

## Contributor workflow playbooks and reviewer automation

Standardize contributor flows and automate reviewer assignment to reduce latency and improve review quality.

### Reviewer rotation and assignment

- Implement a small steward rotation system that assigns a primary reviewer for a given area for a week; use CODEOWNERS as a fallback.
- Use automation to suggest reviewers based on file paths, recent contributors, and past approvals.

### Fork security gating

- For public forks, restrict secret access and run a minimal set of safe checks (lint, static analysis) before exposing more sensitive CI steps.
- For sensitive checks (e.g., deployment, secrets scanning), require the PR to be merged by an integrator with access to protected secrets.

### Onboarding checklist for new contributors

- Read CONTRIBUTING.md and sign CLA if required.
- Run local tests and linters via provided `bootstrap.sh`.
- Open a small first PR to validate the contributor pipeline.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Item</th><th>Expectation</th><th>SLA</th></tr>
  </thead>
  <tbody>
    <tr><td>New PR triage</td><td>Label, assign reviewers</td><td>< 24h</td></tr>
    <tr><td>Security review</td><td>Run specialized checks</td><td>< 48h</td></tr>
    <tr><td>Final merge</td><td>Reviewed and CI green</td><td>< 72h</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Automation safety notes

- When assigning reviewers automatically, allow maintainers to opt-out and provide a simple override path.
- Maintain rate-limits on auto-assignment to avoid reviewer overload.

## Exercises: Collaboration

1. Implement a reviewer-suggestion GitHub Action that uses changed paths to pick reviewers and apply a label.
2. Draft an onboarding checklist and `bootstrap.sh` for new contributors and test the flow with a friend or teammate.

---

## Appendix: Triage Timelines, Contributor SLAs, and Automation

Add these small templates and SLA tables to help scale contributor workflows and reduce latency in large projects.

### Contributor SLA Table (HTML)

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Event</th><th>Expectation</th><th>SLA</th></tr>
  </thead>
  <tbody>
    <tr><td>New PR triaged</td><td>Label, assign reviewers</td><td>&lt; 24h</td></tr>
    <tr><td>Security-labeled PR</td><td>Security triage</td><td>&lt; 48h</td></tr>
    <tr><td>Merge-ready PR</td><td>Merge when CI green</td><td>&lt; 72h</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Automation: sync-notify script (pseudo)

A small script that creates a comment on PRs that are out-of-date with upstream, prompting authors to sync.

```bash
#!/usr/bin/env bash
PR=$1
# check if PR branch is behind upstream/main
git fetch upstream
ahead=$(git rev-list --left-right --count upstream/main...$PR | awk '{print $2}')
if [ "$ahead" -gt 0 ]; then
  gh pr comment $PR --body "Your branch is behind upstream/main by $ahead commits. Please rebase or merge." 
fi
```

### Merge timeline template (copyable)

- PR opened: day 0
- First review requested: day 0–1
- Review completed: day 1–2
- Merge after CI: day 2–3 (depending on tests)

---

<!-- end appended remote collaboration appendix -->

---

## Operational Appendix: SLAs, Sync-Notify, and PR-sync Bot

This appendix contains copyable templates and actionable scripts for maintaining healthy remote collaboration at scale.

### Detailed Contributor SLA (HTML)

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Event</th><th>Responsible</th><th>Expectation</th><th>SLA</th></tr>
  </thead>
  <tbody>
    <tr><td>New PR triaged</td><td>Triage bot / Maintainers</td><td>Label, assign reviewers, apply area tags</td><td>&lt; 12h</td></tr>
    <tr><td>Security-labeled PR</td><td>Security Team</td><td>Initial triage and scope assessment</td><td>&lt; 24h</td></tr>
    <tr><td>Functional review</td><td>Area owners</td><td>Provide actionable review comments</td><td>&lt; 48h</td></tr>
    <tr><td>Final merge</td><td>Maintainers/Integrator</td><td>Merge when CI passes and approvals satisfied</td><td>&lt; 72h</td></tr>
    <tr><td>Stale branch clean-up</td><td>Platform/Bot</td><td>Close or mark stale PRs and notify authors</td><td>&lt; 30d</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

---

### Expanded sync-notify script (practical)

This version detects if a PR branch is behind upstream/main and posts a helpful comment with the specific commit delta and suggested commands. It uses `gh` (GitHub CLI) and expects a token in `GITHUB_TOKEN`.

```bash
#!/usr/bin/env bash
set -euo pipefail
PR_NUMBER=$1
REPO=${2:-$(git remote get-url origin)}
TMPDIR=$(mktemp -d)

# get PR head ref via gh
PR_BRANCH=$(gh pr view "$PR_NUMBER" --json headRefName -q .headRefName)
UPSTREAM=upstream

# ensure upstream exists
git remote add $UPSTREAM https://github.com/UPSTREAM/REPO.git 2>/dev/null || true

git fetch $UPSTREAM --quiet

git fetch origin --quiet

# compute how many commits the PR branch is behind upstream/main
behind=$(git rev-list --left-right --count $UPSTREAM/main...origin/$PR_BRANCH | awk '{print $2}')

if [ "$behind" -gt 0 ]; then
  gh pr comment "$PR_NUMBER" --body "Heads up — your branch `$PR_BRANCH` is behind `upstream/main` by $behind commits. Recommended:

```
# rebase onto upstream/main
git fetch upstream
 git checkout $PR_BRANCH
git rebase upstream/main
# resolve conflicts, then
git push --force-with-lease origin $PR_BRANCH
```

If you prefer, you can ask the bot to rebase automatically by adding the comment `@sync-bot rebase` and it will attempt a safe rebase." 
fi

rm -rf "$TMPDIR"
```

Note: Replace the upstream URL and verify `gh` is available on the runner.

---

### PR-sync bot (safer preview flow)

The following pattern creates a preview branch rather than force-updating the contributor branch. It rebases the contributor branch onto upstream/main, pushes to a `preview/` namespace, and comments a link to the preview branch for reviewers.

```bash
#!/usr/bin/env bash
set -euo pipefail
PR_NUMBER=$1
# discover PR branch
PR_BRANCH=$(gh pr view "$PR_NUMBER" --json headRefName -q .headRefName)
PREVIEW="preview/${PR_BRANCH}"
UPSTREAM=upstream

git fetch origin --quiet
git fetch $UPSTREAM --quiet || true

# create local copy
git checkout -B "$PREVIEW" origin/$PR_BRANCH
if git rebase $UPSTREAM/main; then
  git push --force-with-lease origin "$PREVIEW"
  PREVIEW_URL="https://github.com/$(git remote get-url origin | sed -E 's#.*/(.*)\.git#\1#')/tree/$PREVIEW"
  gh pr comment "$PR_NUMBER" --body "Rebased preview available: $PREVIEW_URL"
else
  git rebase --abort
  gh pr comment "$PR_NUMBER" --body "Automatic preview failed due to conflicts. Please rebase your branch or request help."
  exit 1
fi
```

---

### CODEOWNERS and review routing

Add a `CODEOWNERS` file in the repository that maps paths to owners. Example snippet:

```
# CODEOWNERS
/docs/ @docs-team
/src/cli/ @cli-team @cli-owners
/*.md @tech-writers
```

Combine `CODEOWNERS` with branch protection rules to require reviews from the mapped owners.

---

### Exercises (operational)

1. Implement the expanded `sync-notify` script and run it against open PRs in a sandbox repo. Validate the comment text and suggested commands.
2. Deploy the PR-sync bot as an Actions workflow that runs on a `workflow_dispatch` trigger for selected PRs.
3. Create a `CODEOWNERS` file for your repository and test that the right reviewers are requested when files change.

---

End of Operational Appendix (remote collaboration).
