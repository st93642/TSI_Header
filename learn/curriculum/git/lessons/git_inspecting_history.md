# Lesson 2.3: Exploring History with Log, Show, Diff, and Bisect

## Navigating History

`git log` is your window into project evolution. Customize views with pretty formats, graph visualizations, and filtering.

```bash
git log --oneline --graph --decorate --all
git log --author="Ada" --since="2 weeks"
```

![History Exploration](../../../../resources/git/git_history_log.svg)

Use `git show <commit>` to inspect individual snapshots, including diffs and metadata.

### Diff Inspection Layers

![Git Diff Inspection Layers](../../../../resources/git/git_diff_layers.svg)

Select diff modes to answer specific questions:

- `git diff` compares working tree ↔ index; append `--staged` for index ↔ HEAD.
- `git diff HEAD~3..HEAD` surfaces multi-commit feature deltas before merge.
- Word (`--word-diff`) and function (`--function-context`) views highlight intent for reviewers.

## Understanding Diffs

`git diff` compares snapshots:

- Working tree ↔ index
- Index ↔ HEAD
- Commit ↔ commit

Colorization and word-level diffs enhance readability: `git diff --word-diff`.

## Binary Search with Bisect

When a regression sneaks in, `git bisect` narrows down the offending commit quickly:

```bash
git bisect start
git bisect bad HEAD
git bisect good v1.2.0
# run tests, mark commits as good/bad until culprit found
git bisect reset
```

Automate by providing a script to `git bisect run`.

### Reflog Timeline

![Git Reflog Timeline](../../../../resources/git/git_reflog_timeline.svg)

Reflogs record every ref move locally:

- `git reflog` catalogs branch checkouts, merges, and resets for recovery.
- Entries expire after 90 days by default; tune via `gc.reflogExpire` settings.
- Recover shas with `git checkout HEAD@{n}` or `git branch rescue HEAD@{n}`.

### Practice

- Craft custom log aliases in your global config.
- Use `git range-diff` to compare patch series during reviews.
- Simulate a bug introduction and track it down with `git bisect`.

## Extended Tools: Log Formats, Pickaxe, and Automated Bisect

This section consolidates advanced `git log` formats, the pickaxe search, and scripts to automate bisect-based regressions.

### Handy Log Formats (HTML Table)

<table>
 <thead>
  <tr>
   <th>Command</th>
   <th>Purpose</th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <td><code>git log --oneline --graph --decorate --all</code></td>
   <td>Compact visual history across refs</td>
  </tr>
  <tr>
   <td><code>git log --pretty=format:"%h %an %ad %s" --date=short</code></td>
   <td>Custom one-line format with author and date</td>
  </tr>
  <tr>
   <td><code>git log -S"functionName" --source --all</code></td>
   <td>Pickaxe search for commits that add/remove a string</td>
  </tr>
  <tr>
   <td><code>git log --since="2 weeks" --author="Alice"</code></td>
   <td>Filter by time window and author</td>
  </tr>
 </tbody>
</table>

### Using git bisect in CI

You can automate `git bisect` by providing a test script that returns zero on success and non-zero on failure. Example:

```bash
#!/usr/bin/env bash
set -e

# test script that returns 0 if good, 1 if bad
./run_regression_tests.sh

```

Run:

```bash
git bisect start
git bisect bad HEAD
git bisect good v1.0.0
git bisect run ./bisect-test.sh
git bisect reset
```

### Forensics: Reconstructing a Lost Patch

1. Use `git reflog` to find the last known good HEAD.
2. Use `git show <sha>` to export the diff and save it as a patch.
3. Apply the patch in a throwaway branch for inspection: `git apply --check <patch>` then `git am <patch>`.

### Lab

- Create a small repo and write a test that fails on purpose in one commit, then use `git bisect` to find it.
- Create a custom `git log` alias that prints a compact table of commit SHAs, authors, and files changed.

### Bisect Automation Appendix

Automating `git bisect` helps catch regressions quickly in CI. Below are recipes and a simple test harness pattern.

#### Bisect Runner Script (example)

`bisect-test.sh` should return 0 for good commits and non-zero for bad commits. Keep tests hermetic and deterministic.

```bash
#!/usr/bin/env bash
set -euo pipefail

# Example test harness that runs unit tests and exits non-zero on failure
./run_unit_tests.sh
```

Run bisect in automation:

```bash
git bisect start --term-old=good --term-new=bad
git bisect bad HEAD
git bisect good v1.0.0
git bisect run ./bisect-test.sh
```

#### Reporting and Triage

When `git bisect` identifies a commit, automatically create an issue with:

- Commit SHA and short message
- Reproducer steps
- Test output

Attach a link to the failing CI run and assign to the owning team based on CODEOWNERS.

---

<!-- markdownlint-disable MD033 MD010 -->

## Inspecting History — Advanced Forensics & Reporting

When investigating complex regressions or security incidents, capture evidence, produce reproducible repros, and automate triage.

### Evidence Collection Checklist

- Capture `git reflog` and the last known good commit SHA.
- Export diffs: `git show <sha> > evidence.diff`.
- Save environment (build logs, dependency versions) alongside the evidence bundle.

---

### Automated Reporting Example (pseudo)

When `git bisect` concludes, a small script can assemble a report and file an issue automatically.

```bash
#!/usr/bin/env bash
sha=$1
title="Regression found: $sha"
body=$(git show --pretty=fuller --stat $sha)
# use GitHub CLI to create an issue
gh issue create --title "$title" --body "$body" --label regression
```

---

### Exercises

1. Extend the bisect automation to upload evidence bundles to object storage and include presigned links in the generated issue.
2. Build a small dashboard that lists recent bisect runs and their results for easier on-call review.

<!-- markdownlint-enable MD033 MD010 -->

---

### Advanced Forensics: Reconstructing Partial Changes

If a developer amended or rebased history that removed a change, use a combination of `git reflog` and `git fsck --lost-found` to locate orphaned objects. Export diffs with `git show` and reapply as patches in a rescue branch.

---

### Exercises

1. Create a deterministic failing test and practice automating `git bisect run` in a fresh repo.
2. Build a minimal CI job that triggers bisect when a smoke test fails on the main branch and files an issue automatically.

---

End of Bisect Automation Appendix.

## Advanced inspection and automated bisecting

When bugs are intermittent or regressions are hard to reproduce, `git bisect` automation can speed triage. Use a reproducible test script that returns 0 for good and non-zero for bad.

### Automated bisect runner (example)

```bash
#!/usr/bin/env bash
set -euo pipefail
# Usage: bisect-run.sh test-command
TEST_CMD="$1"

# ensure clean state
git bisect start
git bisect bad
git bisect good origin/main~200 || git bisect good origin/main~100

# run bisect with the provided test
git bisect run bash -c "$TEST_CMD"

# bisect ends with a commit marked as first-bad
git bisect reset
```

Notes:

- Keep bisect test commands deterministic and fast; use a minimized reproduction where possible.

- For flaky tests, consider sampling multiple runs per commit and marking a commit as bad only when a threshold of failures is exceeded.

### Using pickaxe to find content changes

`git log -S"searchstring" --source --all --patch` helps find commits that added or removed a specific string. It's useful for tracking where a feature or bug text entered the code.

### Forensics: evidence collection script

A consistent evidence package helps incident response and postmortems. The script below collects commits, pack info, and relevant logs.

```bash
#!/usr/bin/env bash
set -euo pipefail
OUTDIR=${1:-/tmp/git-evidence-$(date +%F)}
mkdir -p "$OUTDIR"

# recent commits
git log --graph --pretty=fuller -n 200 >"$OUTDIR/commits.txt"
# pack and object info
git count-objects -vH >"$OUTDIR/count-objects.txt"
ls -lh .git/objects/pack >"$OUTDIR/packfiles.txt"
# current refs
git show-ref >"$OUTDIR/refs.txt"
# last 200 reflog entries
git reflog --all -n 200 >"$OUTDIR/reflog.txt"

# archive evidence
tar -czf "$OUTDIR.tar.gz" -C $(dirname "$OUTDIR") $(basename "$OUTDIR")

echo "Evidence package created: $OUTDIR.tar.gz"
```

### Reporting template for a history inspection incident

- Summary: short description of observed behavior
- Reproducer: command(s) to reproduce the issue
- Bisect result: commit SHA and author
- Artifacts: link to evidence package
- Next steps: fix plan and release cadence

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Evidence</th><th>Rationale</th><th>Stored at</th></tr>
  </thead>
  <tbody>
    <tr><td>Commits (200)</td><td>Shows recent change graph for context</td><td>commits.txt</td></tr>
    <tr><td>Packfiles</td><td>Assist diagnosing large objects or corruption</td><td>packfiles.txt</td></tr>
    <tr><td>Reflog</td><td>Shows local history and recoverable refs</td><td>reflog.txt</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Exercises and follow-ups

- Exercise: automate a bisect run against a flaky integration test by wrapping the test to retry and return a consistent pass/fail.
- Exercise: produce an evidence package after rebasing a feature branch and demonstrate how to extract a commit that introduced a regression.

<!-- end appended inspecting history content -->

## Parallel bisect and noisy-test handling

Bisecting flaky tests or long-running reproductions benefits from parallelization and statistical thresholds.

### Parallel bisect strategy (concept)

- Split the commit range into chunks and run multiple bisect processes in parallel, then reconcile candidate commits with additional sampling.
- Use a lightweight reproducible test harness when possible; for slow tests, use binary search over integration test times by narrowing ranges.

### Noisy-test mitigation

- Run each bisect check multiple times and record a failure ratio. Consider a commit bad only if failure ratio exceeds a threshold (e.g., 60%).

### Automated bisect wrapper (pseudo)

```bash
#!/usr/bin/env bash
set -euo pipefail
TEST_CMD="$1"
RETRIES=3

git bisect start
git bisect bad
git bisect good origin/main~200

git bisect run bash -c "for i in $(seq 1 $RETRIES); do $TEST_CMD && exit 0 || true; done; exit 1"

git bisect reset
```

### Evidence packaging and summarization

- The evidence package should include: bisect result, test logs, reproducer scripts, packfile summary, and commit metadata.
- Provide a short summary (first-bad SHA, author, date) and attach the evidence archive to the issue tracker.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Artifact</th><th>Purpose</th><th>Storage</th></tr>
  </thead>
  <tbody>
    <tr><td>Bisect result</td><td>Identifies first-bad commit</td><td>Issue attachment</td></tr>
    <tr><td>Reproducer script</td><td>Automates reproduction</td><td>Test infra</td></tr>
    <tr><td>Pack/objects snapshot</td><td>Forensics</td><td>Forensic S3</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Exercises: Bisect & Evidence

1. Build a bisect wrapper that retries flaky tests and records failure ratios before marking a commit as bad.
2. Create an evidence package script that zips logs, pack snapshots, and the bisect summary for attachment to a bug report.

<!-- end appended inspecting history content -->

---

## Appendix: Evidence Packaging, Reporting Automation, and Dashboards

When investigating complex regressions and incidents, automate evidence collection and reporting to reduce time-to-triage.

### Evidence Packaging Script (enhanced)

```bash
#!/usr/bin/env bash
set -euo pipefail
OUTDIR=${1:-/tmp/git-evidence-$(date +%F-%H%M%S)}
mkdir -p "$OUTDIR"

# recent commits and verbose graph
git log --graph --pretty=fuller -n 500 >"$OUTDIR/commits.txt"

# pack and object stats
git count-objects -vH >"$OUTDIR/count-objects.txt"
ls -lh .git/objects/pack >"$OUTDIR/packfiles.txt"

# refs and reflog
git show-ref >"$OUTDIR/refs.txt"
git reflog --all -n 500 >"$OUTDIR/reflog.txt"

# auto-collect CI logs if CI job id provided (pseudo)
# if [ -n "${CI_JOB_ID:-}" ]; then fetch logs via API and store in $OUTDIR/ci-logs

# compress
tar -czf "$OUTDIR.tar.gz" -C $(dirname "$OUTDIR") $(basename "$OUTDIR")

echo "Evidence package created: $OUTDIR.tar.gz"
```

### Automated Issue Reporter (pseudo)

When bisect finishes, generate a concise issue body and attach the evidence package.

```bash
#!/usr/bin/env bash
set -euo pipefail
SHA=$1
TITLE="Regression found: $SHA"
BODY=$(git show --pretty=fuller --stat $SHA)
# gh issue create --title "$TITLE" --body "$BODY" --label regression
```

### Dashboard Metrics to Monitor for Regressions

Track these signals so you can surface regressions more quickly:

- Frequency of bisect runs and average time-to-detection
- Number of failing PRs per day and median time to triage
- CI flakiness rate by test suite
- Packfile growth and large-blob counts

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Metric</th><th>Alert Threshold</th><th>Action</th></tr>
  </thead>
  <tbody>
    <tr><td>Bisect runs/day</td><td>&gt; 5</td><td>Investigate CI or upstream changes</td></tr>
    <tr><td>CI flakiness</td><td>&gt; 1% failures</td><td>Quarantine flaky tests</td></tr>
    <tr><td>Large blobs added</td><td>&gt; 1/week</td><td>Audit & move to LFS</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Example: CI hook that triggers bisect on broken main

- When main fails a smoke test, a scheduled job can attempt a quick bisect over the last N merges by running a minimized test harness and submit results automatically to a dashboard for on-call review.

---

<!-- markdownlint-disable MD033 MD010 -->

## Investigator Playbook: Rapid Triage & Bisect CI

This playbook provides runnable steps, example CI wiring, and communication templates you can adopt when a regression or incident requires fast history inspection.

### Playbook: Rapid Triage Steps

1. Confirm the failure on main and collect the failing job id / log link.
2. Snapshot the job environment (node, container image, deps) into an evidence bundle.
3. Try a minimal local reproducer; if it reproduces deterministically, prepare a bisect test harness.
4. If flaky, instrument the test harness to sample multiple runs and report a failure ratio.
5. Kick off `git bisect run` with the harness and collect the candidate commit SHA.
6. Auto-file an issue with evidence, stack traces, and the bisect summary. Assign to the CodeOwner team for the file path.

### HTML Triage Matrix (Roles & Responsibilities)

<table>
  <thead>
    <tr><th>Role</th><th>Responsibility</th><th>Action on First-Bad</th></tr>
  </thead>
  <tbody>
    <tr><td>On-call</td><td>Confirm & capture failing CI run</td><td>Trigger bisect CI and paste evidence link</td></tr>
    <tr><td>Author</td><td>Triages candidate commit</td><td>Provide fix PR or rollback plan</td></tr>
    <tr><td>Release Manager</td><td>Coordinates rollbacks/patch releases</td><td>Approve emergency release if needed</td></tr>
  </tbody>
</table>

<!-- markdownlint-enable MD033 MD010 -->

### Example: GitHub Actions job to run a bisect harness (minimal)

```yaml
name: bisect-on-failure
on:
  workflow_run:
    workflows: ["Main CI"]
    types: [completed]

jobs:
  bisect:
    runs-on: ubuntu-latest
    if: ${{ github.event.workflow_run.conclusion == 'failure' }}
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0 # need full history for bisect
      - name: Setup toolchain
        run: |
          sudo apt-get update && sudo apt-get install -y build-essential
      - name: Run bisect harness
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        run: |
          git fetch --all
          # example: bisect between last green tag and HEAD
          git bisect start
          git bisect bad HEAD
          git bisect good $(git describe --tags --abbrev=0)
          git bisect run ./ci/bisect-test.sh
          git bisect reset
      - name: Upload evidence
        uses: actions/upload-artifact@v4
        with:
          name: bisect-evidence
          path: ./bisect-evidence-*.tar.gz
```

### Bisect dashboard uploader (script)

```bash
#!/usr/bin/env bash
set -euo pipefail
EVIDENCE_TGZ=${1:-/tmp/bisect-evidence.tar.gz}
API_URL=${2:-"https://example.org/api/bisect"}
API_KEY=${3:-"$BISect_API_KEY"}

curl -X POST -H "Authorization: Bearer $API_KEY" \
  -F "evidence=@${EVIDENCE_TGZ}" \
  "$API_URL/upload"

echo "Uploaded evidence to $API_URL"
```

### Pre-bisect checklist (automatable)

- Reproduce failure locally with `--verbose` and collect logs.
- Ensure test harness is hermetic (db fixtures, network mocks).
- Limit bisect range to recent merges to reduce CI cost.
- Use a reproducible container image and pin versions.
- Add sampling/retry logic for flaky suites.

### Investigator Notes: Common bisect pitfalls

- bisect will run build/test per commit — avoid full e2e suites (use minimized smoke tests).
- Binary files, submodules, and LFS can affect bisect runs; ensure consistent environment.
- Force-pushed branches and rewritten history may hide original commits — check reflogs on local clones.

### Exercises: Investigator scenarios

1. Create a tiny repository with a deterministic unit test that fails in one commit. Automate `git bisect run` and produce an evidence bundle uploaded to a local HTTP server.

2. Simulate a flaky test by randomizing a seed in a test; implement a bisect wrapper that runs the test 5 times per commit and marks a commit bad only when failures exceed 60%.

3. Wire a GitHub Actions job that triggers the bisect workflow above when main's workflow fails and auto-creates a GitHub issue with the candidate commit and upload link.

### Small utilities & snippets

```bash
# quick: save last 50 reflog entries and compress evidence
git reflog --all -n 50 > /tmp/reflog.txt
tar -czf /tmp/bisect-evidence-$(date +%s).tar.gz /tmp/reflog.txt ./logs || true
```

### Communication template (issue body)

```
Title: Regression detected in CI — bisect candidate: <SHA>

Summary:
- Brief description of the failing test

Bisect result:
- Candidate commit: <SHA>
- Short message: <message>

Reproducer:
- Commands to reproduce locally

Evidence:
- Link to evidence artifact

Action requested:
- Author triage, propose fix or rollback
```

---

End of Investigator Playbook appendix.
