# Lesson 4.3: Tags, Release Branches, and Backporting

## Release Tagging Strategy

Tags mark immutable points in history. Annotated tags capture release notes, authors, and signatures.

```bash
git tag -a v2.3.0 -m "Release 2.3.0"
git push origin v2.3.0
```

![Release Process](../../../../resources/git/git_release_process.svg)

## Managing Release Branches

- Create stabilization branches for regression fixes while mainline work continues.
- Backport bug fixes with `git cherry-pick`.
- Document version policies (semantic versioning, calendar releases) in `CONTRIBUTING.md`.

## Hotfix Workflow

1. Branch from the latest release tag.
2. Apply urgent fixes.
3. Tag and ship.
4. Merge hotfix back into main and other active release branches.

### Release Gate Checklist

![Release Gate Checklist](../../../../resources/git/git_release_gate_checklist.svg)

Before shipping, ensure:

- Test suites (unit, integration, smoke) pass consistently.
- Release notes summarize changes, risks, and rollback steps.
- Observability dashboards and incident response owners are on-call.

### Hotfix Flow

![Hotfix Flow](../../../../resources/git/git_hotfix_flow.svg)

Coordinate urgent patches without losing track:

- Branch from the released tag to isolate the fix.
- Cherry-pick into active release and mainline branches after verification.
- Tag patched versions with suffixes (`v2.3.0-hotfix.1`) for traceability.

### Practice

- Create annotated and lightweight tags, inspect metadata with `git show`.
- Simulate backporting a fix to multiple release branches.
- Generate release notes by combining `git log` with commit message conventions.

## Release Engineering Appendix — Practical Playbook

This appendix expands on tagging, automation, backporting, hotfix workflows, rollbacks, signing, and runbooks. The content below is practical and intended to be copy-pastable into your CI and runbooks.

### Tagging and Artifact Policies

Tags are the immutable anchor of releases. Use annotated tags for releases and prefer signed annotated tags for production releases that must be auditable.

<table>
  <thead>
    <tr>
      <th>Tag Type</th>
      <th>Command</th>
      <th>Use Case</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Annotated</td>
      <td><code>git tag -a v1.2.0 -m "Release 1.2.0"</code></td>
      <td>Preferred for release records and notes</td>
    </tr>
    <tr>
      <td>Signed</td>
      <td><code>git tag -s v1.2.0 -m "Signed release"</code></td>
      <td>Compliance and supply chain verification</td>
    </tr>
    <tr>
      <td>Lightweight</td>
      <td><code>git tag v1.2.0</code></td>
      <td>Ad-hoc markers (not recommended for audit)</td>
    </tr>
  </tbody>
</table>

Always push tags explicitly with `git push origin --tags` or `git push origin v1.2.0` to avoid accidental omission.

---

### Generating Release Notes from Git

If your team follows Conventional Commits or a structured commit policy, generate release notes automatically. Here's a minimal script that builds release notes between two tags:

```bash
#!/usr/bin/env bash
set -euo pipefail

from=${1:-$(git describe --tags --abbrev=0 HEAD~1)}
to=${2:-HEAD}
outfile=${3:-RELEASE_NOTES_${to}.md}

echo "# Release notes for ${to}" > ${outfile}
echo "From: ${from}" >> ${outfile}
echo "" >> ${outfile}
git log --pretty=format:"* %h %s (%an)" ${from}..${to} >> ${outfile}

echo "Wrote ${outfile}"
```

For structured changelogs, prefer tools like `auto-changelog`, `git-chglog`, or `conventional-changelog` which group by commit type and support templates.

---

### CI/CD: Tag-Driven Release Pipeline (Concept)

Use tag-based triggers to guarantee reproducible releases. Example (conceptual):

- On push of annotated tag `vX.Y.Z`:
  - Checkout tag (detached HEAD)
  - Run full test suite and smoke tests
  - Build artifacts and sign them
  - Upload to artifact repository with metadata linking to tag and commit SHA
  - Publish release notes and notify stakeholders

Below is a simplified GitHub Actions snippet demonstrating the pattern.

```yaml
name: Release

on:
  push:
    tags:
      - 'v*.*.*'

jobs:
  release:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Set up environment
        run: |
          echo "Setting up"
      - name: Run tests
        run: ./ci/run_tests.sh
      - name: Build
        run: ./ci/build.sh
      - name: Create GitHub Release
        uses: softprops/action-gh-release@v1
        with:
          tag_name: ${{ github.ref_name }}
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
```

---

### Hotfix and Backport Workflows

Hotfixes often follow an emergency path where a fix is branched from the release tag and then merged/cherry-picked to other active branches. Example commands:

```bash
# create hotfix branch from tag
git checkout -b hotfix/v1.2.1 v1.2.0
# apply fix, test, tag
git commit -am "Fix: critical vulnerability"
git tag -a v1.2.1 -m "Hotfix v1.2.1"
git push origin hotfix/v1.2.1
git push origin v1.2.1

# then cherry-pick to main or other release branches
git checkout main
git cherry-pick <hotfix-commit-sha>
git push origin main
```

When automating backports, a script can attempt cherry-picks across configured branches and open PRs for manual resolution if conflicts occur.

---

### Rollback Strategies and Reverts

Prefer revert commits for rollbacks to preserve history and maintain traceability. To revert a previous release commit:

```bash
git revert -m 1 <merge-commit-sha>
git push origin HEAD
```

If you must redeploy an older artifact, ensure your deploy system maps tags to artifact versions and that you document the rollback reason in the release notes.

---

### Signing and Supply Chain Assurance

Integrate GPG/SSH signing for commits and tags (and sign artifacts when possible). Example: sign a tag with GPG:

```bash
git tag -s v1.2.1 -m "Signed release"
git push origin v1.2.1
```

In CI, verify signatures before publishing: `git verify-tag v1.2.1` and `git log --show-signature -1 v1.2.1`.

---

### Release Runbook Template (Copyable)

Title: Release vX.Y.Z

- Release owner: @oncall
- Date/time:
- Pre-release:
  - [ ] All PRs merged
  - [ ] CI green for main and release branch
  - [ ] Security scans passed
  - [ ] Database migration plan reviewed
- Release steps:
  1. Create signed tag: `git tag -s vX.Y.Z -m "release"`
  2. Push tag: `git push origin vX.Y.Z`
  3. Verify CI completed and artifacts uploaded
  4. Promote canary targets (10% -> 50% -> 100%)
- Post-release checks:
  - [ ] Monitor error rate for 30 minutes
  - [ ] Verify health checks are green
  - [ ] Announce release
- Rollback plan:
  - [ ] If severe issue, revert release commit and deploy previous artifact

---

### Exercises

1. Implement a tag-driven CI pipeline for a sample app that creates a release artifact and publishes release notes.
2. Practice hotfix creation and backporting in a sandbox repository; time yourself and document the steps.
3. Create a small script that verifies all release tags are signed by a known set of public keys.

---

## Release engineering appendix: pipelines, tagging, and rollbacks

Release engineering ties branching, tagging, CI, and deployments together. Below are operational patterns and example snippets for robust releases.

### Release strategies

- Release train (timeboxed): releases happen on a fixed cadence (e.g., weekly); useful for predictable delivery.
- Canary releases: progressively deploy to a subset of users before full rollout.
- Tag-driven releases: create an annotated tag to trigger release artifacts and publishing pipelines.

### Annotated and signed tags

Create signed annotated tags for releases:

```bash
git tag -a v2.3.0 -m "Release v2.3.0" --sign
git push origin v2.3.0
```

### CI snippet (GitHub Actions) for tag-driven release

```yaml
name: Release
on:
  push:
    tags:
      - 'v*.*.*'
jobs:
  release:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Set up Node
        uses: actions/setup-node@v4
      - name: Build and publish
        run: |
          npm ci
          npm run build
          npm publish --access public
```

### Hotfix and backport workflow

1. Create `hotfix/x.y.z` branch from the stable tag or main.
2. Apply fix, create tests, and merge to main and release branches as necessary.
3. Create a signed tag and publish a patch release.

### Rollback playbook

- If a release fails: revert the release commit or deploy the previous artifact and notify stakeholders.
- Maintain a hotfix branch template to expedite critical fixes.

## Release rollback automation and canary checklist

Releases sometimes fail despite checks. Prepare automated rollback steps and a canary rollout checklist to limit blast radius.

### Automated rollback script (example)

```bash
#!/usr/bin/env bash
# Simple rollback: revert the last release tag and redeploy previous artifact
set -euo pipefail
TAG=${1:-}
if [ -z "$TAG" ]; then
  echo "Usage: $0 vX.Y.Z" >&2
  exit 1
fi
# find previous tag
PREV=$(git for-each-ref --sort=-creatordate --format '%(refname:short)' refs/tags | grep -E '^v[0-9]+' | sed -n '2p')
if [ -z "$PREV" ]; then
  echo "No previous tag found" >&2
  exit 1
fi
# create a revert commit on main (or use deploy system to rollback artifact)
git checkout main
git revert -m 1 $(git rev-list -n 1 $TAG) -m "Revert release $TAG -> rollback to $PREV"
git push origin main
# trigger deployment pipeline for $PREV via CI API (pseudo)
# curl -X POST -H "Authorization: Bearer $CI_TOKEN" https://ci.example.com/deploy?tag=$PREV

echo "Rollback started to $PREV"
```

### Canary rollout checklist

- Deploy to a small percentage of users/nodes and monitor error rates and latency.
- Verify health checks and run smoke tests against canary instances.
- Wait an observation window (e.g., 30–60 minutes) before broader rollout.
- If errors exceed threshold, trigger automated rollback and notify stakeholders.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Role</th><th>Responsibility</th><th>On-call</th></tr>
  </thead>
  <tbody>
    <tr><td>Release Engineer</td><td>Coordinate release and rollback</td><td>Primary</td></tr>
    <tr><td>Platform SRE</td><td>Monitor and perform rollback actions</td><td>Secondary</td></tr>
    <tr><td>Product Owner</td><td>Communicate impact and customer notices</td><td>As needed</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Post-release audit and metrics

- Collect deployment artifacts, verification logs, and monitoring snapshots.
- Record release metrics: success rate, time-to-rollback, and regression causes.
- Produce a short post-release report attached to the release ticket.

## Emergency release playbook (short)

- If the release is a hotfix, ensure a separate hotfix branch workflow and minimal verification steps to speed delivery while preserving safety.
- For immediate mitigation, consider feature flag toggles to disable the faulty behavior without full rollback.

## Appendix: Release Automation, Signing, and Audit Templates

This appendix provides practical scripts, CI snippets, and audit-table templates to support reliable, auditable releases.

### Automated Tag Verification

A small script to verify that release tags are annotated and signed by a set of trusted keys.

```bash
#!/usr/bin/env bash
set -euo pipefail
TRUSTED_KEYS=("ABCDEF1234567890" "0123456789ABCDEF")
for tag in $(git tag -l "v*"); do
  echo "Checking $tag"
  if git verify-tag "$tag" >/dev/null 2>&1; then
    echo "  signed"
  else
    echo "  WARNING: $tag not signed"
  fi
done
```

### Release Audit Table (HTML)

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Release</th><th>Tag</th><th>Signed?</th><th>Artifacts</th></tr>
  </thead>
  <tbody>
    <tr><td>v2.3.0</td><td>refs/tags/v2.3.0</td><td>Yes</td><td>S3://artifacts/org/v2.3.0/</td></tr>
    <tr><td>v2.2.0</td><td>refs/tags/v2.2.0</td><td>No (deprecated)</td><td>S3://artifacts/org/v2.2.0/</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### CI Snippet: Tag-Driven Release (extended)

```yaml
name: Release
on:
  push:
    tags:
      - 'v*.*.*'

jobs:
  verify-and-publish:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
        with:
          fetch-depth: 0
      - name: Verify tag signature
        run: |
          git fetch --tags
          if git verify-tag ${{ github.ref_name }}; then
            echo "Tag signed"
          else
            echo "Tag not signed"; exit 1
          fi
      - name: Build and sign artifacts
        run: |
          ./ci/build.sh
          ./ci/sign_artifacts.sh
      - name: Publish
        run: ./ci/publish.sh
```

### Rollback Automation (snippet)

```bash
#!/usr/bin/env bash
# rollback-to-previous.sh: revert last release and redeploy previous artifact
set -euo pipefail
CURRENT_TAG=${1:-}
if [ -z "$CURRENT_TAG" ]; then
  echo "Usage: $0 <current-tag>" >&2
  exit 2
fi
PREV_TAG=$(git for-each-ref --sort=-creatordate --format '%(refname:short)' refs/tags | grep -E '^v[0-9]+' | sed -n '2p')
if [ -z "$PREV_TAG" ]; then
  echo "No previous tag found" >&2
  exit 3
fi
# revert merge commit created by tagged release
git checkout main
git revert -m 1 $(git rev-list -n 1 $CURRENT_TAG) -m "Revert release $CURRENT_TAG -> rollback to $PREV_TAG"
git push origin main
# trigger CI deploy for $PREV_TAG via CI API (pseudo)
# curl -X POST -H "Authorization: Bearer $CI_TOKEN" https://ci.example.com/deploy?tag=$PREV_TAG
```

### Release Metadata and Audit Record (JSON Template)

```json
{
  "release": "vX.Y.Z",
  "tag_sha": "<sha>",
  "signed": true,
  "artifact_location": "s3://artifacts/org/vX.Y.Z/",
  "released_by": "release-bot",
  "released_at": "2025-10-04T00:00:00Z",
  "notes": "Automated release via tag-driven pipeline"
}
```

### Post-Release Verification Checklist

- Verify CI artifacts exist for the tag and checksums match expected values.
- Confirm signature verification for the tag and artifacts.
- Verify canary targets show stable metrics for the observation window.
- Publish a short audit note with links to artifacts and monitoring snapshots.

---

<!-- end release appendix -->
