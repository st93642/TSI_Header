# Lesson 5.2: Reflog, Stash, and Disaster Recovery Playbooks

## Reflog – Your Safety Net

Reflog records movements of HEAD and branch tips even when commits are orphaned. Use it to recover lost work:

```bash
git reflog
git checkout HEAD@{3}
```

![Recovery Matrix](../../../../resources/git/git_recovery_matrix.svg)

## Stashing Work in Progress

`git stash` saves unfinished changes without committing:

```bash
git stash push -m "WIP: refactor auth"
git stash list
git stash apply stash@{0}
```

Use `git stash --keep-index` to stage subset changes while stashing the rest.

### Recovery Workflow

![Recovery Workflow](../../../../resources/git/git_recovery_workflow.svg)

Respond methodically when something breaks:

- Audit the situation with `git status`, reflog entries, and remote state.
- Determine if the fix is a checkout, merge, or cherry-pick from historical commits.
- Record remediation steps in runbooks to shorten future recovery times.

## Disaster Scenarios

- **Accidental Reset** – restore with `git reflog`.
- **Force Push Overwrite** – fetch remote reflogs or coordinate with teammates.
- **Corrupted Repository** – verify with `git fsck`, recover from backups.

### Safety Net Layers

![Safety Net Layers](../../../../resources/git/git_safety_net_layers.svg)

Layer safeguards to minimize data loss:

- Reflogs capture local history movements for 90+ days.
- Stashes and worktrees isolate experiments without polluting main branches.
- Remote backups and bundles ensure bare clones can be restored quickly.

## Troubleshooting & Recovery Appendix — Incident Runbooks

This appendix supplies concrete steps, example commands, and checklists for common and severe repository incidents.

### Incident 1: Recovering Lost Commits After `reset --hard`

1. Stop doing any writes to the repository (don't `git gc` or re-clone yet).
2. Inspect the reflog: `git reflog --date=iso` and locate the SHA of the lost commit.
3. Create a branch at that SHA to preserve it: `git branch recover/forgotten <sha>`.
4. Push the branch to a remote backup: `git push origin recover/forgotten`.

Example:

```bash
sha=$(git reflog --pretty=%h | sed -n '1p')
git branch recover/$sha $sha
git push origin recover/$sha
```

### Incident 2: Undoing an Accidental Force-Push

If a collaborator force-pushed and overwrote history, perform these steps:

1. Ask everyone to stop pushing to the affected branch.
2. On a clone that still has the old history, create a mirror and push it back: `git push --mirror origin` from the clone with intact refs.
3. If no clone has the old refs, look at remote hosting provider backups (some providers keep refs for a limited window) or use the hosting UI to find the previous commit SHA.

If you find the lost commit SHA, restore it locally and push a new branch or force push carefully after coordination.

### Incident 3: Dangling Objects and Corruption

Use `git fsck --full` to detect corruption and dangling blobs/commits. For dangling commits, you can inspect with `git show <sha>` and recreate refs.

If pack files are corrupted, replace them from mirror backups or recreate packs with `git repack -a -d --window=250 --depth=250` after restoring object files.

### Emergency Rescue Commands Quick Reference

<table>
  <thead>
    <tr>
      <th>Problem</th>
      <th>Command</th>
      <th>Notes</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Find lost commits</td>
      <td><code>git reflog</code></td>
      <td>Reflog shows HEAD movements</td>
    </tr>
    <tr>
      <td>Recover dangling commit</td>
      <td><code>git show &lt;sha&gt;</code></td>
      <td>Inspect object contents</td>
    </tr>
    <tr>
      <td>Recreate pack</td>
      <td><code>git repack -a -d</code></td>
      <td>Run after restoring objects</td>
    </tr>
  </tbody>
</table>

---

### Post-Incident Checklist

- Document timeline: who did what and when.
- Preserve copies of relevant clones and bundles.
- Restore services in read-only mode until integrity is validated.
- Run `git fsck --full` on restored replicas.
- Communicate to stakeholders with a postmortem summarizing root cause and follow-ups.

---

### Recovery Exercises

1. Simulate a force-push overwrite in a sandbox and practice recovery using a mirror clone.
2. Create a script that scans reflogs across multiple clones and reports unreachable SHAs to a central dashboard.
3. Practice creating and restoring from bundles, then verify `git fsck` passes.

---

## Multi-Repo Incident Playbook — Cross-Repo Forensics and Communication

In organizations with many repositories, incidents often span multiple repos. This playbook helps coordinate multi-repo investigations.

### Triage Steps

1. Identify the impact surface: which repos, services, and CI jobs are affected.
2. Create a central incident namespace (e.g., `incident/<id>`) and push temporary diagnostic refs there for safe sharing.
3. Collect `git reflog`, `git fsck`, and recent push metadata from each affected mirror.

---

### Artifact Collection Script (diagnostics)

```bash
#!/usr/bin/env bash
set -euo pipefail
OUTDIR=/tmp/incident-$1
mkdir -p "$OUTDIR"
for repo in $@; do
  name=$(basename "$repo")
  mkdir -p "$OUTDIR/$name"
  git --git-dir="$repo/.git" reflog show --date=iso > "$OUTDIR/$name/reflog.txt" 2>&1 || true
  git --git-dir="$repo/.git" fsck --no-progress --unreachable > "$OUTDIR/$name/fsck.txt" 2>&1 || true
done
```

---

### Communication Template (short)

- Subject: [INC-<id>] Repository integrity incident — initial findings
- Summary: Short description of impact and affected repos
- Actions taken: list of commands and backups created
- Next steps: who is owning the restore and timeline

---

### Post-Incident Remediation

- Rotate any deploy keys or credentials exposed during investigation.
- Schedule a follow-up to harden automation (prevent recurrence) and update runbooks.
- Publish a blameless postmortem with timelines, root cause, and action items.

---

<!-- markdownlint-disable MD033 MD010 -->

## Recovery Runbook Templates & Play Actions

Use the following runbook templates during incidents to coordinate actions and communication.

### Immediate Play: Lost Commits

- Owner: Repo oncall
- Impact: local commits lost or overwritten
- Steps:
  1. Preserve current state: `git bundle create /tmp/current.bundle --all`
  2. Inspect `git reflog` for last known commit
  3. Create rescue branch: `git branch rescue/<id> <sha>`
  4. Push rescue branch to a backup remote

### Immediate Play: Corrupted Pack

- Owner: Platform SRE
- Steps:
  1. Isolate the repo (read-only)
  2. Restore latest bundle from backups into a temp mirror
  3. Run `git fsck --full` and compare object lists
  4. Replace corrupted packs on the primary only after validation

---

## Incident runbook: repository corruption and recovery

When corruption or accidental deletion occurs, follow a calm, reproducible runbook to collect evidence, mitigate impact, and restore service.

### Immediate actions

1. Preserve the current state: create a mirror bundle (`git bundle create /tmp/repo-preserve.bundle --all`).
2. Run `git fsck --full` to identify problems and capture output to an evidence file.
3. If a replica exists, fail over reads to the replica and stop writes until recovery plan is in place.

### Dealing with lost refs and dangling objects

```bash
# try to recover by inspecting lost-found
git fsck --lost-found
# inspect lost-found objects
ls -lh .git/lost-found/commit
for o in .git/lost-found/commit/*; do git show $o; done
```

### Reflog recovery recipe

```bash
# recover a lost branch from reflog
git reflog show --all > /tmp/reflog-all.txt
# find commit of interest, then recreate branch
git checkout -b recovered-branch <sha>
```

### Corruption mitigation steps

- If packfile corruption is detected, retrieve last known-good bundle and use it to repopulate a staging remote.
- For partial corruption, extract healthy objects with `git cat-file` and recreate missing refs.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Severity</th><th>Action</th><th>Owner</th></tr>
  </thead>
  <tbody>
    <tr><td>High (production outage)</td><td>Fail-over to replica, restore from bundle</td><td>Platform on-call</td></tr>
    <tr><td>Medium (data inconsistency)</td><td>Investigate, restore affected refs, schedule full repo audit</td><td>Repo maintainers</td></tr>
    <tr><td>Low (local corruption)</td><td>Use reflog recovery and notify owner</td><td>Contributor</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Post-incident

- Run a full audit and record lessons learned; automate checks to catch similar issues earlier.
- Maintain a rotating verified backup and test restoration periodically.

### Exercises: Recovery Practice

1. Simulate a corrupted packfile in a sandbox and practice restoring from a bundle.
2. Practice reflog recovery after an accidental reset and create an evidence package to share in a postmortem.

<!-- end appended troubleshooting content -->

## Monitoring, retention, and automated checks

Having runbooks is necessary but not sufficient—automate detection and capture of repository anomalies.

### Automated health-check script (example)

```bash
#!/usr/bin/env bash
# quick healthcheck: packfile sizes, loose objects, last commit times
set -euo pipefail
OUTDIR=${1:-/tmp/git-health}
mkdir -p "$OUTDIR"

echo "Repository: $(basename $(pwd))" >"$OUTDIR/summary.txt"
# packfiles
ls -lh .git/objects/pack | tail -n +2 >"$OUTDIR/packfiles.txt"
# object counts
git count-objects -vH >>"$OUTDIR/summary.txt"
# last commits by branch
for b in $(git for-each-ref --format='%(refname:short)' refs/heads); do
  echo "$b: $(git log -1 --pretty=format:'%ci %h' $b)" >>"$OUTDIR/summary.txt"
done

echo "Healthcheck written to $OUTDIR"
```

### Retention policy sample

- Keep reflog entries 90+ days for critical repos; adjust with `git config gc.reflogExpire` and `gc.reflogExpireUnreachable`.
- Maintain weekly forensic bundles stored in cold object storage for 1 year.
- Rotate copies of mirror bundles offsite quarterly.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Artifact</th><th>Retention</th><th>Owner</th></tr>
  </thead>
  <tbody>
    <tr><td>Forensic bundle</td><td>1 year</td><td>Platform</td></tr>
    <tr><td>Mirror backup</td><td>90 days</td><td>Platform/Infra</td></tr>
    <tr><td>Reflog snapshots</td><td>90 days</td><td>Repo owner</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Alerting triggers and play activation

- Trigger an incident when `git fsck` reports corruption or when packfile sizes suddenly increase by >50% week-over-week.
- Automatically collect an evidence package (use the artifact collection script) and create a ticket with the package attached.

### Escalation matrix (sample)

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Severity</th><th>Escalation</th><th>Response SLA</th></tr>
  </thead>
  <tbody>
    <tr><td>Critical</td><td>Platform on-call -> Eng director</td><td>15 min</td></tr>
    <tr><td>High</td><td>Platform on-call</td><td>1 hour</td></tr>
    <tr><td>Medium</td><td>Repo maintainers</td><td>24 hours</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Postmortem checklist (short)

- Attach evidence package and commands executed.
- Identify root cause and three action items with owners and due dates.
- Schedule a follow-up to validate mitigations.

## Incident ticket template and evidence attachment guidelines

A structured ticket accelerates response. Include the following fields and attach the evidence package created by your diagnostics script.

- Title: short description and affected repo(s)
- Severity: Critical / High / Medium / Low
- Summary: one-paragraph description
- Steps to reproduce / Observed behavior
- Evidence package location: S3 path or artifact link
- Action taken and timeline

### Automated evidence collection integration (CI hook)

Integrate the artifact-collection script into your CI or platform automation to automatically collect a package on failures of integrity checks.

```bash
# CI step (pseudo)
- name: Collect git evidence
  run: |
    ./scripts/collect-evidence.sh $REPO_PATH /tmp/evidence-$RUN_ID
    aws s3 cp /tmp/evidence-$RUN_ID.tar.gz s3://forensics/$REPO/$RUN_ID.tar.gz
```

### Retention and access controls for forensic bundles

- For sensitive incidents, encrypt forensic bundles and restrict access to a small set of auditors.
- Keep forensic bundles for a minimum retention window appropriate to compliance (example: 1 year) and ensure manifests record checksums and creators.

## Extended exercises

1. Implement the CI evidence-collection step and attach the artifact to a test incident ticket automatically.
2. Run a simulated corruption scenario and exercise the full runbook, from detection to postmortem publishing.

---

## Appendix: Advanced Incident Response, Automation, and Communication Templates

This appendix adds automation helpers, escalation playbooks, and auditing artifacts to speed triage for repository incidents.

### Rapid Evidence Snapshot (single command)

A one-liner to quickly snapshot key repository metadata for triage.

```bash
OUT=/tmp/evidence-$(basename $(pwd))-$(date +%F-%H%M%S)
mkdir -p $OUT
git log -n 200 --pretty=fuller > $OUT/commits.txt
git reflog --all -n 200 > $OUT/reflog.txt
git count-objects -vH > $OUT/count-objects.txt
ls -lh .git/objects/pack > $OUT/packfiles.txt
tar -czf $OUT.tar.gz -C $(dirname $OUT) $(basename $OUT)
echo "Snapshot: $OUT.tar.gz"
```

### Automated triage script (pseudo)

A script that runs basic checks and creates a ticket when it finds signs of corruption.

```bash
#!/usr/bin/env bash
set -euo pipefail
OUT=$(mktemp -d)

if ! git fsck --full > $OUT/fsck.txt 2>&1; then
  # upload evidence and file ticket (pseudo)
  tar -czf $OUT.tar.gz -C $OUT .
  # curl -X POST -H "Authorization: Bearer $TOKEN" -F "file=@$OUT.tar.gz" https://ticket.example.com/api/issues
  echo "Issue created with evidence: $OUT.tar.gz"
fi
```

### Escalation email template (short)

Subject: [INC] Repo integrity alert  $(basename $(pwd))

Body:

- Summary: `git fsck` produced errors
- Evidence: link to artifact bundle
- Immediate action: fail-over to read-only replica and assign platform on-call


### Playbook: force-push recovery (coordinated)

1. Identify last known-good commit using reflog or mirror clones.
2. Notify contributors and pause pushes to affected branches.
3. Create rescue branches for lost commits and push to a protected backup remote.
4. Coordinate with team to merge or reapply necessary commits after verifying integrity.

### Post-incident reporting JSON (template)

```json
{
  "incident_id":"INC-2025-10-04-01",
  "repo":"$(basename $(pwd))",
  "detected_by":"git fsck",
  "evidence":"s3://forensics/.../evidence.tar.gz",
  "owner":"platform-oncall@example.com"
}
```

### Exercises: Triage Automation

1. Integrate the rapid snapshot command into your incident runbook and test it by creating a simulated issue.
2. Implement the triage script to auto-file tickets when `git fsck` fails in a scheduled maintenance job.

---

<!-- markdownlint-disable MD033 MD010 -->

## Recovery Playbook: Safe Rollbacks & Emergency Procedures

This appendix provides practical rollback scripts, a CI rollback job example, a short HTML incident checklist, and reproducible exercises to practice safe recovery.

### Safe rollback principles

- Prefer targeted fix-forward PRs when possible; rollbacks are for emergency mitigation.
- Coordinate with stakeholders before any force push.
- Preserve evidence (bundles, reflogs) before any destructive operation.
- Use a temporary rescue branch to stage a rollback instead of immediately force-pushing to protected branches.

### Quick rollback recipe (script)

```bash
#!/usr/bin/env bash
set -euo pipefail
BRANCH=${1:-main}
RESCUE_BRANCH=rescue/rollback-$(date +%F-%H%M%S)
TARGET_SHA=$2

# create rescue branch at target sha
git fetch --all
git checkout -b "$RESCUE_BRANCH" "$TARGET_SHA"
# run smoke checks here (placeholder)
# push rescue branch for review
git push origin "$RESCUE_BRANCH"

echo "Created rescue branch: $RESCUE_BRANCH -> $TARGET_SHA"
```

### Safe rollback CI job (GitHub Actions snippet)

```yaml
name: safe-rollback
on:
  workflow_dispatch:

jobs:
  rollback:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
+        with:
+          fetch-depth: 0
      - name: Run rollback script
        env:
          GITHUB_TOKEN: ${{ secrets.GITHUB_TOKEN }}
        run: |
          TARGET_SHA=${{ inputs.target_sha }}
          ./scripts/prepare-rescue.sh "$TARGET_SHA"
```

### Incident checklist (HTML table)

<table>
  <thead>
    <tr><th>Step</th><th>Who</th><th>Confirm</th></tr>
  </thead>
  <tbody>
    <tr><td>Preserve evidence bundle</td><td>On-call</td><td>Bundle uploaded</td></tr>
    <tr><td>Create rescue branch</td><td>Engineer</td><td>Branch pushed</td></tr>
    <tr><td>Run smoke tests</td><td>QA/On-call</td><td>All green</td></tr>
  </tbody>
</table>

<!-- markdownlint-enable MD033 MD010 -->

### Rollback play: Force-push coordination template

- Notify team in the incident channel with the rescue branch link and rationale.
- Hold a 15-minute window for monitoring after rollback.
- If rollback requires a force push to the protected branch, get explicit approval from Release Manager and document the SHA mapping in the ticket.

### Exercises

1. Implement `prepare-rescue.sh` that takes a target SHA and creates a rescue branch, runs a minimal smoke test, and uploads an evidence bundle if tests fail.
2. Configure a GitHub Actions `safe-rollback` workflow that can be triggered manually and requires approval before merging the rescue branch back into `main`.
3. Practice a rollback in a sandbox: create a repo, introduce a regression, then use the rescue flow to recover and validate.

---

End of Recovery Playbook appendix.
