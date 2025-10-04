# Lesson 2.1: Initializing, Cloning, and Structuring Repositories

## Choosing the Right Start

Git workflows usually begin with either `git init` or `git clone`.

- `git init` bootstraps an empty repository with a new `.git` directory.
- `git clone <url>` copies an existing history, including branches and tags.

![Repository Topology](../../../../resources/git/git_repository_topology.svg)

When initializing, consider `.gitignore` templates, default branches (`git symbolic-ref HEAD refs/heads/main`), and repository scaffolding.

## Repository Layout Best Practices

## Managing Remotes

Each clone begins with an `origin` remote. Additional remotes facilitate upstream contributions.

```bash

### Remote Synchronization Cycle

![Git Remote Synchronization Cycle](../../../../resources/git/git_remote_sync_cycle.svg)

Synchronization hinges on references and fetch specs:

- `git fetch` updates remote-tracking branches without altering local work.
- `git push` advances destination refs, subject to fast-forward policies.
- Mirror remotes (`--mirror`) replicate both refs and tags for disaster recovery.

### Maintenance Planner

![Git Repository Maintenance Planner](../../../../resources/git/git_repository_maintenance.svg)

Healthy repositories follow recurring tasks:

- Run `git fsck` and `git gc` in scheduled jobs to detect corruption early.
- Prune stale branches using policies tied to review tools and deployment cadence.
- Archive long-lived refs into bundles to keep active repositories lean.
git remote add upstream git@github.com:org/project.git
git fetch upstream
```

Use `git remote set-url` to swap protocols (HTTPS ↔ SSH) and `git remote prune` to delete stale references.

## Practice

- Initialize a repository with a starter README, `.gitignore`, and license file.
- Clone a remote repository using both SSH and HTTPS to compare credential flows.
- Add a secondary remote and examine how `git remote -v` reflects it.

## Repository Management Appendix — Operational Playbook

This appendix covers maintenance, backups, archival, migrations, permission ownership, and automation recipes. It is intended to be a practical runbook for repo administrators.

### Ownership and Access Matrix

<table>
  <thead>
    <tr>
      <th>Area</th>
      <th>Owner</th>
      <th>Frequency</th>
      <th>Action</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Garbage collection</td>
      <td>Platform Team</td>
      <td>Weekly</td>
      <td>Run `git gc --auto` on mirror replicas and monitor pack sizes</td>
    </tr>
    <tr>
      <td>Integrity checks</td>
      <td>Platform Team</td>
      <td>Daily</td>
      <td>Run `git fsck --full` and report failures</td>
    </tr>
    <tr>
      <td>Branch pruning</td>
      <td>Repo Owner</td>
      <td>Monthly</td>
      <td>Archive stale branches and remove refs older than policy</td>
    </tr>
    <tr>
      <td>Backups</td>
      <td>SRE</td>
      <td>Daily</td>
      <td>Create mirror bundles and verify restore process</td>
    </tr>
  </tbody>
</table>

---

### Maintenance Scripts (Examples)

Small scripts automate `fsck`, `gc`, and backup creation. Example weekly maintenance script:

```bash
#!/usr/bin/env bash
set -euo pipefail
repo=$1
cd "$repo"

echo "Running fsck..."
git fsck --full || { echo "fsck failed"; exit 1; }

echo "Running garbage collection..."
git gc --prune=now --aggressive

echo "Creating bundle backup..."
bundle="/backups/$(basename $repo)-$(date +%F).bundle"
git bundle create "$bundle" --all
echo "Bundle created: $bundle"
```

Automate this with your scheduler (cron, systemd timers, or platform jobs) and ensure bundles are replicated off-site.

---

### Migration and Archival Guidance

For large repositories, consider using `git clone --mirror` to create a bare mirror and `git bundle` to transport history. For partial migrations, use `git filter-repo` to split or extract subtrees safely.

Migration checklist:

1. Audit repository size and large files with `git count-objects -vH` and `git verify-pack -v .git/objects/pack/pack-*.idx`.
2. Identify large blobs with `git rev-list --objects --all | sort -k2 | tail -n 50` and inspect with `git cat-file -p`.
3. If rewriting history is necessary, use `git filter-repo` (preferred) with a tested script in a cloned mirror, and retain an immutable archive of the original mirror.
4. Test consumer checkouts and CI against the migrated mirror before switching DNS/CNAME for any git HTTP endpoints.

---

### Disaster Recovery Quick Commands

- Restore from bundle:

```bash
git clone --mirror /backups/project-2023-09-01.bundle project.git
cd project.git
git remote add origin git@github.com:org/project.git
git push --mirror origin
```

- Verify integrity after restore: `git fsck --full`

---

## Archival Retention and Bundle Policies

Define retention windows for bundle archives and mirror snapshots. Example retention policy:

- Daily bundles: retain 14 days
- Weekly bundles: retain 12 weeks
- Monthly bundles: retain 12 months
- Immutable archival snapshot: retain indefinitely (cold storage)

Store bundle metadata (source commit, size, validation hash) in a small manifest JSON alongside bundles for discoverability.

---

### Automated Pruning and Replication

Automation should prune stale refs and replicate bundles to secondary storage. Key points:

- Prune refs older than policy and move to an archive location before deletion.
- Replicate bundles to an off-site object store using signed uploads.
- Validate each replication with `git fsck` on a restored mirror.

Example: replicate bundle to S3-compatible storage

```bash
aws s3 cp "/backups/$(basename $repo)-$(date +%F).bundle" s3://git-archives/$repo/ --acl private
```

---

### Exercises: Backup & Archival

1. Create a small tool that reads an ownership manifest and emits a CODEOWNERS file; test it on a multi-package repo.
2. Implement a scheduled monitoring job that writes repo-size reports and alerts when pack sizes grow rapidly.

---

## Repository lifecycle and governance playbook

This playbook expands governance, backup, archiving, migration, and access-control practices that teams can adopt. It includes checklists, sample scripts, and an ownership matrix in HTML format (per repository).

### Repository creation checklist

- Define purpose and scope: single-service, shared library, monorepo component.
- Choose repository type: bare, monorepo, microservice split.
- Initialize with CODEOWNERS, README, CONTRIBUTING, LICENSE, and a basic CI pipeline.
- Decide large-file strategy (Git LFS or external artifact storage).
- Add a maintenance schedule and assign an owner (team and primary contact).

### Backup and snapshot recipes

A simple, reliable backup strategy is to create bare backups and maintain rolling snapshots on object storage.

```bash
#!/usr/bin/env bash
REPO_PATH="$1"
BACKUP_DIR="$2"
DATE=$(date +%F)

if [ -z "$REPO_PATH" ] || [ -z "$BACKUP_DIR" ]; then
  echo "Usage: $0 /path/to/repo.git /path/to/backup-dir"
  exit 1
fi
mkdir -p "$BACKUP_DIR/$DATE"
cd "$REPO_PATH"

# create a bundle of refs and metadata
git bundle create "$BACKUP_DIR/$DATE/$(basename "$REPO_PATH").bundle" --all

# capture pack info for diagnostics
git count-objects -vH >"$BACKUP_DIR/$DATE/pack-info.txt"

# tar the bare repo for fast restores
tar -czf "$BACKUP_DIR/$DATE/$(basename "$REPO_PATH").tar.gz" -C "$(dirname "$REPO_PATH")" "$(basename "$REPO_PATH")"

# remove older backups (example retention: 90 days)
find "$BACKUP_DIR" -maxdepth 1 -type d -mtime +90 -exec rm -rf {} +
```

### Migration checklist (splitting a repo)

- Identify logical boundaries and module ownership.
- Prepare a migration plan: history to keep, tags to retain, and references to map.
- Use `git filter-repo` or `git subtree` exported trees to carve the repository.
- Run migration on a staging system and validate CI builds and package artifacts.
- Communicate with dependent teams and prepare deprecation timelines.

### Access & ownership matrix (HTML)

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Area</th><th>Responsible</th><th>Permission Level</th><th>Review cadence</th></tr>
  </thead>
  <tbody>
    <tr><td>Repository creation</td><td>Platform team</td><td>Write/merge</td><td>On change</td></tr>
    <tr><td>Release management</td><td>Release engineers</td><td>Admin (tags, releases)</td><td>Per release</td></tr>
    <tr><td>Code reviews</td><td>Maintainers / CODEOWNERS</td><td>Merge</td><td>Weekly</td></tr>
    <tr><td>Security</td><td>Security team</td><td>Audit / read</td><td>Quarterly</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Large file & binary strategy

- Prefer Git LFS for files that change infrequently but are large (for example, large assets or VM images). LFS shifts storage to dedicated servers and keeps the main pack sizes manageable.
- For very large binary artifacts (build outputs, container images), prefer dedicated artifact registries (e.g., S3, GCR, Docker Registry) and keep only sources and small build metadata in Git.

### Archival policy

- Define retention windows: which branches/tags must be retained indefinitely and which can be archived.
- Use `git bundle` for stable archival snapshots; store bundles in a cold object store and point to them from repository metadata.

### Example: archiving old branches

```bash
# archive feature branches older than 1 year into a bundle
REPO_DIR="$1"
cd "$REPO_DIR"

for br in $(git for-each-ref --format='%(refname:short) %(authordate:iso8601)' refs/heads | awk '$2 < "2024-01-01" {print $1}'); do
  git bundle create "/archives/$(basename "$REPO_DIR")-$br.bundle" "$br"
done
```

### Disaster recovery runbook (short)

1. If the primary git server is unavailable, bring up a read-only replica using the latest backup bundle.
2. Validate `git fsck` and check for dangling objects.
3. If the repository is corrupted, restore from the most recent verified bundle. Keep forensic copies of corrupted data for investigation.
4. Notify stakeholders and set a postmortem to examine the cause and improve monitoring.

### Governance: lifecycles and naming conventions

- Adopt consistent branch naming (e.g., `main`, `develop`, `release/*`, `hotfix/*`), tag conventions (semantic tags), and PR templates.
- Enforce via CODEOWNERS, branch protection rules, and CI gating.

### Sample audit script

```bash
# Summarize repo size, last commit times, and pack info for inventory
REPO="$1"
cd "$REPO"

echo "Repo: $(basename $REPO)"
git rev-parse --is-bare-repository || true
echo "Head: $(git rev-parse --abbrev-ref HEAD)"
echo "Last commit:"; git log -1 --pretty=fuller
git count-objects -vH
ls -lh .git/objects/pack | tail -n +2
```

### Governance matrix: when to split vs keep (high level)

- Keep single repo when: teams frequently cross-change files, CI scope is small, or releases are tightly coupled.
- Split when: ownership is clear, build/test cost becomes large per PR, or scaling of CI is limited by repo size.

## Appendix: templates and PR governance

- Include a PR template that requires: purpose, related issue, risk, tests, and rollback plan.
- Document merge policies: squash vs merge, who can approve for each area, and emergency bypass procedures.

## Migration automation and CODEOWNERS generation

When migrating repositories or splitting monorepos, automate the tedious parts: CODEOWNERS generation, redirects, and dependency mapping.

### Generate CODEOWNERS from an ownership manifest

A small script can convert a JSON manifest into a CODEOWNERS file so ownership stays machine-readable and single-sourced.

```bash
#!/usr/bin/env bash
jq -r 'to_entries[] | "\(.key) \(.value)"' ownership.json > CODEOWNERS
```

### Repo-size monitoring script

Track repo growth over time to spot regressions from large binaries or accidental commits.

```bash
#!/usr/bin/env bash
REPO=$(basename $(pwd))
OUT=/tmp/repo-size-$(date +%F).txt
echo "Repo: $REPO" >$OUT
# total objects and packfile sizes
git count-objects -vH >>$OUT
ls -lh .git/objects/pack >>$OUT
# top blob contributors
git rev-list --objects --all | \
  git cat-file --batch-check='%(objecttype) %(objectname) %(objectsize) %(rest)' | \
  awk '$1=="blob"{print $2" "$3" "$4}' | sort -k2 -n -r | head -n 50 >>$OUT

echo "Wrote $OUT"
```

### Migration status table (HTML)

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Repository</th><th>Status</th><th>Owner</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>repo-a</td><td>In progress</td><td>team-a</td><td>Splitting packages/service-a</td></tr>
    <tr><td>repo-b</td><td>Planned</td><td>platform</td><td>Archive old history</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Archive & deprecation policies

- Provide clear deprecation steps: update docs, create redirects, and leave read-only mirrors for compliance.
- Maintain a `DEPRECATED.md` with pointers to new canonical repos and a migration timeline.

## Exercises: Repo Management

1. Create a small tool that reads an ownership manifest and emits a CODEOWNERS file; test it on a multi-package repo.
2. Implement a scheduled monitoring job that writes repo-size reports and alerts when pack sizes grow rapidly.

---

## Appendix: Automation Recipes, Size Enforcement, and Alerting

This appendix provides concrete automation recipes for ownership, size enforcement, backups, alerting, and restoration playbooks you can drop into your platform tooling.

### Enforce repository maximum blob size (pre-receive hook)

A pre-receive hook can reject pushes that introduce blobs larger than a configured size. Drop this into your server-side `/hooks/pre-receive` (adapt to your hosting environment).

```bash
#!/usr/bin/env bash
# pre-receive hook: reject pushes introducing blobs > 5MB
MAX_BYTES=$((5*1024*1024))
while read old new ref; do
  for obj in $(git rev-list $old..$new); do
    git ls-tree -r -l $obj | awk '{print $4,$3}' | while read mode size path; do
      if [ "$size" != "-" ] && [ "$size" -gt $MAX_BYTES ]; then
        echo "Rejected: $path is larger than 5MB ($size bytes)" >&2
        exit 1
      fi
    done
  done
done
exit 0
```

Notes: on very large pushes this hook can be expensive; use a background verification and fast-fail heuristics where possible (e.g., check refs/pack indexes or use hosting provider APIs if available).

### Automated blob scanner: find new large files in a branch

Run this in CI to warn about large additions before merging.

```bash
#!/usr/bin/env bash
BASE=${BASE_REF:-origin/main}
CHANGED=$(git diff --name-only "$BASE"...HEAD)
for f in $CHANGED; do
  if [ -f "$f" ]; then
    size=$(stat -c%s "$f")
    if [ "$size" -gt $((5*1024*1024)) ]; then
      echo "Large file detected: $f ($size bytes)"
    fi
  fi
done
```

### Automated packfile sanity check (scheduled job)

Run this on mirror clones to detect anomalous pack growth and trigger an alert.

```bash
#!/usr/bin/env bash
set -euo pipefail
REPO_DIR=$1
cd "$REPO_DIR"
# compute pack totals
total=$(du -sb .git/objects/pack | cut -f1)
# naive threshold example: 1GB
threshold=$((1*1024*1024*1024))
if [ $total -gt $threshold ]; then
  echo "Packfiles exceed threshold: $total" | mail -s "Repo pack growth alert: $(basename $REPO_DIR)" ops@example.com
fi
```

### Backup validation and restore checklist

1. Create a mirror bundle: `git clone --mirror <repo> repo.git && git bundle create /backups/repo-$(date +%F).bundle --all`.
2. Verify bundle integrity by cloning: `git clone /backups/repo-YYYY-MM-DD.bundle test-repo && git fsck --full`.
3. Test restore to staging remote and run smoke CI builds before cutover.

### Replication to object storage (S3 example)

```bash
bundle=/backups/$(basename $repo)-$(date +%F).bundle
aws s3 cp "$bundle" s3://git-archives/$(basename $repo)/ --acl private
# verify checksum after upload
localsum=$(sha256sum "$bundle" | cut -d' ' -f1)
remotesum=$(aws s3api head-object --bucket git-archives --key "$(basename $repo)/$(basename $bundle)" --query ETag --output text)
```

### Alerting table (HTML)

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Alert</th><th>Trigger</th><th>Owner/Action</th></tr>
  </thead>
  <tbody>
    <tr><td>Packfile growth</td><td>Pack size &gt; configured threshold</td><td>Platform team: run `git verify-pack` and scan recent commits</td></tr>
    <tr><td>Failed fsck</td><td>`git fsck --full` returns errors</td><td>Platform on-call: restore from bundle and investigate packs</td></tr>
    <tr><td>Large file push</td><td>Push introducing blob &gt; 5MB</td><td>Reject push and notify author with remediation steps</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Policy JSON: bundle retention example

```json
{
  "daily": 14,
  "weekly": 12,
  "monthly": 12,
  "archive": "s3://cold-storage/git-archives/"
}
```

### Exercises: Size Enforcement & Alerts

1. Implement the pre-receive size-check hook in a test bare repository and measure its performance for large pushes.
2. Create a scheduled job that runs the packfile sanity check and opens an issue automatically when thresholds are exceeded.

---

<!-- end appended repository management appendix -->

## Repository Management — Extended Exercises & Quick Scripts

### Quick script: rename default branch across many clones

```bash
#!/usr/bin/env bash
# rename-default-branch.sh
OLD=${1:-master}
NEW=${2:-main}
for repo in "$@"; do
  echo "Processing $repo"
  git clone --quiet "$repo" tmp-$RANDOM
  cd tmp-$RANDOM
  git branch -m "$OLD" "$NEW" || true
  git push origin -u "$NEW"
  git push origin --delete "$OLD" || true
  cd ..
  rm -rf tmp-*
done
```

### Quick script: bulk archive old repositories

```bash
#!/usr/bin/env bash
INVENTORY=$1
while IFS=, read -r repo owner; do
  echo "Archiving $repo"
  git clone --mirror "$repo" /tmp/archives/$(basename "$repo").git
  tar -czf /tmp/archives/$(basename "$repo").tar.gz -C /tmp/archives $(basename "$repo").git
  aws s3 cp /tmp/archives/$(basename "$repo").tar.gz s3://git-archives/$(basename "$repo")/
done < "$INVENTORY"
```

### Extended checklist (copyable)

- Ensure CODEOWNERS exists and is up-to-date for critical paths.
- Validate pre-receive hooks for size and policy checks.
- Run a `git fsck --full` on mirror clones weekly and escalate failures.
- Maintain a manifest of archive bundles with checksums and creator metadata.

### Extended exercises (unique)

1. Build an automation that scans all repos in an organization and reports those lacking CODEOWNERS, creating issues for repo owners.
2. Implement a bulk-archive pipeline that creates bundles for repos inactive for >6 months and uploads them to cold storage.
3. Create a short onboarding checklist and a `bootstrap.sh` script that sets up a sparse checkout for new contributors and enforces local gitconfig recommendations.

---

End of Repository Management — Extended Exercises & Quick Scripts.
