# Lesson 5.1: Rewriting History with Filter-Repo and Filter-Branch

## Why Rewrite History?

Removing sensitive data, restructuring repositories, or extracting components sometimes requires rewriting history. Git’s reflog and backups make it reversible when executed carefully.

![Filter Repo Flow](../../../../resources/git/git_filter_repo_flow.svg)

## Tools of Choice

- `git filter-repo` (recommended) – performant, flexible, and actively maintained.
- `git filter-branch` – legacy tool; slower and more error-prone.
- `BFG Repo-Cleaner` – simplified interface for removing large files or secrets.

```bash
# Remove a directory from history
git filter-repo --path build/ --invert-paths
```

## Safety Checklist

1. Communicate with collaborators; force pushes impact everyone.
2. Backup by cloning or copying `.git` before rewriting.
3. Invalidate credentials if secrets were leaked.

### Guardrails for History Rewrites

![History Rewrite Guardrails](../../../../resources/git/git_history_rewrite_guardrails.svg)

Mitigate fallout by combining:

- Branch protection exceptions scoped to trusted maintainers.
- Bundles and offsite archives stored before destructive operations.
- Communication plans outlining impact, timelines, and rollback steps.

### Force Push Risk Matrix

![Force Push Risk Matrix](../../../../resources/git/git_force_push_matrix.svg)

Evaluate when a force push is acceptable:

- Low-risk personal branches can be rewritten freely.
- Shared integration branches require coordination and automated notifications.
- Release branches rarely accept rewrites; prefer follow-up commits and revert trees.

### Practice

- Use `git filter-repo` to remove a large binary from sample history.
- Rewrite author information across commits.
- Compare repository sizes before and after rewriting.

## Extended Guide: Planning and Executing Large-Scale Rewrites

Rewriting history at scale requires a project plan, test runbooks, and automated verification. The following sections walk through interactive rebase tasks, scripted filter-repo operations, and a coordinated rollout with minimal disruption.

### Planning Checklist (HTML Table)

<table>
 <thead>
  <tr>
   <th>Step</th>
   <th>Action</th>
   <th>Why It Matters</th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <td>Scope</td>
   <td>Identify affected branches, tags, and contributors</td>
   <td>Ensures you know who will be impacted by rewritten SHAs</td>
  </tr>
  <tr>
   <td>Backup</td>
   <td>Create a `git bundle --all` and mirror clones</td>
   <td>Provides a safe rollback and forensic snapshot</td>
  </tr>
  <tr>
   <td>Dry Run</td>
   <td>Run `git filter-repo --path` in a cloned testbed and validate</td>
   <td>Validates scripts and measures resulting repo size changes</td>
  </tr>
  <tr>
   <td>Coordination</td>
   <td>Notify teams and freeze writes during push window</td>
   <td>Avoids push conflicts and minimizes lost work risk</td>
  </tr>
  <tr>
   <td>Rollout</td>
   <td>Push rewritten branches to a staging remote, run CI, then replace canonical refs</td>
   <td>Phased cutover reduces surprise and lets automation catch regressions</td>
  </tr>
 </tbody>
</table>

### Example: Removing a Large File with git-filter-repo (Scripted)

The recommended tool for complex, high-performance rewrites is `git-filter-repo`. Example script to remove a `secrets.txt` file from all history and replace it with a placeholder commit:

```bash
#!/usr/bin/env bash
set -euo pipefail

git clone --mirror git@github.com:example/org-repo.git repo-mirror.git
cd repo-mirror.git

# Dry-run first
git filter-repo --path secrets.txt --invert-paths --dry-run

# If dry-run looks good, run for real
git filter-repo --path secrets.txt --invert-paths

# Optional: add a placeholder commit explaining removal
git reflog expire --expire=now --all
git gc --prune=now --aggressive

# Push to new, locked remote and run CI verification
git push --mirror git@github.com:example/org-repo-clean.git
```

### Communicating with Contributors

Coordinate via email/issue trackers and include precise instructions for collaborators to rebase their work onto the new history (for example: `git fetch origin && git rebase origin/main`), or provide branch-mapping tables and short-lived compatibility refs.

### Recovery: If Something Goes Wrong

- Use the backup bundle to restore the prior state: `git clone repo.bundle restored`.
- Re-run verification scripts to locate missing tags or notes.
- Reopen communication channels and offer a migration window for teams to reapply work.

### Hands-On Lab

1. In a sandbox repo, intentionally commit a secret file and practice removing it with `git filter-repo`.
2. Measure repository size before and after (`git count-objects -vH`).
3. Document the exact sequence you would run on a production repo and review it with a peer.

## Safe history rewrite: policy and recipes

Rewriting history can be necessary (remove secrets, shrink repo, or tidy metadata). Because it changes commit hashes, it must be done with a documented plan, communication, and a recovery path.

### Policy checklist before rewriting history

1. Confirm purpose and scope: what paths, time-range, or patterns will be changed?
2. Identify consumers: which downstream repositories, tags, CI jobs, and mirrors will be affected?
3. Communicate widely: give an embargo window and step-by-step rollback instructions.
4. Prepare a frozen branch: create an immutable snapshot of main (bundle) for forensic and rollback use.
5. Coordinate tag handling: decide whether to re-sign or re-tag releases.

### Common tools and when to use them

- `git filter-repo`: recommended for most modern rewrites—fast, flexible, scriptable, and safer than `filter-branch`.
- `git subtree` / `git fast-export` + `git fast-import`: when carving out directories into new repos while preserving curated history.
- `bfg-repo-cleaner`: targeted for removing large files or passwords quickly; simpler but less flexible than `git filter-repo`.

### Example: removing a file pattern using git-filter-repo

```bash
# remove all files matching *.secret and rewrite history
git clone --mirror git@github.com:org/repo.git repo.git
cd repo.git
# make a safe backup
git bundle create ../repo-before-filter.bundle --all

# run filter-repo to remove file pattern
git filter-repo --invert-paths --path-glob '*.secret' --refs HEAD --replace-refs delete-no-add

# run sanity checks
git fsck --full
# push to a test remote for validation
git push --mirror git@staging.example.com:repo.git
```

Notes:

- Always run `git filter-repo` on a mirror clone and keep a backup bundle.
- Use `--replace-refs delete-no-add` to avoid leaving refs dangling.

### Mapping author metadata (rewrite author emails)

```bash
# map a set of emails to a canonical author
cat >authors.txt <<'EOF'
Old Name <old@example.com> = New Name <new@example.com>
EOF

git filter-repo --mailmap authors.txt
```

### Rolling out a rewrite (recommended staging flow)

1. Run rewrite in a mirror and push to a staging remote.
2. Validate CI and downstream tooling against the staging remote for a few days.
3. Announce a migration window and coordinate with integrators.
4. Push rewritten repo to production remote (force or mirror push), or create a new canonical repository and update references.
5. Deprecate old remote gradually and provide a mapping table for old → new commit SHAs for critical artifacts.

### Rewriting large binary history safely

- For very large binary histories, consider creating an archival repository with `git bundle` for old history and keep the active repo trimmed to recent history.
- Alternatively, use `git filter-repo` to replace blobs with pointer files and migrate binaries into an artifact registry.

### Recovery playbook after accidental rewrite

1. If humans notice a problem quickly, stop any further changes and preserve logs.
2. Restore from the last verified bundle: `git clone <bundle> repo && git push --mirror origin` to restore original refs.
3. For data recovery of accidentally removed data, retain copies of the unmodified mirror and use `git reflog` or low-level object recovery tools (`git fsck`, `git cat-file`) to extract objects.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Rewrite scenario</th><th>Tool</th><th>Risk</th><th>Recommended mitigation</th></tr>
  </thead>
  <tbody>
    <tr><td>Remove secrets</td><td>git-filter-repo / BFG</td><td>High (breaks refs)</td><td>Mirror clone, backup bundle, staging validation</td></tr>
    <tr><td>Split repo</td><td>filter-repo / fast-export</td><td>Medium (dependency mapping)</td><td>Staging migration, CI validation, cross-repo redirects</td></tr>
    <tr><td>Compact history</td><td>filter-repo / grafting</td><td>Low-Medium</td><td>Archive old history; document mapping</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Appendices: scripts for safe rewrite audits

### Create a pre-rewrite forensic bundle

```bash
git clone --mirror git@github.com:org/repo.git repo.git
cd repo.git
# create a forensic snapshot
git bundle create ../forensic-$(date +%F).bundle --all
# record pack and object stats
git count-objects -vH >../forensic-$(date +%F)-count-objects.txt
ls -lh .git/objects/pack >../forensic-$(date +%F)-packs.txt
```

### Generate a mapping of old to new commit IDs (post rewrite)

```bash
# after rewrite, produce a mapping for critical tags
git for-each-ref --format='%(refname) %(objectname)' refs/tags | while read ref sha; do
  echo "$ref $sha"
done >../tags-before.txt
# after rewrite produce tags-after.txt and use a diff tool to detect changes
```

## Tag mapping and release reconciliation

When you rewrite history, tags may point to different commit IDs. Maintain a mapping table and consider re-signing tags.

### Generate tag mapping before and after rewrite

```bash
# tags-before.txt
git for-each-ref --format='%(refname:short) %(objectname)' refs/tags > tags-before.txt
# after rewrite run same command and diff the results to find changed tag targets
```

### Re-signing tags workflow

- Extract current tag messages and associated release notes.
- Create new signed tags on the rewritten commits and publish them with a clear note linking to the original tag mapping.

## Legal & audit notes

- When rewriting history to remove sensitive data, document the change and retain a secure, access-controlled forensic bundle for auditors.
- Keep a signed attest that the rewrite was performed, who authorized it, and when it was applied.

## Large-subtree extraction example (fast-export / fast-import)

```bash
# export a subtree of history for packages/service-a
git clone --no-local --bare git@github.com:org/repo.git repo.git
cd repo.git
# export with fast-export filtering
git fast-export --all -- path packages/service-a | (cd ../new-service-a && git init; git fast-import)
cd ../new-service-a
git remote add origin git@github.com:org/service-a.git
git push origin --all
```

### Exercise

- Practice extracting a subtree and validate that CI for the extracted repository functions as expected before deprecating the original paths.

## Governance templates and stakeholder communication

When planning a history rewrite, it's crucial to communicate to stakeholders and provide machine-readable mappings to ease downstream updates.

### Stakeholder announcement template

- Subject: Planned history rewrite for `repo` — impact and timeline
- Body: Purpose, affected refs, migration steps, expected downtime, contact points, and roll-back instructions.

### Redirect strategy for consumers

- For major rewrites, consider creating a new canonical repository and add `README` pointers from the legacy repo to the new location.
- Provide a mapping file `OLD_TO_NEW_SHA.csv` for critical releases and artifacts so tooling can translate SHAs when verifying artifacts.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Item</th><th>Action</th><th>Owner</th></tr>
  </thead>
  <tbody>
    <tr><td>Old tags</td><td>Generate mapping and re-sign on new commits</td><td>Release team</td></tr>
    <tr><td>CI refs</td><td>Update CI to point at new remote or mapping layer</td><td>CI engineers</td></tr>
    <tr><td>External mirrors</td><td>Coordinate update and provide migration tool</td><td>Platform</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Programmatic mapping example (CSV)

```
old_sha,new_sha,ref
abc123,def456,refs/tags/v1.2.0
```

### Audit record example (JSON)

```json
{
  "rewrite_id": "rw-2025-10-04-01",
  "author": "platform-team",
  "timestamp": "2025-10-04T03:00:00Z",
  "notes": "Removed PII blobs from history",
  "bundle": "s3://forensics/repo-forensic-2025-10-04.bundle"
}
```

## Post-migration checks

- Validate CI builds against the rewritten repo and run integration smoke tests.
- Verify tag signatures and artifact provenance for all critical releases.

## Mapping automation: small Python example

When you rewrite history it's useful to produce a machine-readable mapping of old→new SHAs. The example below parses `git for-each-ref` output and creates a CSV mapping.

```python
#!/usr/bin/env python3
# map-tags.py: produce old->new tag mapping
import csv, subprocess

def tags(refs_cmd):
    out = subprocess.check_output(refs_cmd, shell=True, text=True)
    for line in out.splitlines():
        if line.strip():
            name, sha = line.split()
            yield name, sha

if __name__ == '__main__':
    before_cmd = "git for-each-ref --format='%(refname:short) %(objectname)' refs/tags"
    before = dict(tags(before_cmd))
    # after: assume we run this after rewrite in the rewritten repo
    after_cmd = before_cmd
    after = dict(tags(after_cmd))
    with open('old_to_new_tags.csv', 'w', newline='') as f:
        w = csv.writer(f)
        w.writerow(['ref', 'old_sha', 'new_sha'])
        for ref in sorted(set(before) | set(after)):
            w.writerow([ref, before.get(ref, ''), after.get(ref, '')])
```

### Verification steps after rewrite

- Run `git fsck --full` on the rewritten mirror and on a restored clone to ensure object integrity.
- Validate critical tags and artifact signatures; if signatures changed, produce a signed attestation linking old and new tags.

### Chain-of-custody and audit artifacts

- Keep a signed audit record of who authorized the rewrite and a secure forensic bundle for legal/audit review.
- Record the mapping file and store it in a write-once location (e.g., an append-only log or a secure object store).

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Stakeholder</th><th>Responsibility</th><th>Contact</th></tr>
  </thead>
  <tbody>
    <tr><td>Platform Team</td><td>Perform rewrite and backups</td><td>platform@example.com</td></tr>
    <tr><td>Release Team</td><td>Re-sign and publish releases</td><td>releases@example.com</td></tr>
    <tr><td>Legal</td><td>Ensure compliance with retention and audit requirements</td><td>legal@example.com</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Post-rewrite validation checklist

- Verify downstream CI jobs against the rewritten repo.
- Confirm artifacts' provenance and SBOM where applicable.
- Publish migration notes with mapping file links and contact points.

---

## Appendix: Rewriting History — Cross-Repo Mapping, Audit, and Automation

This appendix provides extra automation for producing machine-readable mappings, re-signing tags, and auditing rewritten repos. These artifacts are useful for legal, CI, and dependent repositories.

### Automated tag re-signing helper (bash)

```bash
#!/usr/bin/env bash
# resign-tags.sh: re-sign tags after a rewrite given a mapping CSV old->new
set -euo pipefail
MAPPING=${1:-old_to_new_tags.csv}
while IFS=, read -r old new ref; do
  if [ -n "$new" ]; then
    git tag -a "$ref-new" -m "Re-signed $ref after rewrite" $new
    git push origin "$ref-new"
  fi
done < "$MAPPING"
```

### Mapping consumer: translate verification logs

When CI or downstream tools refer to old SHAs, use a small mapping consumer to translate and verify artifacts.

```python
#!/usr/bin/env python3
import csv, sys
mapping = {}
with open('old_to_new_tags.csv') as f:
    r = csv.reader(f)
    next(r, None)
    for old,new,ref in r:
        mapping[old]=new
sha=sys.argv[1]
print(mapping.get(sha, sha))
```

### Audit table (HTML)

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Item</th><th>Pre-rewrite</th><th>Post-rewrite</th><th>Action</th></tr>
  </thead>
  <tbody>
    <tr><td>Tag v1.2.0</td><td>abc123</td><td>def456</td><td>Re-sign and publish mapping</td></tr>
    <tr><td>Release artifacts</td><td>s3://artifacts/v1.2.0</td><td>verify after re-sign</td><td>Re-publish if checksum mismatch</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Verification pipeline step (CI snippet)

```bash
# translate sha and verify artifact
NEW_SHA=$(python3 map-sha.py $OLD_SHA)
# verify artifact metadata points to NEW_SHA
# if mismatch, fail and attach mapping to issue
```

### Post-rewrite validation checklist (copyable)

- Verify all tags referenced by downstream CI map to new SHAs.
- Run `git fsck --full` on rewritten mirror and restored clones.
- Verify artifact signatures and SBOMs against re-signed tags.
- Publish mapping CSV to a known location and add it to release notes.

---

<!-- end appended history-rewrite appendix -->
