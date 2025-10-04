# Lesson 6.2: Monorepo Strategies, Submodules, and Subtrees

## The Monorepo Landscape

Large organizations often centralize code into a single repository with shared tooling, CI, and dependency management. Git supports this with sparse checkouts, partial clones, and architectural conventions.

![Monorepo Topology](../../../../resources/git/git_monorepo_topology.svg)

## Techniques for Repository Composition

- **Submodules** – embed another repository, tracking exact commits.
- **Subtrees** – vendor code into subdirectories without extra metadata.
- **Partial Clone** – download commits on demand (`git clone --filter=blob:none`).

### Sparse Checkouts

Limit working directory footprint:

```bash
git sparse-checkout init --cone
git sparse-checkout set services/api services/web
```

### Dependency Mesh

![Monorepo Dependency Mesh](../../../../resources/git/git_monorepo_dependency_mesh.svg)

Visualizing dependencies reduces coupling surprises:

- Ownership files (`CODEOWNERS`) clarify which teams steward directories.
- Package managers (Yarn workspaces, pnpm, Cargo) map library-to-service relationships.
- Build graphs (Bazel query, Nx dep-graph) inform impact analysis before merging.

## Trade-Offs

<!-- markdownlint-disable MD033 MD010 -->
<table>
	<thead>
		<tr>
			<th>Strategy</th>
			<th>Pros</th>
			<th>Cons</th>
		</tr>
	</thead>
	<tbody>
		<tr>
			<td>Submodules</td>
			<td>Precise dependencies</td>
			<td>Extra commands, CI complexity</td>
		</tr>
		<tr>
			<td>Subtrees</td>
			<td>Single repository</td>
			<td>History duplication</td>
		</tr>
		<tr>
			<td>Monorepo</td>
			<td>Unified tooling</td>
			<td>Requires disciplined ownership</td>
		</tr>
	</tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### CI Matrix Strategy

![Monorepo CI Matrix](../../../../resources/git/git_monorepo_ci_matrix.svg)

Scale automation by sharding workloads:

- Change detection pipelines run only affected packages per commit.
- Matrix builds test combinations of runtimes, platforms, and feature gates.
- Caching strategies (remote execution, shared caches) keep large builds under control.

### Practice

- Convert a multi-repo project into a monorepo trial using subtrees.
- Experiment with sparse checkout to limit clones to a single service.
- Evaluate tooling (Bazel, Nx, Pants) that thrives in monorepo environments.

## Monorepo Operational Playbook

Moving to a monorepo is as much organizational as technical. This playbook covers ownership, CI scaling, and repo hygiene.

### Ownership and Access (HTML Table)

<table>
 <thead>
  <tr>
   <th>Scope</th>
   <th>Ownership Pattern</th>
   <th>Access Controls</th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <td>Service directories</td>
   <td>Team-level CODEOWNERS per path</td>
   <td>Require PR approvals from code owners</td>
  </tr>
  <tr>
   <td>Shared libraries</td>
   <td>Library maintainers and release automation</td>
   <td>Restrict merges to maintainers and CI gates</td>
  </tr>
  <tr>
   <td>Tooling and infra</td>
   <td>Platform team with scoped write access</td>
   <td>Protect branches and require CI for merges</td>
  </tr>
 </tbody>
</table>

### CI Scaling Patterns

- Use change detection to run only affected package tests and builds per commit.
- Cache dependency artifacts and build outputs in remote caches (e.g., Bazel remote cache, Azure Artifacts, GitHub Packages).
- Shard test suites by component and parallelize across workers.

### Partial Clone and Sparse Checkout Recipes

```bash
# Shallow and partial clone for CI
git clone --filter=blob:none --no-checkout git@github.com:org/mono.git
cd mono
git sparse-checkout init --cone
git sparse-checkout set packages/service-a
git checkout main
```

### Migration Strategy

1. Import repositories with `git subtree` to preserve history, or `git filter-repo --subdirectory-filter` when extracting subtrees.
2. Introduce CODEOWNERS and path-based CI rules gradually.
3. Pilot monorepo for a subset of teams and measure CI performance.

### Migration Runbook and Rollback Considerations

When moving to a monorepo, treat the migration as a release with clear rollback steps.

1. Create a mirror of all source repositories and tag the mirrors with migration timestamps.
2. Run a dry import into a sandbox monorepo and validate change-detection tooling and CI pipelines.
3. Coordinate cross-team migration windows and provide a rollback plan (keep the old repos read-only and accessible via archive links).

Rollback options:

- Restore old repositories from mirror bundles if migration fails.
- Switch CI endpoints back to pre-migration mirrors and route new PR traffic to fallback repos.

---

### Partial Clone Tuning for Monorepos

Tune server-side filters and client heuristics for efficient partial clones:

- Serve `--filter=blob:none` for large monorepo reads.
- Pre-warm the commit-graph and pack indexes for hot paths.
- Provide per-team mirrors optimized for their component sets.

---

### Decision Table: Submodule vs Subtree vs Monorepo

<table>
  <thead>
    <tr>
      <th>Scenario</th>
      <th>Submodule</th>
      <th>Subtree</th>
      <th>Monorepo</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Independent release cadence</td>
      <td>Good — separate repos and versions</td>
      <td>Possible — vendor in with history
</td>
      <td>Challenging — teams must coordinate
</td>
    </tr>
    <tr>
      <td>Tight integration and frequent cross-cutting changes</td>
      <td>Poor — coordination overhead</td>
      <td>Mixed — need tooling
</td>
      <td>Good — single PR can change multiple components
</td>
    </tr>
    <tr>
      <td>Binary heavy artifacts</td>
      <td>Good — keep binaries separate</td>
      <td>Mixed — history grows
</td>
      <td>Poor — repo size grows quickly without LFS
</td>
    </tr>
  </tbody>
</table>

---

## Monorepo operational patterns: tooling and CI partitioning

Monorepos bring developer ergonomics and code-sharing benefits but require tooling and CI design to scale. This section catalogs pragmatic strategies and automation recipes.

### Tooling choices: submodules, subtrees, or monorepo tools

- Submodules: lightweight, preserves separate histories, but increases operational complexity for contributors.
- Subtrees or `git subtree`: useful for infrequent syncs of external projects without introducing submodule workflows.
- Monorepo-specific tools (Bazel, Nx, Rush, Pants): provide dependency graph awareness, targeted builds, and caching to scale CI.

### Partial clone and sparse-checkout for large codebases

Partial clone and sparse-checkout help developers avoid downloading unnecessary data.

```bash
# enable partial clone (server must support it)
git clone --filter=blob:none --no-checkout origin.git myrepo
cd myrepo
git sparse-checkout init --cone
git sparse-checkout set packages/service-a
git checkout main
```

### CI partitioning and targeted builds

Design CI so that only affected projects are built/tested for a given PR. Steps:

1. Compute changed paths between the PR head and base.
2. Map to projects/packages using an ownership manifest.
3. Trigger targeted build/test jobs for impacted projects; run full builds only on main or release branches.

Example `make`-style helper used in CI (bash):

```bash
#!/usr/bin/env bash
BASE=${BASE_REF:-origin/main}
CHANGED=$(git diff --name-only "$BASE"...HEAD)
# derive a set of projects
declare -A projects
for f in $CHANGED; do
  if [[ $f == packages/* ]]; then
    proj=$(echo "$f" | cut -d/ -f2)
    projects[$proj]=1
  fi
done
for p in "${!projects[@]}"; do
  echo "Triggering build for $p"
  # call CI orchestration API or emit metadata for pipeline
done
```

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Decision</th><th>When to choose</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Keep monorepo</td><td>Tight cross-project changes, shared release cadence</td><td>Invest in caching and selective CI</td></tr>
    <tr><td>Split repos</td><td>Independent ownership, isolated CI</td><td>Requires more cross-repo coordination</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Migration patterns

Use `git filter-repo` to extract subtrees while preserving history. Example to export a directory into a new bare repo:

```bash
git clone --bare https://example.com/original.git
cd original.git
git filter-repo --path packages/service-a --tag-rename '':'service-a-'
# push to new repo
git push --mirror git@github.com:org/service-a.git
```

### Example: build cache and remote caching

- Use remote caches (like Bazel remote cache or Nx Cloud) to avoid rebuilding unchanged artifacts.
- CI should publish cache artifacts for main/master and use them on PR builds where applicable.

## Appendix: monorepo ownership manifest

A simple ownership manifest maps paths to owning teams; use this to drive targeted builds and CODEOWNERS generation.

```json
{
  "packages/service-a": "team-a",
  "packages/service-b": "team-b",
  "libs/shared": "platform-team"
}
```

## CI caching and remote cache patterns

Efficient CI for monorepos hinges on remote caches. Design your pipeline to publish and consume cache artifacts keyed by inputs (commit hash, dependency graph, target). Remote cache examples include Bazel Remote Cache, Nx Cloud, or custom S3-backed caches.

### Publish cache example (CI snippet)

```bash
# Pseudo-step: compute cache key and upload
CACHE_KEY=$(git rev-parse --short HEAD)-$(sha256sum package.json | cut -d' ' -f1)
# build artifacts go to cache/<cache_key>
aws s3 cp build/. s3://monorepo-cache/$CACHE_KEY/ --recursive
```

### Consume cache (CI)

```bash
if aws s3 ls s3://monorepo-cache/$CACHE_KEY/; then
  aws s3 cp s3://monorepo-cache/$CACHE_KEY/ ./build/ --recursive
else
  # full build and publish cache
fi
```

### Git LFS and large-binary governance

- For large binaries, require Git LFS and set size limits in repository policies. Monitor LFS storage usage and set alerts when it grows beyond budget.
- Store build artifacts in dedicated registries rather than in Git.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Decision</th><th>Use case</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>Use remote cache</td><td>Monorepo with costly builds</td><td>Invest in cache key hygiene and eviction policies</td></tr>
    <tr><td>Split repo</td><td>Independent teams and builds</td><td>Prefer when CI complexity outweighs code-sharing benefits</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Advanced sparse-checkout patterns

- Use `cone` mode for simple inclusion of top-level packages and programmatically update the sparse set in developer scripts.
- For onboarding, provide a `bootstrap.sh` that sets up a sparse-checkout for the developer's team.

### Bootstrap example

```bash
git sparse-checkout init --cone
git sparse-checkout set packages/service-a packages/shared
```

## Exercises

- Implement a CI cache publish/consume flow using S3 or your CI's cache primitives and measure build-time improvements.
- Draft an LFS policy and enforce it via pre-push hooks and CI checks.

## Appendix: Observability, Diagnostics, and Large-Repo Operations

This appendix provides operational recipes and a concise troubleshooting checklist for large monorepos. The focus is on actionable commands, observability metrics, and safe maintenance scripts you can copy into runbooks.

### Observability & Metrics

Track these metrics to surface growth and performance regressions in monorepos:

- Packfile size and growth rate
- Average pack unpack time during fetch/clone
- Number of blobs exceeding threshold (e.g., > 5 MB)
- LFS storage growth and quota alerts
- CI cache hit rate

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Metric</th><th>Healthy Range</th><th>Action on Alert</th></tr>
  </thead>
  <tbody>
    <tr><td>Packfile size (per month growth)</td><td>&lt; 2% per month</td><td>Run blob analysis; identify large new files; migrate to LFS</td></tr>
    <tr><td>Average clone unpack time</td><td>&lt; 30s</td><td>Investigate pack fragmentation; run `git repack -ad` and prewarm commit-graph</td></tr>
    <tr><td>Large blobs (&gt;5MB)</td><td>0 per week ideally</td><td>Audit commits; use `git filter-repo` to migrate binaries to LFS or artifact registries</td></tr>
    <tr><td>CI cache hit rate</td><td>&gt;80%</td><td>Check cache keys and restore steps; ensure cache publish on main</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Quick Diagnostics

Run these one-liners in a mirror clone or CI maintenance job to collect diagnostics.

```bash
# show largest blobs by size (human readable)
git rev-list --objects --all | \
  git cat-file --batch-check='%(objecttype) %(objectname) %(rest)' | \
  awk '$1=="blob"{print $2" "$3" "$4}' | \
  sort -k3 -n -r | head -n 50

# show packfile sizes
ls -lh .git/objects/pack/*.pack

# check count-objects summary
git count-objects -vH
```

### Archive and Backup Recipes

Create a forensic bundle before large maintenance operations so you can restore state quickly.

```bash
# create a forensic snapshot for archive purposes
git bundle create /tmp/repo-before-maintenance.bundle --all
```

### Maintenance Scripts: prune, repack, and commit-graph

```bash
#!/usr/bin/env bash
set -euo pipefail
# run maintenance on a mirror clone
git reflog expire --expire=now --all
git gc --prune=now --aggressive
git repack -a -d --depth=250 --window=250
git commit-graph write --reachable --changed-paths
```

### Detect large files added recently (CI-friendly)

```bash
#!/usr/bin/env bash
BASE=${BASE_REF:-origin/main}
# list files added larger than 5MB in PR
git fetch origin $BASE
for sha in $(git rev-list $BASE..HEAD); do
  git diff-tree --no-commit-id --name-only -r $sha || true
done | sort -u | xargs -r du -sh | awk '$1 ~ /[0-9]+M/ {print}'
```

### Partial-clone CI snippet (optimize runners)

```yaml
# GitHub Actions example: use partial clone to speed up checkout
- uses: actions/checkout@v4
  with:
    repository: org/mono-repo
    fetch-depth: 0
    # minimal options may vary by runner; demonstrate partial clone
    sparse-checkout: 'packages/service-a'

# then set up sparse-checkout programmatically in a step
- name: Setup sparse
  run: |
    git sparse-checkout init --cone
    git sparse-checkout set packages/service-a
```

### Ownership manifest generator (example)

Use a small script to generate `CODEOWNERS` entries from a JSON manifest and keep it in CI so ownership changes produce pull requests automatically.

```python
#!/usr/bin/env python3
import json
m = json.load(open('ownership.json'))
with open('CODEOWNERS', 'w') as out:
    for path, owner in m.items():
        out.write(f"{path} {owner}\n")
```

### Guided Recovery Steps

1. If a forced rewrite is required (e.g., remove secrets), create a backup bundle and push to a staging remote.
2. Run `git filter-repo` in a mirror and publish to staging for verification.
3. Notify teams and schedule a cutover window.
4. Provide a mapping file old->new for critical tags.

---

<!-- end monorepo appendix -->
