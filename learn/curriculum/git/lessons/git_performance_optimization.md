# Lesson 6.3: Scaling Git for Large Repositories and Distributed Teams

## Performance Challenges

As repositories grow, operations like clone, fetch, and status can slow down. Mitigate with smart workflows and server optimizations.

![Performance Tuning](../../../../resources/git/git_performance_tuning.svg)

## Client Techniques

- Enable partial clones and commit graphs (`git config --global core.commitGraph true`).
- Use `git gc --aggressive` periodically to optimize object storage.
- Monitor file status using `git status --short --branch` to reduce noise.

## Server-Side Optimizations

- Run `git gc --aggressive` on bare repositories.
- Enable pack-based protocols and CDN caching for large binaries.
- Use Git alternates and mirrors to reduce network round-trips.

### Garbage Collection Lifecycle

![Git GC Lifecycle](../../../../resources/git/git_gc_lifecycle.svg)

Keep object databases lean:

- Schedule background `git gc` runs on servers after large merges or imports.
- Use `git multi-pack-index` to accelerate object lookup across packfiles.
- Monitor pack sizes and repack thresholds to avoid repository bloat.

## Distributed Team Considerations

- Mirror repositories closer to regional teams.
- Encourage shallow clones for CI (`git clone --depth=1`).
- Adopt large file storage (Git LFS) for assets that don’t delta compress well.

### Parallel Clone Flow

![Parallel Clone Flow](../../../../resources/git/git_parallel_clone.svg)

Accelerate onboarding and CI setup:

- Deploy Git servers with protocol v2, enabling partial fetches.
- Serve clones via multiple mirrors with geolocation routing.
- Cache dependencies and submodules to avoid repetitive downloads.

### Practice

- Benchmark clone times before and after enabling partial clones.
- Run `git count-objects -vH` to inspect repository health.
- Configure LFS for large media files and observe repository size improvements.

## Deep Performance Recipes

This section collects practical recipes and server-side knobs to keep large repositories performant.

### Client-side Tips

- Enable commit-graph to speed up history traversal:

```bash
git config core.commitGraph true
git commit-graph write --reachable
```

- Use shallow clones for CI jobs that don't need full history:

```bash
git clone --depth 1 --branch main git@github.com:org/large-repo.git
```

- Prefer `git status --porcelain=v2` for machine-readable fast status checks in scripts.

### Server-side and Infrastructure

- Enable partial clone (protocol v2) and pack indexes on servers to reduce network transfers.
- Run regular `git gc --auto` and schedule full repacks during low-traffic windows.
- Use Git alternates to share object databases between mirrors or checkouts.

### HTML Table: Performance Metrics to Track

<table>
    <thead>
        <tr>
            <th>Metric</th>
            <th>Where to Measure</th>
            <th>Target</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Clone time (cold)</td>
            <td>CI bootstrapping</td>
            <td>< 2 minutes for fast-path jobs</td>
        </tr>
        <tr>
            <td>Fetch size per change</td>
            <td>Developer workflows</td>
            <td>< 10 MB typical (use partial clone otherwise)</td>
        </tr>
        <tr>
            <td>GC duration</td>
            <td>Maintenance windows</td>
            <td>Under 60 minutes for large repos</td>
        </tr>
    </tbody>
</table>

### Lab

- Measure and plot clone/fetch times with and without partial clone; document the delta and choose a threshold for when to enable partial clones in CI.
- Create a small script that runs `git count-objects -vH` and alerts when loose objects exceed a configured threshold.

## Performance Deep Dive — Recipes, Scripts, and Monitoring

This deep dive collects practical recipes for large-scale Git hosting and developer convenience. Use these as a runbook for platform teams and as checklists for repo owners.

### Understanding Packfiles and Repacking

Git stores objects in packfiles. Over time, many loose objects and suboptimal packs can degrade performance. Use `git count-objects -vH` and `git verify-pack -v .git/objects/pack/pack-*.idx` to inspect pack health.

Best practices:

- Avoid frequent `git gc --aggressive` on active repositories; prefer scheduled repacks during maintenance windows.
- Use `git repack -a -d --window=250 --depth=250` on large repositories to produce optimized packs.
- Enable `core.packedGitWindowSize` and `core.packedGitLimit` tuning on high-memory servers.

Example repack script for mirrors:

```bash
#!/usr/bin/env bash
set -euo pipefail
repo=$1
cd "$repo"

# safety: ensure we're in a bare mirror
if [ ! -d .git ]; then
  echo "Expecting a repo with .git directory" >&2
fi

echo "Verifying current pack status..."
git count-objects -vH

echo "Repacking with tuned options..."
git repack -a -d --window=250 --depth=250

echo "Writing multi-pack-index for faster lookup..."
git multi-pack-index write

# Optional: remove old packs only after verification
# git prune-packed

echo "Repack complete"
```

### Commit-Graph and Connectivity Optimization

Commit-graph dramatically speeds up commit walker operations. Configure servers to maintain commit-graphs and populate them frequently.

Commands for commit-graph maintenance:

```bash
# write commit-graph for reachable history
git commit-graph write --reachable --changed-paths

# verify commit-graph
git commit-graph verify
```

Platform tip: schedule incremental commit-graph writes after pushes to keep the graph warm.

---

### Partial Clone and Sparse-Checkout Patterns

Partial clone and sparse-checkout let clients avoid downloading irrelevant blobs and trees. For very large monorepos, combine partial clone with sparse-checkout and server-side filters.

Client-side example:

```bash
git clone --filter=blob:limit=1m --no-checkout https://example.com/org/huge-repo.git
cd huge-repo
git sparse-checkout init --cone
git sparse-checkout set path/to/component
git checkout main
```

Server-side: enable protocol v2 and blob filtering on your Git server implementation (Gitaly, GitHub/GitLab managed settings or custom server flags).

---

### Large File Storage (LFS) Operations and Pruning

Large binary files should be managed via Git LFS or external artifact stores. When retiring large files, do not rewrite shared history lightly; prefer archive strategies or move to separate storage.

LFS housekeeping commands:

```bash
# list lfs objects referenced by this repo
git lfs ls-files

# prune local LFS cache
git lfs prune

# migrate files to LFS (careful: rewrites history)
git lfs migrate import --include="*.psd,*.mov" --exclude-ref=refs/heads/main
```

If you must rewrite history to remove large objects, use `git filter-repo` (modern, faster) or `bfg` (specialized) and preserve an immutable mirror of the pre-rewrite repository.

---

### Monitoring and Alerting — What to Track

Monitor these metrics to ensure healthy performance and to trigger maintenance windows:

<table>
  <thead>
    <tr>
      <th>Metric</th>
      <th>Description</th>
      <th>Alert Condition</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Pack file growth</td>
      <td>Rate of increase in total packfile size</td>
      <td>Growth > 10% week-over-week</td>
    </tr>
    <tr>
      <td>Loose object count</td>
      <td>Number of loose objects in object DB</td>
      <td>Loose objects &gt; 10k</td>
    </tr>
    <tr>
      <td>Average clone time</td>
      <td>Time to clone cold in CI</td>
      <td>Median > 5 minutes</td>
    </tr>
    <tr>
      <td>GC duration</td>
      <td>Time taken by scheduled gc/repack</td>
      <td>Duration > maintenance window length</td>
    </tr>
  </tbody>
</table>

Collect metrics using exporters that interrogate `git count-objects -vH`, packfile stats, and HTTP server timing headers. Ship values to Prometheus/Grafana or your monitoring stack and build dashboards around clone/fetch latency and packfile sizes.

---

### CI and Developer Workflows to Reduce Load

- Use shallow clones with `--depth` for CI jobs that only need latest tree.
- Cache `.git` objects between CI runs when runs are long-lived using your CI cache; store `objects` and `refs` selectively.
- Avoid full `git fetch --all` in CI unless necessary.

Sample GitHub Actions snippet that performs a shallow clone and caches objects:

```yaml
name: CI Shallow with Cache

on: [push, pull_request]

jobs:
  build:
    runs-on: ubuntu-latest
    steps:
      - name: Restore cache
        uses: actions/cache@v4
        with:
          path: |
            .git/objects
            .git/refs
          key: ${{ runner.os }}-git-objects-${{ hashFiles('**/package-lock.json') }}
      - name: Checkout shallow
        uses: actions/checkout@v4
        with:
          fetch-depth: 1
      - name: Run tests
        run: ./ci/run_tests.sh
```

Notes: caching Git objects across CI runners may reduce clone times but must be combined with integrity checks to avoid stale refs.

---

### Decision Matrix: When to Split or Keep a Monorepo

For large organizations, monorepo vs multirepo is a significant architectural decision. Use the matrix below to guide decisions.

<table>
  <thead>
    <tr>
      <th>Criterion</th>
      <th>Keep Monorepo</th>
      <th>Split to Multiple Repos</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Cross-repo changes frequency</td>
      <td>High (atomic changes across many components)</td>
      <td>Low (modules evolve independently)</td>
    </tr>
    <tr>
      <td>Tooling cost</td>
      <td>Invest in CI scaling and partial clone tooling</td>
      <td>Invest in dependency mapping and release coord</td>
    </tr>
    <tr>
      <td>Repository size</td>
      <td>Manageable with partial clone & LFS</td>
      <td>Large binary/artifacts favor split</td>
    </tr>
  </tbody>
</table>

---

### Packfile Forensics — Finding Large Objects

To find the largest blobs in history, run:

```bash
git rev-list --objects --all \
  | sed -n '1,10000p' \
  | cut -d' ' -f1 \
  | git cat-file --batch-check='%(objecttype) %(objectname) %(rest)' \
  | grep '^blob' \
  | while read -r _ sha _; do git cat-file -s "$sha"; echo "$sha"; done \
  | awk '{print $1" "$2}' \
  | sort -n -r \
  | head -n 50
```

This pipeline lists the biggest 50 blobs. Use the output to decide if migration to LFS or history rewrite is justified.

---

### Automated Maintenance Cron Example

A robust maintenance job runs on mirrors and bare repositories and reports results.

```bash
#!/usr/bin/env bash
set -euo pipefail
REPOS_ROOT=/srv/git/mirrors
REPORT=/var/log/git/maintenance/$(date +%F)-report.txt

for repo in "$REPOS_ROOT"/*.git; do
  echo "Processing $repo" >> "$REPORT"
  cd "$repo"
  git count-objects -vH >> "$REPORT"
  if git fsck --no-progress --unreachable --strict > /tmp/fsck.out 2>&1; then
    echo "fsck OK: $repo" >> "$REPORT"
  else
    echo "fsck FAIL: $repo" >> "$REPORT"
    cat /tmp/fsck.out >> "$REPORT"
  fi
  git repack -a -d --window=250 --depth=250 >> "$REPORT" 2>&1 || echo "repack failed for $repo" >> "$REPORT"
  git multi-pack-index write >> "$REPORT" 2>&1 || true
  echo "---" >> "$REPORT"
done

# rotate report or upload to central storage
```

Schedule via systemd timers or a cluster job scheduler, and send reports to an SRE Slack channel or incident dashboard.

---

### Benchmarks and Continuous Measurement

Set up periodic benchmarks that run in a controlled environment to measure the impact of changes:

- Cold clone time for representative CI pipeline (three runs, median).
- Fetch time for an incremental change set representative of developer workflows.
- GC duration for full repack.

Store benchmark results in a time-series DB and annotate maintenance changes to correlate improvements or regressions.

---

## Performance Maintenance Playbook: practical recipes

The following recipes are proven operational steps for maintaining large repositories. They are intentionally prescriptive so teams can adopt them directly. Use these as a baseline and adapt to your infra and retention policy.

### Nightly repack + commit-graph update (safe default)

This script runs a safe maintenance pass that creates a commit-graph (speeds up history walks), repacks loose objects, and GC's what's safe. It is conservative and suitable for teams that can't afford aggressive pruning.

```bash
#!/usr/bin/env bash
set -euo pipefail
REPO_DIR="/srv/git/myrepo.git"
LOCKFILE="/var/lock/git-maintenance.lock"

# basic locking to avoid concurrent runs
exec 9>${LOCKFILE}
if ! flock -n 9; then
  echo "Maintenance already running"
  exit 0
fi

cd "$REPO_DIR"

# write remote-maintenance log for audit
LOGDIR="/var/log/git-maintenance"
mkdir -p "$LOGDIR"
LOGFILE="$LOGDIR/$(basename "$REPO_DIR")-$(date +%F).log"

# Update commit-graph (speeds up history operations)
{
  echo "== commit-graph write --reachable --changed-paths"
  git commit-graph write --reachable --changed-paths --stdin-writeexpire
} >>"$LOGFILE" 2>&1 || echo "commit-graph step failed, continuing"

# Repack aggressively but keep some safety: use --max-pack-size to avoid huge packs
{
  echo "== git repack -a -d --depth=250 --window=250"
  git repack -a -d --depth=250 --window=250
} >>"$LOGFILE" 2>&1 || echo "repack failed"

# Prune unreachable objects older than 30 days (adjust per retention policy)
{
  echo "== git reflog expire --expire=30.days --all"
  git reflog expire --expire=30.days --all
  git gc --prune=30.days --aggressive --auto
} >>"$LOGFILE" 2>&1 || echo "gc/prune failed"

# report some metrics for monitoring
{
  echo "== packfiles:"; ls -lh .git/objects/pack | tail -n +2
  echo "== loose objects:"; git count-objects -vH
} >>"$LOGFILE" 2>&1

# rotate logs older than 90 days
find "$LOGDIR" -type f -mtime +90 -delete

echo "Maintenance finished at $(date)" >>"$LOGFILE"

# exit cleanly
exit 0
```

### Cron example (run weekly)

```cron
# run maintenance every Sunday at 03:30
30 3 * * 0 /usr/local/bin/git-maintenance.sh
```

### Monitoring and alerting checklist

- Track: average clone time, fetch time, time to create shallow clone, number of loose objects, packfile count and sizes.
- Alert when: average clone time increases > 25% week-over-week, or loose objects > 1M, or packfile count grows unusually.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Metric</th><th>Why it matters</th><th>Action</th><th>Threshold (example)</th></tr>
  </thead>
  <tbody>
    <tr><td>Average full clone time</td><td>Shows end-user wait and CI cold-start impact</td><td>Investigate large files, enable shallow clones, run repack</td><td>> 5min</td></tr>
    <tr><td>Loose object count</td><td>Loose objects increase IO; indicates many small writes/failed gc</td><td>Run git gc, investigate failing hooks or frequent force-pushes</td><td>> 100k</td></tr>
    <tr><td>Packfile size distribution</td><td>Very large packfiles may slow transfers; too many small packs indicate inefficient repack</td><td>Repack with tuned window/depth</td><td>Median pack > 50MB</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Case study: speeding up CI by enabling shallow clones

Problem: CI jobs perform full clones in 2–3 minutes adding significant latency. Solution: change CI to use a shallow clone (depth=50) for most jobs and fetch full history only for release builds or tooling jobs that require it.

Example: GitHub Actions snippet (partial) — default to depth 50

```yaml
- uses: actions/checkout@v4
  with:
    fetch-depth: 50
```

Note: Audit and rebuild steps that require full history should explicitly fetch the missing refs.

### Archive strategies for very large history

- Keep an active repository with recent history (e.g., last 2 years) for day-to-day development, and move older commits to an archival repository (with the long-term retention and indexing system). Use `git bundle` or `git fast-export`/`fast-import` to create archived snapshots.

```bash
# create a bundle of everything before a cutoff date
git bundle create repo-archive-2022.bundle --branches --tags "--since=2022-01-01"

# import into an archive repo for cold storage
mkdir /mnt/archives/repo-archive-2022
cd /mnt/archives/repo-archive-2022
git init --bare
git bundle verify ../repo-archive-2022.bundle
git fetch ../repo-archive-2022.bundle "refs/*:refs/*"
```

### Developer guidance: when to split vs archive

- Split the repo when active development across unrelated subprojects causes developer workflows to slow (e.g., PR builds take much longer because the codebase is large). Prefer splitting when teams are organizationally separate and ownership boundaries are clear.
- Archive when history is large but actively worked portion is small. Archiving preserves history for audits while keeping active repo small.

### Quick reference commands

- Rebuild commit-graph (fast history queries): `git commit-graph write --reachable --changed-paths`
- Basic aggressive repack: `git repack -a -d --depth=250 --window=250`
- Count loose objects: `git count-objects -vH`
- Create a bundle for archival: `git bundle create repo-archive.bundle --all`

## Performance appendix: tuning heuristics and tradeoffs

This appendix explains why the default heuristics are chosen and some common tradeoffs.

- Repack depth/window: Higher values produce smaller packs but require more CPU and memory to compute. If servers have limited memory, tune the values down and run more frequently.
- commit-graph benefits: many commands (git log, git blame, git merge-base) can use commit-graph to avoid walking the DAG frequently. Commit-graph write is fast and typically should be part of maintenance.

## Exercises and workshop ideas

- Workshop: Measure clone time before/after implementing weekly repack and commit-graph write. Collect metrics and present a short report.
- Exercise: In a test repo, create 10,000 small commits, then practice shrink/pack strategies and measure packfile size before/after.

<!-- end of appended performance content -->
