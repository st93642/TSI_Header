# Lesson 1.3: Snapshots, Objects, and the Three-Tree Architecture

## Git Objects Demystified

Git stores data as content-addressable objects. There are four types:

- **Blob** – file contents
- **Tree** – directory structure referencing blobs and other trees
- **Commit** – snapshot metadata pointing to a tree plus parents
- **Tag** – annotated names for commits or other objects

![Git Object Model](../../../../resources/git/git_object_model.svg)

Each object is compressed and named by the SHA-1 (or SHA-256 when configured) of its content, ensuring deduplication and integrity.

### Object Storage Layers

![Git Object Storage Layers](../../../../resources/git/git_object_storage_layers.svg)

Git stores objects as loose files before compacting them into packs:

- Loose objects reside under `.git/objects/aa/bb...` where `aa` is the first two hex digits.
- `git gc` consolidates loose objects into packfiles and generates a multi-pack index for large repos.
- Alternate object directories let monorepos share storage through `objects/info/alternates`.

### Commit Graph Anatomy

![Git Commit Graph Anatomy](../../../../resources/git/git_commit_graph_anatomy.svg)

Commits form a directed acyclic graph:

- Parents point backward in time, enabling operations like `git log --graph` and `git merge-base`.
- Branch refs are movable pointers to specific commit IDs stored under `.git/refs/`.
- Reachability determines garbage-collection safety; orphaned commits eventually expire when reflogs age out.

## Exploring the Object Database

Use plumbing commands to inspect Git internals:

```bash
git hash-object README.md

# list loose objects
git cat-file -p <sha>
```

The `.git/objects` directory stores loose and packed objects. Packing optimizes storage by delta-compressing similar snapshots.

## The Three Trees Revisited

Git tracks three stages simultaneously:

1. **Head Tree** – `git show HEAD`
2. **Index Tree** – `git ls-files -s`
3. **Working Tree** – your filesystem

Commands move changes between these stages:

- `git add` copies from working tree → index
- `git commit` copies from index → new commit (head)
- `git checkout` copies from commit → working tree (and index)

## Visualizing States with Status

`git status` interprets diffs between HEAD, index, and working tree. Understanding its output ensures predictable commits.

### Hands-On

- Create a file, add content, and inspect the object store.
- Stage changes and use `git diff --staged` to preview commit snapshots.
- Inspect `.git/index` size as your repository grows.

## Extended Deep Dive: Object Lifecycle and Practical Forensics

Understanding how Git stores data unlocks powerful troubleshooting and optimization techniques. This extended section walks through the lifecycle of a change from working tree to packed object, how to interrogate the database, and common forensic patterns you will use when auditing repositories.

### From Edit to Packfile: Lifecycle Steps

1. Edit files in the working tree.
2. `git add` stages blobs and updates the index. Internally, this may create blob objects (loose) when `git add -p` or `git hash-object -w` are used.
3. `git write-tree` (invoked by `git commit`) serializes the index into tree objects and writes them to `.git/objects/`.
4. `git commit` creates a commit object pointing to the tree and parent commits.
5. Background housekeeping (`git gc`, `git repack`) consolidates loose objects into packfiles located under `.git/objects/pack/` for efficient storage.

### Practical Commands for Forensics

Use these plumbing commands to inspect and recover artifacts:

```bash
# Show object type and size
git cat-file -t <sha>
git cat-file -s <sha>

# Pretty-print an object (blob, tree, commit)
git cat-file -p <sha>

# List all loose objects (careful in very large repos)
find .git/objects -type f -printf "%p\n" | sed -E 's|.*/([0-9a-f]{2})([0-9a-f]{38})$|\1\2|g'

# Inspect packfile index entries
git verify-pack -v .git/objects/pack/pack-*.idx | sort -k3 -n | tail -n 20
```

### Common Audit Recipes

- Recover accidentally deleted file contents by locating the blob SHA and printing it with `git cat-file -p`.
- Find large objects with `git rev-list --objects --all | sort -k2 -nr | head -n 50` combined with `git verify-pack` on packfiles.
- Detect duplicate content by exploring packfile delta chains — `git verify-pack -v` shows delta depths.

### HTML Decision Matrix: When to Repack vs Leave Loose Objects

<!-- table: repack-decision -->
<table>
 <thead>
  <tr>
   <th>Repository State</th>
   <th>Action</th>
   <th>Rationale</th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <td>Many loose objects after large import</td>
   <td>Run `git gc --aggressive` and `git repack -a -d`</td>
   <td>Compacts objects into packfiles, reduces disk and lookup overhead</td>
  </tr>
  <tr>
   <td>Active development with frequent small commits</td>
   <td>Avoid aggressive repack on developer machines; schedule server-side</td>
   <td>Local repack can be expensive and interfere with dev flow</td>
  </tr>
  <tr>
   <td>Mono-repo shared object store using alternates</td>
   <td>Use pack-based replication and multi-pack-index</td>
   <td>Shared packs centralize storage and speed up object lookup</td>
  </tr>
 </tbody>
</table>

### Exercises: Object Internals Lab

1. Create a new repo: `git init lab` and add a file. Use `git hash-object -w` and inspect the generated blob.
2. Use `git write-tree` and `git commit-tree` (plumbing) to create commits without the porcelain commands. Verify with `git log`.
3. Simulate a leaked secret by committing a file, then use `git filter-repo` (in a throwaway clone) to remove it and compare repository sizes.

### FAQs

Q: Why are some objects missing from `.git/objects` but available via `git log`?  
A: The objects may be packed into `.git/objects/pack/pack-*.pack` and thus not present as loose files. Use `git verify-pack -v` to inspect pack contents.

Q: Can I safely delete `.git/objects` files?  
A: No — deleting object files corrupts history. Use `git gc` and `git prune` to properly remove unreachable objects under Git control.

## Further Reading and References

- Pro Git, Chapter "Git Internals - Git Objects" (paraphrased and adapted): <https://git-scm.com/book/en/v2/Git-Internals-Git-Objects>
- `git-repack` and `git gc` manuals: `git help gc`, `git help repack`

## Extended Appendix: Practical Patterns and Long-Form Exercises

This appendix collects longer exercises, forensic examples, and pattern references you can use to teach teams or run workshops. Each exercise is intended to be done in a sandbox repository and includes verification steps.

### Exercise 1 — Build Your Own Mini-Git (Conceptual)

Objective: implement a minimal content-addressable store in a scripting language (bash/Python/Ruby) to understand how trees, blobs, and commits are built.

Steps:

1. Start a new folder and create files `file1.txt`, `file2.txt` with sample content.
2. Write a small script that computes SHA-1 of a file's contents and writes the zlib-deflated header+content into `.mygit/objects/` using the same storage layout as Git (two-char dir + 38-char filename).
3. Implement a simple `write-tree` that serializes the staging-area index as a tree object. At this stage, you can cheat by building a text representation `mode type sha filename` and hashing it.
4. Implement a `commit-tree` equivalent that writes a commit object with tree, parent, author, committer, and message.

Verification:

- Use `git cat-file -p` against your objects if you reuse Git's object format; otherwise, inspect the raw files and compare computed hashes.

### Exercise 2 — Audit and Reduce Repo Size

Objective: find the largest objects in a repository and reduce overall size by migrating large binaries to Git LFS.

Steps:

1. Run `git rev-list --objects --all | sort -k2 -n` to list objects by size (combine with `git cat-file -s` as needed).
2. Identify files over threshold (for example, 5MB) and decide whether they belong in LFS or can be removed from history.
3. If migrating to LFS, follow these steps in a fresh clone: add patterns to `.gitattributes`, migrate with `git lfs migrate import --include="*.png,*.zip"`, verify, then push to a new remote.

Verification:

- Compare repository size before/after with `git count-objects -vH` and packfile sizes under `.git/objects/pack/`.

### Cheat Sheet: Useful Plumbing Commands

```bash
# List all objects reachable from refs
git rev-list --objects --all

# Show size of an object
git cat-file -s <sha>

# Verify and inspect packfile contents
git verify-pack -v .git/objects/pack/pack-*.idx | sort -k3 -n | tail -n 40

# Create a bundle backup
git bundle create repo.bundle --all

# Rebuild commit-graph for faster traversal
git commit-graph write --reachable
```

<!-- markdownlint-disable MD033 MD010 -->

### Object Forensics Workshop — Instructor Notes and Solutions

This workshop walks students through identifying large objects, extracting lost blobs, and validating pack integrity.

#### Setup

1. Create a sandbox repository and populate it with sample files, including one large binary (>5MB) to simulate a problematic commit.
2. Make a series of small commits, then run `git gc` to generate packfiles.

#### Tasks

- Task A: Identify the largest blobs using `git rev-list --objects --all` and `git cat-file -s`.
- Task B: Extract a blob and reconstruct the original file using `git cat-file -p <sha> > recovered.bin`.
- Task C: Simulate a corrupted pack by truncating a `.pack` file and practice restoring from a bundle.

#### Instructor Solutions (high level)

- For Task A, students will use the provided pipeline in the cheat sheet to surface the top blobs.
- For Task B, show how `git cat-file -p` recreates the blob stream and how to verify its integrity with `git hash-object -t blob -w recovered.bin`.
- For Task C, demonstrate restoring from a bundle created prior to the simulated corruption and validate with `git fsck`.

<!-- markdownlint-enable MD033 MD010 -->

## Deep dive: Git object model and packfile internals

Understanding Git's object model helps diagnose performance and corruption issues. Git stores objects (blobs, trees, commits, tags) identified by SHA-1/SHA-256 and uses packfiles to optimize storage and transfer.

### Useful plumbing commands

- `git cat-file -p <sha>`: show object contents
- `git cat-file -t <sha>`: show object type
- `git rev-list --objects --all | grep <file>`: find objects referencing a path
- `git verify-pack -v .git/objects/pack/pack-*.idx`: inspect packfile contents and reachability

### Packfile structure and heuristics

- Packfiles group objects and delta-compress similar objects to save space. Delta compression trades CPU/memory for smaller transfer sizes.
- `git repack -a -d --depth=250 --window=250` is an aggressive pack strategy; tune depth/window for available memory.

### Garbage collection and prune

- `git gc --auto` runs default maintenance; `git gc --aggressive` does more work but takes longer.
- `git reflog` stores references that can keep unreachable objects alive until reflog expiration.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Concept</th><th>Why it matters</th><th>Tooling</th></tr>
  </thead>
  <tbody>
    <tr><td>Blob</td><td>File contents</td><td>git cat-file</td></tr>
    <tr><td>Tree</td><td>Directory snapshot</td><td>git ls-tree</td></tr>
    <tr><td>Commit</td><td>History node</td><td>git log</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Forensics quick commands

- Find large objects: `git rev-list --objects --all | sort -k2 -n` and inspect with `git cat-file -p`.
- Inspect dangling objects: `git fsck --lost-found` and check `.git/lost-found/` for potential recovery.

### Advanced concerns: hash migration, collisions, and verification

- Git historically used SHA-1; newer repositories and forks may adopt SHA-256. When auditing or migrating, be aware of fingerprint formats.
- Collision risk for SHA-1 is low for normal use but treat any suspected collision as a high-severity incident and rotate to SHA-256-aware tooling.

### Packfile introspection script (practical)

```bash
#!/usr/bin/env bash
# summarize packfile sizes and top contributors to repository size
set -euo pipefail
for idx in .git/objects/pack/pack-*.idx; do
  echo "Pack: $idx"
  git verify-pack -v "$idx" | sort -k3 -n -r | head -n 20
done
```

### Parsing verify-pack output into CSV

```bash
git verify-pack -v .git/objects/pack/pack-*.idx | awk '$1 ~ /^[0-9a-f]{40}$/ {print $1","$3","$4}' > pack-large-objects.csv
```

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Command</th><th>Use</th><th>Advice</th></tr>
  </thead>
  <tbody>
    <tr><td><code>git verify-pack -v</code></td><td>Inspect pack internals</td><td>Run on maintenance host with adequate memory</td></tr>
    <tr><td><code>git cat-file -p &lt;sha&gt;</code></td><td>Dump object</td><td>Use to extract blobs for analysis</td></tr>
    <tr><td><code>git fsck --full</code></td><td>Verify integrity</td><td>Run periodically and after restores</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Instructor notes and teaching exercises

- Exercise: simulate a repo with a large packfile, run `git verify-pack`, and prepare a remediation plan (prune, repack, archive).
- Teaching tip: show how `git gc` and `git repack` affect repository storage over time by comparing pack sizes before and after.

<!-- end appended core concepts content -->

# Lesson 1.3: Snapshots, Objects, and the Three-Tree Architecture

## Git Objects Demystified

Git stores data as content-addressable objects. There are four types:

- **Blob** – file contents
- **Tree** – directory structure referencing blobs and other trees
- **Commit** – snapshot metadata pointing to a tree plus parents
- **Tag** – annotated names for commits or other objects

![Git Object Model](../../../../resources/git/git_object_model.svg)

Each object is compressed and named by the SHA-1 (or SHA-256 when configured) of its content, ensuring deduplication and integrity.

### Object Storage Layers

![Git Object Storage Layers](../../../../resources/git/git_object_storage_layers.svg)

Git stores objects as loose files before compacting them into packs:

- Loose objects reside under `.git/objects/aa/bb...` where `aa` is the first two hex digits.
- `git gc` consolidates loose objects into packfiles and generates a multi-pack index for large repos.
- Alternate object directories let monorepos share storage through `objects/info/alternates`.

### Commit Graph Anatomy

![Git Commit Graph Anatomy](../../../../resources/git/git_commit_graph_anatomy.svg)

Commits form a directed acyclic graph:

- Parents point backward in time, enabling operations like `git log --graph` and `git merge-base`.
- Branch refs are movable pointers to specific commit IDs stored under `.git/refs/`.
- Reachability determines garbage-collection safety; orphaned commits eventually expire when reflogs age out.

## Exploring the Object Database

Use plumbing commands to inspect Git internals:

```bash
git hash-object README.md

# list loose objects
git cat-file -p <sha>
```

The `.git/objects` directory stores loose and packed objects. Packing optimizes storage by delta-compressing similar snapshots.

## The Three Trees Revisited

Git tracks three stages simultaneously:

1. **Head Tree** – `git show HEAD`
2. **Index Tree** – `git ls-files -s`
3. **Working Tree** – your filesystem

Commands move changes between these stages:

- `git add` copies from working tree → index
- `git commit` copies from index → new commit (head)
- `git checkout` copies from commit → working tree (and index)

## Visualizing States with Status

`git status` interprets diffs between HEAD, index, and working tree. Understanding its output ensures predictable commits.

### Hands-On

- Create a file, add content, and inspect the object store.
- Stage changes and use `git diff --staged` to preview commit snapshots.
- Inspect `.git/index` size as your repository grows.

## Extended Deep Dive: Object Lifecycle and Practical Forensics

Understanding how Git stores data unlocks powerful troubleshooting and optimization techniques. This extended section walks through the lifecycle of a change from working tree to packed object, how to interrogate the database, and common forensic patterns you will use when auditing repositories.

### From Edit to Packfile: Lifecycle Steps

1. Edit files in the working tree.
2. `git add` stages blobs and updates the index. Internally, this may create blob objects (loose) when `git add -p` or `git hash-object -w` are used.
3. `git write-tree` (invoked by `git commit`) serializes the index into tree objects and writes them to `.git/objects/`.
4. `git commit` creates a commit object pointing to the tree and parent commits.
5. Background housekeeping (`git gc`, `git repack`) consolidates loose objects into packfiles located under `.git/objects/pack/` for efficient storage.

### Practical Commands for Forensics

Use these plumbing commands to inspect and recover artifacts:

```bash
# Show object type and size
git cat-file -t <sha>
git cat-file -s <sha>

# Pretty-print an object (blob, tree, commit)
git cat-file -p <sha>

# List all loose objects (careful in very large repos)
find .git/objects -type f -printf "%p\n" | sed -E 's|.*/([0-9a-f]{2})([0-9a-f]{38})$|\1\2|g'

# Inspect packfile index entries
git verify-pack -v .git/objects/pack/pack-*.idx | sort -k3 -n | tail -n 20
```

### Common Audit Recipes

- Recover accidentally deleted file contents by locating the blob SHA and printing it with `git cat-file -p`.
- Find large objects with `git rev-list --objects --all | sort -k2 -nr | head -n 50` combined with `git verify-pack` on packfiles.
- Detect duplicate content by exploring packfile delta chains — `git verify-pack -v` shows delta depths.

### HTML Decision Matrix: When to Repack vs Leave Loose Objects

<!-- table: repack-decision -->
<table>
 <thead>
  <tr>
   <th>Repository State</th>
   <th>Action</th>
   <th>Rationale</th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <td>Many loose objects after large import</td>
   <td>Run `git gc --aggressive` and `git repack -a -d`</td>
   <td>Compacts objects into packfiles, reduces disk and lookup overhead</td>
  </tr>
  <tr>
   <td>Active development with frequent small commits</td>
   <td>Avoid aggressive repack on developer machines; schedule server-side</td>
   <td>Local repack can be expensive and interfere with dev flow</td>
  </tr>
  <tr>
   <td>Mono-repo shared object store using alternates</td>
   <td>Use pack-based replication and multi-pack-index</td>
   <td>Shared packs centralize storage and speed up object lookup</td>
  </tr>
 </tbody>
</table>

### Exercises: Object Internals Lab

1. Create a new repo: `git init lab` and add a file. Use `git hash-object -w` and inspect the generated blob.
2. Use `git write-tree` and `git commit-tree` (plumbing) to create commits without the porcelain commands. Verify with `git log`.
3. Simulate a leaked secret by committing a file, then use `git filter-repo` (in a throwaway clone) to remove it and compare repository sizes.

### FAQs

Q: Why are some objects missing from `.git/objects` but available via `git log`?  
A: The objects may be packed into `.git/objects/pack/pack-*.pack` and thus not present as loose files. Use `git verify-pack -v` to inspect pack contents.

Q: Can I safely delete `.git/objects` files?  
A: No — deleting object files corrupts history. Use `git gc` and `git prune` to properly remove unreachable objects under Git control.

## Further Reading and References

- Pro Git, Chapter "Git Internals - Git Objects" (paraphrased and adapted): <https://git-scm.com/book/en/v2/Git-Internals-Git-Objects>
- `git-repack` and `git gc` manuals: `git help gc`, `git help repack`

## Extended Appendix: Practical Patterns and Long-Form Exercises

This appendix collects longer exercises, forensic examples, and pattern references you can use to teach teams or run workshops. Each exercise is intended to be done in a sandbox repository and includes verification steps.

### Exercise 1 — Build Your Own Mini-Git (Conceptual)

Objective: implement a minimal content-addressable store in a scripting language (bash/Python/Ruby) to understand how trees, blobs, and commits are built.

Steps:

1. Start a new folder and create files `file1.txt`, `file2.txt` with sample content.
2. Write a small script that computes SHA-1 of a file's contents and writes the zlib-deflated header+content into `.mygit/objects/` using the same storage layout as Git (two-char dir + 38-char filename).
3. Implement a simple `write-tree` that serializes the staging-area index as a tree object. At this stage, you can cheat by building a text representation `mode type sha filename` and hashing it.
4. Implement a `commit-tree` equivalent that writes a commit object with tree, parent, author, committer, and message.

Verification:

- Use `git cat-file -p` against your objects if you reuse Git's object format; otherwise, inspect the raw files and compare computed hashes.

### Exercise 2 — Audit and Reduce Repo Size

Objective: find the largest objects in a repository and reduce overall size by migrating large binaries to Git LFS.

Steps:

1. Run `git rev-list --objects --all | sort -k2 -n` to list objects by size (combine with `git cat-file -s` as needed).
2. Identify files over threshold (for example, 5MB) and decide whether they belong in LFS or can be removed from history.
3. If migrating to LFS, follow these steps in a fresh clone: add patterns to `.gitattributes`, migrate with `git lfs migrate import --include="*.png,*.zip"`, verify, then push to a new remote.

Verification:

- Compare repository size before/after with `git count-objects -vH` and packfile sizes under `.git/objects/pack/`.

### Cheat Sheet: Useful Plumbing Commands

```bash
# List all objects reachable from refs
git rev-list --objects --all

# Show size of an object
git cat-file -s <sha>

# Verify and inspect packfile contents
git verify-pack -v .git/objects/pack/pack-*.idx | sort -k3 -n | tail -n 40

# Create a bundle backup
git bundle create repo.bundle --all

# Rebuild commit-graph for faster traversal
git commit-graph write --reachable
```

<!-- markdownlint-disable MD033 MD010 -->

### Object Forensics Workshop — Instructor Notes and Solutions

This workshop walks students through identifying large objects, extracting lost blobs, and validating pack integrity.

#### Setup

1. Create a sandbox repository and populate it with sample files, including one large binary (>5MB) to simulate a problematic commit.
2. Make a series of small commits, then run `git gc` to generate packfiles.

#### Tasks

- Task A: Identify the largest blobs using `git rev-list --objects --all` and `git cat-file -s`.
- Task B: Extract a blob and reconstruct the original file using `git cat-file -p <sha> > recovered.bin`.
- Task C: Simulate a corrupted pack by truncating a `.pack` file and practice restoring from a bundle.

#### Instructor Solutions (high level)

- For Task A, students will use the provided pipeline in the cheat sheet to surface the top blobs.
- For Task B, show how `git cat-file -p` recreates the blob stream and how to verify its integrity with `git hash-object -t blob -w recovered.bin`.
- For Task C, demonstrate restoring from a bundle created prior to the simulated corruption and validate with `git fsck`.

<!-- markdownlint-enable MD033 MD010 -->

## Deep dive: Git object model and packfile internals

Understanding Git's object model helps diagnose performance and corruption issues. Git stores objects (blobs, trees, commits, tags) identified by SHA-1/SHA-256 and uses packfiles to optimize storage and transfer.

### Useful plumbing commands

- `git cat-file -p <sha>`: show object contents
- `git cat-file -t <sha>`: show object type
- `git rev-list --objects --all | grep <file>`: find objects referencing a path
- `git verify-pack -v .git/objects/pack/pack-*.idx`: inspect packfile contents and reachability

### Packfile structure and heuristics

- Packfiles group objects and delta-compress similar objects to save space. Delta compression trades CPU/memory for smaller transfer sizes.
- `git repack -a -d --depth=250 --window=250` is an aggressive pack strategy; tune depth/window for available memory.

### Garbage collection and prune

- `git gc --auto` runs default maintenance; `git gc --aggressive` does more work but takes longer.
- `git reflog` stores references that can keep unreachable objects alive until reflog expiration.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Concept</th><th>Why it matters</th><th>Tooling</th></tr>
  </thead>
  <tbody>
    <tr><td>Blob</td><td>File contents</td><td>git cat-file</td></tr>
    <tr><td>Tree</td><td>Directory snapshot</td><td>git ls-tree</td></tr>
    <tr><td>Commit</td><td>History node</td><td>git log</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Forensics quick commands

- Find large objects: `git rev-list --objects --all | sort -k2 -n` and inspect with `git cat-file -p`.
- Inspect dangling objects: `git fsck --lost-found` and check `.git/lost-found/` for potential recovery.

### Advanced concerns: hash migration, collisions, and verification

- Git historically used SHA-1; newer repositories and forks may adopt SHA-256. When auditing or migrating, be aware of fingerprint formats.
- Collision risk for SHA-1 is low for normal use but treat any suspected collision as a high-severity incident and rotate to SHA-256-aware tooling.

### Packfile introspection script (practical)

```bash
#!/usr/bin/env bash
# summarize packfile sizes and top contributors to repository size
set -euo pipefail
for idx in .git/objects/pack/pack-*.idx; do
  echo "Pack: $idx"
  git verify-pack -v "$idx" | sort -k3 -n -r | head -n 20
done
```

### Parsing verify-pack output into CSV

```bash
git verify-pack -v .git/objects/pack/pack-*.idx | awk '$1 ~ /^[0-9a-f]{40}$/ {print $1","$3","$4}' > pack-large-objects.csv
```

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Command</th><th>Use</th><th>Advice</th></tr>
  </thead>
  <tbody>
    <tr><td><code>git verify-pack -v</code></td><td>Inspect pack internals</td><td>Run on maintenance host with adequate memory</td></tr>
    <tr><td><code>git cat-file -p &lt;sha&gt;</code></td><td>Dump object</td><td>Use to extract blobs for analysis</td></tr>
    <tr><td><code>git fsck --full</code></td><td>Verify integrity</td><td>Run periodically and after restores</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Instructor notes and teaching exercises

- Exercise: simulate a repo with a large packfile, run `git verify-pack`, and prepare a remediation plan (prune, repack, archive).
- Teaching tip: show how `git gc` and `git repack` affect repository storage over time by comparing pack sizes before and after.

<!-- end appended core concepts content -->

# Lesson 1.3: Snapshots, Objects, and the Three-Tree Architecture

## Git Objects Demystified

Git stores data as content-addressable objects. There are four types:

- **Blob** – file contents
- **Tree** – directory structure referencing blobs and other trees
- **Commit** – snapshot metadata pointing to a tree plus parents
- **Tag** – annotated names for commits or other objects

![Git Object Model](../../../../resources/git/git_object_model.svg)

Each object is compressed and named by the SHA-1 (or SHA-256 when configured) of its content, ensuring deduplication and integrity.

### Object Storage Layers

![Git Object Storage Layers](../../../../resources/git/git_object_storage_layers.svg)

Git stores objects as loose files before compacting them into packs:

- Loose objects reside under `.git/objects/aa/bb...` where `aa` is the first two hex digits.
- `git gc` consolidates loose objects into packfiles and generates a multi-pack index for large repos.
- Alternate object directories let monorepos share storage through `objects/info/alternates`.

### Commit Graph Anatomy

![Git Commit Graph Anatomy](../../../../resources/git/git_commit_graph_anatomy.svg)

Commits form a directed acyclic graph:

- Parents point backward in time, enabling operations like `git log --graph` and `git merge-base`.
- Branch refs are movable pointers to specific commit IDs stored under `.git/refs/`.
- Reachability determines garbage-collection safety; orphaned commits eventually expire when reflogs age out.

## Exploring the Object Database

Use plumbing commands to inspect Git internals:

```bash
git hash-object README.md

# list loose objects
git cat-file -p <sha>
```

The `.git/objects` directory stores loose and packed objects. Packing optimizes storage by delta-compressing similar snapshots.

## The Three Trees Revisited

Git tracks three stages simultaneously:

1. **Head Tree** – `git show HEAD`
2. **Index Tree** – `git ls-files -s`
3. **Working Tree** – your filesystem

Commands move changes between these stages:

- `git add` copies from working tree → index
- `git commit` copies from index → new commit (head)
- `git checkout` copies from commit → working tree (and index)

## Visualizing States with Status

`git status` interprets diffs between HEAD, index, and working tree. Understanding its output ensures predictable commits.

### Hands-On

- Create a file, add content, and inspect the object store.
- Stage changes and use `git diff --staged` to preview commit snapshots.
- Inspect `.git/index` size as your repository grows.

## Extended Deep Dive: Object Lifecycle and Practical Forensics

Understanding how Git stores data unlocks powerful troubleshooting and optimization techniques. This extended section walks through the lifecycle of a change from working tree to packed object, how to interrogate the database, and common forensic patterns you will use when auditing repositories.

### From Edit to Packfile: Lifecycle Steps

1. Edit files in the working tree.
2. `git add` stages blobs and updates the index. Internally, this may create blob objects (loose) when `git add -p` or `git hash-object -w` are used.
3. `git write-tree` (invoked by `git commit`) serializes the index into tree objects and writes them to `.git/objects/`.
4. `git commit` creates a commit object pointing to the tree and parent commits.
5. Background housekeeping (`git gc`, `git repack`) consolidates loose objects into packfiles located under `.git/objects/pack/` for efficient storage.

### Practical Commands for Forensics

Use these plumbing commands to inspect and recover artifacts:

```bash
# Show object type and size
git cat-file -t <sha>
git cat-file -s <sha>

# Pretty-print an object (blob, tree, commit)
git cat-file -p <sha>

# List all loose objects (careful in very large repos)
find .git/objects -type f -printf "%p\n" | sed -E 's|.*/([0-9a-f]{2})([0-9a-f]{38})$|\1\2|g'

# Inspect packfile index entries
git verify-pack -v .git/objects/pack/pack-*.idx | sort -k3 -n | tail -n 20
```

### Common Audit Recipes

- Recover accidentally deleted file contents by locating the blob SHA and printing it with `git cat-file -p`.
- Find large objects with `git rev-list --objects --all | sort -k2 -nr | head -n 50` combined with `git verify-pack` on packfiles.
- Detect duplicate content by exploring packfile delta chains — `git verify-pack -v` shows delta depths.

### HTML Decision Matrix: When to Repack vs Leave Loose Objects

<!-- table: repack-decision -->
<table>
 <thead>
  <tr>
   <th>Repository State</th>
   <th>Action</th>
   <th>Rationale</th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <td>Many loose objects after large import</td>
   <td>Run `git gc --aggressive` and `git repack -a -d`</td>
   <td>Compacts objects into packfiles, reduces disk and lookup overhead</td>
  </tr>
  <tr>
   <td>Active development with frequent small commits</td>
   <td>Avoid aggressive repack on developer machines; schedule server-side</td>
   <td>Local repack can be expensive and interfere with dev flow</td>
  </tr>
  <tr>
   <td>Mono-repo shared object store using alternates</td>
   <td>Use pack-based replication and multi-pack-index</td>
   <td>Shared packs centralize storage and speed up object lookup</td>
  </tr>
 </tbody>
</table>

### Exercises: Object Internals Lab

1. Create a new repo: `git init lab` and add a file. Use `git hash-object -w` and inspect the generated blob.
2. Use `git write-tree` and `git commit-tree` (plumbing) to create commits without the porcelain commands. Verify with `git log`.
3. Simulate a leaked secret by committing a file, then use `git filter-repo` (in a throwaway clone) to remove it and compare repository sizes.

### FAQs

Q: Why are some objects missing from `.git/objects` but available via `git log`?  
A: The objects may be packed into `.git/objects/pack/pack-*.pack` and thus not present as loose files. Use `git verify-pack -v` to inspect pack contents.

Q: Can I safely delete `.git/objects` files?  
A: No — deleting object files corrupts history. Use `git gc` and `git prune` to properly remove unreachable objects under Git control.

## Further Reading and References

- Pro Git, Chapter "Git Internals - Git Objects" (paraphrased and adapted): <https://git-scm.com/book/en/v2/Git-Internals-Git-Objects>
- `git-repack` and `git gc` manuals: `git help gc`, `git help repack`

## Extended Appendix: Practical Patterns and Long-Form Exercises

This appendix collects longer exercises, forensic examples, and pattern references you can use to teach teams or run workshops. Each exercise is intended to be done in a sandbox repository and includes verification steps.

### Exercise 1 — Build Your Own Mini-Git (Conceptual)

Objective: implement a minimal content-addressable store in a scripting language (bash/Python/Ruby) to understand how trees, blobs, and commits are built.

Steps:

1. Start a new folder and create files `file1.txt`, `file2.txt` with sample content.
2. Write a small script that computes SHA-1 of a file's contents and writes the zlib-deflated header+content into `.mygit/objects/` using the same storage layout as Git (two-char dir + 38-char filename).
3. Implement a simple `write-tree` that serializes the staging-area index as a tree object. At this stage, you can cheat by building a text representation `mode type sha filename` and hashing it.
4. Implement a `commit-tree` equivalent that writes a commit object with tree, parent, author, committer, and message.

Verification:

- Use `git cat-file -p` against your objects if you reuse Git's object format; otherwise, inspect the raw files and compare computed hashes.

### Exercise 2 — Audit and Reduce Repo Size

Objective: find the largest objects in a repository and reduce overall size by migrating large binaries to Git LFS.

Steps:

1. Run `git rev-list --objects --all | sort -k2 -n` to list objects by size (combine with `git cat-file -s` as needed).
2. Identify files over threshold (for example, 5MB) and decide whether they belong in LFS or can be removed from history.
3. If migrating to LFS, follow these steps in a fresh clone: add patterns to `.gitattributes`, migrate with `git lfs migrate import --include="*.png,*.zip"`, verify, then push to a new remote.

Verification:

- Compare repository size before/after with `git count-objects -vH` and packfile sizes under `.git/objects/pack/`.

### Cheat Sheet: Useful Plumbing Commands

```bash
# List all objects reachable from refs
git rev-list --objects --all

# Show size of an object
git cat-file -s <sha>

# Verify and inspect packfile contents
git verify-pack -v .git/objects/pack/pack-*.idx | sort -k3 -n | tail -n 40

# Create a bundle backup
git bundle create repo.bundle --all

# Rebuild commit-graph for faster traversal
git commit-graph write --reachable
```

<!-- markdownlint-disable MD033 MD010 -->

### Object Forensics Workshop — Instructor Notes and Solutions

This workshop walks students through identifying large objects, extracting lost blobs, and validating pack integrity.

#### Setup

1. Create a sandbox repository and populate it with sample files, including one large binary (>5MB) to simulate a problematic commit.
2. Make a series of small commits, then run `git gc` to generate packfiles.

#### Tasks

- Task A: Identify the largest blobs using `git rev-list --objects --all` and `git cat-file -s`.
- Task B: Extract a blob and reconstruct the original file using `git cat-file -p <sha> > recovered.bin`.
- Task C: Simulate a corrupted pack by truncating a `.pack` file and practice restoring from a bundle.

#### Instructor Solutions (high level)

- For Task A, students will use the provided pipeline in the cheat sheet to surface the top blobs.
- For Task B, show how `git cat-file -p` recreates the blob stream and how to verify its integrity with `git hash-object -t blob -w recovered.bin`.
- For Task C, demonstrate restoring from a bundle created prior to the simulated corruption and validate with `git fsck`.

<!-- markdownlint-enable MD033 MD010 -->

## Deep dive: Git object model and packfile internals

Understanding Git's object model helps diagnose performance and corruption issues. Git stores objects (blobs, trees, commits, tags) identified by SHA-1/SHA-256 and uses packfiles to optimize storage and transfer.

### Useful plumbing commands

- `git cat-file -p <sha>`: show object contents
- `git cat-file -t <sha>`: show object type
- `git rev-list --objects --all | grep <file>`: find objects referencing a path
- `git verify-pack -v .git/objects/pack/pack-*.idx`: inspect packfile contents and reachability

### Packfile structure and heuristics

- Packfiles group objects and delta-compress similar objects to save space. Delta compression trades CPU/memory for smaller transfer sizes.
- `git repack -a -d --depth=250 --window=250` is an aggressive pack strategy; tune depth/window for available memory.

### Garbage collection and prune

- `git gc --auto` runs default maintenance; `git gc --aggressive` does more work but takes longer.
- `git reflog` stores references that can keep unreachable objects alive until reflog expiration.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Concept</th><th>Why it matters</th><th>Tooling</th></tr>
  </thead>
  <tbody>
    <tr><td>Blob</td><td>File contents</td><td>git cat-file</td></tr>
    <tr><td>Tree</td><td>Directory snapshot</td><td>git ls-tree</td></tr>
    <tr><td>Commit</td><td>History node</td><td>git log</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Forensics quick commands

- Find large objects: `git rev-list --objects --all | sort -k2 -n` and inspect with `git cat-file -p`.
- Inspect dangling objects: `git fsck --lost-found` and check `.git/lost-found/` for potential recovery.

### Advanced concerns: hash migration, collisions, and verification

- Git historically used SHA-1; newer repositories and forks may adopt SHA-256. When auditing or migrating, be aware of fingerprint formats.
- Collision risk for SHA-1 is low for normal use but treat any suspected collision as a high-severity incident and rotate to SHA-256-aware tooling.

### Packfile introspection script (practical)

```bash
#!/usr/bin/env bash
# summarize packfile sizes and top contributors to repository size
set -euo pipefail
for idx in .git/objects/pack/pack-*.idx; do
  echo "Pack: $idx"
  git verify-pack -v "$idx" | sort -k3 -n -r | head -n 20
done
```

### Parsing verify-pack output into CSV

```bash
git verify-pack -v .git/objects/pack/pack-*.idx | awk '$1 ~ /^[0-9a-f]{40}$/ {print $1","$3","$4}' > pack-large-objects.csv
```

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Command</th><th>Use</th><th>Advice</th></tr>
  </thead>
  <tbody>
    <tr><td><code>git verify-pack -v</code></td><td>Inspect pack internals</td><td>Run on maintenance host with adequate memory</td></tr>
    <tr><td><code>git cat-file -p &lt;sha&gt;</code></td><td>Dump object</td><td>Use to extract blobs for analysis</td></tr>
    <tr><td><code>git fsck --full</code></td><td>Verify integrity</td><td>Run periodically and after restores</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Instructor notes and teaching exercises

- Exercise: simulate a repo with a large packfile, run `git verify-pack`, and prepare a remediation plan (prune, repack, archive).
- Teaching tip: show how `git gc` and `git repack` affect repository storage over time by comparing pack sizes before and after.

<!-- end appended core concepts content -->

# Lesson 1.3: Snapshots, Objects, and the Three-Tree Architecture

## Git Objects Demystified

Git stores data as content-addressable objects. There are four types:

- **Blob** – file contents
- **Tree** – directory structure referencing blobs and other trees
- **Commit** – snapshot metadata pointing to a tree plus parents
- **Tag** – annotated names for commits or other objects

![Git Object Model](../../../../resources/git/git_object_model.svg)

Each object is compressed and named by the SHA-1 (or SHA-256 when configured) of its content, ensuring deduplication and integrity.

### Object Storage Layers

![Git Object Storage Layers](../../../../resources/git/git_object_storage_layers.svg)

Git stores objects as loose files before compacting them into packs:

- Loose objects reside under `.git/objects/aa/bb...` where `aa` is the first two hex digits.
- `git gc` consolidates loose objects into packfiles and generates a multi-pack index for large repos.
- Alternate object directories let monorepos share storage through `objects/info/alternates`.

### Commit Graph Anatomy

![Git Commit Graph Anatomy](../../../../resources/git/git_commit_graph_anatomy.svg)

Commits form a directed acyclic graph:

- Parents point backward in time, enabling operations like `git log --graph` and `git merge-base`.
- Branch refs are movable pointers to specific commit IDs stored under `.git/refs/`.
- Reachability determines garbage-collection safety; orphaned commits eventually expire when reflogs age out.

## Exploring the Object Database

Use plumbing commands to inspect Git internals:

```bash
git hash-object README.md

# list loose objects
git cat-file -p <sha>
```

The `.git/objects` directory stores loose and packed objects. Packing optimizes storage by delta-compressing similar snapshots.

## The Three Trees Revisited

Git tracks three stages simultaneously:

1. **Head Tree** – `git show HEAD`
2. **Index Tree** – `git ls-files -s`
3. **Working Tree** – your filesystem

Commands move changes between these stages:

- `git add` copies from working tree → index
- `git commit` copies from index → new commit (head)
- `git checkout` copies from commit → working tree (and index)

## Visualizing States with Status

`git status` interprets diffs between HEAD, index, and working tree. Understanding its output ensures predictable commits.

### Hands-On

- Create a file, add content, and inspect the object store.
- Stage changes and use `git diff --staged` to preview commit snapshots.
- Inspect `.git/index` size as your repository grows.

## Extended Deep Dive: Object Lifecycle and Practical Forensics

Understanding how Git stores data unlocks powerful troubleshooting and optimization techniques. This extended section walks through the lifecycle of a change from working tree to packed object, how to interrogate the database, and common forensic patterns you will use when auditing repositories.

### From Edit to Packfile: Lifecycle Steps

1. Edit files in the working tree.
2. `git add` stages blobs and updates the index. Internally, this may create blob objects (loose) when `git add -p` or `git hash-object -w` are used.
3. `git write-tree` (invoked by `git commit`) serializes the index into tree objects and writes them to `.git/objects/`.
4. `git commit` creates a commit object pointing to the tree and parent commits.
5. Background housekeeping (`git gc`, `git repack`) consolidates loose objects into packfiles located under `.git/objects/pack/` for efficient storage.

### Practical Commands for Forensics

Use these plumbing commands to inspect and recover artifacts:

```bash
# Show object type and size
git cat-file -t <sha>
git cat-file -s <sha>

# Pretty-print an object (blob, tree, commit)
git cat-file -p <sha>

# List all loose objects (careful in very large repos)
find .git/objects -type f -printf "%p\n" | sed -E 's|.*/([0-9a-f]{2})([0-9a-f]{38})$|\1\2|g'

# Inspect packfile index entries
git verify-pack -v .git/objects/pack/pack-*.idx | sort -k3 -n | tail -n 20
```

### Common Audit Recipes

- Recover accidentally deleted file contents by locating the blob SHA and printing it with `git cat-file -p`.
- Find large objects with `git rev-list --objects --all | sort -k2 -nr | head -n 50` combined with `git verify-pack` on packfiles.
- Detect duplicate content by exploring packfile delta chains — `git verify-pack -v` shows delta depths.

### HTML Decision Matrix: When to Repack vs Leave Loose Objects

<!-- table: repack-decision -->
<table>
 <thead>
  <tr>
   <th>Repository State</th>
   <th>Action</th>
   <th>Rationale</th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <td>Many loose objects after large import</td>
   <td>Run `git gc --aggressive` and `git repack -a -d`</td>
   <td>Compacts objects into packfiles, reduces disk and lookup overhead</td>
  </tr>
  <tr>
   <td>Active development with frequent small commits</td>
   <td>Avoid aggressive repack on developer machines; schedule server-side</td>
   <td>Local repack can be expensive and interfere with dev flow</td>
  </tr>
  <tr>
   <td>Mono-repo shared object store using alternates</td>
   <td>Use pack-based replication and multi-pack-index</td>
   <td>Shared packs centralize storage and speed up object lookup</td>
  </tr>
 </tbody>
</table>

### Exercises: Object Internals Lab

1. Create a new repo: `git init lab` and add a file. Use `git hash-object -w` and inspect the generated blob.
2. Use `git write-tree` and `git commit-tree` (plumbing) to create commits without the porcelain commands. Verify with `git log`.
3. Simulate a leaked secret by committing a file, then use `git filter-repo` (in a throwaway clone) to remove it and compare repository sizes.

### FAQs

Q: Why are some objects missing from `.git/objects` but available via `git log`?  
A: The objects may be packed into `.git/objects/pack/pack-*.pack` and thus not present as loose files. Use `git verify-pack -v` to inspect pack contents.

Q: Can I safely delete `.git/objects` files?  
A: No — deleting object files corrupts history. Use `git gc` and `git prune` to properly remove unreachable objects under Git control.

## Further Reading and References

- Pro Git, Chapter "Git Internals - Git Objects" (paraphrased and adapted): <https://git-scm.com/book/en/v2/Git-Internals-Git-Objects>
- `git-repack` and `git gc` manuals: `git help gc`, `git help repack`

## Extended Appendix: Practical Patterns and Long-Form Exercises

This appendix collects longer exercises, forensic examples, and pattern references you can use to teach teams or run workshops. Each exercise is intended to be done in a sandbox repository and includes verification steps.

### Exercise 1 — Build Your Own Mini-Git (Conceptual)

Objective: implement a minimal content-addressable store in a scripting language (bash/Python/Ruby) to understand how trees, blobs, and commits are built.

Steps:

1. Start a new folder and create files `file1.txt`, `file2.txt` with sample content.
2. Write a small script that computes SHA-1 of a file's contents and writes the zlib-deflated header+content into `.mygit/objects/` using the same storage layout as Git (two-char dir + 38-char filename).
3. Implement a simple `write-tree` that serializes the staging-area index as a tree object. At this stage, you can cheat by building a text representation `mode type sha filename` and hashing it.
4. Implement a `commit-tree` equivalent that writes a commit object with tree, parent, author, committer, and message.

Verification:

- Use `git cat-file -p` against your objects if you reuse Git's object format; otherwise, inspect the raw files and compare computed hashes.

### Exercise 2 — Audit and Reduce Repo Size

Objective: find the largest objects in a repository and reduce overall size by migrating large binaries to Git LFS.

Steps:

1. Run `git rev-list --objects --all | sort -k2 -n` to list objects by size (combine with `git cat-file -s` as needed).
2. Identify files over threshold (for example, 5MB) and decide whether they belong in LFS or can be removed from history.
3. If migrating to LFS, follow these steps in a fresh clone: add patterns to `.gitattributes`, migrate with `git lfs migrate import --include="*.png,*.zip"`, verify, then push to a new remote.

Verification:

- Compare repository size before/after with `git count-objects -vH` and packfile sizes under `.git/objects/pack/`.

### Cheat Sheet: Useful Plumbing Commands

```bash
# List all objects reachable from refs
git rev-list --objects --all

# Show size of an object
git cat-file -s <sha>

# Verify and inspect packfile contents
git verify-pack -v .git/objects/pack/pack-*.idx | sort -k3 -n | tail -n 40

# Create a bundle backup
git bundle create repo.bundle --all

# Rebuild commit-graph for faster traversal
git commit-graph write --reachable
```

<!-- markdownlint-disable MD033 MD010 -->

### Object Forensics Workshop — Instructor Notes and Solutions

This workshop walks students through identifying large objects, extracting lost blobs, and validating pack integrity.

#### Setup

1. Create a sandbox repository and populate it with sample files, including one large binary (>5MB) to simulate a problematic commit.
2. Make a series of small commits, then run `git gc` to generate packfiles.

#### Tasks

- Task A: Identify the largest blobs using `git rev-list --objects --all` and `git cat-file -s`.
- Task B: Extract a blob and reconstruct the original file using `git cat-file -p <sha> > recovered.bin`.
- Task C: Simulate a corrupted pack by truncating a `.pack` file and practice restoring from a bundle.

#### Instructor Solutions (high level)

- For Task A, students will use the provided pipeline in the cheat sheet to surface the top blobs.
- For Task B, show how `git cat-file -p` recreates the blob stream and how to verify its integrity with `git hash-object -t blob -w recovered.bin`.
- For Task C, demonstrate restoring from a bundle created prior to the simulated corruption and validate with `git fsck`.

<!-- markdownlint-enable MD033 MD010 -->

## Deep dive: Git object model and packfile internals

Understanding Git's object model helps diagnose performance and corruption issues. Git stores objects (blobs, trees, commits, tags) identified by SHA-1/SHA-256 and uses packfiles to optimize storage and transfer.

### Useful plumbing commands

- `git cat-file -p <sha>`: show object contents
- `git cat-file -t <sha>`: show object type
- `git rev-list --objects --all | grep <file>`: find objects referencing a path
- `git verify-pack -v .git/objects/pack/pack-*.idx`: inspect packfile contents and reachability

### Packfile structure and heuristics

- Packfiles group objects and delta-compress similar objects to save space. Delta compression trades CPU/memory for smaller transfer sizes.
- `git repack -a -d --depth=250 --window=250` is an aggressive pack strategy; tune depth/window for available memory.

### Garbage collection and prune

- `git gc --auto` runs default maintenance; `git gc --aggressive` does more work but takes longer.
- `git reflog` stores references that can keep unreachable objects alive until reflog expiration.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Concept</th><th>Why it matters</th><th>Tooling</th></tr>
  </thead>
  <tbody>
    <tr><td>Blob</td><td>File contents</td><td>git cat-file</td></tr>
    <tr><td>Tree</td><td>Directory snapshot</td><td>git ls-tree</td></tr>
    <tr><td>Commit</td><td>History node</td><td>git log</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Forensics quick commands

- Find large objects: `git rev-list --objects --all | sort -k2 -n` and inspect with `git cat-file -p`.
- Inspect dangling objects: `git fsck --lost-found` and check `.git/lost-found/` for potential recovery.

### Advanced concerns: hash migration, collisions, and verification

- Git historically used SHA-1; newer repositories and forks may adopt SHA-256. When auditing or migrating, be aware of fingerprint formats.
- Collision risk for SHA-1 is low for normal use but treat any suspected collision as a high-severity incident and rotate to SHA-256-aware tooling.

### Packfile introspection script (practical)

```bash
#!/usr/bin/env bash
# summarize packfile sizes and top contributors to repository size
set -euo pipefail
for idx in .git/objects/pack/pack-*.idx; do
  echo "Pack: $idx"
  git verify-pack -v "$idx" | sort -k3 -n -r | head -n 20
done
```

### Parsing verify-pack output into CSV

```bash
git verify-pack -v .git/objects/pack/pack-*.idx | awk '$1 ~ /^[0-9a-f]{40}$/ {print $1","$3","$4}' > pack-large-objects.csv
```

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Command</th><th>Use</th><th>Advice</th></tr>
  </thead>
  <tbody>
    <tr><td><code>git verify-pack -v</code></td><td>Inspect pack internals</td><td>Run on maintenance host with adequate memory</td></tr>
    <tr><td><code>git cat-file -p &lt;sha&gt;</code></td><td>Dump object</td><td>Use to extract blobs for analysis</td></tr>
    <tr><td><code>git fsck --full</code></td><td>Verify integrity</td><td>Run periodically and after restores</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Instructor notes and teaching exercises

- Exercise: simulate a repo with a large packfile, run `git verify-pack`, and prepare a remediation plan (prune, repack, archive).
- Teaching tip: show how `git gc` and `git repack` affect repository storage over time by comparing pack sizes before and after.

<!-- end appended core concepts content -->

# Lesson 1.3: Snapshots, Objects, and the Three-Tree Architecture

## Git Objects Demystified

Git stores data as content-addressable objects. There are four types:

- **Blob** – file contents
- **Tree** – directory structure referencing blobs and other trees
- **Commit** – snapshot metadata pointing to a tree plus parents
- **Tag** – annotated names for commits or other objects

![Git Object Model](../../../../resources/git/git_object_model.svg)

Each object is compressed and named by the SHA-1 (or SHA-256 when configured) of its content, ensuring deduplication and integrity.

### Object Storage Layers

![Git Object Storage Layers](../../../../resources/git/git_object_storage_layers.svg)

Git stores objects as loose files before compacting them into packs:

- Loose objects reside under `.git/objects/aa/bb...` where `aa` is the first two hex digits.
- `git gc` consolidates loose objects into packfiles and generates a multi-pack index for large repos.
- Alternate object directories let monorepos share storage through `objects/info/alternates`.

### Commit Graph Anatomy

![Git Commit Graph Anatomy](../../../../resources/git/git_commit_graph_anatomy.svg)

Commits form a directed acyclic graph:

- Parents point backward in time, enabling operations like `git log --graph` and `git merge-base`.
- Branch refs are movable pointers to specific commit IDs stored under `.git/refs/`.
- Reachability determines garbage-collection safety; orphaned commits eventually expire when reflogs age out.

## Exploring the Object Database

Use plumbing commands to inspect Git internals:

```bash
git hash-object README.md

# list loose objects
git cat-file -p <sha>
```

The `.git/objects` directory stores loose and packed objects. Packing optimizes storage by delta-compressing similar snapshots.

## The Three Trees Revisited

Git tracks three stages simultaneously:

1. **Head Tree** – `git show HEAD`
2. **Index Tree** – `git ls-files -s`
3. **Working Tree** – your filesystem

Commands move changes between these stages:

- `git add` copies from working tree → index
- `git commit` copies from index → new commit (head)
- `git checkout` copies from commit → working tree (and index)

## Visualizing States with Status

`git status` interprets diffs between HEAD, index, and working tree. Understanding its output ensures predictable commits.

### Hands-On

- Create a file, add content, and inspect the object store.
- Stage changes and use `git diff --staged` to preview commit snapshots.
- Inspect `.git/index` size as your repository grows.

## Extended Deep Dive: Object Lifecycle and Practical Forensics

Understanding how Git stores data unlocks powerful troubleshooting and optimization techniques. This extended section walks through the lifecycle of a change from working tree to packed object, how to interrogate the database, and common forensic patterns you will use when auditing repositories.

### From Edit to Packfile: Lifecycle Steps

1. Edit files in the working tree.
2. `git add` stages blobs and updates the index. Internally, this may create blob objects (loose) when `git add -p` or `git hash-object -w` are used.
3. `git write-tree` (invoked by `git commit`) serializes the index into tree objects and writes them to `.git/objects/`.
4. `git commit` creates a commit object pointing to the tree and parent commits.
5. Background housekeeping (`git gc`, `git repack`) consolidates loose objects into packfiles located under `.git/objects/pack/` for efficient storage.

### Practical Commands for Forensics

Use these plumbing commands to inspect and recover artifacts:

```bash
# Show object type and size
git cat-file -t <sha>
git cat-file -s <sha>

# Pretty-print an object (blob, tree, commit)
git cat-file -p <sha>

# List all loose objects (careful in very large repos)
find .git/objects -type f -printf "%p\n" | sed -E 's|.*/([0-9a-f]{2})([0-9a-f]{38})$|\1\2|g'

# Inspect packfile index entries
git verify-pack -v .git/objects/pack/pack-*.idx | sort -k3 -n | tail -n 20
```

### Common Audit Recipes

- Recover accidentally deleted file contents by locating the blob SHA and printing it with `git cat-file -p`.
- Find large objects with `git rev-list --objects --all | sort -k2 -nr | head -n 50` combined with `git verify-pack` on packfiles.
- Detect duplicate content by exploring packfile delta chains — `git verify-pack -v` shows delta depths.

### HTML Decision Matrix: When to Repack vs Leave Loose Objects

<!-- table: repack-decision -->
<table>
 <thead>
  <tr>
   <th>Repository State</th>
   <th>Action</th>
   <th>Rationale</th>
  </tr>
 </thead>
 <tbody>
  <tr>
   <td>Many loose objects after large import</td>
   <td>Run `git gc --aggressive` and `git repack -a -d`</td>
   <td>Compacts objects into packfiles, reduces disk and lookup overhead</td>
  </tr>
  <tr>
   <td>Active development with frequent small commits</td>
   <td>Avoid aggressive repack on developer machines; schedule server-side</td>
   <td>Local repack can be expensive and interfere with dev flow</td>
  </tr>
  <tr>
   <td>Mono-repo shared object store using alternates</td>
   <td>Use pack-based replication and multi-pack-index</td>
   <td>Shared packs centralize storage and speed up object lookup</td>
  </tr>
 </tbody>
</table>

### Exercises: Object Internals Lab

1. Create a new repo: `git init lab` and add a file. Use `git hash-object -w` and inspect the generated blob.
2. Use `git write-tree` and `git commit-tree` (plumbing) to create commits without the porcelain commands. Verify with `git log`.
3. Simulate a leaked secret by committing a file, then use `git filter-repo` (in a throwaway clone) to remove it and compare repository sizes.

### FAQs

Q: Why are some objects missing from `.git/objects` but available via `git log`?  
A: The objects may be packed into `.git/objects/pack/pack-*.pack` and thus not present as loose files. Use `git verify-pack -v` to inspect pack contents.

Q: Can I safely delete `.git/objects` files?  
A: No — deleting object files corrupts history. Use `git gc` and `git prune` to properly remove unreachable objects under Git control.

## Further Reading and References

- Pro Git, Chapter "Git Internals - Git Objects" (paraphrased and adapted): <https://git-scm.com/book/en/v2/Git-Internals-Git-Objects>
- `git-repack` and `git gc` manuals: `git help gc`, `git help repack`

## Extended Appendix: Practical Patterns and Long-Form Exercises

This appendix collects longer exercises, forensic examples, and pattern references you can use to teach teams or run workshops. Each exercise is intended to be done in a sandbox repository and includes verification steps.

### Exercise 1 — Build Your Own Mini-Git (Conceptual)

Objective: implement a minimal content-addressable store in a scripting language (bash/Python/Ruby) to understand how trees, blobs, and commits are built.

Steps:

1. Start a new folder and create files `file1.txt`, `file2.txt` with sample content.
2. Write a small script that computes SHA-1 of a file's contents and writes the zlib-deflated header+content into `.mygit/objects/` using the same storage layout as Git (two-char dir + 38-char filename).
3. Implement a simple `write-tree` that serializes the staging-area index as a tree object. At this stage, you can cheat by building a text representation `mode type sha filename` and hashing it.
4. Implement a `commit-tree` equivalent that writes a commit object with tree, parent, author, committer, and message.

Verification:

- Use `git cat-file -p` against your objects if you reuse Git's object format; otherwise, inspect the raw files and compare computed hashes.

### Exercise 2 — Audit and Reduce Repo Size

Objective: find the largest objects in a repository and reduce overall size by migrating large binaries to Git LFS.

Steps:

1. Run `git rev-list --objects --all | sort -k2 -n` to list objects by size (combine with `git cat-file -s` as needed).
2. Identify files over threshold (for example, 5MB) and decide whether they belong in LFS or can be removed from history.
3. If migrating to LFS, follow these steps in a fresh clone: add patterns to `.gitattributes`, migrate with `git lfs migrate import --include="*.png,*.zip"`, verify, then push to a new remote.

Verification:

- Compare repository size before/after with `git count-objects -vH` and packfile sizes under `.git/objects/pack/`.

### Cheat Sheet: Useful Plumbing Commands

```bash
# List all objects reachable from refs
git rev-list --objects --all

# Show size of an object
git cat-file -s <sha>

# Verify and inspect packfile contents
git verify-pack -v .git/objects/pack/pack-*.idx | sort -k3 -n | tail -n 40

# Create a bundle backup
git bundle create repo.bundle --all

# Rebuild commit-graph for faster traversal
git commit-graph write --reachable
```

<!-- markdownlint-disable MD033 MD010 -->

### Object Forensics Workshop — Instructor Notes and Solutions

This workshop walks students through identifying large objects, extracting lost blobs, and validating pack integrity.

#### Setup

1. Create a sandbox repository and populate it with sample files, including one large binary (>5MB) to simulate a problematic commit.
2. Make a series of small commits, then run `git gc` to generate packfiles.

#### Tasks

- Task A: Identify the largest blobs using `git rev-list --objects --all` and `git cat-file -s`.
- Task B: Extract a blob and reconstruct the original file using `git cat-file -p <sha> > recovered.bin`.
- Task C: Simulate a corrupted pack by truncating a `.pack` file and practice restoring from a bundle.

#### Instructor Solutions (high level)

- For Task A, students will use the provided pipeline in the cheat sheet to surface the top blobs.
- For Task B, show how `git cat-file -p` recreates the blob stream and how to verify its integrity with `git hash-object -t blob -w recovered.bin`.
- For Task C, demonstrate restoring from a bundle created prior to the simulated corruption and validate with `git fsck`.

<!-- markdownlint-enable MD033 MD010 -->

## Deep dive: Git object model and packfile internals

Understanding Git's object model helps diagnose performance and corruption issues. Git stores objects (blobs, trees, commits, tags) identified by SHA-1/SHA-256 and uses packfiles to optimize storage and transfer.

### Useful plumbing commands

- `git cat-file -p <sha>`: show object contents
- `git cat-file -t <sha>`: show object type
- `git rev-list --objects --all | grep <file>`: find objects referencing a path
- `git verify-pack -v .git/objects/pack/pack-*.idx`: inspect packfile contents and reachability

### Packfile structure and heuristics

- Packfiles group objects and delta-compress similar objects to save space. Delta compression trades CPU/memory for smaller transfer sizes.
- `git repack -a -d --depth=250 --window=250` is an aggressive pack strategy; tune depth/window for available memory.

### Garbage collection and prune

- `git gc --auto` runs default maintenance; `git gc --aggressive` does more work but takes longer.
- `git reflog` stores references that can keep unreachable objects alive until reflog expiration.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Concept</th><th>Why it matters</th><th>Tooling</th></tr>
  </thead>
  <tbody>
    <tr><td>Blob</td><td>File contents</td><td>git cat-file</td></tr>
    <tr><td>Tree</td><td>Directory snapshot</td><td>git ls-tree</td></tr>
    <tr><td>Commit</td><td>History node</td><td>git log</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Forensics quick commands

- Find large objects: `git rev-list --objects --all | sort -k2 -n` and inspect with `git cat-file -p`.
- Inspect dangling objects: `git fsck --lost-found` and check `.git/lost-found/` for potential recovery.

### Advanced concerns: hash migration, collisions, and verification

- Git historically used SHA-1; newer repositories and forks may adopt SHA-256. When auditing or migrating, be aware of fingerprint formats.
- Collision risk for SHA-1 is low for normal use but treat any suspected collision as a high-severity incident and rotate to SHA-256-aware tooling.

### Packfile introspection script (practical)

```bash
#!/usr/bin/env bash
# summarize packfile sizes and top contributors to repository size
set -euo pipefail
for idx in .git/objects/pack/pack-*.idx; do
 