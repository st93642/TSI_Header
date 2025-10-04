# Lesson 3.2: Fast-Forward, Three-Way Merges, and Conflict Strategy

## Merge Mechanics

Git merges create a new commit that ties divergent histories back together. There are two primary flavors:

- **Fast-Forward Merge** – no divergent commits; branch pointer simply advances.
- **Three-Way Merge** – merge commit records both parents and resolves combined changes.

![Merge Strategies](../../../../resources/git/git_merge_strategies.svg)

## Conflict Resolution Workflow

1. Identify conflict markers in files (`<<<<<<<`, `=======`, `>>>>>>>`).
2. Use tooling (`git mergetool`, IDE diff tools) to resolve.
3. Stage resolved files and run verification tests.
4. Complete merge with `git commit` (or `git merge --continue`).

### Strategic Choices

- Prefer fast-forward merges for linear history when branch feature scope is small.
- Use merge commits when you need to preserve context or merge multiple feature branches.
- Enable `git config --global pull.rebase false` when merges are default, or `true` when you want rebase-first pulls.

### Conflict Resolution Matrix

![Conflict Resolution Matrix](../../../../resources/git/git_conflict_resolution_matrix.svg)

Choose remediation tactics based on the scenario:

- Trivial whitespace conflicts resolve automatically via `git merge -X ours/theirs` options.
- Semantic conflicts benefit from running targeted unit tests before committing.
- Repeated conflicts can be automated with `git rerere` to cache resolutions.

### Merge Decision Tree

![Merge Decision Tree](../../../../resources/git/git_merge_decision_tree.svg)

Decide between merge, rebase, or cherry-pick:

- Rebasing maintains linear history for short-lived branches with clean commits.
- Merging preserves the true chronology of collaborative efforts.
- Cherry-picking isolates hotfixes when you need to move a single change between branches.

### Practice

- Create conflicting branches and resolve using both CLI and GUI tools.
- Configure a merge driver for a custom file type (e.g., lockfiles).
- Explore `git rerere` to reuse conflict resolutions across repeated merges.
