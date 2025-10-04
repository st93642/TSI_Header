# Lesson 3.3: Interactive Rebases, Autosquashing, and Rewrite Policies

## Why Rebase?

Rebasing rewrites commit history so your work sits atop the latest upstream changes. It keeps history linear and easier to review.

![Rebase Flow](../../../../resources/git/git_rebase_flow.svg)

### Common Rebase Flows

```bash
# Update feature branch with latest main
git fetch origin
git rebase origin/main

# Interactive rebase for clean history
git rebase -i HEAD~5
```

During an interactive rebase you can reorder commits, squash fixups, or edit messages. Use `fixup!` and `squash!` prefixes alongside `git commit --fixup` to pre-arrange autosquashing.

### Rebase vs. Merge Comparison

![Rebase vs Merge Comparison](../../../../resources/git/git_rebase_vs_merge.svg)

Understand trade-offs before rewriting history:

- Rebasing yields linear history but rewrites commit IDs; coordinate before pushing.
- Merging preserves branch context and is safer for shared integration branches.
- Hybrid flows rebase locally, then merge protected branches for auditability.

### Interactive Rebase Checklist

![Interactive Rebase Checklist](../../../../resources/git/git_interactive_rebase.svg)

Prepare before running `git rebase -i`:

- Confirm branch cleanliness with `git status` and stash uncommitted work.
- Decide which commits to squash, reword, or drop ahead of time.
- Configure `rebase.autosquash` and `rebase.autoStash` to streamline workflows.

### Diagram Quick Reference

Use the accompanying visuals to reinforce each workflow before practicing:

- [Interactive Rebase Checklist](../../../../resources/git/git_interactive_rebase.svg) — revisit the staged command flow after each rehearsal.
- [Rebase vs Merge Comparison](../../../../resources/git/git_rebase_vs_merge.svg) — contrast key trade-offs when choosing strategies.
- [Rebase Flow](../../../../resources/git/git_rebase_flow.svg) — review the linearized history that results from a clean rebase.

## Policies for Safe Rewrites

- Never rebase commits that have been pushed to shared branches without coordination.
- Prefer `git pull --rebase` to avoid merge commits during daily sync if your team agrees on linear history.
- Protect long-lived branches with server-side settings that reject non-fast-forward updates.

### Practice

- Use interactive rebase to consolidate noisy commits into a narrative change set.
- Experiment with `git rebase --exec "npm test"` to run tests after each commit replay.
- Observe how reflog retains your original branch state, enabling rollback if needed.
