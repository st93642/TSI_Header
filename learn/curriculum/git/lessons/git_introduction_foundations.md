# Lesson 1.1: Git in the Software Delivery Lifecycle

## Learning Objectives

- Understand how Git’s distributed design underpins modern software delivery.
- Describe core concepts such as commits, branches, refs, and the three-tree model.
- Practice setting up repositories with policies that scale from solo work to enterprise programs.
- Evaluate common collaboration workflows and choose the right branching strategy for your team.
- Connect Git automation, security, and observability practices back to the product lifecycle.

## Why Version Control Matters

Git is a distributed version control system that captures the full evolution of your source code.
Every developer owns a complete, cryptographically verifiable history across commits, trees, and blobs.
Offline clones eliminate bottlenecks, enable fearless experimentation, and provide rapid disaster recovery.
By replicating the entire commit graph locally, collaboration happens without waiting on a central server.
Immutable commit IDs build a tamper-evident audit log that underpins compliance and traceability.

![Version Control Landscape](../../../../resources/git/git_version_control_landscape.svg)

Key takeaways:

- Distributed clones eliminate a single point of failure and allow high-velocity collaboration.
- Commits are immutable snapshots, making it easy to reason about when features shipped and how bugs emerged.
- Branching provides parallel universes of work that can later be reconciled through merges or rebases.
- Rewriting history is possible, but it requires precision and clear communication to avoid data loss.
- Git’s plumbing and porcelain commands offer low-level insight and high-level convenience alike.

## Historical Context and Evolution

Git was born out of necessity when the Linux kernel community lost their proprietary tooling.
Linus Torvalds designed a system that prized speed, integrity, and full decentralization.
Within weeks, Git became the backbone of Linux development and attracted a broader open-source audience.
Key milestones include the introduction of smart HTTP, lightweight tags, and efficient packfiles.
Modern Git clients add user-friendly interfaces while preserving compatibility with the original data model.
Hosting platforms such as GitHub, GitLab, and Bitbucket bring collaboration workflows to the forefront.
Enterprise adoption grew as organizations recognized the benefits of distributed version control.
Today, Git powers everything from solo hacker projects to multi-thousand contributor monorepos.

## Git Object Model Deep Dive

Git stores four fundamental object types backed by content-addressable SHA hashes.
Understanding how these objects work illuminates why Git can perform powerful operations quickly.

- **Blob** objects hold file contents, compressed and deduplicated.
- **Tree** objects map filenames to blobs and subtrees, representing directories.
- **Commit** objects capture metadata, parent references, author info, and top-level tree pointers.
- **Annotated tag** objects wrap commits with signatures, release notes, or version numbers.

Whenever you record changes, Git writes blobs and trees, then links them via commits.
The hash of each object depends on its content, forming a Merkle DAG.
If even a single byte changes, downstream hashes change, protecting history integrity.
The object model encourages small, focused commits because each commit becomes a reusable building block.

### Plumbing vs. Porcelain

Git provides low-level plumbing commands that manipulate objects directly.
High-level porcelain commands wrap plumbing into ergonomic workflows.
Knowing the distinction helps you debug issues and script advanced automation.

- `git hash-object` creates blobs.
- `git cat-file -p` inspects object contents.
- `git update-index` modifies the staging area.
- `git commit` orchestrates multiple plumbing steps in a single porcelain command.

## Distributed Collaboration Topology

![Git Distributed Topology](../../../../resources/git/git_distributed_topology.svg)

The Git network forms a mesh rather than a single hub, enabling multi-remote setups.
Teams mirror repositories across geographic regions for resilience and compliance.
Developers can clone from the closest mirror and push to a centrally governed origin.
Pull requests and merge trains coordinate integration without slowing down local work.
Signed commits and protected branches enforce trust across distributed contributors.

### Multi-Remote Patterns

- **Origin and upstream**: Fork-based contributions for open-source collaboration.
- **Geo mirrors**: Regional replicas reduce latency while replicating via `git push --mirror`.
- **Read-only audit clones**: Security teams maintain separate copies for forensic analysis.
- **CI farm clones**: Build systems fetch from cache servers instead of hammering the main origin.
- **Temporary feature remotes**: Large features incubate in dedicated remotes before merging.

## The Three Trees State Machine

![Git Three Trees Interaction](../../../../resources/git/git_three_trees_interaction.svg)

Each command transfers changes between the working tree, index, and HEAD.
Treat these as explicit states in a deterministic machine that you can reason about.

- `git add` copies modified files into the index, preparing them for the next commit.
- `git commit` snapshots the index into a new commit referenced by HEAD.
- `git checkout` and `git restore` hydrate the working tree from specific commits or staged states.
- `git status` compares the working tree and index to surface pending work.
- `git diff --cached` reviews staged changes without noise from unstaged edits.

When workflows go awry, confident navigation between the three trees keeps you in control.
Understanding the state machine also clarifies why resets, checkouts, and merges behave differently.

## Comparing Centralized and Distributed Models

<!-- markdownlint-disable MD033 MD010 -->
<table>
	<thead>
		<tr>
			<th>Capability</th>
			<th>Centralized Systems</th>
			<th>Git</th>
		</tr>
	</thead>
	<tbody>
		<tr>
			<td>Offline Work</td>
			<td>Rarely supported</td>
			<td>Fully supported (entire history locally)</td>
		</tr>
		<tr>
			<td>Branching Model</td>
			<td>Expensive and often discouraged</td>
			<td>Lightweight and core to daily flow</td>
		</tr>
		<tr>
			<td>Integrity Guarantees</td>
			<td>Server-managed</td>
			<td>Cryptographically signed object IDs</td>
		</tr>
		<tr>
			<td>Collaboration</td>
			<td>File locking; optimistic merges</td>
			<td>Branches, pull requests, frequent sync</td>
		</tr>
		<tr>
			<td>Scalability</td>
			<td>Central server must scale vertically</td>
			<td>Scale horizontally with mirrors and fetch strategies</td>
		</tr>
		<tr>
			<td>Security</td>
			<td>Controlled by server ACLs</td>
			<td>Augmented by signed commits, tags, and verified pushes</td>
		</tr>
	</tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Navigating Refs and Namespaces

Reflogs, symbolic refs, and namespaces allow fine-grained history control.
Git stores references under the `.git/refs` directory or the packed-refs file.
Understanding naming conventions helps avoid accidental deletions.

- `refs/heads/*` keeps local branches.
- `refs/remotes/*` tracks remote branches as fetched snapshots.
- `refs/tags/*` holds lightweight and annotated tags.
- `refs/notes/*` stores commit notes appended after the fact.
- `refs/stash` records your stash stack.

Use `git show-ref` to list references and verify expected destinations.
Symbolic refs such as `HEAD` and `MERGE_HEAD` point to other refs for transient operations.

### Reflog Safety Nets

The reflog keeps a time-ordered log of where refs pointed.
It lets you recover commits even after destructive operations like reset or rebase.

```bash
git reflog show --date=iso HEAD
git reset --hard HEAD@{3}
```

Tuning garbage collection policies extends the retention window for high-change repos.

## Branching Strategies in Practice

Branching is the bread and butter of Git collaboration.
Different strategies trade off isolation, merge frequency, and release cadence.

- **Feature branching** emphasizes isolation with regular merges into `main`.
- **Trunk-based development** keeps branches short-lived and focuses on continuous integration.
- **Gitflow** formalizes separate branches for features, releases, and hotfixes.
- **Release trains** combine scheduled merges with automated quality gates.
- **Environment branches** align with deployment targets like staging or production.

Choose a model that balances velocity with governance.
Document expectations for branch naming, review rules, and merge processes.

### Branch Lifecycle Checklist

- Create from the latest `main` to minimize conflicts.
- Communicate ownership using issue links or code review descriptions.
- Keep branches up to date with rebase or merge workflows.
- Ensure CI and policy checks run before requesting review.
- Delete merged branches to avoid clutter and drift.

## Commit Craftsmanship

High-quality commits tell a story and make future debugging faster.
Aim for atomic commits that encapsulate a single logical change.

- Use descriptive messages that describe the why, not just the what.
- Reference issue trackers or incident tickets when applicable.
- Leverage conventional commits or team-specific prefixes for consistency.
- Include documentation updates alongside code when behavior changes.
- Sign commits when policy or trust boundaries require cryptographic verification.

### Anatomy of a Commit Message

```text
feat(parser): support optional chaining in AST visitor

Document the new grammar feature.
Update the visitor integration tests.
Record migration notes for downstream tooling.
```

Follow the 50/72 rule by keeping subject lines concise and wrapping body text.

## Staging Area Mastery

The index enables precise control over what enters a commit.
Use partial staging to craft focused diffs even when your working tree is messy.

- `git add -p` interactively adds hunks.
- `git restore --staged <file>` removes files from the index while leaving the working tree untouched.
- `git diff --staged` previews the exact snapshot that will be committed.
- `git commit --amend --no-edit` adjusts the previous commit when you forgot to stage a file.

Harnessing the index separates “work in progress” from “ready to publish” changes.

## Reset, Checkout, and Restore Explained

Reset, checkout, and restore look similar but target different trees.
Misusing them can lead to lost work or corrupted branches.

- `git reset` primarily moves branch pointers and optionally manipulates the index.
- `git checkout` switches branches or hydrates files from commits.
- `git restore` (introduced in Git 2.23) clarifies file-level operations formerly handled by checkout.

### Reset Modes Reference

<!-- markdownlint-disable MD033 MD010 -->
<table>
	<thead>
		<tr>
			<th>Mode</th>
			<th>Branch Pointer</th>
			<th>Index</th>
			<th>Working Tree</th>
			<th>Common Use Case</th>
		</tr>
	</thead>
	<tbody>
		<tr>
			<td>--soft</td>
			<td>Moves</td>
			<td>Unchanged</td>
			<td>Unchanged</td>
			<td>Rebuild previous commit with new staging</td>
		</tr>
		<tr>
			<td>--mixed</td>
			<td>Moves</td>
			<td>Cleared to match target commit</td>
			<td>Unchanged</td>
			<td>Unstage files while keeping edits</td>
		</tr>
		<tr>
			<td>--hard</td>
			<td>Moves</td>
			<td>Resets</td>
			<td>Resets</td>
			<td>Discard local changes entirely</td>
		</tr>
	</tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Merging, Rebasing, and Fast-Forwarding

Merges combine histories by creating a new commit with multiple parents.
Rebases rewrite commits onto a new base to produce a linear sequence.
Fast-forward merges update a branch pointer without introducing a new commit when possible.

Choose the right strategy based on repository conventions and review tooling.
Interactive rebases help tidy up commit series before merging.

```bash
git rebase --interactive origin/main
git merge --no-ff feature/login-audit
```

Communicate rewriting operations to teammates to avoid surprise force-pushes.

## Handling Merge Conflicts

Conflicts are inevitable when work overlaps.
Git marks conflicting regions using `<<<<<<<`, `=======`, and `>>>>>>>` markers.
Resolve conflicts by editing the file, staging the result, and continuing the merge or rebase.

Conflict resolution checklist:

- Run tests immediately after resolving to catch logical regressions.
- Confirm you preserved both contributors’ intent.
- Add regression tests or docs when conflicts expose ambiguous requirements.
- Communicate with collaborators about the solution to avoid repeated conflicts.

## Rebasing With Confidence

Rebase offers a powerful way to create clean history, but it needs guardrails.
Avoid rebasing shared branches once teammates rely on them.
Consider using `git rerere` to record conflict resolutions automatically.

- `git config rerere.enabled true` speeds up repetitive merges.
- `git rebase --autosquash` pairs well with fixup commits.
- `git rebase --keep-base` respects the merge-base when you already integrated upstream.

## Tags, Releases, and Provenance

Tags mark important points in history such as releases or deployment checkpoints.
Lightweight tags simply point to a commit, while annotated tags include metadata.

```bash
git tag -a v2.4.0 -m "Release 2.4.0"
git push origin v2.4.0
```

Signed tags add cryptographic assurance that the release originates from trusted maintainers.
Store SBOMs or release notes alongside tags to aid auditing.

## Hooks and Automation

Hooks trigger scripts around repository events to enforce rules and automate chores.
Local hooks help contributors catch issues before pushing, while server-side hooks protect guardianship.

- `pre-commit` runs linters, scanners, or tests before staging final changes.
- `prepare-commit-msg` scaffolds messages with templates.
- `pre-receive` blocks pushes that violate policy or exceed size limits.
- `post-receive` notifies deployment tools or chatops bots after integration.

Use shared hook managers like Husky, Lefthook, or Overcommit to standardize configuration.

## Continuous Integration Alignment

CI pipelines pull from Git events to validate quality gates.
Structure pipelines around diff-based triggers to avoid redundant work.

- Run fast feedback jobs on every push.
- Schedule nightly pipelines for long-running suites.
- Pin toolchain versions to guarantee reproducibility.
- Fail fast when secrets or credentials leak into commits.

Use status checks and required contexts to enforce merge criteria.
Branch protection rules prevent regressions from entering stable trunks.

## Security and Trust Boundaries

Git history becomes a source of truth only when you defend against tampering.
Adopt signing, scanning, and monitoring practices proportionate to your risk profile.

- Sign commits and tags using GPG, SSH keys, or Sigstore keyless workflows.
- Configure verified commits in hosting platforms to reject unsigned contributions.
- Scan diffs for secrets, sensitive data patterns, or binary blobs.
- Use `git fsck` and `git verify-commit` to validate repository integrity.
- Mirror repositories to immutable storage for forensic recovery.

## Observability and Metrics

Treat Git as an operational system that benefits from observability.
Tracking key metrics uncovers bottlenecks or process smells.

- Lead time from first commit to merge.
- Review turnaround times and queue depth.
- Frequency of force-push or revert events.
- Hotspot files with chronic conflicts.
- Volume of automation overrides or policy bypasses.

Dashboards built on Git logs inform engineering productivity initiatives.

## Scaling to Large Repositories

Large repos strain tooling, requiring disciplined approaches.

- Split codebases into logical modules using sparse checkout or partial clones.
- Leverage `git worktree` to work on multiple branches without duplicating clones.
- Compress history with periodic squash merges while keeping annotated release tags.
- Configure `core.fsmonitor` and delta islands to speed up status checks.
- Archive obsolete branches to shrink refs space.

## Storage Internals and Performance

Packfiles combine multiple objects into compressed bundles for efficient storage.
Delta compression deduplicates similar revisions, reducing network traffic.

- Run `git gc --aggressive` sparingly to repack large repos.
- Store large assets outside the repository using Git LFS.
- Monitor disk usage to avoid hitting quota limits on hosted platforms.
- Evaluate partial clone with `--filter=blob:none` for CI use cases.

## Git and DevOps Tooling Ecosystem

Git integrates with a rich ecosystem of extensions, bots, and automation layers.

- ChatOps bots trigger workflows from pull request comments.
- Release orchestration tools manage version bumps and changelog assembly.
- Issue trackers link work items to commits and deploys.
- Observability pipelines ingest Git metadata for DORA metrics.
- Security scanners annotate pull requests with remediation guidance.

Choose tooling that respects Git’s data model and avoids rewriting history unexpectedly.

## Documentation and Knowledge Sharing

Repositories become living documentation hubs.
Markdown files, diagrams, and ADRs live alongside code.
Keep docs close to source so context evolves in tandem with implementation.

- Store architecture decision records (ADRs) under `docs/adr`.
- Include onboarding playbooks and troubleshooting guides.
- Maintain README badges that reflect build status and coverage.
- Link to diagrams in `/resources` for visual learners.

## Team Agreements and Governance

Explicit agreements reduce friction across teams.

- Define default branch naming conventions and review expectations.
- Document semantic versioning rules and release cadence.
- Establish ownership for triaging failing checks.
- Publish recovery runbooks for accidental deletions or force pushes.

Regularly revisit agreements to adapt as team size and product scope evolve.

## Incident Recovery Scenarios

Proactive rehearsals build confidence when outages occur.

- Practice recovering a deleted branch using the reflog.
- Simulate a compromised key and rotate signing credentials.
- Restore a repository from cold storage when corruption is detected.
- Rebuild deployment history after a forced push to `main`.

Document lessons learned and incorporate improvements into automation.

## Accessibility and Inclusion Considerations

Make Git workflows accessible to all contributors.

- Provide GUI alternatives for contributors uncomfortable with CLI-only flows.
- Offer training on screen reader friendly Git clients.
- Use inclusive language in branch names and documentation.
- Ensure code review platforms meet accessibility standards.

## Integrating Git with IDEs and Editors

Modern editors embed Git features for in-context workflows.

- VS Code provides source control panels, inline diffs, and blame annotations.
- JetBrains IDEs integrate branch management and cherry-pick tools.
- Vim and Neovim users rely on plugins like Fugitive for immersive control.
- Ensure your editor respects `.gitignore` and formatting conventions.

## Automation Recipes for Everyday Productivity

Use scripts to codify repetitive Git tasks.

- Shell aliases to spin up fresh feature branches with templates.
- Commit message hooks that sync Jira issue statuses.
- Chat bots that label pull requests based on diff analytics.
- Scheduled jobs that archive stale branches after inactivity.

## Platform-Specific Considerations

Each hosting platform extends Git differently.

- GitHub introduces actions, checks, and protected branch workflows.
- GitLab offers pipelines, approvals, and push rules at the project level.
- Bitbucket integrates with Atlassian suites and deploy pipelines.
- Azure Repos emphasizes corporate identity and policy integration.

Understand platform capabilities to avoid rebuilding existing functionality.

## Knowledge Checks

1. Explain why Git’s object model leads to immutable histories.
2. Outline the steps Git performs during `git commit`.
3. Describe how reflog entries can rescue a force-pushed branch.
4. Compare feature branching and trunk-based development.
5. Identify three use cases for annotated tags.

## Guided Labs

### Lab 1: Bootstrapping a Repository

1. Initialize a repository with `git init`.
2. Configure your name and email with `git config --global`.
3. Create a `README.md` and commit it with a descriptive message.
4. Add a remote using `git remote add origin` with a mock URL.
5. Push to a remote hosting provider and verify the commit history.

### Lab 2: Branching and Merging

1. Create a feature branch named `feature/branching-lab`.
2. Modify two files to simulate a feature update.
3. Commit changes using atomic messages.
4. Merge the branch back into `main` with `git merge --no-ff`.
5. Resolve any conflicts, rerun tests, and push the updated `main` branch.

### Lab 3: Recovering from Mistakes

1. Delete a branch accidentally using `git branch -D`.
2. Recover it using `git reflog` and `git branch` commands.
3. Reset the working tree with `git reset --hard` and analyze the data loss risk.
4. Use `git fsck` to verify repository integrity.
5. Document the recovery plan in a runbook for teammates.

## Advanced Exercises

- Implement a pre-commit hook that blocks secrets using `git secrets`.
- Experiment with sparse checkout to work on a subset of a monorepo.
- Configure `git worktree` to manage multiple branches simultaneously.
- Analyze performance differences between `git rebase` and `git merge` for a busy branch.
- Build a dashboard that surfaces Git metrics relevant to your team.

## Frequently Asked Questions

**Q: How do I undo the last commit without losing my changes?**
A: Use `git reset --soft HEAD~1` to move the branch pointer back while preserving the index.

**Q: When should I prefer merge over rebase?**
A: Merge preserves history exactly as it happened, which is ideal when multiple collaborators work concurrently.

**Q: What is the best way to maintain clean commit history?**
A: Use interactive rebase before merging and squash fixup commits.

**Q: How do I share Git aliases across teams?**
A: Check a `.gitconfig` template into the repo and source it from contributors’ local configs.

**Q: Can I sign commits without managing long-lived keys?**
A: Yes, Sigstore (via `gitsign`) allows keyless signing bound to workload identities.

## Integration With Issue Tracking

Link commits, pull requests, and deployments to work items.
Workflows stay traceable end-to-end when each change references its driving requirement.

- Include issue IDs in branch names and commit messages.
- Automate transitions in Jira, Azure Boards, or Linear based on Git events.
- Use pull request templates to capture acceptance criteria.
- Backfill historical commits with notes when migrating from legacy systems.

## Documentation Tables in HTML

Some topics benefit from structured tabular data presented with HTML for styling control.
Refer to the templates above when building new comparison tables.

<!-- markdownlint-disable MD033 MD010 -->
<table>
	<thead>
		<tr>
			<th>Scenario</th>
			<th>Recommended Command</th>
			<th>Safety Level</th>
			<th>Recovery Tip</th>
		</tr>
	</thead>
	<tbody>
		<tr>
			<td>Forgotten staged changes</td>
			<td><code>git stash push --staged</code></td>
			<td>Safe</td>
			<td>Apply with <code>git stash pop</code> after resetting</td>
		</tr>
		<tr>
			<td>Need to edit last commit message</td>
			<td><code>git commit --amend</code></td>
			<td>Safe</td>
			<td>Do not amend after pushing to shared branches</td>
		</tr>
		<tr>
			<td>Accidentally committed secrets</td>
			<td><code>git filter-repo</code> or <code>bfg</code></td>
			<td>Risky</td>
			<td>Coordinate force pushes and rotate credentials immediately</td>
		</tr>
	</tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Visual Learning Aids

Leverage diagrams stored in `resources/git` to reinforce complex topics.
Pair visuals with callouts inside lessons to bridge gaps between mental models.

- `git_version_control_landscape.svg` illustrates centralized versus distributed systems.
- `git_distributed_topology.svg` demonstrates mesh collaboration.
- `git_three_trees_interaction.svg` diagrams working tree, index, and HEAD interplay.
- `git_ci_status_matrix.svg` and related assets connect Git practices to CI/CD flows.

## Troubleshooting Playbook

When Git behaves unexpectedly, follow a structured diagnostic approach.

1. Capture `git status --short` and `git branch --show-current` output for context.
2. Inspect recent reflog entries to trace pointer movements.
3. Run `git fsck --full` to check object integrity.
4. Verify remotes with `git remote -v` and confirm authentication.
5. Consult logs under `.git/logs` for detailed event timelines.

## Working With Submodules and Subtrees

Submodules link external repositories, while subtree merges embed history directly.
Choose based on autonomy requirements and release cadence alignment.

- Submodules keep upstream history separate but require extra commands.
- Subtrees enable vendor drops without extra clones but duplicate history.
- Document upgrade processes regardless of approach.

## Policy Automation Blueprint

Ensure repositories enforce compliance requirements automatically.

- Configure branch protections with required reviews, status checks, and signed commits.
- Enable push rules to block force pushes to protected branches.
- Integrate dependency scanning and license checks into CI pipelines.
- Maintain CODEOWNERS to route reviews to the right experts.
- Audit access controls regularly using platform APIs.

## Culture and Communication

Git tooling succeeds when paired with healthy team culture.

- Encourage frequent small commits and open communication.
- Celebrate transparent postmortems when mistakes happen.
- Mentor new contributors through pair programming and code reviews.
- Recognize documentation contributions alongside code changes.

## Glossary

- **Blob**: Binary large object storing file contents.
- **Tree**: Directory structure mapping names to blobs or subtrees.
- **Commit**: Snapshot pointing to a tree and referencing parent commits.
- **Reflog**: History of reference updates for recovery.
- **Fast-forward**: Merge that moves a pointer without creating a new commit.
- **Detached HEAD**: State where HEAD points directly to a commit instead of a branch.
- **Upstream**: Remote branch that a local branch tracks.
- **Bare repository**: Repo without a working tree, used for central sharing.
- **Ancestor**: A commit reachable from another commit via parent links.
- **Cherry-pick**: Apply a specific commit onto the current branch.

## Reflection Prompts

- Which Git workflows have caused friction on your team recently?
- How can you visualize your commit history to improve situational awareness?
- What policies or hooks could reduce repeated incidents?
- Which metrics would demonstrate improvement after process changes?
- How will you onboard new contributors to these workflows?

## Further Reading and References

- Git documentation: <https://git-scm.com/docs>
- Pro Git book: <https://git-scm.com/book>
- Atlassian Git tutorials: <https://www.atlassian.com/git/tutorials>
- GitHub Flow guide: <https://docs.github.com/en/get-started/quickstart/git-and-github-learning-resources>
- Google Engineering Practices documentation on code review.

## Practice Retrospective Template

Use this template after completing Git training modules to reinforce learning.

- What went well during branching and merging exercises?
- Which commands felt unintuitive, and how can you practice them further?
- What automation or tooling would have reduced manual effort?
- Are team agreements aligned with the practices you just rehearsed?

## Summary

Git empowers teams with distributed collaboration, resilient history, and flexible automation hooks.
Mastering the underlying concepts unlocks higher velocity and quality across the SDLC.
Revisit these sections as you encounter new scenarios—Git’s depth rewards continual exploration.

## Exercises, Labs, and Practice

- Initialize a new repository and capture the first commit.
- Clone a public repository and inspect its commit graph locally.
- Identify scenarios in your current workflow where branching could reduce merge conflicts.
- Configure signing policies and test enforcement with simulated violations.
- Build a small automation script that posts deployment notes after merges.

Next up: establishing your identity so teammates can trace authorship reliably.
