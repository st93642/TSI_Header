# Lesson 2.2: Staging Strategies, Clean Commits, and Hooks

## The Purpose of the Staging Area

The staging area (index) lets you curate exactly what enters the next commit. It supports selective staging via `git add --patch`, partial files, and commit granularity that tells a coherent story.

![Staging Pipeline](../../../../resources/git/git_staging_pipeline.svg)

### Common Staging Commands

<!-- markdownlint-disable MD033 MD010 -->
<table>
	<thead>
		<tr>
			<th>Command</th>
			<th>Description</th>
		</tr>
	</thead>
	<tbody>
		<tr>
			<td><code>git add &lt;file&gt;</code></td>
			<td>Stage entire file</td>
		</tr>
		<tr>
			<td><code>git add -p</code></td>
			<td>Interactively stage hunks</td>
		</tr>
		<tr>
			<td><code>git restore --staged &lt;file&gt;</code></td>
			<td>Unstage changes</td>
		</tr>
		<tr>
			<td><code>git commit --amend</code></td>
			<td>Amend last commit (careful on shared branches)</td>
		</tr>
	</tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Commit Message Anatomy

![Commit Message Anatomy](../../../../resources/git/git_commit_message_anatomy.svg)

Craft clear history by structuring commits:

- Subjects summarize intent in the imperative mood and stay under 50 characters.
- Bodies explain motivation, context, and follow-up steps wrapped to 72 characters.
- Trailers (`Co-authored-by`, `Refs`) automate attribution and workflow hooks.

### Hook Execution Flow

![Commit Hook Flow](../../../../resources/git/git_commit_hook_flow.svg)

Client-side hooks execute in sequence before a commit succeeds:

- `pre-commit` validates staged content (lint, type-check, secret scan).
- `prepare-commit-msg` seeds templates or ensures ticket references.
- `commit-msg` enforces final message shape before the commit is recorded.

## Writing Excellent Commit Messages

Follow the seven rules from Chris Beams:

1. Separate subject from body with a blank line.
2. Limit subject to 50 characters.
3. Capitalize subject line.
4. Do not end subject with a period.
5. Use imperative mood.
6. Wrap body at 72 characters.
7. Explain what and why, not how.

## Client-Side Hooks

Git runs hooks during lifecycle events. Popular ones:

- `pre-commit`: lint or format code.
- `prepare-commit-msg`: inject templates or metadata.
- `commit-msg`: enforce message standards.

Install using tools like Husky or Lefthook for polyglot teams.

## Commit hygiene: templates, automation, and CI checks

Good commit hygiene reduces friction for reviewers and helps automated tooling (changelogs, bisect, blame). The sections below provide practical recipes, hooks, and enforcement strategies.

### Conventional Commits and changelog automation

Conventional Commits provide a predictable message format for automation.

Examples:

- feat(core): add caching to the build
- fix(auth): correct token refresh
- docs: update README for install

When teams adopt conventional commits, use a changelog generator (e.g., `standard-version` or `conventional-changelog`) in the release pipeline to auto-generate release notes.

### Sample prepare-commit-msg hook (auto-add issue reference)

Create `.git/hooks/prepare-commit-msg` (make executable) to prepopulate messages when a branch is named after an issue.

```bash
#!/usr/bin/env bash
# append issue key from branch name to commit message
BRANCH=$(git rev-parse --abbrev-ref HEAD)
ISSUE=$(echo "$BRANCH" | grep -Eo '[A-Z]+-[0-9]+' || true)
if [ -n "$ISSUE" ]; then
  sed -i "1s/^/[$ISSUE] /" "$1"
fi
```

### Enforcing signed commits in CI

- Enforce via CI by checking commits in PRs are signed: `git log --show-signature --pretty=oneline -1 <commit>` and fail the job if not signed.
- Alternatively, require a signed merge from the integrator (release) team.

### Commit message template

A short template helps authors write high-quality messages. Add this as `.git/commit-template` and configure `git config --local commit.template .git/commit-template` in repo.

```text
Short summary (50 chars max)

More detailed explanation (wrap at 72 chars)

- Why this change is needed
- What affects it
- How to test

Co-authored-by: Name <email>
```

### Semantic responsibilities (HTML table)

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Role</th><th>Responsibility</th><th>Enforcement</th></tr>
  </thead>
  <tbody>
    <tr><td>Author</td><td>Clear commit message, tests</td><td>Pre-commit hooks, PR template</td></tr>
    <tr><td>Reviewer</td><td>Code correctness, changelog note</td><td>Review checklist</td></tr>
    <tr><td>Integrator</td><td>Merge policy, build gating</td><td>Branch protection rules</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Pre-commit hook examples

- Lint staged changes (ESLint, rubocop) using `lint-staged` or similar.
- Run tests that are fast and cover changed code paths. Slow tests belong in CI.

Example `.husky/pre-commit` snippet (npm/husky):

```bash
#!/usr/bin/env sh
. "$(dirname "$0")/_/husky.sh"

npx lint-staged
```

`lint-staged` sample config in `package.json`:

```json
"lint-staged": {
  "*.js": ["eslint --fix", "git add"],
  "*.md": ["markdownlint -f"]
}
```

### Changelog automation example (release step)

- During release pipeline, run conventional changelog generator and attach generated notes to the release.

```bash
# generate CHANGELOG.md and create annotated tag
npx standard-version --release-as patch
git push --follow-tags origin main
```

### Rewriting history vs merge safety

- Avoid rewriting shared branches (e.g., `main`, `release/*`). For feature branches, rebase locally before PR if team policy permits.
- Use `--force-with-lease` for safer force pushes and educate contributors on when it's acceptable.

### Exercise: write a commit policy

- Draft a short commit policy for your team: rules for branching, signing, squashing in merges, and when to rewrite history. Review quarterly.

## Advanced hooks and pre-push enforcement

The `pre-push` hook is a valuable place to run fast integration checks (lightweight tests, dry-run deployments, lint checks that need repository state). Keep pre-push hooks fast (under a few seconds) by limiting the scope to changed packages or files.

Example `pre-push` that runs fast tests for changed packages in a monorepo (bash):

```bash
#!/usr/bin/env bash
set -euo pipefail
# Determine changed files between local branch and origin/main
BASE_REF="origin/main"
CHANGED_FILES=$(git diff --name-only "$BASE_REF"...HEAD)

# map changed files to packages (example: packages/*)
PACKAGES_TO_TEST=()
for f in $CHANGED_FILES; do
  case "$f" in
    packages/*)
      pkg=$(echo "$f" | cut -d/ -f1-2)
      PACKAGES_TO_TEST+=("$pkg")
      ;;
  esac
done

# deduplicate and run lightweight test commands
for pkg in $(printf "%s\n" "${PACKAGES_TO_TEST[@]}" | sort -u); do
  echo "Running fast tests in $pkg"
  (cd "$pkg" && npm test --silent || { echo "Fast tests failed in $pkg"; exit 1; })
done

exit 0
```

### Pre-push ergonomics

- Keep hooks idempotent and tolerant to CI differences. Use `--silent` and limited timeouts.
- Offer `SKIP_HOOKS=1 git push` as a documented escape for emergency situations; require checks in CI for gated merges.
- Prefer server-side checks for anything that is slow or authoritative (security scans, heavy tests).

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Failure mode</th><th>Symptom</th><th>Immediate action</th></tr>
  </thead>
  <tbody>
    <tr><td>Hook environment mismatch</td><td>Hook passes locally but fails on CI</td><td>Ensure hook uses repo's toolchain (nvm, pyenv); standardize dev environments</td></tr>
    <tr><td>Sluggish pre-push hooks</td><td>Slow developer workflows</td><td>Move expensive checks to CI; keep local checks fast</td></tr>
    <tr><td>Unsigned commits slip through</td><td>CI rejects merge</td><td>Enforce via branch protection and add sign-checks in CI</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Communication patterns around commits

- Use PR descriptions to summarize the intent and any manual verification steps.
- When you rewrite history (feature-branch), leave a comment in the PR describing the reason and commands used so integrators can verify.

## Workshop: write and test a hook

- Task: Implement a `pre-commit` hook that runs linters for staged files only and a `pre-push` hook to run fast integration tests for changed packages. Measure developer-perceived time and tune.

### Example: lint-staged usage (package.json)

```json
"lint-staged": {
  "*.js": ["eslint --fix", "git add"],
  "*.py": ["ruff check"],
  "*.md": ["markdownlint -f"]
}
```

## Appendix: recommended commit policy template

- Enforce a minimal 3-line commit message for changes that affect behavior: subject, short description, test note.
- Use branch names that include issue keys and short descriptors: `PROJ-1234/add-new-cache`.
- Require changelog entries for user-facing changes.

## Enforcing commit signing and message quality in CI

Commit and tag signatures should be validated in CI for critical branches. Combine signature verification with commit message linting for release-quality history.

### GitHub Actions: verify commit signatures and message format

```yaml
name: Validate Commit
on: [pull_request]
jobs:
  check:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Import trusted keys
        run: |
          # CI runner should get a trusted keyring from secrets or an internal store
          gpg --import /secrets/trusted-keys.gpg || true
      - name: Verify commit signatures
        run: |
          sha=$(git rev-parse HEAD)
          if ! git log -1 --show-signature $sha | grep -q "Good signature"; then
            echo "Commit is not signed or signature untrusted"; exit 1
          fi
      - name: Lint commit messages
        run: |
          git log --pretty=format:%B -n 20 | some-commit-lint-tool || exit 1
```

### Advanced prepare-commit-msg: auto-insert changelog entries

A `prepare-commit-msg` hook can seed a short changelog template when a PR prefix is present.

```bash
#!/usr/bin/env bash
MSG_FILE="$1"
BRANCH=$(git rev-parse --abbrev-ref HEAD)
if echo "$BRANCH" | grep -qE '^chore/|^feat/|^fix/'; then
  echo "Changelog: <short description>" >> "$MSG_FILE"
fi
```

### Enforcing via server-side checks

- Use branch protection rules and required status checks to ensure local bypasses are caught by CI.
- Keep a minimal set of fast checks locally and run heavier checks in CI.

## Server-side enforcement and contributor guidance

Local hooks help, but authoritative checks must run in CI or on the server. Combine pre-receive hooks with branch protection and transparent failure messages.

### Pre-receive hook (simple signed-commit check)

Create `/hooks/pre-receive` in the bare repo on the server to reject unsigned commits to `main`:

```bash
#!/usr/bin/env bash
while read old new ref; do
  if [[ "$ref" == "refs/heads/main" ]]; then
    for commit in $(git rev-list $old..$new); do
      if ! git verify-commit $commit >/dev/null 2>&1; then
        echo "Rejected unsigned commit $commit" >&2
        exit 1
      fi
    done
  fi
done
exit 0
```

## Pre-receive hook patterns and server-side message enforcement

Pre-receive hooks on the server enforce repo-wide policies (commit-message format, signed commits, disallowed files). They must be fast and provide clear errors to contributors.

### Example: pre-receive hook that enforces Conventional Commits (bash)

```bash
#!/usr/bin/env bash
while read old new ref; do
  # only check branches of interest
  if [[ "$ref" =~ refs/heads/.* ]]; then
    for commit in $(git rev-list $old..$new); do
      msg=$(git log --format=%B -n 1 $commit)
      if ! echo "$msg" | grep -qE '^(feat|fix|docs|chore|refactor|perf|test)\(.*\): .{1,}' ; then
        echo "Commit $commit does not follow Conventional Commits format" >&2
        exit 1
      fi
    done
  fi
done
exit 0
```

### Contributor-friendly failure messages

- Pre-receive hooks should return a message with a suggested fix and a link to the contribution docs or a small snippet showing a corrected commit message.
- Provide a local helper script `validate-commit-msg` that developers can run before pushing to avoid CI/server rejection.

### CI gating improvements

- Use fast pre-merge checks for message format and quick linting; run heavier verification (security scans, long tests) in post-merge pipelines or merge-preview runners.
- Record failing reasons in the PR comment via CI so contributors get actionable feedback.

## Exercises: Enforcement

1. Implement the pre-receive hook above in a test bare repo and simulate rejected pushes.
2. Create a small `validate-commit-msg` script that runs locally and integrates with developer IDEs as a pre-push hook.

---

## Appendix: Commit Enforcement, Hooks, and Server-side Policies

This appendix contains examples for pre-commit, prepare-commit-msg, pre-push hooks, and server-side pre-receive checks to enforce commit hygiene, signing, and message format.

### Enforcing commit message format locally (prepare-commit-msg)

```bash
#!/usr/bin/env bash
# prepare-commit-msg: insert issue key from branch name
MSG_FILE="$1"
BRANCH=$(git rev-parse --abbrev-ref HEAD)
ISSUE=$(echo "$BRANCH" | grep -Eo '[A-Z]+-[0-9]+' || true)
if [ -n "$ISSUE" ]; then
  sed -i "1s/^/[$ISSUE] /" "$MSG_FILE"
fi
```

### Pre-commit: lint staged files (husky + lint-staged)

```bash
#!/usr/bin/env sh
. "$(dirname "$0")/_/husky.sh"

npx lint-staged
```

`lint-staged` config example:

```json
"lint-staged": {
  "*.js": ["eslint --fix", "git add"],
  "*.py": ["ruff check"],
  "*.md": ["markdownlint -f"]
}
```

### Pre-push: run lightweight integration tests only for changed packages

(See earlier monorepo examples for pre-push strategy.)

### Server-side pre-receive: enforce signed commits and conventional commit messages

```bash
#!/usr/bin/env bash
# pre-receive: reject unsigned commits and non-conventional messages
while read old new ref; do
  for commit in $(git rev-list $old..$new); do
    # verify signature
    if ! git log -1 --show-signature $commit | grep -q "Good signature"; then
      echo "Rejected: commit $commit is not signed" >&2; exit 1
    fi
    # enforce Conventional Commits
    msg=$(git log --format=%B -n 1 $commit)
    if ! echo "$msg" | grep -qE '^(feat|fix|docs|chore|refactor|perf|test)(\(.+\))?: .{1,}'; then
      echo "Rejected: commit $commit does not follow Conventional Commits" >&2; exit 1
    fi
  done
done
exit 0
```

### Checklist: commit hygiene enforcement (HTML)

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Check</th><th>Where</th><th>Action on fail</th></tr>
  </thead>
  <tbody>
    <tr><td>Commit message format</td><td>pre-receive/CI</td><td>Reject push or fail job</td></tr>
    <tr><td>Signed commits</td><td>pre-receive/CI</td><td>Reject push or fail job</td></tr>
    <tr><td>Lint/staged tests</td><td>pre-commit</td><td>Block commit locally</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Exercises

1. Implement the server-side pre-receive hook in a test environment and validate it rejects unsigned commits and bad messages.
2. Add a `prepare-commit-msg` hook that seeds changelog entries and a `pre-push` hook that runs fast tests.

---

## Extra Templates and Example Policies

### Minimal `CONTRIBUTING.md` snippet for commit policy

```text
Commit Policy
- Use imperative subject and limit to 50 chars
- Include a body when the change is non-trivial
- Use `Co-authored-by:` for pair contributions
- Sign release commits/tags when producing releases
```

### Example PR template (add to .github/PULL_REQUEST_TEMPLATE.md)

```markdown
## Summary

<!-- Describe what this PR does -->

## Checklist
- [ ] Tests added/updated
- [ ] Commit messages follow project policy
- [ ] Signed release tag included (if applicable)
```

---

## Small server-side pre-receive example: reject large files & unsigned tags

```bash
#!/usr/bin/env bash
# pre-receive: reject big files and unsigned release tags
while read old new ref; do
  # check size of objects in push
  for commit in $(git rev-list $old..$new); do
    # reject very large blobs
    if git ls-tree -r $commit | awk '{print $3}' | xargs -I{} git cat-file -s {} | awk '$1>52428800{print $1}' | grep -q .; then
      echo "Push rejected: contains files >50MB" >&2
      exit 1
    fi
  done
  # if this is a tag ref, ensure tag is signed
  if [[ $ref =~ refs/tags/ ]]; then
    if ! git tag -v $(basename $ref) >/dev/null 2>&1; then
      echo "Tag $(basename $ref) is not verifiable" >&2; exit 1
    fi
  fi
done
exit 0
```

---

## Extra Exercises and Automation Ideas

- Add a local `validate-commit-msg` script that contributors can run locally and wire it into a `pre-push` hook to reduce CI failures.
- Create a tiny GitHub Action that comments on PRs with missing checklist items (automate the PR template checks).
- Add a metrics emitter that counts how often `git commit --amend` is used on `main` branches (indicates potential policy issues).

-- end appended staging & committing content --
