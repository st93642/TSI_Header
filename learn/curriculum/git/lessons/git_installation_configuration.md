# Lesson 1.2: Installing Git and Establishing Identity

## Learning Objectives

- Install Git on Windows, macOS, and Linux with repeatable, auditable steps.
- Configure global, system, and repository-scoped settings that capture identity and workflow preferences.
- Harden credential storage and signing policies from day one of repository usage.
- Build automation scripts that bootstrap fresh workstations in minutes.
- Troubleshoot installation failures using logs, diagnostics, and structured runbooks.

## Why This Lesson Matters

A Git lesson without Git on the machine is a non-starter. Teams lose momentum when new hires spend days configuring their environments. By codifying installation and configuration, you sustain velocity, compliance, and trust in the history your team creates. Treat this lesson as a foundation you can reuse each time you acquire new hardware or spin up a CI runner.

## Pre-Installation Checklist

- Confirm you have administrative privileges on the target system.
- Ensure outbound HTTPS access to `https://git-scm.com`, `https://github.com`, and any internal mirrors.
- Gather company policies for commit signing, credential rotation, and proxy configuration.
- Determine which shells, editors, and IDEs the developer will use daily.
- Locate existing dotfiles or configuration repositories that should be cloned.
- Validate that antivirus or endpoint protection tools allow command-line installer execution.
- Allocate disk space for repositories, caches, and logs.
- Identify whether Git Large File Storage (LFS) or submodules are required immediately.
- Record the expected Git version so you can verify the install succeeded.
- Draft a rollback plan in case you must remove or downgrade the installation.

## Environment Matrix

<!-- markdownlint-disable MD033 MD010 -->
<table>
    <thead>
        <tr>
            <th>Platform</th>
            <th>Preferred Package Source</th>
            <th>Credential Helper</th>
            <th>Primary Shell</th>
            <th>Notes</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Windows</td>
            <td>Git for Windows installer</td>
            <td>manager-core</td>
            <td>PowerShell, Git Bash</td>
            <td>Includes OpenSSH, optional VS Code integration</td>
        </tr>
        <tr>
            <td>macOS (Intel)</td>
            <td>Homebrew</td>
            <td>osxkeychain</td>
            <td>zsh</td>
            <td>Install Xcode CLI tools for compilers and SDKs</td>
        </tr>
        <tr>
            <td>macOS (Apple Silicon)</td>
            <td>Homebrew under /opt/homebrew</td>
            <td>osxkeychain</td>
            <td>zsh</td>
            <td>Ensure Rosetta is installed for Intel-only tooling</td>
        </tr>
        <tr>
            <td>Ubuntu/Fedora</td>
            <td>apt / dnf</td>
            <td>libsecret</td>
            <td>bash</td>
            <td>Consider compiling latest Git when distro lag is large</td>
        </tr>
        <tr>
            <td>Arch Linux</td>
            <td>pacman</td>
            <td>libsecret</td>
            <td>bash, fish</td>
            <td>Rolling release keeps Git near upstream HEAD</td>
        </tr>
    </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Windows Installation Workflow

1. Download the latest Git for Windows installer from the official website.
2. Verify the SHA256 checksum using PowerShell: `Get-FileHash .\Git-*.exe -Algorithm SHA256`.
3. Choose the default editor for Git—VS Code and Notepad++ are popular options.
4. Select "Git from the command line" to ensure Command Prompt and PowerShell pick up Git.
5. Enable the optional Unix tools if you plan to use Git Bash as your primary shell.
6. Configure the HTTPS transport layer to use OpenSSL unless corporate policy mandates Secure Channel.
7. Accept the suggested Git Credential Manager Core integration.
8. Opt into the new line ending conversion that checks out Windows-style and commits Unix-style.
9. Install Git LFS if you expect to work with large binary assets.
10. Finish the installation, open Git Bash, and run `git --version` to confirm the version.
11. Update Windows Terminal settings to include Git Bash and an ANSI-friendly color scheme.
12. Configure `git config --global credential.helper manager-core` if the installer skipped this step.
13. Enable long path support in Windows if you work on repositories with deep directory structures: `git config --system core.longpaths true`.
14. Install optional extras like `posh-git` to improve the PowerShell prompt.
15. Document the installation steps in your internal wiki or onboarding runbook.

### Windows Maintenance Tasks

- Keep Git current by running the bundled `git update-git-for-windows` command monthly.
- Use Windows Update or enterprise patching tools to update the OpenSSH client.
- Backup your `.ssh` directory and GPG keys to an encrypted location.
- Audit credential helper entries in the Windows Credential Manager.
- Leverage Windows Defender exclusions for directories storing large repositories to improve performance.

## macOS Installation Workflow

1. Install Xcode Command Line Tools with `xcode-select --install`.
2. Install Homebrew if needed; run `/bin/bash -c "$(curl -fsSL https://raw.githubusercontent.com/Homebrew/install/HEAD/install.sh)"`.
3. Install Git via Homebrew: `brew install git`.
4. Confirm the installation with `git --version`.
5. Configure the keychain credential helper: `git config --global credential.helper osxkeychain`.
6. Install GPG Suite or `brew install gnupg` if commit signing is mandatory.
7. Set `git config --global user.name` and `git config --global user.email` with corporate values.
8. Update the PATH in your shell profile to include `/opt/homebrew/bin` on Apple Silicon.
9. Configure the default branch for new repositories: `git config --global init.defaultBranch main`.
10. Use `brew install git-lfs` if working with large binaries.
11. Integrate with Terminal and iTerm2 by customizing prompts via `zsh` or `fish` frameworks.
12. Sync dotfiles using tools like `chezmoi`, `yadm`, or `stow`.
13. Validate Git operations inside VS Code, JetBrains IDEs, and other editors.
14. Test network operations using both HTTPS and SSH to confirm firewall allowances.
15. Document the entire workflow in a shareable gist or internal confluence page.

### macOS Maintenance Tasks

- Run `brew upgrade git` regularly.
- Use `softwareupdate --install --all` to keep macOS security patches current.
- Update signing certificates before they expire to avoid blocked commits.
- Automate backups of the `.gnupg` directory using encrypted Time Machine snapshots.
- Periodically cleaning the Keychain ensures stale tokens are removed.

## Linux Installation Workflow

1. Update package repositories: `sudo apt update`, `sudo dnf update`, or `sudo pacman -Sy`.
2. Install Git using your distribution's package manager.
3. Verify the version with `git --version`.
4. Configure `libsecret` by installing `gnome-keyring` or `libsecret-tools`.
5. Set the credential helper: `git config --global credential.helper /usr/lib/git-core/git-credential-libsecret`.
6. Install `gnupg2` for signing operations.
7. Configure your default editor: `git config --global core.editor "vim"` or `"nano"`.
8. Set the default branch name for new repositories.
9. Configure proxies if required: `git config --global http.proxy http://user:pass@proxy:8080`.
10. Harden SSH usage by generating a key pair and adding it to `ssh-agent`.
11. Enable auto-completion by sourcing `/usr/share/git/completion/git-completion.bash`.
12. Configure prompt enhancements via `starship`, `powerlevel10k`, or custom scripts.
13. Install optional tools like `tig`, `delta`, and `hub` to augment Git workflows.
14. Validate that Git works inside containerized environments such as Docker or Podman.
15. Document findings in your infrastructure-as-code repository.

### Linux Maintenance Tasks

- Run `sudo apt upgrade git` or equivalent to stay close to upstream releases.
- Schedule `git gc --aggressive` on large bare repositories maintained by internal services.
- Archive and remove old clones from CI worker nodes to reclaim disk space.
- Monitor syslog for SSH or HTTPS authentication failures.
- Maintain parity across developer machines using configuration management tools like Ansible, Chef, or Puppet.

## Building Git from Source

1. Install dependencies: compilers, libcurl, openssl, zlib, expat, gettext, and tcl.
2. Clone the Git source: `git clone https://github.com/git/git.git`.
3. Checkout the tag you want: `git checkout v2.47.0`.
4. Run `make configure` followed by `./configure --prefix=/usr/local`.
5. Compile with `make all`.
6. Execute the test suite: `make test`.
7. Install with `sudo make install`.
8. Update PATH to include `/usr/local/bin` if necessary.
9. Record build metadata in your configuration management inventory.
10. Automate upgrades via CI pipelines to minimize manual steps.

## Foundational Configuration

```bash
# Identity
git config --global user.name "Sample Engineer"
git config --global user.email "sample.engineer@example.com"

# Defaults
git config --global init.defaultBranch main
git config --global pull.rebase false
git config --global push.default current

# Tooling
git config --global core.editor "code --wait"
git config --global merge.tool vscode
git config --global mergetool.vscode.cmd "code --wait --diff $LOCAL $REMOTE"

# Usability
git config --global color.ui auto
git config --global diff.algorithm histogram
git config --global merge.conflictStyle zdiff3

# Hygiene
git config --global fetch.prune true
git config --global rerere.enabled true
```

## Extended `.gitconfig` Reference

```ini
[user]
    name = Sample Engineer
    email = sample.engineer@example.com
    signingkey = 0A1B2C3D4E5F6A7B
[core]
    editor = code --wait
    autocrlf = input
    excludesfile = ~/.gitignore_global
    ignorecase = false
    longpaths = true
    fscache = true
[color]
    ui = auto
[diff]
    navigate = true
    algorithm = histogram
[difftool "delta"]
    command = delta --side-by-side --line-numbers
[merge]
    ff = only
    conflictstyle = zdiff3
    tool = vscode
[pull]
    rebase = false
[push]
    default = current
    followTags = true
[commit]
    gpgsign = true
    template = ~/.config/git/commit-template.txt
[rerere]
    enabled = true
[alias]
    st = status -sb
    co = checkout
    br = branch
    ci = commit
    lg = log --graph --decorate --oneline
    last = log -1 HEAD
    unstage = reset HEAD --
    count = shortlog -sn --all
[difftool]
    prompt = false
[credential]
    helper = manager-core
[http]
    sslVerify = true
[includeIf "gitdir:~/work/"]
    path = ~/.config/git/work.gitconfig
[includeIf "gitdir:~/personal/"]
    path = ~/.config/git/personal.gitconfig
[pager]
    diff = delta
    log = delta
    show = delta
[interactive]
    diffFilter = delta --color-only
```

## Sample Global `.gitignore`

```text
# OS generated files
.DS_Store
Thumbs.db
*.swp
*.tmp

# IDE metadata
.vscode/
.idea/
*.code-workspace

# Build artifacts
build/
dist/
out/
.DS_Store

# Dependencies
node_modules/
venv/
.venv/
Pods/

# Logs
*.log
logs/
```

## Credential Management Options

- HTTPS with Git Credential Manager Core: integrates with Azure Active Directory, GitHub, GitLab, and Bitbucket.
- SSH keys stored in agents: ideal for automation or air-gapped environments.
- Personal access tokens stored in encrypted password managers.
- Hardware-backed keys such as YubiKeys for FIDO2 or PIV authentication.
- Short-lived credentials minted by cloud identity providers.

## Proxy and Firewall Considerations

1. Request outbound firewall exceptions for Git protocols.
2. Configure `git config --global http.proxy` and `https.proxy` if proxies are unavoidable.
3. Install root certificates for proxies performing TLS inspection.
4. Mirror repositories within your network to reduce latency.
5. Document fallback URLs and authentication methods for remote workers.

## Shell Integration Enhancements

- Source Git completion scripts to enable branch-aware tab completion.
- Customize prompts with branch, status, and ahead/behind indicators.
- Use `direnv` or `.envrc` files to load project-specific environment variables.
- Integrate `fzf` for fuzzy-finding branches, tags, and commits.
- Configure `tmux` key bindings for rapid staging or log inspection.

## IDE and Editor Integration

- Enable built-in Git features in VS Code, JetBrains IDEs, Sublime Text, and Vim plugins.
- Configure auto-fetch intervals to keep local state fresh.
- Ensure editors respect `.editorconfig` files to enforce formatting.
- Install diff viewers like Kaleidoscope or Beyond Compare for complex merges.
- Validate accessibility settings for color-sensitive or screen-reader users.

## Validation Commands

```bash
which git
git --version
git config --list --show-origin
git status
git init sandbox && cd sandbox && git status && cd .. && rm -rf sandbox
git credential-manager-core --help 2>/dev/null || echo "credential manager not available"
ssh -T git@github.com || true
git ls-remote https://github.com/git/git.git HEAD
GIT_TRACE=1 git ls-remote https://github.com/git/git.git HEAD
```

## Troubleshooting Guide

1. `command not found`: update PATH and relaunch terminals.
2. Permission errors: run installers as admin or use `sudo`.
3. TLS failures: update certificates or disable SSL inspection temporarily.
4. Proxy authentication loops: supply credentials via environment variables.
5. Stale credentials: clear caches with `git credential-manager-core erase`.
6. Signing failures: verify the signing key is trusted and unlocked.
7. `fatal: unable to access`: inspect firewalls, DNS, and network routing.
8. Slow operations: enable tracing or adjust antivirus exclusions.
9. Conflicting binaries: remove older Git versions from `/usr/bin` or `C:\Program Files`.
10. IDE integration issues: update extensions and restart the editor.

## Scenario Catalog

### When provisioning a brand-new laptop

- Clone dotfiles repository.
- Run bootstrap script.
- Verify Git installation.
- Configure SSH keys.
- Test both HTTPS and SSH connectivity.

### When rotating credentials

- Revoke old tokens or keys.
- Generate new credentials.
- Update Git configuration.
- Test authentication flows.
- Document the rotation for audit purposes.

### When onboarding contractors

- Provide read-only credentials if necessary.
- Share documentation for temporary environments.
- Require separate SSH keys per contractor.
- Enable logging for access monitoring.
- Schedule deprovisioning tasks.

### When setting up CI runners

- Install Git in the base image.
- Configure non-interactive credential storage.
- Cache dependencies for faster builds.
- Enforce consistent git versions across runners.
- Monitor disk usage in workspace directories.

### When migrating to managed developer environments

- Capture existing configuration exports.
- Translate settings to dev container definitions.
- Validate CLI behavior within the new environment.
- Ensure signing keys can be used securely.
- Provide rollback instructions.

## Large Reference Table of Configuration Keys

<!-- markdownlint-disable MD033 MD010 -->
<table>
    <thead>
        <tr>
            <th>Category</th>
            <th>Key</th>
            <th>Description</th>
            <th>Suggested Value</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>Identity</td>
            <td>user.name</td>
            <td>Name recorded in commits</td>
            <td>Full legal or preferred name</td>
        </tr>
        <tr>
            <td>Identity</td>
            <td>user.email</td>
            <td>Email recorded in commits</td>
            <td>Company email or no-reply alias</td>
        </tr>
        <tr>
            <td>Workflow</td>
            <td>init.defaultBranch</td>
            <td>Default branch name for new repos</td>
            <td>main</td>
        </tr>
        <tr>
            <td>Workflow</td>
            <td>pull.rebase</td>
            <td>Whether `git pull` rebases</td>
            <td>false</td>
        </tr>
        <tr>
            <td>Security</td>
            <td>commit.gpgsign</td>
            <td>Sign commits automatically</td>
            <td>true</td>
        </tr>
        <tr>
            <td>Security</td>
            <td>tag.forceSignAnnotated</td>
            <td>Require signed annotated tags</td>
            <td>true</td>
        </tr>
        <tr>
            <td>Performance</td>
            <td>pack.window</td>
            <td>Number of objects to consider for delta compression</td>
            <td>50</td>
        </tr>
        <tr>
            <td>Cleanup</td>
            <td>gc.auto</td>
            <td>Automatic garbage collection threshold</td>
            <td>256</td>
        </tr>
        <tr>
            <td>History</td>
            <td>blame.ignoreRevsFile</td>
            <td>Ignore revisions when running blame</td>
            <td>.git-blame-ignore-revs</td>
        </tr>
        <tr>
            <td>UX</td>
            <td>color.ui</td>
            <td>Enable color output</td>
            <td>auto</td>
        </tr>
    </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Hands-On Labs

### Lab 1: Automated Bootstrap Script

1. Write a shell script that installs Git using the platform package manager.
2. Script global configuration commands for identity and signing.
3. Pull down dotfiles and symlink configuration files.
4. Install optional utilities like Git LFS, delta, and tig.
5. Run validation commands and output a summary report.

### Lab 2: Secure Credential Storage

1. Configure HTTPS credential helper.
2. Generate SSH keys and add them to the agent.
3. Store personal access tokens securely using a password manager CLI.
4. Simulate credential revocation and rotation.
5. Document the recovery steps if credentials are lost.

### Lab 3: Troubleshooting Clinic

1. Intentionally break PATH and watch error messages.
2. Remove trust from the signing key and observe commit failures.
3. Introduce a proxy misconfiguration and track the resulting logs.
4. Repair each issue using the strategies described earlier.
5. Share lessons learned during a team retro.

## Automation Blueprint for CI Containers

```bash
#!/usr/bin/env bash
set -euxo pipefail

if ! command -v git >/dev/null; then
  if command -v apt >/dev/null; then
    apt update
    apt install -y git
  elif command -v dnf >/dev/null; then
    dnf install -y git
  elif command -v pacman >/dev/null; then
    pacman -Sy --noconfirm git
  else
    echo "No supported package manager" >&2
    exit 1
  fi
fi

git config --global user.name "CI Bot"
git config --global user.email "ci@example.com"
git config --global credential.helper store

echo "machine github.com login ci-bot password ${GITHUB_TOKEN}" >>~/.netrc
```

## Frequently Asked Questions

- **Can I install multiple Git versions side by side?** Yes, manage them with custom prefixes and adjust PATH per project.
- **How do I uninstall Git cleanly?** Use the package manager or the Windows uninstaller; remove lingering configuration files if desired.
- **Do I need Git LFS?** Only when handling large binaries; otherwise it adds unnecessary complexity.
- **What if corporate policy forbids downloading binaries from the internet?** Use internal mirrors or artifact repositories that proxy official downloads.
- **How do I validate that commit signing works?** Run `git log --show-signature` after creating a signed commit.

## Reflection Prompts

- Which steps in your installation process are still manual and error-prone?
- How will you keep your configuration synchronized across machines?
- What is your plan if the credential helper stops working unexpectedly?
- Do you need to provide alternative instructions for contractors or interns with limited permissions?
- How will you track future updates to Git and dependencies?

## Summary

Installing Git is the gateway to collaboration. By investing in reliable automation, secure defaults, and documented playbooks, your organization turns setup from a frustration into a fast lane. Carry these patterns forward as you dive into core Git concepts, branching strategies, and advanced history manipulation in the upcoming lessons.

## Practice Checklist

- [ ] Install Git and confirm the version.
- [ ] Configure identity, signing, and credential helpers.
- [ ] Customize editors, diff tools, and prompts.
- [ ] Validate connectivity to remote hosting platforms.
- [ ] Document configuration steps for future onboarding.
- [ ] Automate the bootstrap process for repeatability.

Next up: explore the core Git concepts that define repositories, commits, trees, and blobs.
