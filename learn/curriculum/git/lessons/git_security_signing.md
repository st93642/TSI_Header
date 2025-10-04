# Lesson 5.3: Commit Signing, Verified Releases, and Supply Chain Trust

## Why Signing Matters

Signed commits and tags provide a verifiable trust chain, ensuring releases originate from authorized maintainers.

![Signing Trust Chain](../../../../resources/git/git_signing_trust_chain.svg)

### Generating Keys

- GPG: `gpg --full-generate-key`
- SSH signing (Git 2.34+): `ssh-keygen -t ed25519 -C "signing key"`

Configure Git to sign automatically:

```bash
git config --global user.signingkey <key-id>
git config --global commit.gpgsign true
git config --global tag.gpgsign true
```

## Verifying Signatures

- `git log --show-signature`
- Hosting platforms display verified badges when signatures match uploaded keys.

### Signature Validation Flow

![Signature Validation Flow](../../../../resources/git/git_signature_validation_flow.svg)

Understand verification end-to-end:

- Git computes commit hashes, decrypts signatures, and compares against trusted public keys.
- GPG or SSH agents manage private keys; store them securely with passphrases or hardware tokens.
- CI pipelines can fail builds when commits or tags lack valid signatures.

## Protecting the Supply Chain

- Enforce signed commits via branch protection rules.
- Rotate keys periodically and maintain revocation lists.
- Combine signing with release artifact checksums and SBOMs.

### Policy Enforcement Matrix

![Policy Enforcement Matrix](../../../../resources/git/git_policy_enforcement.svg)

Balance velocity and assurance:

- Development branches may allow unsigned commits with daily audits.
- Main and release branches should require signatures and verified statuses.
- Artifact signing extends trust to binaries using tools like Sigstore or Cosign.

### Practice

- Generate a signing key and configure Git to use it.
- Sign a commit and verify on a remote hosting platform.
- Audit repository settings to enforce signed tags for releases.

## Key Management & CI Verification Appendix

This appendix covers practical key lifecycle management, rotation, CI verification, and revocation practices.

### Key Types and Rotation Cadence

<table>
  <thead>
    <tr>
      <th>Key Type</th>
      <th>Recommended Rotation</th>
      <th>Storage</th>
      <th>Use</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>GPG user key</td>
      <td>1–2 years</td>
      <td>Hardware token (YubiKey) or secure HSM</td>
      <td>Commit and tag signing</td>
    </tr>
    <tr>
      <td>CI signing key</td>
      <td>6–12 months</td>
      <td>CI secrets manager (vault)</td>
      <td>Artifact signing in pipeline</td>
    </tr>
    <tr>
      <td>SSH signing key</td>
      <td>1 year</td>
      <td>Hardware token or encrypted store</td>
      <td>SSH-based signing and verification</td>
    </tr>
  </tbody>
</table>

---

### CI Verification Snippets

In CI, verify that the commit that triggered the build is signed by a trusted key before publishing artifacts. Example (bash):

```bash
git fetch --no-tags origin +refs/tags/*:refs/tags/*
sha=$(git rev-parse HEAD)
if git log -1 --show-signature $sha | grep -q "Good signature"; then
  echo "Signature verified"
else
  echo "Signature missing or invalid"; exit 1
fi
```

For artifact signing, use Cosign or Sigstore in your pipeline and keep the signing key in a secure vault. Rotate pipeline keys on schedule and revoke compromised keys immediately.

---

### Revocation and Recovery

- Publish key revocation certificates for GPG keys and update trust stores across CI and developer machines.
- Maintain a minimal list of emergency signing keys stored in an HSM for disaster scenarios.
- When rotating keys, update repository-level trust metadata and CI secrets atomically to avoid breaking builds.

---

### Exercises: Key Lifecycle Drills

1. Configure a GitHub Action to verify GPG-signed commits and fail the workflow on missing or invalid signatures.
2. Create a key rotation plan for CI signing keys and simulate rotating a key and updating pipelines.
3. Generate and publish a revocation certificate, then validate that revoked keys no longer verify.

---

End of Key Management & CI Verification Appendix.

## Verification at Scale — Cosign, Sigstore, and CI Patterns

Artifact signing and verification scales differently than commit signing. Use Sigstore/Cosign for container and artifact verification and integrate checks into CI.

### Cosign Example (container signing)

```bash
# sign an image
COSIGN_PASSWORD=$(cat /path/to/password) cosign sign --key cosign.key ghcr.io/org/image:tag

# verify
cosign verify --key cosign.pub ghcr.io/org/image:tag
```

### Pipeline gate examples

- Pre-release: verify commit & tag signatures and artifact signatures in the release job.
- Pre-merge: require that the PR contains commits signed by a trusted key (CI check).
- Post-deploy: perform signature verification of deployed artifacts before scaling.

---

### Revocation Templates and Emergency Procedures

- Create and store revocation certificates for GPG keys in a secure location and publish a revocation notice in the project security page.
- CI should check a centralized revocation list before accepting a signature as valid.

---

### Verification Gates (HTML Table)

<table>
  <thead>
    <tr>
      <th>Gate</th>
      <th>What it checks</th>
      <th>Action on failure</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>Pre-merge</td>
      <td>Commit signatures, PR author identity</td>
      <td>Block merge, request signed commit</td>
    </tr>
    <tr>
      <td>Release</td>
      <td>Tag signatures, artifact signatures</td>
      <td>Fail release pipeline</td>
    </tr>
    <tr>
      <td>Post-deploy</td>
      <td>Artifact signature verification</td>
      <td>Rollback or halt scale-up</td>
    </tr>
  </tbody>
</table>

---

### Exercises: Verification Checks

1. Add a Cosign verify step to an example release workflow.
2. Create an automated check that consults a revocation list before accepting commit signatures.
3. Draft a security notice template for publishing key revocations and compromised-key procedures.

---

End of Verification at Scale.

## Key lifecycle, rotation, and CI verification playbook

This appendix covers operational key management, rotation policies, and CI verification recipes (GPG, SSH, and Sigstore/Cosign). It includes sample commands and a compact compromise/recovery playbook.

### Key types and short guidance

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Key Type</th><th>Use Case</th><th>Rotation Frequency</th><th>Recovery Options</th></tr>
  </thead>
  <tbody>
    <tr><td>GPG (commit/tag signing)</td><td>Signing commits and tags</td><td>2y (routine), immediate if compromise</td><td>Revoke key, rotate, re-sign tags as needed</td></tr>
    <tr><td>SSH deploy keys</td><td>Server-to-server access for CI</td><td>1y or per-employee exit</td><td>Revoke from server, rotate key pair</td></tr>
    <tr><td>Sigstore / Cosign</td><td>Container and artifact signing</td><td>Short-lived keys via OIDC</td><td>Rotate token provider, re-sign artifacts</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### CI verification of signatures (GPG example)

- Ensure the CI environment has a truststore of allowed public keys. These keys should be stored in your secret management system and applied to CI runners at runtime.

Example verification step in CI:

```bash
# import public key
gpg --import /secrets/trusted-keys.gpg

# verify last commit signature
git log -1 --show-signature

# exit non-zero on unsigned commit
if ! git log -1 --show-signature | grep -q "Good signature"; then
  echo "Commit is not signed or signature not trusted"
  exit 1
fi
```

### Cosign example for container signatures

- Use OIDC-based short-lived credentials in workflows to sign artifacts without long-lived secrets.

```yaml
- name: Set up cosign
  uses: sigstore/cosign-installer@v1

- name: Sign container image
  run: |
    cosign sign --key $COSIGN_KEY $IMAGE

- name: Verify
  run: |
    cosign verify --key $COSIGN_PUB $IMAGE
```

### Key compromise quick playbook

1. Revoke compromised key immediately (GPG: `gpg --gen-revoke KEYID`), and publish the revocation certificate where your org expects it.
2. Rotate CI and server keys that depended on the compromised key: replace deploy keys, tokens, and update trusted keysets.
3. Rebuild and re-sign critical artifacts where feasible. For commits, document which tags/releases need re-signing and consider issuing new signed tags.
4. Communicate to stakeholders with an incident timeline and action items.

### Automating rotation: sample rotation script (GPG public refresh)

```bash
#!/usr/bin/env bash
# refresh trusted keyring from a central keyserver of trusted keys
TRUSTED_KEYS_URL="https://keys.example.com/trusted-keys.gpg"
OUTDIR="/tmp/trusted-keys"
mkdir -p "$OUTDIR"
curl -fsS "$TRUSTED_KEYS_URL" -o "$OUTDIR/trusted-keys.gpg"
# rotate into CI secrets or runner image pipeline
# pipeline-specific: inject into runner container image build or mount via secret store
```

### Verification policy and attestation

- Keep a list of allowed key fingerprints and rotate them via documented process.
- For artifacts and container images, require both signature verification and provenance checks (SBOMs, reproducible builds where possible).

### Appendix: signing examples and tags

- Create an annotated, signed tag: `git tag -a v1.2.0 -m "Release v1.2.0" --sign`
- Verify a tag locally: `git tag -v v1.2.0`

### Audit and reporting

- Record when keys were rotated and by whom in a central audit log.
- CI pipelines should emit verification results as structured logs to your observability system so security teams can query verification success rates.

### Exercises: Rotation & Monitoring

- Practice a key rotation drill on a staging repository: generate new keys, rotate the truststore in CI, and verify signed builds continue to pass.
- Run a simulated compromise: mark a key revoked, ensure CI fails for unsigned commits, and rebuild with rotated keys.

## Operational key rotation drills and automation

Regular drills reduce mistakes during real key compromise events. Below is an automated rotation checklist and an example pipeline snippet to rotate CI signing keys safely.

### Rotation checklist (practice drill)

1. Prepare a new keypair in a secure environment and create a signed attestation of the key's fingerprint.
2. Publish the public key to the organization's trusted keystore and update CI runner images to include the new public key.
3. Re-sign critical release artifacts (if feasible) and publish a migration notice linking old to new signatures.
4. Revoke old keys and publish revocation certificates to all trust stores.

### CI key rotation sample (high-level)

- Stage: build image with new key (do not replace the live signing key yet).
- Validate: run signing and verification on a staging pipeline.
- Cutover: rotate secrets in CI (atomic update), switch pipelines to the new key, and monitor signing success.

### Sigstore / Cosign advanced pattern

- Use ephemeral tokens via OIDC to sign artifacts without long-lived secrets.
- Record transparency log entries and integrate verification step in release gating.

```yaml
- name: Sign artifact with cosign (OIDC)
  run: |
    cosign sign --keyless $IMAGE
- name: Verify transparency
  run: |
    cosign verify --keyless $IMAGE
```

### Revocation communication template

- Subject: [SEC] Key rotation/revocation for repository X
- Body: explain the reason, provide the new key fingerprint, list impacted artifacts, and provide contact info for rollback.

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Key Role</th><th>Rotation Frequency</th><th>Owner</th><th>Notes</th></tr>
  </thead>
  <tbody>
    <tr><td>GPG user keys</td><td>1-2y</td><td>Maintainers</td><td>Use hardware tokens</td></tr>
    <tr><td>CI signing keys</td><td>6-12 months</td><td>CI team</td><td>Use vault/secret manager</td></tr>
    <tr><td>Cosign OIDC tokens</td><td>Short-lived</td><td>Platform</td><td>Prefer keyless signing where possible</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

## Audit and compliance notes

- Keep structured logs of rotation events (who rotated, when, and why) and retain them for audits.
- For regulated environments, include attestations and audit trails in the revocation package.

## Exercises

- Run a rotation drill in a staging repo: generate a new key, update CI, and validate that signed releases pass verification.
- Implement a monitoring script that checks signature verification success across recent releases and alerts if verification fails.

<!-- end of appended security signing content -->

---

## Appendix: Signing Automation, Compliance, and Incident Playbooks

This appendix provides scripts and templates to automate verification, rotate keys, and respond to signature compromise incidents.

### Automated Signing Verification (CI-friendly)

Add a short CI step that fails the job when the commit or tag is unsigned or not in the trusted keyring.

```bash
#!/usr/bin/env bash
set -euo pipefail

# import allowed public keys
gpg --import /secrets/trusted-keys.gpg

sha=$(git rev-parse HEAD)
if git log -1 --show-signature $sha | grep -q "Good signature"; then
  echo "commit signature verified"
else
  echo "Missing or invalid commit signature"; exit 1
fi
```

### Cosign Integration (artifact signing in CI)

Example GitHub Actions job (cosign + OIDC):

```yaml
name: Sign Artifact
on: [push]
jobs:
  sign:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Install cosign
        uses: sigstore/cosign-installer@v1
      - name: Sign image (OIDC)
        run: cosign sign --keyless ghcr.io/org/image:tag
      - name: Verify signature
        run: cosign verify --keyless ghcr.io/org/image:tag
```

### Compliance Table (HTML)

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Requirement</th><th>Automated Check</th><th>Enforcement</th></tr>
  </thead>
  <tbody>
    <tr><td>All release tags must be signed</td><td>CI verifies tag signatures</td><td>Fail release job on missing signature</td></tr>
    <tr><td>CI artifacts signed</td><td>Cosign verification step</td><td>Prevent promotion to registry on failure</td></tr>
    <tr><td>Revocation list consulted</td><td>CI checks central revocation list</td><td>Fail if key is revoked</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Incident Playbook: Compromised Signing Key

1. Revoke the compromised key and publish the revocation certificate.
2. Rotate CI keys and update truststores.
3. Re-sign critical artifacts where feasible and publish notices linking old->new signatures.
4. Produce an audit package with the timeline and mitigations.

### Key Rotation Script (high-level)

```bash
#!/usr/bin/env bash
set -euo pipefail
# rotate-ci-key.sh: rotate CI signing key in a safe, staged manner
# Stage: create new keypair in secure environment
# Stage: publish public key to trusted keyserver and update CI secrets
# Stage: test signing in staging pipeline
# Stage: cutover by updating live CI secrets

# This script is a placeholder for your platform-specific secret management steps
echo "Rotate keys using your secret manager (vault/awsssm) and test in staging"
```

### Audit & Reporting Template (JSON)

```json
{
  "incident": "compromised-key-2025-10-04",
  "actions": ["revoked-key", "rotated-ci-secrets", "re-signed-artifacts"],
  "owner": "security-team@example.com",
  "timestamp": "2025-10-04T00:00:00Z"
}
```

### Exercises

1. Add a CI check that refuses to merge PRs that modify release-related scripts unless the commit is signed by a maintainer key.
2. Simulate revoking a GPG key and confirm that CI rejects commits signed with the revoked key.
3. Implement an automated alert that triggers when a tag is pushed without a signature.

---

## Quick Reference: Signing & Verification Cheat Sheet

Keep this short cheat sheet in your contributor docs and CI runbooks.

- Create a new GPG key (interactive):

```bash
gpg --full-generate-key
# list keys
gpg --list-secret-keys --keyid-format LONG
```

- Configure Git to use a key:

```bash
git config --global user.signingkey <KEYID>
git config --global commit.gpgsign true
git config --global tag.gpgsign true
```

- Create an annotated signed tag:

```bash
git tag -a v1.2.0 -m "Release v1.2.0" --sign
git push origin v1.2.0
```

- Quick CI verification (one-liner to use in pipelines):

```bash
if ! git log -1 --show-signature | grep -q "Good signature"; then echo "Unsigned commit"; exit 1; fi
```

---

### HTML: Minimal signing policy for README (copyable)

<!-- markdownlint-disable MD033 MD010 -->
<table>
  <thead>
    <tr><th>Policy</th><th>Description</th><th>Where Enforced</th></tr>
  </thead>
  <tbody>
    <tr><td>Signed release tags</td><td>All release tags must be GPG- or cosign-signed</td><td>Release CI job</td></tr>
    <tr><td>Trusted keyring</td><td>CI imports only keys from org-keystore</td><td>CI runner startup</td></tr>
    <tr><td>Compromise procedure</td><td>Revoke and communicate within 1 hour</td><td>Security page + CI hard-fail</td></tr>
  </tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

---

## Quick-play: Practical Exercises (automatable)

1. Add a GitHub Action that runs a `verify-signature` step on pull requests and fails if the tip commit is unsigned.
2. Implement a staging pipeline which uses cosign `--keyless` signing for ephemeral build images and verifies transparency logs.
3. Create a monitoring job that queries the last 30 tags and emits a metric for how many have verified signatures.

---

## Troubleshooting checklist

- "Good signature" not shown: confirm the public key is in the CI truststore and that the key is not expired.
- Local verification works but CI fails: ensure the runner imports the same public keys and has GPG configured (e.g., GPG_TTY, keyring path).
- Cosign verify failing for OIDC: confirm the issuer and audience match the cosign verification expectations and that the transparency log entries exist.

-- end appended signing quick-reference --
