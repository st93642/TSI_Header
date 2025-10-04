# Lesson 3.1: Feature Branching, Trunk-Based, and Gitflow

## Selecting a Branching Model

Branching policies shape collaboration velocity. Choose pragmatically:

![Branching Models](../../../../resources/git/git_branching_models.svg)

## Decision Matrix

<!-- markdownlint-disable MD033 MD010 -->
<table>
	<thead>
		<tr>
			<th>Criteria</th>
			<th>Feature Branching</th>
			<th>Trunk-Based</th>
			<th>Gitflow</th>
		</tr>
	</thead>
	<tbody>
		<tr>
			<td>Deployment Frequency</td>
			<td>Moderate</td>
			<td>Rapid</td>
			<td>Scheduled</td>
		</tr>
		<tr>
			<td>Review Model</td>
			<td>Pull Requests</td>
			<td>Pairing, CI gates</td>
			<td>Pull Requests</td>
		</tr>
		<tr>
			<td>Rollback Simplicity</td>
			<td>Medium</td>
			<td>High</td>
			<td>Medium</td>
		</tr>
		<tr>
			<td>Complexity</td>
			<td>Low</td>
			<td>Low</td>
			<td>High</td>
		</tr>
	</tbody>
</table>
<!-- markdownlint-enable MD033 MD010 -->

### Branch Policy Matrix

![Branch Policy Matrix](../../../../resources/git/git_branch_policy_matrix.svg)

Codify branch rules to maintain momentum:

- Require status checks and reviews before merging into protected branches.
- Use automation to delete merged branches and encourage short-lived work.
- Gate release branches with deployment smoke tests and service-level objectives.

### Environment Promotion Path

![Environment Promotion Path](../../../../resources/git/git_environment_promotion.svg)

Map code promotion to infrastructure:

- Feature branches deploy to ephemeral preview environments.
- Mainline merges trigger staging deployments with synthetic monitoring.
- Release branches promote to production via progressive delivery (canary or blue/green).

## Aligning with CI/CD

Frequent integration requires automated testing. Combine branch protection rules with status checks to enforce quality.

### Practice

- Map your team’s release cadence against these models.
- Configure branch protection rules and required reviews in a hosting provider.
- Simulate trunk-based development with a 24-hour integration window.
