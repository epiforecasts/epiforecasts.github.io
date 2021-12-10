
<!-- README.md is generated from README.Rmd. Please edit that file -->

## How to create or edit a team member ID card

This folder contains all the (current and former) team member ID cards.

When a new person join the team, they should create a new ID card. When
a person leaves the team, they should switch the `current-member` flag
to `false` (but not delete their card as it is used to list the former
team members).

The validity of the yaml file is automatically checked with a [GitHub
Action](https://github.com/epiforecasts/epiforecasts-distill/blob/main/.github/workflows/validate-team-member-id.yaml)
on pushes and pull requests.

### Mandatory fields

#### `name`

`string`

Your name. It is only used for display and thus doesn’t have to match
your administrative name.

#### `current-member`

`boolean`

A boolean saying whether you’re currently a team member or not.

### Optional fields

#### `github`

`string`

Your github username (*without* the ‘@’)

#### `twitter`

`string`

Your twitter username (with and without @ both work)

#### `orcid`

`string`

Your ORCiD

#### `description`

`string`

A short text about yourself and your work within the team.

#### `webpage`

`string`

A link to your personal webpage
