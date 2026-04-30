# epiforecasts website

This repository contains the source code for the epiforecasts website.
This website is built with [quarto](https://quarto.org/) and relies on automation wherever possible.

## Adding yourself as a team member

The 'people' page (https://epiforecasts.io/people.html) is generated automatically from the yml files in [`_data/team/`](_data/team/). To add yourself:

1. On GitHub, fork this repository (or create a branch if you have write access).
2. Add a new file `_data/team/firstname-lastname.yml` (lowercase, hyphenated). The easiest way is to copy an existing one, e.g. [`sebastian-funk.yml`](_data/team/sebastian-funk.yml), and edit it.
3. Open a pull request. A GitHub Action will validate your yml against [`_schema.yml`](_data/team/_schema.yml).

A minimal template:

```yaml
name: Your Name
github: your-github-username        # optional, without the '@'
orcid: 0000-0000-0000-0000          # optional
bluesky: yourname.bsky.social       # optional
webpage: https://your.webpage       # optional
position:
  - type: PhD                       # one of: Staff, PhD, Visitor
    start: 2026-01-01               # YYYY-MM-DD
description: >
  A short paragraph about you and your work in the team.
```

When you leave the team, add an `end:` date to your current `position` entry (don't delete the file — it's used to list former members). If you take on a new role, append another entry to `position` rather than overwriting the old one.

The required fields are `name` and `position`; everything else is optional. See [`_data/team/_schema.yml`](_data/team/_schema.yml) for the full schema.

## Updating the 'Software' page

Used to generate this page: https://epiforecasts.io/software.html.

The 'software' page is automatically generated from the content of the [epiforecasts r-universe](https://epiforecasts.r-universe.dev/). If you want to add a new package to the list, you thus have to add it in https://github.com/epiforecasts/universe/blob/main/packages.json with the property `"display_website": true`.

## Updating the list of publications

Used to generate this page: https://epiforecasts.io/pubs.html.

The list of publications is updated semi-automatically each month. Everything is explained in the relevant issue: https://github.com/epiforecasts/epiforecasts.github.io/issues/3

## Writing a new blog post

Used to generate this page: https://epiforecasts.io/blog.html (and the new blog post page).

Create a new folder `YYYY-mm-dd-slug/` under `posts/` and a file named `index.md` inside this new folder. You can start your `index.md` by copying one of the existing blog posts.

### Advertising the blog post

The blog is syndicated via an [RSS feed](https://en.wikipedia.org/wiki/RSS): https://epiforecasts.io/blog.xml. People subscribed to this feed will automatically get a notification when you publish a new post.

Additionally, you should advertise it on twitter via the [`@epiforecasts`](https://twitter.com/epiforecasts) (ask for the password if you don't it or ask someone to post it for you) and via your personal account if you have one.
