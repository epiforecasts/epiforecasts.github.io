# epiForecasts website

This repository contains the source code for the epiForecasts website. 
This website is built with [quarto](https://quarto.org/) and relies on automation wherever possible.

## Adding a new team member

Used to generate this page: https://epiforecasts.io/people.html.

Adding a new team member only requires adding a new team member ID card in [`_data/team/`](_data/team/). The `README` there details how to do it.

The 'people' page will automatically include the new member.

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
