
<!-- README.md is generated from README.Rmd. Please edit that file -->

# RpkgCheck

<!-- badges: start -->
<!-- badges: end -->

### Create package in a new directory (RStudio)

Create new project (right top of RStudio or File -\> New Project) and
select R package in a new directory. This creates package and the
directory structure. Start with deleting `R/hello.R`, `man/hello.Rd` and
`NAMESPACE` files (last one will be automatically recreated when
`devtools::document()` is run).

### Add function

Create R script, write a function with `roxygen` comments
(documentation) and save the file in `R` directory, e.g.`pkg/R/funct.R`.

To render function documentation and update `NAMESPACE` file, run
`devtools::document()`.

### Add unit tests

Recommended testing framework is `testthat`. To initialize test file
structure, run `usethis::use_testthat()`. This also adds `testthat`
package to `Suggests` field in `DESCRIPTION` file.

Create an R script with unit test and save it as
`pkg/tests/testthat/test-<funct_name>.R`

To run full test suite locally, use `devtools::test()`

### Update `DESCRIPTION` file

-   Add authors, maintainers, title and description,
-   update license,
-   bump package version.

### initialize GitHub repo

-   create new repo on GitHub
-   go to your R package directory
-   `git init`
-   `git branch -m "main"` change name from `master` to `main` if that
    was the default
-   `git remote add origin https://github.com/username/reponame`
-   `git add <dir1> <file1> ...`
-   `git commit -m <commit_msg>`
-   `git push origin main`

### GitHub Readme

To create README template run `usethis::use_readme_rmd()`. Update
`README.Rmd`, knit with `Cmd + Shift + Enter` and `commit` `push` both
files.

### Generate GitHub token

Log in to your github.com account and click on your user, then choose
`Settings`. Select `Developer Settings` at the bottom and then
`Personal access tokens`. Click `Generate new token`. Name the token,
set expiration time and tick `workflow` as scope (and others if needed).

Use this token in your credential manager (Windows) or Keychain access
(Mac). Look for Keychain Access app on your Mac and type `git` in
searchbar. Double click on `github.com` and then the token goes to
password field.

### Add GitHub Actions

Run `usethis::use_github_action("check-standard")`

This creates `.github` directory:

    .github
    │   .gitignore
    │
    └─── workflows
           check-standard.yaml

-   `.gitignore` specifies intentionally untracked files to ignore by
    git. Simply files you don’t want to commit and push to remote
    repository. Each file is in new line and config accepts regex
    patterns, e.g. `*.html` ignores all html files.
-   `workflows` subdirectory with GitHub Actions workflows configuration
    files,
    -   `check-standard.yaml` - workflow configuration file -
        `R CMD check`. File can be renamed to more informative name,
        e.g. `R-CMD-check.yaml`. This directory can store multiple
        `.yaml` files for different workflows.

Here are specified branches and events that will trigger the workflow.
Default template is for `main` and `master` only:

      on:
      push:
        branches: [main, master]
      pull_request:
        branches: [main, master]

***Note:*** to trigger `R CMD check` on every branch, substitute
`[main, master]` with `'**'`.

[Click](https://github.com/r-lib/actions/tree/master/examples) for more
details on GitHub Actions workflows for R.

### Protected branches

Go to your repo on GitHub and click on `Settings`. Select `Branches`.

Use branch `main` as your default branch (i.e. cloning repo and creating
branches will default to `main`).

Next, click `Add branch protection rule` and tick (recommended):

-   `Require a pull request before merging`,
-   `Require approvals`,
-   `Require status checks to pass before merging`,
-   `Require branches to be up to date before merging`.

…and anything else, if you want to be more specific about the repository
rules.

Repository owner can merge branches / pull requests without approvals.
*With great power comes great responsibility* :muscle: :whale2:
