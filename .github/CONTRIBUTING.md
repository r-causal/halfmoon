# Contributing to halfmoon

This outlines how to propose a change to halfmoon, an R package for propensity score diagnostics and visualizations for causal inference.

## Fixing typos

You can fix typos, spelling mistakes, or grammatical errors in the documentation directly using the GitHub web interface, as long as the changes are made in the _source_ file. 
This generally means you'll need to edit [roxygen2 comments](https://roxygen2.r-lib.org/articles/roxygen2.html) in an `.R`, not a `.Rd` file. 
You can find the `.R` file that generates the `.Rd` by reading the comment in the first line.

## Bigger changes

If you want to make a bigger change, it's a good idea to first file an issue and make sure someone from the team agrees that it’s needed. 
If you've found a bug, please file an issue that illustrates the bug with a minimal 
[reprex](https://www.tidyverse.org/help/#reprex) (this will also help you write a unit test, if needed).

### Pull request process

*   Fork the package and clone onto your computer. If you haven't done this before, we recommend using `usethis::create_from_github("malcolmbarrett/halfmoon", fork = TRUE)`.

*   Install all development dependencies with `devtools::install_dev_deps()`, and then make sure the package passes R CMD check by running `devtools::check()`. 
    If R CMD check doesn't pass cleanly, it's a good idea to ask for help before continuing. 
*   Create a Git branch for your pull request (PR). We recommend using `usethis::pr_init("brief-description-of-change")`.

*   Make your changes, commit to git, and then create a PR by running `usethis::pr_push()`, and following the prompts in your browser.
    The title of your PR should briefly describe the change.
    The body of your PR should contain `Fixes #issue-number`.

*  For user-facing changes, add a bullet to the top of `NEWS.md` (i.e. just below the first header).

### Code style

*   We generally follow tidyverse style conventions. You can use `air format .` to format the code, but please don't restyle code that has nothing to do with your PR.  

*  We use [roxygen2](https://cran.r-project.org/package=roxygen2), with [Markdown syntax](https://cran.r-project.org/web/packages/roxygen2/vignettes/rd-formatting.html), for documentation.
    - Use `@inheritParams` to avoid duplicating parameter documentation (see `R/utils-documentation.R` for common parameter templates)
    - Add `@family` tags to group related functions
    - Include examples that demonstrate the function's use

*  We use [testthat](https://cran.r-project.org/package=testthat) for unit tests. 
   Contributions with test cases included are easier to accept.
   - For visual tests, use the vdiffr helper in `tests/testthat/helper-vdiffr.R`
   - Ensure all existing tests pass

### Coding standards specific to halfmoon

*  **Error handling**: Use `abort()` for errors and `warn()` for warnings (defined in `R/utils.R`)

*  **Iteration**: Use purrr functions instead of loops where appropriate

*  **Code organization**:
   - Use existing helper functions from utils files when possible
   - Extract anonymous functions >5 lines into named functions
   - Place helper functions at the end of the file where they're used

*  **Helper functions**: Before creating new utilities, check these existing helpers:
   - **Validation**: `R/utils-validation.R` - input validation functions
   - **Group handling**: `R/utils-group.R` - treatment group operations  
   - **Confidence intervals**: `R/utils-ci.R` - CI calculation helpers
   - **General utilities**: `R/utils.R` - error handling, AUC calculation, etc.  

## Code of Conduct

Please note that the halfmoon project is released with a [Contributor Code of Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html). By contributing to this project you agree to abide by its terms.
