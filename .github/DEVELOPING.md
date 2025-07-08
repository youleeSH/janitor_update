# Developer Guide for janitor

Thank you for your interest in contributing to the `janitor` package! This guide provides information for developers working on new features, bug fixes, or improvements to the package. It outlines best practices for local development, testing, documentation, and submitting pull requests.

## Setting Up Your Development Environment

To begin, fork the `janitor` repository and clone it to your local machine. If you haven’t already, set up the original repository as the upstream remote:
  
```
git remote add upstream https://github.com/sfirke/janitor.git
```

Before starting new work, always sync your local copy with the upstream to avoid merge conflicts:

```
git pull upstream main
```

You can load the package locally using the devtools package:

```
devtools::load_all("path/to/janitor")
```

Make sure you have all required dependencies installed, including testthat, dplyr, tidyr, and tibble.

## Project Structure

The key directories in the project are as follows:

- `R/` contains the implementation of all package functions.

- `tests/testthat/` includes unit tests written using the `testthat` framework.

- `man/` stores generated documentation files from roxygen2 comments.

- `vignettes/` includes longer-form documentation with real usage examples.

- `data/` and `inst/` contain package datasets and supporting resources.

## Writing Tests

All new features must include unit tests to ensure reliability and maintainability. Tests should be placed in `tests/testthat/` with a file name matching the target function (e.g., `test-clean_names.R`).

```
devtools::test()  # runs all tests
# or for a specific file:
testthat::test_file("tests/testthat/test-yourfunction.R")
```

## Documenting Functions

All exported functions should be documented using roxygen2-style comments directly above their definitions in the `R/` folder.

Each function should include:

- `@description`: A concise summary of what the function does.

- `@param`: Description of each input argument.

- `@return`: Description of the output (including type).

- `@examples`: Short examples of usage.

- `@export`: To make the function available to users.

After writing or modifying documentation, update the `man/` files by running:

```
devtools::document()
```

This will regenerate .Rd help files and update the NAMESPACE.

## Style Guide

Please follow the tidyverse style guide for consistent code formatting:

- Use `snake_case` for function and variable names.

- Indent with two spaces (not tabs).

- Limit lines to 80 characters where possible.

- Place a space after commas and around operators (`=`, `<-`, etc.).

Example:

```
clean_names = function(data, case = "snake") {
  ...
}
```

We recommend using the `{styler}` and `{lintr}` packages to automatically format and lint code:

```
styler::style_pkg()
lintr::lint_package()
```

## Adding New FUnctions

If you're adding a new function:

1. Create the `.R` file in the R/ directory.

2. Write comprehensive roxygen2 documentation above the function.

3. Add unit tests in `tests/testthat/`.

4. Include examples or use cases in a vignette (if applicable).

5. Run `devtools::document()` to update docs.

6. Confirm all tests pass: `devtools::test()`

## Building and Rendering Documentation

To update the README:

```
devtools::build_readme()
```

To generate the pkgdown website (if applicable):

```
pkgdown::build_site()
```

To render a specific vignette:

```
rmarkdown::render("vignettes/your-vignette.Rmd")
```

## Submitting a Pull Request

Before submitting:

- Ensure all tests pass (`devtools::test()`)

- Check no documentation warnings (`devtools::check()`)

- Rebuild documentation (`devtools::document()`)

Then:

1. Commit your changes with a clear and concise message.

2. Push to your fork on GitHub.

3. Open a pull request (PR) against the `main` branch of the upstream repository.

4. In the PR, describe:

  - What the change does

  - Why it’s needed

  - Any important implementation notes

## Communicating with Maintainers

If you’re unsure about a proposed change or feature:

- Open an Issue to discuss it first.

- Use respectful, constructive language when suggesting changes or improvements.

## Versioning and Changelog

When making changes, especially user-facing ones, update the NEWS.md file with a short bullet under a new version heading, e.g.:

```
## janitor 2.2.1.9000

- unreleased development version
```

The maintainer will handle version bumps before releases.

## Thanks for Contributing!

Your contributions help make `janitor` more powerful and user-friendly.
Whether you're fixing bugs, adding features, improving tests, or enhancing documentation — thank you!
