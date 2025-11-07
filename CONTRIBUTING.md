# Contributing to rOFI

First off, thank you for considering contributing to rOFI! This package was created as an educational tool, and contributions that enhance its teaching value are especially welcome.

## How Can I Contribute?

### Reporting Bugs

Before creating bug reports, please check existing issues to avoid duplicates. When creating a bug report, please include:

* A clear and descriptive title
* Steps to reproduce the behavior
* Expected behavior
* Actual behavior
* Your R session info (`sessionInfo()`)
* Minimal reproducible example

### Suggesting Enhancements

Enhancement suggestions are tracked as GitHub issues. When creating an enhancement suggestion, please include:

* A clear and descriptive title
* Step-by-step description of the suggested enhancement
* Specific examples to demonstrate the steps
* Explanation of why this enhancement would be useful

### Pull Requests

1. Fork the repository
2. Create a new branch (`git checkout -b feature/amazing-feature`)
3. Make your changes
4. Add tests for new functionality
5. Run the test suite (`devtools::test()`)
6. Update documentation if needed
7. Commit your changes (`git commit -m 'Add amazing feature'`)
8. Push to the branch (`git push origin feature/amazing-feature`)
9. Open a Pull Request

## Development Setup

```r
# Clone your fork
git clone https://github.com/your-username/rOFI_package.git

# Install development dependencies
install.packages(c("devtools", "testthat", "roxygen2"))

# Load the package for development
devtools::load_all()

# Run tests
devtools::test()

# Check the package
devtools::check()
```

## Style Guide

* Follow the [tidyverse style guide](https://style.tidyverse.org/)
* Use meaningful variable names
* Comment complex logic
* Write tests for new functions
* Update documentation for API changes

## Testing

* Write unit tests for new functionality
* Ensure all tests pass before submitting PR
* Aim for high test coverage
* Test edge cases

## Documentation

* Use roxygen2 for function documentation
* Include examples in function documentation
* Update vignettes if adding major features
* Keep README.md up to date

## Code of Conduct

Please note that this project is released with a Contributor Code of Conduct. By participating in this project you agree to abide by its terms.

## Questions?

Feel free to open an issue with the "question" label or reach out to the maintainer.

Thank you for contributing to rOFI! 🎉
