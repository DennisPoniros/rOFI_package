# GitHub Repository Setup Guide for rOFI Package

## Step-by-Step Instructions

### 1. Download the Package Files
First, download the complete rOFI package from this environment to your local machine.

### 2. Initialize Git (if needed)
Open a terminal/command prompt in the rOFI directory and run:

```bash
cd path/to/rOFI
git init
```

### 3. Connect to Your GitHub Repository

```bash
git remote add origin https://github.com/DennisPoniros/rOFI_package.git
```

### 4. Create Initial Commit

```bash
# Add all files
git add .

# Create the initial commit
git commit -m "Initial commit: Complete rOFI package for order-flow imbalance analysis

- Core functions: compute_ofi(), plot_ofi(), simulate_orders(), as_ofi()
- Comprehensive documentation with roxygen2
- Full test suite with ~100 unit tests
- Vignette with educational examples
- GitHub Actions for CI/CD
- Ready for STAT 611 Honors Option presentation"
```

### 5. Push to GitHub

```bash
# Create and push to main branch
git branch -M main
git push -u origin main
```

## What Gets Uploaded

### ✅ Package Core Files
- `DESCRIPTION` - Package metadata
- `NAMESPACE` - Function exports
- `LICENSE` - MIT license
- `NEWS.md` - Version history
- `README.md` - Main documentation

### ✅ R Code (`R/` directory)
- `as_ofi.R` - Data standardization function
- `compute_ofi.R` - OFI calculation engine
- `plot_ofi.R` - Visualization functions
- `simulate_orders.R` - Synthetic data generation
- `rOFI-package.R` - Package documentation

### ✅ Tests (`tests/` directory)
- Complete test suite for all functions
- ~100 unit tests ensuring reliability

### ✅ Documentation (`vignettes/` directory)
- `getting-started.Rmd` - Comprehensive tutorial

### ✅ Data Generation (`data-raw/` directory)
- `make_demo_data.R` - Script to generate demo dataset

### ✅ GitHub-Specific Files
- `.gitignore` - Excludes unnecessary files
- `.github/workflows/R-CMD-check.yaml` - Automated testing
- `CONTRIBUTING.md` - Contribution guidelines
- `.Rbuildignore` - R package build exclusions

## After Pushing

### 1. Enable GitHub Actions
- Go to your repository's "Actions" tab
- Enable workflows if prompted
- The R-CMD-check will run automatically on each push

### 2. Update Repository Settings
- Go to Settings → About (gear icon on main page)
- Add description: "R package for computing and visualizing order-flow imbalance patterns"
- Add topics: `r`, `r-package`, `market-microstructure`, `order-flow`, `finance`, `education`
- Set website: Link to your Quarto site when ready

### 3. Create a Release (Optional)
```bash
git tag -a v0.1.0 -m "Initial release: rOFI package v0.1.0"
git push origin v0.1.0
```

Then on GitHub:
- Go to "Releases" → "Create a new release"
- Choose tag `v0.1.0`
- Release title: "rOFI v0.1.0 - Initial Release"
- Describe main features
- Attach `rOFI_0.1.0.tar.gz` if you've built it

### 4. Add Badges to README
The README already includes badges that will activate once you push:
- R-CMD-check status (will show after first Action run)
- MIT License badge (already active)

### 5. Optional Enhancements

#### Add Code Coverage
```yaml
# Add to .github/workflows/test-coverage.yaml
- uses: codecov/codecov-action@v3
```

#### Add pkgdown Site
```yaml
# Add to .github/workflows/pkgdown.yaml
- uses: r-lib/actions/setup-r-dependencies@v2
  with:
    extra-packages: any::pkgdown, local::.
    
- name: Build site
  run: pkgdown::build_site_github_pages()
```

## Testing Your Repository

After pushing, verify:

1. ✅ All files are visible on GitHub
2. ✅ README displays correctly with images
3. ✅ GitHub Actions runs successfully (green check mark)
4. ✅ Installation works: `devtools::install_github("DennisPoniros/rOFI_package")`

## Troubleshooting

### If push is rejected (non-fast-forward)
```bash
# Pull first, then push
git pull origin main --allow-unrelated-histories
git push origin main
```

### If you need to force push (careful!)
```bash
git push --force origin main
```

### If Actions fail
- Check the Actions tab for error details
- Most common issues: missing dependencies or test failures
- The workflow file is already configured correctly

## Quick Command Summary

```bash
# One-line setup (run from rOFI directory)
git init && git add . && git commit -m "Initial commit: Complete rOFI package" && git branch -M main && git remote add origin https://github.com/DennisPoniros/rOFI_package.git && git push -u origin main
```

## Success! 🎉

Once pushed, your repository will:
- Be publicly available for installation
- Run automated checks on each commit
- Be ready for your Honors presentation
- Serve as a portfolio piece

Remember to mention in your presentation:
- GitHub Actions for CI/CD (professional practice)
- Comprehensive testing (~100 tests)
- Full documentation and vignettes
- Ready for CRAN submission (passes R CMD check)
