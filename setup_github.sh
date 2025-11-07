#!/bin/bash

# GitHub Setup Script for rOFI Package
# Run this from within the rOFI directory

echo "==================================="
echo "rOFI Package - GitHub Setup Script"
echo "==================================="
echo ""

# Check if we're in the rOFI directory
if [ ! -f "DESCRIPTION" ]; then
    echo "Error: This script must be run from the rOFI package directory"
    echo "Please cd to the rOFI directory and try again"
    exit 1
fi

# Check if git is installed
if ! command -v git &> /dev/null; then
    echo "Error: Git is not installed. Please install git first."
    exit 1
fi

echo "Setting up Git repository..."

# Initialize git if needed
if [ ! -d ".git" ]; then
    git init
    echo "✓ Git repository initialized"
else
    echo "✓ Git repository already initialized"
fi

# Add remote origin
if git remote | grep -q "origin"; then
    echo "✓ Remote 'origin' already exists"
    echo "  Current URL: $(git remote get-url origin)"
    read -p "Do you want to update it to https://github.com/DennisPoniros/rOFI_package.git? (y/n) " -n 1 -r
    echo
    if [[ $REPLY =~ ^[Yy]$ ]]; then
        git remote set-url origin https://github.com/DennisPoniros/rOFI_package.git
        echo "✓ Remote URL updated"
    fi
else
    git remote add origin https://github.com/DennisPoniros/rOFI_package.git
    echo "✓ Remote 'origin' added"
fi

# Add all files
echo ""
echo "Adding all package files to git..."
git add .
echo "✓ Files staged for commit"

# Show status
echo ""
echo "Current git status:"
git status --short

# Create commit
echo ""
echo "Creating initial commit..."
git commit -m "Initial commit: Complete rOFI package for order-flow imbalance analysis

- Core functions: compute_ofi(), plot_ofi(), simulate_orders(), as_ofi()
- Comprehensive documentation with roxygen2
- Full test suite with ~100 unit tests
- Vignette with educational examples
- GitHub Actions for CI/CD
- Ready for STAT 611 Honors Option presentation"

echo "✓ Commit created"

# Set branch to main
git branch -M main
echo "✓ Branch set to 'main'"

# Push to GitHub
echo ""
echo "Ready to push to GitHub!"
echo "Repository: https://github.com/DennisPoniros/rOFI_package"
echo ""
read -p "Do you want to push now? (y/n) " -n 1 -r
echo

if [[ $REPLY =~ ^[Yy]$ ]]; then
    echo "Pushing to GitHub..."
    git push -u origin main
    
    if [ $? -eq 0 ]; then
        echo ""
        echo "==================================="
        echo "✓ SUCCESS! Package pushed to GitHub"
        echo "==================================="
        echo ""
        echo "Your package is now available at:"
        echo "https://github.com/DennisPoniros/rOFI_package"
        echo ""
        echo "Others can install it with:"
        echo "devtools::install_github('DennisPoniros/rOFI_package')"
        echo ""
        echo "Next steps:"
        echo "1. Check GitHub Actions at: https://github.com/DennisPoniros/rOFI_package/actions"
        echo "2. Add repository description and topics in Settings"
        echo "3. Create a release if desired"
    else
        echo ""
        echo "Push failed. Please check your GitHub credentials and try:"
        echo "git push -u origin main"
    fi
else
    echo ""
    echo "Setup complete! When ready, push with:"
    echo "git push -u origin main"
fi
