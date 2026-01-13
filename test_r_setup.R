# Quick test script to verify R is working in Cursor
# Run this with: Rscript test_r_setup.R

cat("=== R Setup Test ===\n\n")

# Test 1: R Version
cat("1. R Version:\n")
cat("   ", R.version.string, "\n\n")

# Test 2: Working Directory
cat("2. Current Working Directory:\n")
cat("   ", getwd(), "\n\n")

# Test 3: Check if project files exist
cat("3. Project Files Check:\n")
files_to_check <- c("server.R", "ui.R", "global.R", "Verktyg.Rproj")
for (file in files_to_check) {
  if (file.exists(file)) {
    cat("   ✓", file, "found\n")
  } else {
    cat("   ✗", file, "NOT found\n")
  }
}

# Test 4: Check required packages
cat("\n4. Required Packages Check:\n")
required_packages <- c("shiny", "ggplot2", "tidyverse", "scales",
                       "tableHTML", "shinydashboard", "rsconnect",
                       "shinythemes", "markdown", "jpeg", "shinyWidgets")

missing_packages <- c()
for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("   ✓", pkg, "\n")
  } else {
    cat("   ✗", pkg, "- MISSING\n")
    missing_packages <- c(missing_packages, pkg)
  }
}

# Test 5: Summary
cat("\n=== Summary ===\n")
if (length(missing_packages) == 0) {
  cat("✓ All checks passed! You're ready to run your Shiny app.\n")
  cat("\nTo run your app, use:\n")
  cat("  library(shiny)\n")
  cat("  runApp()\n")
} else {
  cat("⚠ Some packages are missing. Install them with:\n")
  cat("  install.packages(c(", paste0('"', missing_packages, '"', collapse = ", "), "))\n")
}

cat("\n=== Test Complete ===\n")
