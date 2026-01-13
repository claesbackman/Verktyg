# Test script for Shiny application
# This checks if all packages are installed and the app can load

cat("Testing Shiny application...\n\n")

# Check if required packages are installed
required_packages <- c("shiny", "ggplot2", "tidyverse", "scales",
                       "tableHTML", "shinydashboard", "rsconnect",
                       "shinythemes", "markdown", "jpeg", "shinyWidgets")

cat("Checking required packages:\n")
missing_packages <- c()

for (pkg in required_packages) {
  if (requireNamespace(pkg, quietly = TRUE)) {
    cat("  ✓", pkg, "\n")
  } else {
    cat("  ✗", pkg, "- MISSING\n")
    missing_packages <- c(missing_packages, pkg)
  }
}

if (length(missing_packages) > 0) {
  cat("\nMissing packages detected. Installing...\n")
  install.packages(missing_packages)
  cat("\nPackages installed. Please run this script again.\n")
  quit(save = "no")
}

cat("\n✓ All required packages are installed!\n\n")

# Try to load the app files
cat("Loading server.R and ui.R...\n")

tryCatch({
  source("server.R", local = TRUE)
  cat("  ✓ server.R loaded successfully\n")
}, error = function(e) {
  cat("  ✗ Error loading server.R:\n")
  cat("   ", e$message, "\n")
  quit(save = "no")
})

tryCatch({
  source("ui.R", local = TRUE)
  cat("  ✓ ui.R loaded successfully\n")
}, error = function(e) {
  cat("  ✗ Error loading ui.R:\n")
  cat("   ", e$message, "\n")
  quit(save = "no")
})

cat("\n✓ All checks passed!\n")
cat("\nYou can now run the app with:\n")
cat("  library(shiny)\n")
cat("  runApp()\n")
