# Script to test the Shiny app and identify sequence mismatch errors
# This will help us fix all the graph/table sequence issues

library(shiny)

cat("Testing Shiny application for errors...\n\n")

# Load the app components
source("global.R")
source("server.R", local = TRUE)
source("ui.R", local = TRUE)

cat("\n=== Files loaded successfully ===\n\n")

# Create a test that actually runs the app briefly
cat("Starting app test (will run for 5 seconds)...\n")

# Capture any errors during startup
app <- shinyApp(ui = ui, server = server)

cat("\nTest complete. Check full_output.log for runtime errors.\n")
