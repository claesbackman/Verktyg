# Start Shiny App from organized structure
# Navigate to app folder and run the Shiny app

$appPath = Join-Path $PSScriptRoot "..\app"

Write-Host "Starting Shiny app from: $appPath" -ForegroundColor Green

# Check if R is available
$rCommand = Get-Command Rscript -ErrorAction SilentlyContinue

if (-not $rCommand) {
    Write-Host "Error: R is not installed or not in PATH" -ForegroundColor Red
    Write-Host "Please install R from https://www.r-project.org/" -ForegroundColor Yellow
    exit 1
}

# Create temporary R script to run the app
$tempScript = @"
# Set working directory to app folder
setwd('$appPath')

# Load required libraries
library(shiny)

# Run the app
runApp()
"@

# Save and run the script
$tempFile = [System.IO.Path]::GetTempFileName() + ".R"
$tempScript | Out-File -FilePath $tempFile -Encoding UTF8

Write-Host "Running Shiny app..." -ForegroundColor Cyan
Rscript $tempFile

# Clean up
Remove-Item $tempFile -ErrorAction SilentlyContinue
