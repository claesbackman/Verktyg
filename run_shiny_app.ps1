# PowerShell script to run your Shiny app
# Usage: .\run_shiny_app.ps1

# Use environment variable to avoid encoding issues
$userProfile = $env:USERPROFILE
$rscriptPath = Join-Path $userProfile "AppData\Local\Programs\R\R-4.5.2\bin\x64\Rscript.exe"

if (-not (Test-Path $rscriptPath)) {
    Write-Host "Error: Rscript.exe not found at $rscriptPath" -ForegroundColor Red
    Write-Host "Please verify your R installation path." -ForegroundColor Yellow
    exit 1
}

Write-Host "Starting Shiny application..." -ForegroundColor Green
Write-Host "Working directory: $(Get-Location)" -ForegroundColor Cyan
Write-Host ""

& $rscriptPath -e "library(shiny); runApp()"
