# Simple script to start the Shiny app with full output
$userProfile = $env:USERPROFILE
$rscriptPath = Join-Path $userProfile "AppData\Local\Programs\R\R-4.5.2\bin\x64\Rscript.exe"

Write-Host "Starting Shiny app..." -ForegroundColor Green
Write-Host "Working directory: $(Get-Location)" -ForegroundColor Cyan

& $rscriptPath -e "shiny::runApp('.')"
