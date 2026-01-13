# Start Shiny app with NO CACHING
$userProfile = $env:USERPROFILE
$rscriptPath = Join-Path $userProfile "AppData\Local\Programs\R\R-4.5.2\bin\x64\Rscript.exe"

Write-Host "Clearing R environment and starting fresh..." -ForegroundColor Green
Write-Host "Working directory: $(Get-Location)" -ForegroundColor Cyan

# Force R to not use bytecode cache
$env:R_COMPILE_PKGS = "0"
$env:R_DISABLE_HTTPD = "1"

# Run with explicit options to disable caching
& $rscriptPath -e "options(shiny.autoreload = TRUE); options(shiny.autoload.r = FALSE); shiny::runApp('.', launch.browser = FALSE)"
