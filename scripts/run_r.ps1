# PowerShell script to run R with the correct path
# Usage: .\run_r.ps1 [R command or script]

# Use environment variable to avoid encoding issues
$userProfile = $env:USERPROFILE
$rPath = Join-Path $userProfile "AppData\Local\Programs\R\R-4.5.2\bin\x64\R.exe"
$rscriptPath = Join-Path $userProfile "AppData\Local\Programs\R\R-4.5.2\bin\x64\Rscript.exe"

if (-not (Test-Path $rPath)) {
    Write-Host "Error: R.exe not found at $rPath" -ForegroundColor Red
    Write-Host "Please verify your R installation path." -ForegroundColor Yellow
    exit 1
}

if ($args.Count -eq 0) {
    # No arguments - start interactive R session
    Write-Host "Starting R interactive session..." -ForegroundColor Green
    & $rPath
} else {
    # Arguments provided - run as Rscript
    Write-Host "Running R script/command..." -ForegroundColor Green
    & $rscriptPath $args
}
