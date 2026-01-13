# PowerShell script to find and fix all sequence mismatches in server.R
# This identifies where table sequences don't match graph sequences

Write-Host "Analyzing server.R for sequence mismatches..." -ForegroundColor Cyan

$serverFile = "server.R"
$content = Get-Content $serverFile -Raw

# Pattern to find table reactive definitions and their sequences
$tablePattern = '(\w+)\s*<-\s*reactive\(\{[^}]*?seq\s*<-\s*seq\(from=([^,]+),\s*to=([^,]+),\s*by=([^)]+)\)'

# Pattern to find corresponding graph output definitions
$graphPattern = 'output\$graf(\w+)\s*<-\s*renderPlot\(\{[^}]*?seq\s*<-\s*seq\(from=([^,]+),\s*to=([^,]+),\s*by=([^)]+)\)'

Write-Host "`nFound sequences to analyze..." -ForegroundColor Yellow

# Read errors from log
if (Test-Path "full_output.log") {
    $errors = Select-String -Path "full_output.log" -Pattern "arguments imply differing number of rows" -Context 10

    if ($errors) {
        Write-Host "`nFound sequence mismatch errors:" -ForegroundColor Red

        foreach ($error in $errors) {
            $errorLine = $error.Line
            $context = $error.Context.PostContext

            # Extract line number and graph name from error
            $lineMatch = $context | Select-String -Pattern 'renderPlot \[.*:(\d+)\]'
            $graphMatch = $context | Select-String -Pattern 'output\$(\w+)'

            if ($lineMatch -and $graphMatch) {
                $lineNum = $lineMatch.Matches.Groups[1].Value
                $graphName = $graphMatch.Matches.Groups[1].Value

                Write-Host "  - Line $lineNum : $graphName" -ForegroundColor Yellow
            }
        }
    } else {
        Write-Host "`nNo sequence mismatch errors found!" -ForegroundColor Green
    }
} else {
    Write-Host "`nNo log file found. Run the app first to generate errors." -ForegroundColor Yellow
}

Write-Host "`nTo fix these errors, we need to ensure graph sequences match table sequences." -ForegroundColor Cyan
Write-Host "Use the R script to apply fixes automatically." -ForegroundColor Cyan
