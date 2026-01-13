# Running R in Cursor

This guide will help you set up and run R in Cursor.

## Prerequisites

### 1. Install R

If R is not installed on your system:

1. Download R from: https://cran.r-project.org/bin/windows/base/
2. Install R (default location: `C:\Program Files\R\R-4.x.x\`)
3. Add R to your system PATH, or note the installation path

### 2. Install R Extension (Optional but Recommended)

1. Open Cursor
2. Go to Extensions (Ctrl+Shift+X)
3. Search for "R" and install one of these:
   - **R Extension for Visual Studio Code** (by REditorSupport)
   - **R** (by Ikuyadeu)

## Running R in Cursor

### Method 1: Using Terminal

1. Open a terminal in Cursor (`` Ctrl+` `` or Terminal → New Terminal)
2. Use the helper script:
   ```powershell
   .\run_r.ps1
   ```
   Or use the full path directly:
   ```powershell
   & "C:\Users\ClaesBäckman\AppData\Local\Programs\R\R-4.5.2\bin\x64\R.exe"
   ```

### Method 2: Running R Scripts

1. Open any `.R` file (e.g., `test_app.R`)
2. Select code you want to run
3. Use the R extension's "Run Selection/Line" command (usually `Ctrl+Enter`)

### Method 3: Running Your Shiny App

**Easy way - use the helper script:**
```powershell
.\run_shiny_app.ps1
```

**Or manually:**
1. Open a terminal in Cursor
2. Navigate to your project directory (if not already there):
   ```powershell
   cd C:\Github\Verktyg
   ```
3. Start R:
   ```powershell
   & "C:\Users\ClaesBäckman\AppData\Local\Programs\R\R-4.5.2\bin\x64\R.exe"
   ```
4. In the R console, run:
   ```r
   library(shiny)
   runApp()
   ```

Or run directly from terminal:
```powershell
& "C:\Users\ClaesBäckman\AppData\Local\Programs\R\R-4.5.2\bin\x64\Rscript.exe" -e "library(shiny); runApp()"
```

## Your R Installation

Your R is installed at:
```
C:\Users\ClaesBäckman\AppData\Local\Programs\R\R-4.5.2\bin\x64
```

This path is already configured in `.vscode/settings.json`.

## Testing Your Setup

Run this command to test if R is accessible:

```powershell
.\run_r.ps1 --version
```

Or test with a simple R script:

```powershell
.\run_r.ps1 -e "cat('R is working!\n'); print(R.version.string)"
```

Or use the test script:

```powershell
& "C:\Users\ClaesBäckman\AppData\Local\Programs\R\R-4.5.2\bin\x64\Rscript.exe" test_r_setup.R
```

## Troubleshooting

### R not found
- Check if R is installed
- Verify the path in `.vscode/settings.json`
- Add R to your system PATH environment variable

### Packages missing
- Run `test_app.R` to check and install missing packages:
  ```powershell
  Rscript test_app.R
  ```

### Shiny app won't start
- Make sure all required packages are installed (see `global.R`)
- Check that you're in the correct directory
- Review `server.R` and `ui.R` for syntax errors
