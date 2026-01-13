# Project Structure

This document provides an overview of the organized project structure.

## Directory Layout

```
Verktyg/
├── 📱 app/                          # Shiny Application
│   ├── server.R                    # Server-side logic and calculations
│   ├── ui.R                        # User interface definition
│   ├── global.R                    # Global settings and library imports
│   ├── rsconnect/                  # Deployment configuration for shinyapps.io
│   └── www/                        # Static assets (images, CSS, JS)
│
├── 📚 docs/                         # Documentation
│   ├── APP_DESCRIPTION.md          # Detailed application functionality description
│   ├── DEPLOYMENT.md               # Deployment guide (shinyapps.io + GitHub Pages)
│   ├── R_SETUP.md                  # R environment setup instructions
│   ├── TEST_PLAN.md                # Testing strategy and plan
│   ├── SEQUENCE_FIX_GUIDE.md       # Guide for fixing sequence issues
│   └── FINAL_FIX_AND_RESTART.md    # Final fixes documentation
│
├── 🔧 scripts/                      # Utility Scripts
│   ├── test_app.R                  # Application testing script
│   ├── test_hyraFunction.R         # Function-level tests
│   ├── test_r_setup.R              # R setup verification
│   ├── analyze_errors.R            # Error analysis utilities
│   ├── auto_fix_sequences.R        # Automated sequence fixing
│   ├── fix_sequences.R             # Manual sequence fixes
│   ├── start_app.ps1               # PowerShell starter (old structure)
│   ├── start_app_new.ps1           # PowerShell starter (new structure)
│   ├── start_app_nocache.ps1       # Start app without cache
│   ├── run_r.ps1                   # Run R commands
│   ├── run_shiny_app.ps1           # Alternative app starter
│   └── find_and_fix_sequences.ps1  # PowerShell sequence fixer
│
├── 📖 reference/                    # Reference Materials
│   ├── Bolånekalkyl.xlsx           # Mortgage calculation spreadsheet
│   ├── Köpa eller hyra_1.xlsx      # Buy vs rent analysis spreadsheet
│   ├── Formler.docx                # Formula documentation
│   └── Förklaringar/               # Explanatory documents
│
├── 🌐 Web Files (Root Level)
│   ├── index.html                  # GitHub Pages landing page
│   ├── README.md                   # Project README
│   ├── PROJECT_STRUCTURE.md        # This file
│   └── Verktyg.Rproj               # RStudio project file
│
└── ⚙️ Configuration Files
    ├── .gitignore                  # Git ignore rules
    └── .DS_Store                   # macOS folder metadata
```

## File Descriptions

### Application Files (`app/`)

- **server.R**: Contains all server-side logic including:
  - `hyraFunction()` - Main calculation engine
  - Reactive values and tables
  - Plot rendering functions for all 10 graphs
  - Cost breakdown calculations

- **ui.R**: Defines the user interface including:
  - Input sliders and controls
  - Conditional panels
  - Plot outputs
  - Table displays

- **global.R**: Loads required R packages:
  - shiny, ggplot2, tidyverse, scales
  - tableHTML, shinydashboard, shinythemes

### Documentation (`docs/`)

- **APP_DESCRIPTION.md**: Comprehensive description of:
  - Application purpose and functionality
  - Calculation methodology
  - Financial formulas
  - Tax treatment details

- **DEPLOYMENT.md**: Step-by-step guide for:
  - Deploying to shinyapps.io
  - Setting up GitHub Pages
  - Troubleshooting common issues
  - Updating deployed apps

### Scripts (`scripts/`)

Testing and utility scripts for development and debugging:
- Test suites for app and function validation
- Error analysis and fixing tools
- Various startup scripts for different scenarios

### Reference (`reference/`)

Original Excel calculators and documentation that informed the app design:
- Excel-based mortgage and rent vs buy calculators
- Formula documentation in Word format
- Supporting explanatory materials

## Running the Application

### From RStudio

1. Open `Verktyg.Rproj`
2. Navigate to `app/` folder
3. Open `app/ui.R` or `app/server.R`
4. Click "Run App" button

### From Command Line

Using PowerShell:
```powershell
cd scripts
.\start_app_new.ps1
```

Using R:
```r
setwd("app")
shiny::runApp()
```

## Deployment Workflow

### Local Development
```
Edit files in app/ → Test locally → Commit changes
```

### Deploy to Production
```
1. Update app files in app/
2. Navigate to app/ folder
3. Run: rsconnect::deployApp(appName = "Verktyg")
4. Update index.html with new URL (if needed)
5. Push to GitHub
```

## Key Technologies

- **R Shiny**: Web application framework
- **ggplot2**: Data visualization
- **tidyverse**: Data manipulation
- **GitHub Pages**: Static site hosting
- **shinyapps.io**: Shiny app hosting

## Important Notes

### Path References

All paths in documentation and scripts now reference the organized structure:
- App files: `app/server.R`, `app/ui.R`, `app/global.R`
- Documentation: `docs/*.md`
- Scripts: `scripts/*.R`, `scripts/*.ps1`

### Git Ignored Files

The following are excluded from version control:
- `.Rproj.user/` - RStudio user settings
- `.Rhistory` - R command history
- `.RData` - R workspace data
- `*.log` - Log files
- `tmpclaude-*` - Temporary files

### Old Root Files

The original `server.R`, `ui.R`, `global.R` files in root have been:
- Copied to `app/` folder
- Removed from root
- Added to `.gitignore` to prevent re-addition

## Maintenance

### Adding New Features

1. Edit files in `app/`
2. Test locally using `start_app_new.ps1`
3. Document changes in `docs/`
4. Update README.md if needed
5. Deploy to shinyapps.io

### Updating Documentation

1. Edit files in `docs/`
2. Update README.md if structure changes
3. Update index.html if public-facing info changes
4. Commit and push to GitHub

### Testing

Run tests from scripts:
```r
source("scripts/test_app.R")
source("scripts/test_hyraFunction.R")
```

## Links

- **Live App**: https://YOUR-USERNAME.shinyapps.io/Verktyg/
- **GitHub Pages**: https://claesbackman.github.io/Verktyg/
- **Repository**: https://github.com/claesbackman/Verktyg

## Support

For issues or questions:
- Check `docs/DEPLOYMENT.md` for deployment help
- Check `docs/APP_DESCRIPTION.md` for functionality details
- Review test scripts in `scripts/` for debugging examples
