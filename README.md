# Verktyg - Köpa eller Hyra Kalkylator

En omfattande finansiell kalkylator för att hjälpa dig bestämma om det är mer ekonomiskt fördelaktigt att köpa eller hyra en bostad.

## 🚀 Snabbstart

**Live Demo:** [Öppna kalkylatorn](https://verktyg.claesbackman.com/index.html)

**Dokumentation:** [GitHub Pages](https://verktyg.claesbackman.com/documentation.html)

## 📁 Projektstruktur

```
Verktyg/
├── app/                    # Shiny applikationen
│   ├── server.R           # Server-logik
│   ├── ui.R               # Användargränssnitt
│   ├── global.R           # Globala inställningar och bibliotek
│   ├── rsconnect/         # Deployment konfiguration
│   └── www/               # Statiska filer (bilder, CSS, etc.)
│
├── docs/                   # Dokumentation
│   ├── APP_DESCRIPTION.md # Detaljerad beskrivning av appen
│   ├── DEPLOYMENT.md      # Deployment guide
│   ├── R_SETUP.md         # R miljö setup
│   └── ...                # Övrig dokumentation
│
├── scripts/                # Utility scripts
│   ├── test_*.R           # Test scripts
│   ├── start_app.ps1      # PowerShell starter
│   └── ...                # Övriga scripts
│
├── reference/              # Referensmaterial
│   ├── *.xlsx             # Excel-kalkylatorer
│   ├── *.docx             # Formel dokumentation
│   └── Förklaringar/      # Förklarande dokument
│
├── index.html             # GitHub Pages hemsida
├── README.md              # Denna fil
└── Verktyg.Rproj          # RStudio projekt fil
```

## 💻 Kör lokalt

### Förutsättningar

- R (>= 4.0.0)
- RStudio (rekommenderas)
- Följande R-paket:
  ```r
  install.packages(c("shiny", "ggplot2", "tidyverse", "scales",
                     "tableHTML", "shinydashboard", "shinythemes"))
  ```

### Starta applikationen

1. Klona repositoryt:
   ```bash
   git clone https://github.com/claesbackman/Verktyg.git
   cd Verktyg
   ```

2. Öppna `Verktyg.Rproj` i RStudio

3. Navigera till `app/` mappen

4. Öppna `ui.R` eller `server.R`

5. Klicka på "Run App" knappen i RStudio

**Eller använd PowerShell:**
```powershell
cd scripts
.\start_app.ps1
```

## 📊 Vad gör kalkylatorn?

Kalkylatorn beräknar en **"motsvarande hyra"** - den månadshyra som skulle resultera i samma totalkostnad som att köpa en bostad över en viss tidsperiod.

### Huvudfunktioner

- 📈 **10 interaktiva grafer** som visar känslighetsanalys
- 💰 **Komplett kostnadsanalys** inkl. alternativkostnader
- 🇸🇪 **Svenska skatteregler** - ränteavdrag och kapitalvinstskatt
- 🔮 **Framtidsantaganden** - husprisökning, hyresökning, avkastning
- 📱 **Responsiv design** - fungerar på alla enheter

### Kostnader som beaktas

1. **Initiala kostnader**: Kontantinsats, flyttkostnader
2. **Löpande kostnader**: Ränta, avgifter, försäkring, renoveringar
3. **Alternativkostnader**: Investerad avkastning du går miste om
4. **Kapitalvinster**: Husprisökning minus skatt

Se [docs/APP_DESCRIPTION.md](docs/APP_DESCRIPTION.md) för detaljerad information.

## 🚀 Deployment

### Deploy till shinyapps.io

```r
# Installera rsconnect
install.packages("rsconnect")

# Konfigurera ditt konto (se shinyapps.io för credentials)
rsconnect::setAccountInfo(name='YOUR-USERNAME',
                          token='YOUR-TOKEN',
                          secret='YOUR-SECRET')

# Deploy från app/ mappen
setwd("app")
rsconnect::deployApp(appName = "Verktyg")
```

Se [docs/DEPLOYMENT.md](docs/DEPLOYMENT.md) för fullständig guide.

### GitHub Pages

GitHub Pages är redan konfigurerad via `index.html` i root.

1. Uppdatera app-URL i `index.html` (två ställen)
2. Push till GitHub
3. Aktivera GitHub Pages i repository Settings → Pages
4. Välj branch: `master`, folder: `/ (root)`

## 🛠️ Utveckling

### Testa applikationen

```r
# Kör tester
source("scripts/test_app.R")
source("scripts/test_hyraFunction.R")
```

### Project dependencies

Se `app/global.R` för fullständig lista av dependencies.

## 📖 Dokumentation

- **[APP_DESCRIPTION.md](docs/APP_DESCRIPTION.md)** - Detaljerad beskrivning av funktionalitet
- **[DEPLOYMENT.md](docs/DEPLOYMENT.md)** - Deployment guide
- **[R_SETUP.md](docs/R_SETUP.md)** - R miljö konfiguration

## 🤝 Bidra

Förslag och förbättringar välkomnas! Öppna gärna en issue eller pull request.

## 📝 Licens

Detta projekt är öppen källkod för utbildningsändamål.

## 👤 Författare

**Claes Bäckman**
- Hemsida: [www.claesbackman.com](https://www.claesbackman.com/)
- GitHub: [@claesbackman](https://github.com/claesbackman)

## 🙏 Erkännanden

Baserad på finansiell modellering för bostadsbeslut med hänsyn till:
- Alternativkostnadsteori
- Svenska skatteregler
- Långsiktig finansiell planering

---