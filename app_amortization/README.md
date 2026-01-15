# Köpa vs Hyra: Förmögenhetsjämförelse

## Översikt

Denna Shiny-app jämför förmögenhetsutvecklingen mellan att köpa och hyra en bostad. Du anger dina specifika förutsättningar (bostadens pris, hyra, amortering, investeringsavkastning) och får se hur din förmögenhet utvecklas över tid i båda scenarierna.

## Syfte

Applikationen hjälper dig besvara frågan: **Ska jag köpa eller hyra?**

Till skillnad från enkla kalkylatorer som bara jämför månadskostnader, tar denna hänsyn till:
- Förmögenhetsuppbyggnad genom bostadsägande
- Alternativkostnad (vad kontantinsatsen kunde gett i avkastning)
- Investeringsmöjligheter av sparade pengar vid hyra
- Svenska skatteregler (ränteavdrag och reavinstskatt)

## Funktioner

### Flik 1: Förmögenhetsutveckling
- År-för-år graf som visar hur din förmögenhet utvecklas
- Visar båda scenarierna (köpa och hyra) samtidigt
- Identifierar eventuell brytpunkt där scenarierna blir likvärdiga

### Flik 2: Sammanfattning
- Stapeldiagram med slutlig förmögenhet
- Tabell med exakta siffror
- Tydlig vinnare-indikation
- Detaljerad kostnadsuppdelning för båda alternativen

### Flik 3: Information
- Förklaring av beräkningarna
- Svenska skatteregler som används
- Antaganden och begränsningar
- Tips för tolkning av resultat

## Beräkningslogik

### Köpa
Din förmögenhet vid köp består av:
- **Husvärde** efter prisökning över tid
- **Minus kvarvarande skuld** (lån minus amorteringar)
- **Minus reavinstskatt** vid försäljning (22/30 × vinst × 30%)

### Hyra
Din förmögenhet vid hyra består av:
- **Kontantinsats + flyttkostnader** investerade från start
- **Årliga besparingar** (skillnad mellan köp- och hyrkostnader) investerade
- **Avkastning** på investeringarna

Om köpkostnader > hyrkostnader: du sparar och investerar skillnaden
Om köpkostnader < hyrkostnader: du måste ta från dina investeringar

## Användning

### Starta appen
```r
shiny::runApp("app_amortization")
```

### Justera parametrar

**Bostad och lån:**
- Bostadspris
- Kontantinsats (%)
- Bolåneränta
- Amortering (% av skuld per år)

**Hyra:**
- Månadshyra
- Andra hyreskostnader

**Tidshorisont och tillväxt:**
- Tid i bostaden (år)
- Årlig husprisökning
- Årlig hyresökning

**Investeringar:**
- Investeringsavkastning (sätt till 0% om du inte investerar)

**Kostnader vid köp:**
- Månadsavgift (BRF)
- Försäkring
- Andra månadskostnader
- Renovering
- Flyttkostnader

## Svenska skatteregler

- **Ränteavdrag:** 30% skattelättnad på 70% av räntan
- **Reavinstskatt:** 22/30 av vinsten beskattas med 30%
- **Renovering:** Minskar skattepliktig vinst (ökar anskaffningsvärdet)

## Tips för tolkning

1. **Resultatet beror på antaganden** - Små ändringar i husprisökning eller avkastning ger stora skillnader
2. **Historiska genomsnitt** - Aktier har gett ca 7-8% i snitt, men med stor variation
3. **Lokala förhållanden** - Huspriser varierar mycket mellan regioner
4. **Icke-finansiella faktorer** - Trygghet, flexibilitet och livsstil syns inte i siffrorna

## Filer

- `ui.R` - Användargränssnitt
- `server.R` - Server-logik och beräkningar
- `global.R` - Bibliotek och globala inställningar
- `README.md` - Denna fil
