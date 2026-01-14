# Amorteringsutforskare

## Översikt

Denna Shiny-app hjälper dig utforska hur amorteringsnivån påverkar både kostnaden för att köpa versus hyra och din slutliga förmögenhet.

## Syfte

Applikationen fokuserar på att besvara två nyckelfrågor:

1. **Hur påverkar amortering motsvarande hyra?**
   - Visar vilken månadshyra som gör köp och hyra ekonomiskt likvärdiga
   - Låter dig se hur denna hyra ändras över ett spann av amorteringsnivåer

2. **Hur påverkar amortering din förmögenhet?**
   - Jämför din totala förmögenhet efter vald period vid både köp och hyra
   - Visar optimal amorteringsnivå för maximal förmögenhet

## Funktioner

### Flik 1: Amortering vs Hyra
- Graf som visar motsvarande hyra för olika amorteringsnivåer
- Identifierar vilken amorteringsnivå som ger lägst/högst motsvarande hyra
- Visar skillnaden mellan min och max

### Flik 2: Amortering vs Förmögenhet
- Jämför förmögenhet vid köp vs hyra över amorteringsspektrum
- Två linjer: en för köp, en för hyra
- Identifierar optimal amortering för varje scenario

### Flik 3: Detaljerad jämförelse
- Välj specifik amorteringsnivå
- Se detaljerad kostnadsuppdelning för både köp och hyra
- Jämför slutlig förmögenhet

### Flik 4: Information
- Förklaringar av antaganden
- Viktiga insikter om amortering

## Viktig insikt

**Optimal amortering beror på skillnaden mellan bolåneränta (efter skatt) och investeringsavkastning:**

- **Om investeringsavkastning > effektiv ränta:** Lägre amortering ger högre förmögenhet (investera i stället)
- **Om investeringsavkastning < effektiv ränta:** Högre amortering ger lägre kostnader (amortera aggressivt)

Med svensk skattereduktion (30% på 70% av räntan) blir effektiv ränta: `r × 0.7`

## Användning

### Starta appen
```r
shiny::runApp("app_amortization")
```

### Justera parametrar
1. **Amorteringsspann:** Sätt min, max och steg för amorteringsnivåer att utforska
2. **Bostadsparametrar:** Pris, kontantinsats, ränta
3. **Tillväxtparametrar:** Husprisökning, hyresökning, investeringsavkastning
4. **Kostnader:** Avgifter, försäkring, renovering, etc.

### Tolka resultat
- **Motsvarande hyra:** Om du kan hyra för mindre än detta, är hyra bättre
- **Förmögenhet:** Högre linje = bättre scenario för förmögenhetsbyggande

## Tekniska detaljer

### Beräkningar
- Använder samma `hyraFunction` som huvudappen
- Beräknar förmögenhet som: (tillgångar efter försäljning) - (skuld) för köp
- För hyra: investerat kapital från kontantinsats + sparade betalningar

### Antaganden
- Bolåneränta: 30% skattereduktion på 70% av räntan
- Reavinstskatt: 22/30 av vinsten beskattas med 30%
- Renovering minskar skattepliktig revinst
- Alla belopp i dagens penningvärde

## Filer

- `ui.R` - Användargränssnitt
- `server.R` - Server-logik och beräkningar
- `hyraFunction.R` - Kärnberäkningsfunktion
- `global.R` - Bibliotek och globala inställningar
- `README.md` - Denna fil
