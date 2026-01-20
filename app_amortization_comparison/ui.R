shinyUI(
fluidPage(
  theme = shinytheme("flatly"),

  # Custom CSS for beautiful styling
  tags$head(
    tags$style(HTML("
      /* Remove slider ticks */
      .irs-grid { display: none !important; }
      .irs-grid-pol { display: none !important; }
      .irs-grid-text { display: none !important; }

      /* Modern slider styling */
      .irs--shiny .irs-bar {
        background: linear-gradient(to right, #2E86AB, #1B5E7E);
        border-top: none;
        border-bottom: none;
      }
      .irs--shiny .irs-single {
        background: #2E86AB;
        border-radius: 4px;
      }
      .irs--shiny .irs-handle {
        border: 2px solid #2E86AB;
        background: white;
        box-shadow: 0 2px 4px rgba(0,0,0,0.1);
      }
      .irs--shiny .irs-line {
        background: #e8ecef;
        border-radius: 4px;
      }
      .irs--shiny .irs-min, .irs--shiny .irs-max {
        color: #6c757d;
        font-size: 11px;
      }

      /* Panel styling */
      .well {
        background: linear-gradient(180deg, #f8f9fa 0%, #ffffff 100%);
        border: 1px solid #dee2e6;
        border-radius: 8px;
        box-shadow: 0 2px 8px rgba(0,0,0,0.05);
      }

      /* Section headers */
      h4 {
        color: #2E86AB;
        font-weight: 600;
        margin-top: 15px;
        margin-bottom: 15px;
        padding-bottom: 8px;
        border-bottom: 2px solid #e8ecef;
      }

      /* Title styling */
      h2 {
        color: #1a1a2e;
        font-weight: 700;
      }

      /* Tab styling */
      .nav-tabs > li > a {
        color: #495057;
        font-weight: 500;
      }
      .nav-tabs > li.active > a {
        color: #2E86AB;
        font-weight: 600;
      }

      /* Help text */
      .help-block {
        font-size: 12px;
        color: #6c757d;
        font-style: italic;
      }

      /* Horizontal rule */
      hr {
        border-top: 1px solid #e8ecef;
        margin: 20px 0;
      }

      /* Table styling */
      .table {
        border-radius: 6px;
        overflow: hidden;
      }
      .table-striped > tbody > tr:nth-of-type(odd) {
        background-color: #f8f9fa;
      }

      /* Summary card styling */
      .summary-card {
        background: #ffffff;
        border-radius: 10px;
        padding: 18px;
        border: 1px solid #e8ecef;
        box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        margin-bottom: 15px;
      }
      .summary-card h5 {
        color: #6c757d;
        margin: 0 0 8px 0;
        font-size: 13px;
        text-transform: uppercase;
        letter-spacing: 0.04em;
      }
      .summary-card .value {
        font-size: 22px;
        font-weight: 700;
        color: #1a1a2e;
      }
      .summary-card .value.scenario-a {
        color: #2E86AB;
      }
      .summary-card .value.scenario-b {
        color: #D4763A;
      }
      .summary-card .value.positive {
        color: #28a745;
      }
      .summary-card .value.negative {
        color: #dc3545;
      }

      /* Scenario labels */
      .scenario-label-a {
        display: inline-block;
        padding: 2px 8px;
        background: #2E86AB;
        color: white;
        border-radius: 4px;
        font-size: 12px;
        font-weight: 600;
      }
      .scenario-label-b {
        display: inline-block;
        padding: 2px 8px;
        background: #D4763A;
        color: white;
        border-radius: 4px;
        font-size: 12px;
        font-weight: 600;
      }
    "))
  ),

  titlePanel("Amorteringsjämförelse: Förmögenhetsuppbyggnad"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Lån"),
      sliderInput("lan", "Lånebelopp",
                  value = 3000000, min = 500000, max = 10000000,
                  step = 100000, post = " kr"),

      sliderInput("ranta", "Bolåneränta",
                  value = 3, min = 0, max = 10,
                  step = 0.25, post = " %"),

      hr(),

      h4("Amorteringsgrader"),
      sliderInput("amort_a", "Scenario A: Amortering",
                  value = 2, min = 0, max = 10,
                  step = 0.5, post = " %"),
      helpText("Lägre amortering = mer pengar kvar att investera"),

      sliderInput("amort_b", "Scenario B: Amortering",
                  value = 4, min = 0, max = 10,
                  step = 0.5, post = " %"),
      helpText("Högre amortering = snabbare skuldminskning"),

      hr(),

      h4("Investeringar"),
      sliderInput("avkastning", "Förväntad aktieavkastning",
                  value = 7, min = 0, max = 15,
                  step = 0.5, post = " %"),
      helpText("Historiskt snitt: ca 7-8% per år"),

      hr(),

      h4("Tidshorisont"),
      sliderInput("tid", "Tid",
                  value = 15, min = 1, max = 30,
                  step = 1, post = " år"),

      hr(),

      h4("Skatteavdrag"),
      checkboxInput("skatteavdrag", "Inkludera ränteavdrag (30%)", value = TRUE),
      helpText("30% avdrag på 70% av räntan enligt svenska regler")
    ),

    mainPanel(
      width = 9,

      tabsetPanel(
        # Tab 1: Wealth over time
        tabPanel("Förmögenhetsutveckling",
          br(),
          h3("Total förmögenhet över tid"),
          p("Jämför hur din totala förmögenhet utvecklas med olika amorteringsgrader."),
          tags$ul(
            tags$li(strong("Scenario A:"), " Lägre amortering + investerar skillnaden på börsen"),
            tags$li(strong("Scenario B:"), " Högre amortering + investerar räntebesparingen på börsen")
          ),
          plotOutput("plotWealthOverTime", height = "400px"),
          br(),
          htmlOutput("summaryCards"),
          br(),
          htmlOutput("tolkningWealth")
        ),

        # Tab 2: Components breakdown
        tabPanel("Uppdelning",
          br(),
          h3("Förmögenhetens komponenter"),
          p("Se hur förmögenheten byggs upp i respektive scenario efter ", textOutput("tidText", inline = TRUE), " år."),

          fluidRow(
            column(6,
              h4("Scenario A", style = "color: #2E86AB; border-bottom-color: #2E86AB;"),
              plotOutput("plotBreakdownA", height = "350px"),
              tableOutput("tableBreakdownA")
            ),
            column(6,
              h4("Scenario B", style = "color: #D4763A; border-bottom-color: #D4763A;"),
              plotOutput("plotBreakdownB", height = "350px"),
              tableOutput("tableBreakdownB")
            )
          ),

          hr(),

          h4("Förklaring av komponenter"),
          h5("Scenario A (lägre amortering)"),
          tags$ul(
            tags$li(strong("Amortering (eget kapital):"), "Den del av lånet du har betalat av."),
            tags$li(strong("Investerat belopp:"), "Skillnaden i amortering som istället investeras på börsen."),
            tags$li(strong("Avkastning på investering:"), "Ränta-på-ränta effekten av investerade pengar.")
          ),
          h5("Scenario B (högre amortering)"),
          tags$ul(
            tags$li(strong("Amortering (eget kapital):"), "Den del av lånet du har betalat av - mer än i Scenario A."),
            tags$li(strong("Räntebesparing (investerad):"), "Eftersom skulden minskar snabbare betalar du mindre ränta. Denna besparing investeras."),
            tags$li(strong("Avkastning på räntebesparing:"), "Ränta-på-ränta effekten av investerade räntebesparingar.")
          )
        ),

        # Tab 3: Cash flow analysis
        tabPanel("Kassaflöde",
          br(),
          h3("Månatligt kassaflöde"),
          p("Se hur de månatliga kostnaderna skiljer sig mellan scenarierna."),

          plotOutput("plotCashFlow", height = "350px"),
          br(),
          tableOutput("tableCashFlow"),

          hr(),

          h4("Ackumulerade räntekostnader över tid"),
          plotOutput("plotInterestPaid", height = "350px"),
          br(),
          htmlOutput("interestSummary")
        ),

        # Tab 4: Sensitivity analysis
        tabPanel("Känslighetsanalys",
          br(),
          h3("Hur påverkar avkastningen resultatet?"),
          p("Se hur den förväntade aktieavkastningen påverkar vilken strategi som är mest fördelaktig."),

          plotOutput("plotSensitivity", height = "400px"),
          br(),
          htmlOutput("sensitivityText"),

          hr(),

          h3("Brytpunkt"),
          htmlOutput("breakEvenText")
        ),

        # Tab 5: Information
        tabPanel("Information",
          br(),
          h3("Om denna kalkylator"),
          p("Denna kalkylator jämför två amorteringsstrategier och visar vilken som ger mest förmögenhet över tid."),

          h4("Den grundläggande frågan"),
          p("Ska du amortera mer på bolånet eller investera pengarna på börsen istället?"),
          p("Svaret beror på förhållandet mellan din ", strong("bolåneränta"), " och ", strong("förväntad aktieavkastning"), "."),

          h4("Hur beräkningen fungerar"),
          p("Båda scenarierna antar att du har samma totala budget. Skillnaden är hur du använder pengarna:"),

          h5("Scenario A (lägre amortering)"),
          tags$ul(
            tags$li(strong("Amortering:"), " Du bygger eget kapital genom att betala av lånet."),
            tags$li(strong("Investering:"), " Skillnaden mellan A och B:s amortering investeras på börsen."),
            tags$li(strong("Avkastning:"), " Investeringarna växer med den valda aktieavkastningen.")
          ),

          h5("Scenario B (högre amortering)"),
          tags$ul(
            tags$li(strong("Amortering:"), " Du bygger mer eget kapital genom högre avbetalningar."),
            tags$li(strong("Räntebesparing:"), " Eftersom skulden minskar snabbare betalar du mindre ränta än i Scenario A. ",
                    "Denna besparing investeras på börsen."),
            tags$li(strong("Avkastning:"), " Räntebesparingarna växer med den valda aktieavkastningen.")
          ),

          h4("Varför investeras räntebesparingen?"),
          p("För att göra en rättvis jämförelse måste båda scenarierna göra samma sak med sina 'extra' pengar:"),
          tags$ul(
            tags$li("Scenario A har extra pengar eftersom de amorterar mindre → investeras"),
            tags$li("Scenario B har extra pengar eftersom de betalar mindre ränta → investeras")
          ),
          p("Om vi inte investerade B:s räntebesparingar skulle jämförelsen bli orättvis."),

          h4("Avkastningen på amortering"),
          p("När du amorterar får du en garanterad 'avkastning' i form av lägre räntekostnader. ",
            "Denna avkastning är lika med din effektiva bolåneränta (efter ränteavdrag)."),
          p("Exempel: Med 3% ränta och ränteavdrag blir effektiv ränta ca 2.1%. ",
            "Varje krona du amorterar 'tjänar' alltså 2.1% per år i sparad ränta."),

          h4("Svenska skatteregler"),
          tags$ul(
            tags$li("Ränteavdrag: 30% skattelättnad på 70% av räntan"),
            tags$li("Investeringar antas göras på ISK (schablonbeskattning inräknad i avkastningen)")
          ),

          h4("Viktiga antaganden"),
          tags$ul(
            tags$li("Räntan är konstant över tid"),
            tags$li("Aktieavkastningen är konstant (i verkligheten varierar den kraftigt)"),
            tags$li("Alla belopp är i dagens penningvärde"),
            tags$li("Ingen hänsyn tas till inflation")
          ),

          h4("Risk"),
          p("Investeringar på börsen innebär risk och kan ge både positiva och negativa utfall."),
          p("Beräkningen tar inte hänsyn till risk, volatilitet eller sannolikheten för olika utfall."),

          h4("Kom ihåg"),
          tags$ul(
            tags$li("Historisk avkastning är ingen garanti för framtida avkastning"),
            tags$li("Börsen kan gå ner kraftigt vissa år - amortering är riskfri"),
            tags$li("Lägre skuld ger större trygghet och flexibilitet vid räntehöjningar eller inkomstbortfall"),
            tags$li("Många värdesätter känslan av att vara skuldfri högt")
          )
        )
      )
    )
  )
)
)
