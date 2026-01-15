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
    "))
  ),

  titlePanel("Köpa vs Hyra: Förmögenhetsjämförelse"),

  sidebarLayout(
    sidebarPanel(
      width = 3,

      h4("Bostad och lån"),
      sliderInput("pris", "Bostadspris",
                  value = 3000000, min = 500000, max = 10000000,
                  step = 100000, post = " kr"),

      sliderInput("ki", "Kontantinsats",
                  value = 15, min = 15, max = 100,
                  step = 5, post = " %"),

      sliderInput("ranta", "Bolåneränta",
                  value = 3, min = 0, max = 10,
                  step = 0.25, post = " %"),

      sliderInput("amort", "Amortering",
                  value = 2, min = 0, max = 10,
                  step = 0.5, post = " %"),

      hr(),

      h4("Hyra"),
      sliderInput("hyra", "Månadshyra",
                  value = 12000, min = 3000, max = 30000,
                  step = 500, post = " kr"),

      sliderInput("andraHyra", "Andra hyreskostnader/månad",
                  value = 500, min = 0, max = 5000,
                  step = 100, post = " kr"),

      hr(),

      h4("Tidshorisont och tillväxt"),
      sliderInput("tid", "Tid i bostaden",
                  value = 10, min = 1, max = 30,
                  step = 1, post = " år"),

      sliderInput("huspris", "Årlig husprisökning",
                  value = 3, min = -5, max = 10,
                  step = 0.5, post = " %"),

      sliderInput("hyresok", "Årlig hyresökning",
                  value = 2, min = 0, max = 10,
                  step = 0.5, post = " %"),

      hr(),

      h4("Investeringar"),
      sliderInput("avkastning", "Investeringsavkastning",
                  value = 7, min = 0, max = 15,
                  step = 0.5, post = " %"),
      helpText("Sätt till 0% om du inte investerar överskott"),

      hr(),

      h4("Kostnader vid köp"),
      sliderInput("avgift", "Månadsavgift (BRF)",
                  value = 3000, min = 0, max = 10000,
                  step = 500, post = " kr"),

      sliderInput("forsakring", "Försäkring/månad",
                  value = 300, min = 0, max = 2000,
                  step = 50, post = " kr"),

      sliderInput("andra", "Andra månadskostnader",
                  value = 500, min = 0, max = 5000,
                  step = 100, post = " kr"),

      sliderInput("renovering", "Renovering (% av pris/år)",
                  value = 1, min = 0, max = 5,
                  step = 0.5, post = " %"),

      sliderInput("flytt", "Flyttkostnader (engång)",
                  value = 30000, min = 0, max = 100000,
                  step = 5000, post = " kr")
    ),

    mainPanel(
      width = 9,

      tabsetPanel(
        # Tab 1: Wealth over time (deterministic)
        tabPanel("Förmögenhetsutveckling",
          br(),
          h3("Förmögenhet över tid: Köpa vs Hyra"),
          p("Diagrammet visar hur din förmögenhet utvecklas över tid beroende på om du köper eller hyr."),
          plotOutput("plotWealthOverTime", height = "450px"),
          br(),
          htmlOutput("tolkningWealth")
        ),

        # Tab 2: Wealth breakdown
        tabPanel("Förmögenhetsuppdelning",
          br(),
          h3("Vad består förmögenheten av?"),
          p("Här ser du de olika komponenterna som bygger upp din förmögenhet i respektive scenario."),

          fluidRow(
            column(6,
              h4("Köpa", style = "color: #D4763A; border-bottom-color: #D4763A;"),
              plotOutput("plotBreakdownBuy", height = "250px"),
              tableOutput("tableBreakdownBuy")
            ),
            column(6,
              h4("Hyra", style = "color: #2E86AB; border-bottom-color: #2E86AB;"),
              plotOutput("plotBreakdownRent", height = "250px"),
              tableOutput("tableBreakdownRent")
            )
          ),

          hr(),

          h4("Förklaring av komponenter"),
          fluidRow(
            column(6,
              h5("Köpa"),
              tags$ul(
                tags$li(strong("Kontantinsats:"), "Ditt initiala eget kapital i bostaden"),
                tags$li(strong("Husprisvinst:"), "Värdestegring minus reavinstskatt"),
                tags$li(strong("Amortering:"), "Eget kapital byggt genom avbetalningar"),
                tags$li(strong("Räntebesparing:"), "Sparad ränta tack vare lägre skuld (investeras)"),
                tags$li(strong("Avkastning räntebesparing:"), "Ränta-på-ränta på sparad ränta"),
                tags$li(strong("Alternativkostnad:"), "Utebliven avkastning om kontantinsatsen investerats")
              )
            ),
            column(6,
              h5("Hyra"),
              tags$ul(
                tags$li(strong("Startkapital:"), "Kontantinsats + flyttkostnader som investeras"),
                tags$li(strong("Avkastning startkapital:"), "Ränta-på-ränta på startkapitalet"),
                tags$li(strong("Sparande:"), "Skillnad mellan köp- och hyrkostnader (kan vara negativ)"),
                tags$li(strong("Avkastning sparande:"), "Ränta-på-ränta på det ackumulerade sparandet")
              )
            )
          )
        ),

        # Tab 3: Summary
        tabPanel("Sammanfattning",
          br(),
          h3("Slutlig förmögenhetsjämförelse"),

          fluidRow(
            column(6,
              plotOutput("plotFinalWealth", height = "350px")
            ),
            column(6,
              br(),
              tableOutput("tableSummary"),
              br(),
              htmlOutput("vinnarText")
            )
          ),

          hr(),

          h4("Detaljerad kostnadsuppdelning"),
          fluidRow(
            column(6,
              h5("Vid köp"),
              tableOutput("tableKopaCosts")
            ),
            column(6,
              h5("Vid hyra"),
              tableOutput("tableHyraCosts")
            )
          )
        ),

        # Tab 4: Information
        tabPanel("Information",
          br(),
          h3("Om denna kalkylator"),
          p("Denna app jämför din förmögenhetsutveckling mellan att köpa och hyra en bostad."),

          h4("Hur det fungerar"),
          tags$ul(
            tags$li(strong("Köpa:"), "Du betalar kontantinsats och lån. Bostaden ökar (förhoppningsvis) i värde. Din förmögenhet = husvärde - skuld - skatt."),
            tags$li(strong("Hyra:"), "Kontantinsatsen investeras istället. Skillnaden mellan kostnader för att köpa och hyra investeras också varje år.")
          ),

          h4("Förmögenhetsuppdelning"),
          p("Uppdelningen visar hur din förmögenhet byggs upp av olika komponenter:"),
          tags$ul(
            tags$li(strong("Vid köp:"), "Kontantinsats + husprisvinst + amortering - alternativkostnad"),
            tags$li(strong("Vid hyra:"), "Startkapital + avkastning + sparande + avkastning på sparande")
          ),

          h4("Svenska skatteregler som används"),
          tags$ul(
            tags$li("Ränteavdrag: 30% skattelättnad på 70% av räntan"),
            tags$li("Reavinstskatt: 22/30 av vinsten beskattas med 30%"),
            tags$li("Renovering minskar skattepliktig vinst (ökar anskaffningsvärdet)")
          ),

          h4("Antaganden"),
          tags$ul(
            tags$li("Alla belopp i dagens penningvärde"),
            tags$li("Räntan är konstant över tid"),
            tags$li("Amortering sker linjärt"),
            tags$li("Husprisökning och investeringsavkastning är konstanta varje år")
          ),

          h4("Tips för tolkning"),
          tags$ul(
            tags$li("Resultatet beror starkt på antaganden om husprisökning och investeringsavkastning"),
            tags$li("Historiskt har aktier gett ca 7-8% avkastning i snitt, men med stor variation"),
            tags$li("Huspriserna varierar mycket mellan år och regioner"),
            tags$li("Tänk på att bostadsköp ger stabilitet och trygghet som inte syns i siffrorna")
          )
        )
      )
    )
  )
)
)
