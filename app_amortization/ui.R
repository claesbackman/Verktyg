shinyUI(
fluidPage(
  theme=shinytheme("sandstone"),

  titlePanel("Amortering: Effekter på köp/hyra och förmögenhet"),

  sidebarLayout(
    # Sidebar with all inputs
    sidebarPanel(
      width = 3,

      h4("Bostad och lån"),
      sliderInput("pris", "Bostadspris",
                  value = 1000000, min = 500000, max = 5000000,
                  step = 100000, post = " kr"),

      sliderInput("ki", "Kontantinsats",
                  value = 15, min = 15, max = 100,
                  step = 5, post = " %"),

      sliderInput("ranta", "Bolåneränta",
                  value = 3, min = 0, max = 10,
                  step = 0.5, post = " %"),

      hr(),

      h4("Tidshorisont och tillväxt"),
      sliderInput("tid", "Tid i bostaden",
                  value = 10, min = 5, max = 30,
                  step = 1, post = " år"),

      sliderInput("huspris", "Årlig husprisökning",
                  value = 3, min = 0, max = 10,
                  step = 0.5, post = " %"),

      sliderInput("hyresok", "Årlig hyresökning",
                  value = 2, min = 0, max = 10,
                  step = 0.5, post = " %"),

      sliderInput("avkastning", "Investeringsavkastning",
                  value = 7, min = 0, max = 15,
                  step = 0.5, post = " %"),

      hr(),

      h4("Kostnader"),
      sliderInput("avgift", "Månadsavgift",
                  value = 2000, min = 0, max = 10000,
                  step = 500, post = " kr"),

      sliderInput("forsakring", "Försäkring",
                  value = 1000, min = 0, max = 5000,
                  step = 100, post = " kr"),

      sliderInput("andra", "Andra månadskostnader",
                  value = 1000, min = 0, max = 5000,
                  step = 100, post = " kr"),

      sliderInput("renovering", "Renovering (% av pris/år)",
                  value = 1, min = 0, max = 5,
                  step = 0.5, post = " %"),

      sliderInput("flytt", "Flyttkostnader",
                  value = 10000, min = 0, max = 50000,
                  step = 5000, post = " kr"),

      hr(),

      h4("Hyresparametrar"),
      sliderInput("andraHyra", "Andra hyreskostnader/månad",
                  value = 1000, min = 0, max = 5000,
                  step = 100, post = " kr"),

      sliderInput("deposition", "Deposition",
                  value = 1, min = 0, max = 6,
                  step = 1, post = " månader")
    ),

    # Main panel with outputs
    mainPanel(
      width = 9,

      tabsetPanel(
        # Tab 1: Amortering och motsvarande hyra
        tabPanel("Amortering vs Hyra",
          br(),
          h3("Hur påverkar amorteringen motsvarande hyra?"),
          p("Detta diagram visar vilken månadshyra som skulle ge samma totalkostnad som att köpa, för olika amorteringsnivåer."),
          plotOutput("plotAmortHyra", height = "400px"),
          br(),
          h4("Tolkning:"),
          htmlOutput("tolkningHyra")
        ),

        # Tab 2: Amortering och förmögenhet
        tabPanel("Amortering vs Förmögenhet",
          br(),
          h3("Hur påverkar amorteringen din slutliga förmögenhet?"),
          p("Detta diagram jämför din förmögenhet efter den valda perioden vid olika amorteringsnivåer."),
          plotOutput("plotAmortWealth", height = "400px"),
          br(),
          h4("Uppdelning:"),
          p("Förmögenheten består av:"),
          tags$ul(
            tags$li(strong("Köpa:"), "Husvärde efter skatt + investerat kapital - kvarvarande skuld"),
            tags$li(strong("Hyra:"), "Investerat kapital (kontantinsats + sparade betalningar)")
          ),
          htmlOutput("tolkningWealth")
        ),

        # Tab 3: Detaljerad jämförelse
        tabPanel("Detaljerad jämförelse",
          br(),
          h3("Välj specifik amorteringsnivå för analys"),
          sliderInput("amortVald", "Amortering att analysera",
                      value = 2, min = 0, max = 10,
                      step = 0.5, post = " %"),
          br(),

          fluidRow(
            column(6,
              h4("Kostnader för att köpa"),
              tableOutput("tableKopa")
            ),
            column(6,
              h4("Kostnader för att hyra"),
              p(strong("Motsvarande månadshyra:"),
                textOutput("motsvarandeHyra", inline = TRUE), " kr"),
              br(),
              tableOutput("tableHyra")
            )
          ),

          br(),
          h4("Förmögenhetsjämförelse efter", textOutput("tidText", inline = TRUE), "år"),
          tableOutput("tableWealth")
        ),

        # Tab 4: Information
        tabPanel("Information",
          br(),
          h3("Om denna kalkylator"),
          p("Denna app hjälper dig utforska hur amorteringsnivån påverkar:"),
          tags$ol(
            tags$li(strong("Motsvarande hyra:"), "Vilken månadshyra som gör köp och hyra ekonomiskt likvärdiga"),
            tags$li(strong("Slutlig förmögenhet:"), "Hur mycket pengar du har efter den valda perioden")
          ),

          br(),
          h4("Viktiga insikter:"),
          tags$ul(
            tags$li("Högre amortering minskar räntekostnader men ökar alternativkostnader"),
            tags$li("Optimal amortering beror på skillnaden mellan bolåneränta och investeringsavkastning"),
            tags$li("Om investeringsavkastning > ränta (efter skatt): Lägre amortering kan ge högre förmögenhet"),
            tags$li("Om investeringsavkastning < ränta (efter skatt): Högre amortering ger lägre kostnader")
          ),

          br(),
          h4("Antaganden:"),
          tags$ul(
            tags$li("Bolåneränta: 30% skattelättnad på 70% av räntan (effektiv skattereduktion)"),
            tags$li("Reavinstskatt: 22/30 av vinsten beskattas med 30%"),
            tags$li("Renovering minskar skattepliktig revinst"),
            tags$li("Alla belopp i dagens penningvärde (ingen inflation)")
          )
        )
      )
    )
  )
)
)
