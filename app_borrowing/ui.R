shinyUI(
  fluidPage(
    theme = shinytheme("flatly"),

    tags$head(
      tags$style(HTML("
        .irs-grid { display: none !important; }
        .irs-grid-pol { display: none !important; }
        .irs-grid-text { display: none !important; }

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

        .well {
          background: linear-gradient(180deg, #f8f9fa 0%, #ffffff 100%);
          border: 1px solid #dee2e6;
          border-radius: 8px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.05);
        }

        h4 {
          color: #2E86AB;
          font-weight: 600;
          margin-top: 15px;
          margin-bottom: 15px;
          padding-bottom: 8px;
          border-bottom: 2px solid #e8ecef;
        }

        h2 {
          color: #1a1a2e;
          font-weight: 700;
        }

        .nav-tabs > li > a {
          color: #495057;
          font-weight: 500;
        }
        .nav-tabs > li.active > a {
          color: #2E86AB;
          font-weight: 600;
        }

        .help-block {
          font-size: 12px;
          color: #6c757d;
          font-style: italic;
        }

        hr {
          border-top: 1px solid #e8ecef;
          margin: 20px 0;
        }

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
      "))
    ),

    titlePanel("Låneutrymme: kontantinsats, ränta och amortering"),

    sidebarLayout(
      sidebarPanel(
        width = 3,

        h4("Din kontantinsats"),
        sliderInput("downpayment", "Sparad kontantinsats",
          value = 450000, min = 0, max = 3000000,
          step = 25000, post = " kr"
        ),
        numericInput("downpayment_num", "Skriv belopp (kr)",
          value = 450000, min = 0, max = 3000000, step = 1000
        ),

        h4("Inkomsttak"),
        sliderInput("householdIncome", "Hushållets årsinkomst",
          value = 600000, min = 0, max = 2000000,
          step = 10000, post = " kr"
        ),
        numericInput("householdIncome_num", "Skriv årsinkomst (kr)",
          value = 600000, min = 0, max = 2000000, step = 1000
        ),
        sliderInput("incomeShare", "Max andel av inkomsten",
          value = 30, min = 0, max = 60,
          step = 1, post = " %"
        ),
        numericInput("incomeShare_num", "Skriv andel (%)",
          value = 30, min = 0, max = 60, step = 1
        ),
        htmlOutput("computedCap"),

        hr(),

        h4("Ränta och amortering"),
        sliderInput("interest", "Bolåneränta",
          value = 4, min = 0, max = 10,
          step = 0.25, post = " %"
        ),
        numericInput("interest_num", "Skriv ränta (%)",
          value = 4, min = 0, max = 10, step = 0.25
        ),
        sliderInput("amort", "Amortering per år",
          value = 2, min = 0, max = 5,
          step = 0.25, post = " %"
        ),
        numericInput("amort_num", "Skriv amortering (%)",
          value = 2, min = 0, max = 5, step = 0.25
        ),
        checkboxInput("taxDeduction", "Ränteavdrag (30% på 70% av räntan)", value = TRUE)
      ),

      mainPanel(
        width = 9,

        tabsetPanel(
          tabPanel("Sammanfattning",
            br(),
            fluidRow(
              column(4, htmlOutput("summaryLoan")),
              column(4, htmlOutput("summaryPrice")),
              column(4, htmlOutput("summaryGap"))
            ),
            br(),
            htmlOutput("summaryNote"),
            tableOutput("summaryTable")
          ),

          tabPanel("15% vs 10% kontantinsats",
            br(),
            h3("Hur mycket mer kan du låna om kravet sänks?"),
            p("Resultatet tar hänsyn till både kontantinsatsen och ditt inkomsttak."),
            tableOutput("downpaymentTable"),
            br(),
            htmlOutput("downpaymentInsight")
          ),

          tabPanel("Känslighet",
            br(),
            h3("Så påverkar ränta och amortering ditt maxlån"),
            p("Graferna visar maxlånet när du varierar en parameter i taget."),
            plotOutput("interestPlot", height = "350px"),
            br(),
            plotOutput("amortPlot", height = "350px")
          ),

          tabPanel("Information",
            br(),
            h3("Så räknar vi"),
            tags$ul(
              tags$li("Månadskostnaden består av ränta och amortering."),
              tags$li("Månadstaket baseras på andel av hushållets inkomst."),
              tags$li("Maxlånet beräknas som: L = Månadstak / ((ränta + amortering) / 1200)."),
              tags$li("Om ränteavdrag är påslaget används effektiv ränta = ränta × 0.7."),
              tags$li("Kontantinsatskravet begränsar maxpris: Pris = Kontantinsats / kravet."),
              tags$li("Maxlånet blir det lägsta av inkomsttak och kontantinsatsregeln.")
            )
          )
        )
      )
    )
  )
)
