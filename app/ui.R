shinyUI(
fluidPage(
tags$head(
tags$style(HTML("

    "))
    ),
    theme=shinytheme("sandstone"),
    
        titlePanel("Ska du köpa eller hyra?"),
    
    # huvudpanel
    fixedRow(column(8, 
      p("Det är inte enkelt att jämföra vad det kostar att köpa en bostad med vad det kostar att hyra. 
      För att hjälpa dig bestämma mellan att köpa och att hyra så tar verktyget nedan in de viktigaste kostnaderna 
      för att köpa och beräknar en motsvarande hyra. Om du kan hitta en bostad för en lägre hyra är det bättre att hyra." 
      , HTML("<hr color='grey' >")),
    
    # Pris
    HTML(paste(
    "<h4> Hur mycket kostar bostaden? </h4> En viktig del är hur din nya bostad kostar, men det är inte det enda att tänka på. <p>
    "
    )),
    br(),            
    # HTML("<hr color='grey' >"),         
    fluidRow(
      column(2,
        numericInput("boxPris", "Bostadspris", value = 1000000, min=0, max=10000000, step=1000)
      ),
      column(10,
        plotOutput("grafPris", height="200px", width= "100%")
      )
    ),
    
    # tid 
    br(),            
    HTML("<hr color='grey' >"),
    HTML(paste(
    "<h4> Hur länge planerar du att bo kvar? </h4> Ju längre du planerar att stanna, desto mer sprider du ut de fasta kostnaderna. Men om du ska investera i aktier eller andra tillgångar med hög avkastning blir dina alternativkostnader också högre.  <p>"
    )),                 
    br(),            
    fluidRow(
      column(2,
        numericInput("boxTid", "Tid i bostaden", value = 10, min=1, max=50, step=1)
      ),
      column(10,
        plotOutput("grafTid", height="200px", width= "100%")
      )
    ),
        # Ränta 
    br(),            
    HTML("<hr color='grey' >"),
    HTML(paste(
    "<h4> Hur ser ditt bolån ut?</h4> Din ränta och kontantinsats har stor påverkan på din månadskostnad.<p>"    )),                       
    br(),            
    fluidRow(column(2,
                  numericInput("boxR", "Bolåneränta", value = 3, min=0, max=15, step=0.1)
    ),
    column(10,
         plotOutput("grafRanta", height="200px", width= "100%")
    )
    ),
    br(),            
    HTML("<hr color='grey' >"),
    HTML(paste(
    "<h4> Hur stor är din kontantinsats?</h4> Med en större kontantinsats får du lägre räntebetalningar, men också högre alternativkostnader.<p>"
    )),      
    br(),            
    fluidRow(column(2,
    numericInput("boxKI", "Kontantinsats (%)", value = 15, min=15, max=100, step=1)
    ),
    column(10,
         plotOutput("grafKI", height="200px", width= "100%")
    )
    ),
    # sliderInput("boxRtopp", "Ränta på eventuellt topplån.", value = 5, min=0, max=15, step=0.1, ticks=FALSE, width="95%", post  = " %"),
    br(),            
    HTML("<hr color='grey' >"),
     HTML(paste(
    "<h4> Hur mycket ska du amortera? </h4> Amorteringskravet gör att dina amorteringar beror på din belåningsgrad och 
    din inkomst.<p> Om du väljer att amortera mer än kravet så minskar dina lånekostnad snabbare över tid.<p>"
    )),    
    br(),  
    fluidRow(column(2,
            numericInput("boxInc", "Bruttoinkomst (kr)", value = 200000, min=0, max=2000000, step=1000),
      uiOutput("amortSlider")
    ),
    column(10,
         plotOutput("grafAmort", height="200px", width= "100%")
    )
    ),
    br(),            
    HTML("<hr color='grey' >"),
    # sliderInput("BoxAmort", "Amortering", value = 2, min=0, max=100, step=1, ticks=FALSE, width="95%", post  = " %"),
    HTML(paste(
    "<h4> Vad händer i framtiden? </h4> 
    Framtid ändringar i bostadspriser, hyror och avkastning har stor påverkan ditt beslut, men det är tyvärr
    väldigt svårt att veta vad som kommer hända i framtiden. 
    Vi har fyllt i värden för husprisökningar, hyresökningar, avkastningar på investeringar, och inflation.
    Om du själv vill fylla i värden för framtida huspriser, hyresökningar, avkastning på investeringar 
    och inflation så klicka i boxen nedan. <p>")),
    br(),
    checkboxInput("framtiden", "Välj värden själv"),
    HTML("<hr color='grey' >"), 
    
    conditionalPanel(
    condition = "input.framtiden == true",
    HTML("<hr color='grey' >"), 
    fluidRow(column(2,
                    HTML("<hr color='grey' >"), 
          numericInput("boxDeltaP", "Husprisökning (%)", value = 3, min=0, max=10, step=1)
    ),
    column(10,
           plotOutput("grafDeltaP", height="200px", width= "100%")
    )
    ),
    HTML("<hr color='grey' >"), 
    fluidRow(column(2,
                    numericInput("boxDeltaRent", "Hyresökning (%)", value = 2, min=0, max=10, step=1)
    ),
    column(10,
           plotOutput("grafDeltaHyra", height="200px", width= "100%")
    )
    ),
    HTML("<hr color='grey' >"), 
    
    fluidRow(column(2,
        numericInput("boxDeltaSM", "Avkastning på investeringar (%)", value = 7, min=0, max=15, step=1)
    ),
    column(10,
           plotOutput("grafDeltaSM", height="200px", width= "100%")
    )
    )
    ),
    br(),            
    HTML("<hr color='grey' >"),
    HTML(paste(
      "<h4>Vad har du för andra kostnader? </h4>  Avgifter, försäkringar och andra kostnader har stor påverkan på din månadskostnad. 
      Här kan du fylla i andra kostnader som kommer när du äger din bostad. <p>" 
      )),
    fluidRow(column(2,
                  numericInput("boxAvgift", "Avgift till förening (kr)", value = 2000, min=0, max=15000, step=100),
                  numericInput("boxFörsäkring", "Försäkring (kr)", value = 1000, min=0, max=15000, step=100),
                  numericInput("boxAndraKöpa", "Andra kostnader (kr)", value = 1000, min=0, max=100000, step=100)
    ),
    column(10,
         plotOutput("grafKostnader", height="200px", width= "100%")
    )
    ),
    br(),            
    HTML("<hr color='grey' >"),
    HTML(paste(
      "<h4> Hur mycket ska du renovera? </h4> Löpande renoveringar är en bra idé att ta med i budgeten. 
          Här ökar de månadskostnaden, men de gör även att skatten vid försäljning minskar.  <p>"
    )), 
    fluidRow(column(2,
                  numericInput("boxReno", "Renoveringar (%)", value = 1, min=0, max=20, step=1)
    ),
           column(10,
                  plotOutput("grafReno", height="200px", width= "100%")
           )
    ),
    
    HTML(paste(
      "<h4> Vad kostar det att flytta? </h4> Det kan också kosta pengar att flytta.
          Avgifter för besiktning,  kostnad för att flytta möbler eller liknande engångskostnader påverkar också.<p>  "
    )), 
    numericInput("boxFlytt", "Flyttkostnader (kr)", value = 10000, min=0, max=40000, step=100),
    br(),            
    HTML("<hr color='grey' >"),
    HTML(paste(
      "<h4> Kostnader för att hyra?</h4> Det finns andra kostnader för att hyra, t.ex. 
          en deposition eller en försäkring. <p>"
    )), 
      numericInput("boxAndraHyra", "Andra kostnader per månad för att hyra (kr)", value = 1000, min=0, max=15000, step=100),
    
    numericInput("boxDeposition", "Deposition (mån)", value = 1, min=0, max=12, step=1),
    br()
    ), #  column
    column(4, fixedPanel(top = "30%", # setBackgroundColor("ghostwhite"),
    htmlOutput("hyrabättre1"),
    br(),
    htmlOutput("sumkostnader"),
    br(),
    htmlOutput("filetable")
    # textOutput("initiala_köpa"),
    # textOutput("återkommande"),
    # textOutput("räntekostnader"),
    # textOutput("alternativkostnader"),
    # textOutput("vinster"),
    # textOutput("totala_kostnader_köpa"),
    # br(),
    # textOutput("totala_kostnader_hyra"),
    #  textOutput("månadskostnader_hyra"),
    # br()
    ))
    ), # fluidrow
    br(),
    fluidRow(
    column(12,
           HTML("<hr color='grey' >"), 
           br(),
           
    HTML("Denna sida har designats av <a href='https://www.claesbackman.com/'><font color='1508ff'>Claes Bäckman.</font></a>")
    ), 
    br())
    
    ) # fluidpage 
    
    ) # ShinyUI 
    
