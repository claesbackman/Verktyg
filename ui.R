library(shiny)
library(shinythemes)

shinyUI(
    fluidPage(theme=shinytheme("united"),
      titlePanel("Ska du köpa eller hyra?"),
  
    # huvudpanel
    fluidRow(column(4,
                    textOutput("Text_pris"),
                    sliderInput("boxPris", "Bostadspris"
                                , value = 1000000, min=0, max=10000000, step=10000, ticks=FALSE, width="95%"),  
                    textOutput("text_tid"),
                    sliderInput("boxTid", "Tid i bostaden"
                                , value = 10, min=1, max=50, step=1, ticks=FALSE, width="95%"),
                    textOutput("text_inkomst"),
                    sliderInput("boxInc", "Inkomst", value = 200000, min=0, max=2000000, step=1000, ticks=FALSE, width="95%"),
                    sliderInput("boxR", "Ränta.", value = .03, min=.00, max=0.15, step=.01, ticks=FALSE, width="95%"),
                    sliderInput("boxRtopp", "Ränta på eventuellt topplån.", value = .05, min=.01, max=0.15, step=.01, ticks=FALSE, width="95%"),
                    sliderInput("boxAvgift", "Avgift till förening", value = 2000, min=0, max=15000, step=100, ticks=FALSE, width="95%"),
                    sliderInput("boxKI", "Kontantinsats", value = .15, min=.15, max=1, step=.01, ticks=FALSE, width="95%"),
                    sliderInput("BoxAmort", "Amortering", value = 0.02, min=0.02, max=1, step=.01, ticks=FALSE, width="95%"),
                    sliderInput("boxDeltaP", "Uppskattad husprisökning", value = 0.04, min=0, max=1, step=.01, ticks=FALSE, width="95%"),
                    sliderInput("boxDeltaRent", "Uppskattad hyresökning", value = 0.02, min=0, max=1, step=.01, ticks=FALSE, width="95%"),
                    sliderInput("bpxDeltaSM", "Uppskattad avkastning på investeringar", value = 0.07, min=0, max=1, step=.01, ticks=FALSE, width="95%"),
                    sliderInput("box9", "Uppskattad inflation", value = 0.02, min=0, max=1, step=.01, ticks=FALSE, width="95%"),
                    sliderInput("boxReno", "Renoveringar", value = 0.01, min=0, max= 0.15, step=0.01, ticks=FALSE, width="95%"),
                    sliderInput("boxFörsäkring", "Försäkring", value = 1000, min=0,  max= 100000,step=100, ticks=FALSE, width="95%"),
                    sliderInput("boxAndraKöpa", "Andra kostnader", value = 1000, min=0,  max= 100000, step=100, ticks=FALSE, width="95%"),
                    sliderInput("box13", "Andra kostnader för att hyra", value = 1000, min=0,  max= 100000, step=100, ticks=FALSE, width="95%"),
                    sliderInput("box14", "Försäkring", value = 1000, min=0,  max= 100000, step=100, ticks=FALSE, width="95%")
                    #sliderInput("boxHyra", "Hyra", value = 1000, min=0,  max= 100000, step=100, ticks=FALSE, width="95%")
                    ), #  column
             
             h1("Här summerar vi alla kostnader"),
             column(4, 
                    htmlOutput("sumkostnader"),
                    textOutput("intiala_köpa"),
                    textOutput("återkommande"),
                    textOutput("alternativkostnader"),
                    textOutput("vinster"),
                    textOutput("amorteringar"),
                    textOutput("fasta"),
                   plotOutput("plot1",width= "100%")
             ) # Column 
        ) # fluidrow 
  ) # fluidpage 
    
) # ShinyUI 
  
