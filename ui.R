library(shiny)
library(shinythemes)
library(markdown)

shinyUI(
    fluidPage(
      tags$head(
        tags$style(HTML("

     .affix {
        position:fixed;
      }

    "))
      ),
      theme=shinytheme("sandstone"),
             
      fluidRow(
        column(8,
                 p(img(src="LU_Ekonomihögskolan_RGB_SV.png",height="30%",width="30%"),
                    HTML("<hr color='blue' >") 
                  )
        ),
        column(3, p("Klicka här för att komma till kursen och läsa mer. "))
      ),
      titlePanel("Ska du köpa eller hyra?"),
#       headerPanel("Ska du köpa eller hyra?"),
      

    # huvudpanel
    fixedRow(column(8, 
                    p("Det är mycket att tänka på när du ska köpa en bostad, 
                      och det kan vara svårt att räkna ut om det är bättre att hyra eller att köpa. 
                      För att hjälpa dig med det beslutet så tar verktyget nedan in de viktigaste kostnaderna 
                      för att köpa och beräknar en motsvarande hyra. 
                      Om du kan hitta en bostad för en lägre hyra än det så är det bättre att hyra." 
                    , HTML("<hr color='blue' >")),
                    htmlOutput("Text_pris"),
                    br(),            
                    
                    fluidRow(column(2,
                                    sliderInput("boxPris", "Bostadspris"
                                                , value = 1000000, min=0, max=10000000, step=10000, ticks=FALSE, width="95%", post  = " kr")
                                    ),
                             column(10,
                                    plotOutput("plotPris", height="300px", width= "100%")
                             )
                    ),
                    
                    br(),            
                    HTML("<hr color='blue' >"),
                    htmlOutput("text_tid"),
                    br(),            
                    
                    sliderInput("boxTid", "Tid i bostaden"
                                , value = 10, min=1, max=50, step=1, ticks=FALSE, width="95%", post  = " år"),
                    htmlOutput("text_bolån"),
                    
                    sliderInput("boxR", "Bolåneränta.", value = 3, min=0, max=15, step=0.1, ticks=FALSE, width="95%", post  = " %"),
                    # sliderInput("boxRtopp", "Ränta på eventuellt topplån.", value = 5, min=0, max=15, step=0.1, ticks=FALSE, width="95%", post  = " %"),
                    sliderInput("boxKI", "Kontantinsats. ", value = 15, min=15, max=100, step=1, ticks=FALSE, width="95%", post  = " %"),
                    htmlOutput("text_belåningsgrad"),
                    htmlOutput("text_amortering"),
                    sliderInput("boxInc", "Bruttoinkomst", value = 200000, min=0, max=2000000, step=1000, ticks=FALSE, width="95%", post  = " kr"),
                      conditionalPanel(
                      condition = "input.boxKI <30",
                      sliderInput("BoxAmort", "Amortering", value = 2, min=2, max=100, step=1, ticks=FALSE, width="95%", post  = " %")
                    ),
                    conditionalPanel(
                      condition = "input.boxKI <50 & input.boxKI>=30 ",
                      sliderInput("BoxAmort", "Amortering", value = 1, min=1, max=100, step=1, ticks=FALSE, width="95%", post  = " %")
                    ),
                    conditionalPanel(
                      condition = "input.boxKI >=50",
                      sliderInput("BoxAmort", "Amortering", value = 0, min=0, max=100, step=1, ticks=FALSE, width="95%", post  = " %")
                    ),
                    # sliderInput("BoxAmort", "Amortering", value = 2, min=0, max=100, step=1, ticks=FALSE, width="95%", post  = " %"),
                    htmlOutput("text_framtiden"),
                    checkboxInput("framtiden", "Välj värden själv"),
                    conditionalPanel(
                      condition = "input.framtiden == true",
                        sliderInput("boxDeltaP", "Husprisökning", value = 4, min=0, max=100, step=1, ticks=FALSE, width="95%", post  = " %"),
                        sliderInput("boxDeltaRent", "Hyresökning", value = 2, min=0, max=100, step=1, ticks=FALSE, width="95%", post  = " %"),
                        sliderInput("boxDeltaSM", "Avkastning på investeringar", value = 7, min=0, max=100, step=1, ticks=FALSE, width="95%", post  = " %"),
                        sliderInput("boxInflation", "Inflation", value = 2, min=0, max=100, step=1, ticks=FALSE, width="95%", post  = " %")
                    ),
                    htmlOutput("text_renoveringar"),
                    sliderInput("boxAvgift", "Avgift till förening", value = 2000, min=0, max=15000, step=100, ticks=FALSE, width="95%", post  = " kr"),
                    sliderInput("boxReno", "Renoveringar", value = 1, min=0, max= 15, step=0.1, ticks=FALSE, width="95%", post  = " %"),
                    sliderInput("boxFörsäkring", "Försäkring", value = 1000, min=0,  max= 100000,step=100, ticks=FALSE, width="95%", post  = " kr"),
                    sliderInput("boxAndraKöpa", "Andra kostnader", value = 1000, min=0,  max= 100000, step=100, ticks=FALSE, width="95%", post  = " kr"),
                    sliderInput("boxAndraHyra", "Andra kostnader för att hyra", value = 10000, min=0,  max= 100000, step=100, ticks=FALSE, width="95%", post  = " kr")
                    
                    # 
                    # checkboxInput("smooth", "Smooth"),
                    # conditionalPanel(
                    #   condition = "input.smooth == true",
                    #   selectInput("smoothMethod", "Method",
                    #               list("lm", "glm", "gam", "loess", "rlm"))
                    # )
                    #sliderInput("boxHyra", "Hyra", value = 1000, min=0,  max= 100000, step=100, ticks=FALSE, width="95%")
                    ), #  column
             
             # column(5,
             #        plotOutput("plot1", width= "75%")
             #        ),
             column(1,  fixedPanel(top = "100", width = "45%",
                    htmlOutput("sumkostnader"),
                    textOutput("intiala_köpa"),
                    textOutput("återkommande"),
                    textOutput("alternativkostnader"),
                    textOutput("vinster"),
                    textOutput("totala_kostnader_köpa"), 
                    br(),
                    textOutput("totala_kostnader_hyra")
                   ))
        ) # fluidrow 
  ) # fluidpage 
    
) # ShinyUI 
  
