library(shiny)
library(shinythemes)
library(markdown)
# library(shinydashboard)


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

    
    
    
    fixedRow(
      column(12,
               p(img(src="LU_Ekonomihögskolan_RGB_SV.png",height="30%",width="30%")
      ))
    ),
      
      # column(3, p("Klicka här för läsa mer om kursen. "))
      # ),

      # column(3, p("Klicka här för läsa mer om kursen. "))
    titlePanel("Ska du köpa eller hyra?"),

  # huvudpanel
  fixedRow(column(8, 
                  p("Det är mycket att tänka på när du ska köpa en bostad, 
                    och det kan vara svårt att räkna ut om det är bättre att hyra eller att köpa. 
                    För att hjälpa dig med det beslutet så tar verktyget nedan in de viktigaste kostnaderna 
                    för att köpa och beräknar en motsvarande hyra. 
                    Om du kan hitta en bostad för en lägre hyra är det bättre att hyra." 
                  , HTML("<hr color='grey' >")),
                  
                  # Pris
                  htmlOutput("Text_pris"),
                  br(),            
                  fluidRow(column(2,
                                  sliderInput("boxPris", "Bostadspris"
                                              , value = 1000000, min=0, max=10000000, step=10000, ticks=FALSE, width="95%", post  = " kr")
                                  ),
                           column(10,
                                  plotOutput("grafPris", height="200px", width= "100%")
                           )
                  ),
                  
                  # tid 
                  br(),            
                  HTML("<hr color='grey' >"),
                  htmlOutput("text_tid"),
                  br(),            
                  fluidRow(column(2,
                                  
                                  sliderInput("boxTid", "Tid i bostaden"
                                              , value = 10, min=1, max=50, step=1, ticks=FALSE, width="95%", post  = " år")
                  ),
                  column(10,
                         plotOutput("grafTid", height="200px", width= "100%")
                  )
                  ),
                  # Ränta 
                  br(),            
                  HTML("<hr color='grey' >"),
                  htmlOutput("text_bolån"),
                  br(),            
                  fluidRow(column(2,
                                  
                                  sliderInput("boxR", "Bolåneränta.", value = 3, min=0, max=15, step=0.1, ticks=FALSE, width="95%", post  = " %")
                  ),
                  column(10,
                         plotOutput("grafRanta", height="200px", width= "100%")
                  )
                  ),
                  htmlOutput("text_belåningsgrad"),
                  br(),            
                  fluidRow(column(2,
                                  
                  sliderInput("boxKI", "Kontantinsats. ", value = 15, min=15, max=100, step=1, ticks=FALSE, width="95%", post  = " %")
                  ),
                  column(10,
                         plotOutput("grafKI", height="200px", width= "100%")
                  )
                  ),
                   # sliderInput("boxRtopp", "Ränta på eventuellt topplån.", value = 5, min=0, max=15, step=0.1, ticks=FALSE, width="95%", post  = " %"),
                  htmlOutput("text_amortering"),
                  fluidRow(column(2,
                                  
                            sliderInput("boxInc", "Bruttoinkomst", value = 200000, min=0, max=2000000, step=1000, ticks=FALSE, width="95%", post  = " kr"),
                            conditionalPanel(
                              condition = "input.boxKI <30",
                              sliderInput("BoxAmort", "Amortering", value = 2, min=2, max=15, step=0.1, ticks=FALSE, width="95%", post  = " %")
                            ),
                            conditionalPanel(
                              condition = "input.boxKI <50 & input.boxKI>=30 ",
                              sliderInput("BoxAmort", "Amortering", value = 1, min=1, max=15, step=0.1, ticks=FALSE, width="95%", post  = " %")
                            ),
                            conditionalPanel(
                              condition = "input.boxKI >=50",
                              sliderInput("BoxAmort", "Amortering", value = 0, min=0, max=15, step=0.1, ticks=FALSE, width="95%", post  = " %")
                            )
                  ),
                  column(10,
                         plotOutput("grafAmort", height="200px", width= "100%")
                  )
                  ),
                    
                  # sliderInput("BoxAmort", "Amortering", value = 2, min=0, max=100, step=1, ticks=FALSE, width="95%", post  = " %"),
                  htmlOutput("text_framtiden"),
                  checkboxInput("framtiden", "Välj värden själv"),
                  conditionalPanel(
                    condition = "input.framtiden == true",
                    fluidRow(column(2,
                                    
                          sliderInput("boxDeltaP", "Husprisökning",
                                        value = 4, min=0, max=10, step=1, ticks=FALSE, width="95%", post  = " %")
                    ),
                    column(10,
                           plotOutput("grafDeltaP", height="200px", width= "100%")
                    )
                    ),
                    fluidRow(column(2,
                                    sliderInput("boxDeltaRent", "Hyresökning", value = 2, min=0, max=10, step=1, ticks=FALSE, width="95%", post  = " %")
                                    
                    ),
                    column(10,
                           plotOutput("grafDeltaHyra", height="200px", width= "100%")
                    )
                    ),
                    fluidRow(column(2,
                        sliderInput("boxDeltaSM", "Avkastning på investeringar", value = 7, min=0, max=15, step=1, ticks=FALSE, width="95%", post  = " %")
                    ),
                    column(10,
                           plotOutput("grafDeltaSM", height="200px", width= "100%")
                    )
                    )
                      # sliderInput("boxInflation", "Inflation", value = 2, min=0, max=100, step=1, ticks=FALSE, width="95%", post  = " %")
                  ),
                  htmlOutput("text_renoveringar"),
                  sliderInput("boxAvgift", "Avgift till förening", value = 2000, min=0, max=15000, step=100, ticks=FALSE, width="95%", post  = " kr"),
                  sliderInput("boxFörsäkring", "Försäkring", value = 1000, min=0,  max= 100000,step=100, ticks=FALSE, width="95%", post  = " kr"),
                  sliderInput("boxAndraKöpa", "Andra kostnader", value = 1000, min=0,  max= 100000, step=100, ticks=FALSE, width="95%", post  = " kr"),
                  sliderInput("boxReno", "Renoveringar", value = 1, min=0, max= 15, step=0.1, ticks=FALSE, width="95%", post  = " %"),
                  
                  htmlOutput("text_hyra"),
                  sliderInput("boxAndraHyra", "Andra kostnader för att hyra", value = 10000, min=0,  max= 100000, step=100, ticks=FALSE, width="95%", post  = " kr")
                  
                  # checkboxInput("smooth", "Smooth"),
                  # conditionalPanel(
                  #   condition = "input.smooth == true",
                  #   selectInput("smoothMethod", "Method",
                  #               list("lm", "glm", "gam", "loess", "rlm"))
                  # )
                  #sliderInput("boxHyra", "Hyra", value = 1000, min=0,  max= 100000, step=100, ticks=FALSE, width="95%")
                  ), #  column
           column(4, fixedPanel(top = "30%",
                  htmlOutput("hyrabättre1"),
                  htmlOutput("värdehyra"),
                  htmlOutput("hyrabättre2"),
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
      ) # fluidrow 
) # fluidpage 
  
) # ShinyUI 

