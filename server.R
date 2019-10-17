library(shiny)
library(ggplot2)
library(tidyverse)
shinyServer(function(input, output) {
        
        ################################ 
        # Random 
        ################################ 
        
 
        # uppdatera procent i amorteringar enligt kontantinsats
        
        
        ################################ 
        # Formler 
        ################################
        
        formel_ränta <- reactive({
                tid <- input$boxTid
                pris <- input$boxPris
                ki <- input$boxKI
                amortering <- input$BoxAmort
                ränta <- input$boxR
                pris*(1-ki)
        })

        # initiala kostander köpa 
        formel_intiala_köpa <-  reactive({
                pris <- input$boxPris
                ki <- input$boxKI        
                round(pris*ki)
                })
        
        # Återkommande kostnader
        
        
        # Alternativkostnader
        
        # kontantinsats + amorteringar investerat istället
          # Nu räknas det som att betalningar sparas i slutet av året.
        formel_alternativkostnad <- reactive({
          ki <- input$boxKI *input$boxPris
          skuld <- (1-input$boxKI) *input$boxPris
          r_stocks <- input$bpxDeltaSM
          tid <- input$boxTid
          betalningar <- input$BoxAmort * skuld #+ input$boxReno + input$boxAndraKöpa + input$boxFörsäkring + input$boxAvgift
          round(ki*(1+r_stocks)^tid-ki+(betalningar*(((1 + r_stocks)^(tid) - 1) / (r_stocks)))-betalningar*tid, digit=0)
          })
        
        # Vinster för investeringar
        formel_husprisvinst <-  reactive({
                pris <- input$boxPris
                husprisökning <- input$boxDeltaP     
                round(pris*(1+husprisökning)^input$boxTid-pris, digits=0)
        })
       
        
        # Fasta kostnader
                # Renoveringar
                renoveringar <- reactive({
                      reno <- input$boxReno
                      pris <- input$boxPris
                      round(reno*pris, digits=0)
                })
                
        
                
        # totala amorteringar - amorteringsprocent gånger antal år 
        totamortering <- reactive({
                pris <- input$boxPris
                amortering <- input$BoxAmort
                ki <- input$boxKI
                år <- input$boxTid
                round(pris*amortering*år, digits=2)
                
        })
        
                
        # compound interest formel 
        formula <- reactive({
                pris <- input$boxPris
                tid <- input$boxTid
                ränta <- input$boxR
                ki <- input$boxKI
                round(pris*(1+ränta)^tid, digits=2)
        })
        
        ################################
        # Tabeller 
        ################################
        
        # # Räntekostnader 
        räntaTable <- reactive({
                tid <- input$boxTid
                tid2 <- tid-1
                pris <- input$boxPris
                ki <- input$boxKI
                amortering <- input$BoxAmort
                ränta <- input$boxR*0.7 # ränte gånger 1-skatteavdrag 
                skuld <- pris*(1-ki)
                df  <- skuld*ränta
                for (i in 1:tid) {
                        bal <- skuld*(1-amortering*i )*ränta
                        df <- rbind(df,bal)
                }
                print(df[,1])
                })
                
        räntaSum <- reactive({
                rowSum(räntaTable())
        })        

        # Tabell med vörden 
        # balTable <- reactive({
        #         bal <- input$boxPris
        #         df <- 0
        #         for (i in 1:50) {
        #                 bal <- bal*(1+input$boxR)
        #                 df <- rbind(df, bal-input$boxPris)
        #         }
        #         print(df[,1])
        # })

        ################################ 
        # Grafer
        ################################
                
        # Huvudgrafen- vad kostar det per år? 
        
        
                
        # Compound interest plot
           output$plot1 <- renderPlot({
                   tid <- input$boxTid
                plot(0:tid, räntaTable(), xlab="År", ylab="Räntekostnad")
                # points(tid, formula()-input$boxPris, col = "red", pch = 16, cex = 1.75)
        })

        ################################ 
        # Uppdelade kostnader 
        ################################ 

        # Titeltext
        output$sumkostnader <- renderText({
                paste("<b>Kostnader efter ", input$boxTid, "år </b>")
        })
        # Initiala kostnader köpa 
        output$intiala_köpa <- renderText({
                        paste("Intiala kostnader: ", formel_intiala_köpa(), "kr")
                })
        
        # alternativkostnader 
        output$alternativkostnader <- renderText({
          paste("Alternativkostnader: ", formel_alternativkostnad(), "kr")
        })
        
        # Återkommande kostnader
        output$återkommande <- renderText({
                paste("Återkommande kostnader: ", formel_intiala_köpa(), "kr")
        })
        
        # Vinster
        output$vinster <- renderText({
                paste("Vinster från investeringar: -", formel_husprisvinst(), "kr")
        })
        
        
        output$intiala_köpa <- renderText({
                paste("Intiala kostnader: ", formel_intiala_köpa(), "kr")
        })
        
        ################################ 
        # texter 
        ################################ 
        
        # Räntekostnader 
        output$räntekostnader <- renderText({
                paste("Räntekostnader",formel_ränta(),"kr" )
        })
          
        # amorteringar
        output$amorteringar <- renderText({
                paste("Amorteringar ", totamortering(), "kr ")
        })
        
        # Fasta kostnader 
        output$fasta <- renderText({
                paste("Fasta kostnader ", renoveringar(), "kr ")
        })
        
        # Bostadspris
        output$Text_pris <- renderText({
                "Bostadspris. En väldigt viktig del är hur din nya bostad kostar. 
                Det är inte den enda delen, utan vi behöver titta på fler saker innan vi kan ta ett beslut."
        })
        
        # Tid i bostaden 
        output$text_tid <- renderText({
                "Tid i bostaden. Ju längre du planerar att stanna, desto bättre är det att köpa. 
                              Det är för att fasta kostnader sprids över fler år. "
        })
      
        #inkomst
        output$text_inkomst <- renderText({
                "Om ditt bolån är större än 4.5 gånger din bruttoinkomst
                ska du amortera en procent mer per år. 
                Din bruttoinkomst beräknas genom att XXX..."
        })
        
})




