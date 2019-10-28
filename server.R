library(shiny)
library(ggplot2)
library(tidyverse)

# definera saker 


shinyServer(function(input, output) {
        
        ################################ 
        # Random 
        ################################ 
  
    # Att göra 
        # uppdatera procent i amorteringar enligt kontantinsats och amorteringskravet
        
        ################################
        # initiala kostnader köpa 
        formel_intiala_köpa <-  reactive({
                pris <- input$boxPris
                ki <- input$boxKI        
                round(pris*ki)
                })
        
        ################################
        # Återkommande kostnader
        ################################
        
        # Räntekostnader 
        räntaTable <- reactive({
          tid <- input$boxTid
          tid2 <- tid-1
          pris <- input$boxPris
          ki <- input$boxKI
          amortering <- input$BoxAmort
          ränta <- input$boxR*0.7 # ränte gånger 1-skatteavdrag 
          skuld <- pris*(1-ki)
          df  <- skuld*ränta
          for (i in 1:tid2) {
            bal <- skuld*(1-amortering*i)*ränta
            df <- rbind(df,bal)
          }
          print(df[,1])
        })
        
        # Totala räntekostnader 
        räntaSum <- reactive({sum(räntaTable())
        })
        
        # renoveringar - räknas med en geometrisk formel. Procent av tidigare års bostadspris
        renoveringar <- reactive({
          renovering <-  input$boxReno * input$boxPris
          tid <- input$boxTid
          ökning <- 1+input$boxDeltaP
          renovering * (1-ökning^tid)/(1-ökning)
        })
        
        # totala återkommande kostnader 
        formel_totala_återkommande <- reactive({
          avgifter <- input$boxAvgift
          försäkring <- input$boxFörsäkring
          tid <- input$boxTid
          andra <- input$boxAndraKöpa
          # skatter 
          round(renoveringar() + räntaSum() + (försäkring+andra)*tid,digit=0)
        })
        
        ################################
        # Alternativkostnader
        ################################
        
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
        
        ################################
        # Vinster för investeringar
        ################################
        
        formel_husprisvinst <-  reactive({
                pris <- input$boxPris
                husprisökning <- input$boxDeltaP     
                round(pris*(1+husprisökning)^input$boxTid-pris, digits=0)
        })
    
        # totala amorteringar - amorteringsprocent gånger antal år 
        totamortering <- reactive({
                pris <- input$boxPris
                amortering <- input$BoxAmort
                ki <- input$boxKI
                år <- input$boxTid
                round(pris*amortering*år, digits=2)
                
        })
        
        ################################
        # Sammanställning kostnader för att köpa
        ################################
        
        formel_totala_kostnader <- reactive ({
          formel_intiala_köpa() + formel_totala_återkommande() + formel_alternativkostnad() - formel_husprisvinst()
        })
        
        ################################ 
        # Grafer
        ################################
        
        # Räntekostnader 
        kostnadTable <- reactive({
          tid <- input$boxTid
          tid2 <- tid-1
          pris <- input$boxPris
          deltaP <- (1+input$boxDeltaP)
          ki <- input$boxKI
          amortering <- input$BoxAmort
          ränta <- input$boxR*0.7 # ränte gånger 1-skatteavdrag 
          skuld <- pris*(1-ki)
          reno <- input$boxReno
          andrasammanlagda <- input$boxAvgift +input$boxFörsäkring + input$boxAndraKöpa 
          df2  <- (skuld*ränta + reno*pris + andrasammanlagda)/12
          # Räntekostnader + renoveringar + sammanställda * inflationstakt
            for (i in 1:tid2) {
              bal2 <- (skuld*(1-amortering*i)*ränta + reno*pris*deltaP^i + andrasammanlagda)/12
              df2 <- rbind(df2,bal2)
            }
          print(df2[,1])
        })

        # Totala utgifter
        utgiftTable <- reactive({
          tid <- input$boxTid
          tid2 <- tid-1
          pris <- input$boxPris
          deltaP <- (1+input$boxDeltaP)
          ki <- input$boxKI
          amortering <- input$BoxAmort
          ränta <- input$boxR*0.7 # ränte gånger 1-skatteavdrag 
          skuld <- pris*(1-ki)
          reno <- input$boxReno
          andrasammanlagda <- input$boxAvgift +input$boxFörsäkring + input$boxAndraKöpa 
          df3  <- (skuld*ränta + reno*pris + andrasammanlagda + amortering*pris*(1-ki))/12
            for (i in 1:tid2) {
              bal3 <- (skuld*(1-amortering*i)*ränta + reno*pris*deltaP^i + andrasammanlagda + amortering*pris*(1-ki))/12
              df3 <- rbind(df3,bal3)
            }
          print(df3[,1])
        })

        
          # Graf med kostnader per månad och utgifter per månad
           output$plot1 <- renderPlot({
                   tid <- input$boxTid -1
                plot(0:tid, utgiftTable(), xlab="År",ylab="Kostnader",col="red")
                  lines(0:tid, utgiftTable(),col="green")
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
                paste("Återkommande kostnader: ", formel_totala_återkommande(), "kr")
        })
        
        # Vinster
        output$vinster <- renderText({
                paste("Vinster från investeringar: -", formel_husprisvinst(), "kr")
        })
        
        
        output$intiala_köpa <- renderText({
                paste("Intiala kostnader: ", formel_intiala_köpa(), "kr")
        })
        
        output$totala_kostnader_köpa <- renderText({
          paste("Totala kostnader:", formel_totala_kostnader(), "kr")
        })
        
        
        ################################ 
        # texter 
        ################################ 
        
        # Räntekostnader
        output$räntekostnader <- renderText({
                paste("Räntekostnader",räntaSum(),"kr" )
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
                "<b> Bostadspris. </b> En väldigt viktig del är hur din nya bostad kostar. 
                Det är inte den enda delen, utan vi behöver titta på fler saker innan vi kan ta ett beslut."
        })
        
        # Tid i bostaden 
        output$text_tid <- renderText({
                "<b> Tid i bostaden </b>. Ju längre du planerar att stanna, desto bättre är det att köpa. 
                              Det är för att fasta kostnader sprids över fler år. "
        })
      
        #inkomst
        output$text_inkomst <- renderText({
                "<b> Inkomst </b>. Om ditt bolån är större än 4.5 gånger din bruttoinkomst
                ska du amortera en procent mer per år. 
                Din bruttoinkomst beräknas genom att XXX..."
        })
        
})




