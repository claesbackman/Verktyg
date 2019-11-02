library(shiny)
library(ggplot2)
library(tidyverse)
library(scales)


shinyServer(function(input, output) {
    
    ################################ 
    # Att göra 
    ################################ 

        # uppdatera procent i amorteringar enligt andra amorteringskravet
        # Skatt på investeringar
        # Topplån
        # Ändra så att amorteringar/ränta är noll om skulden är noll
  
        # Just nu: renoveringar uppdateras inte när priset går upp. Så du renoverar en procent av det ursprungliga beloppet
          # Annars så ökar renoveringarna när huspriserna ökar, vilket känns konstigt 
  
  
  ################################ 
  # Function för att beräkna kostnader 
  ################################ 

  hyraFunction<- function(pris,tid,ranta,kontantinsats,inkomst,amortering,deltaHyra,deltaPris,
                          rstocks,avgift,renovering,forsakring,andrakopa,andrahyra)
                          {

    # Definera variabler
    # pris <- input$boxPris
    ki <- kontantinsats/ 100
    skuld <- pris*(1-ki)
    # tid <- input$boxTid
    # amortering <- input$BoxAmort/ 100
    ränta <- ranta* 0.7 / 100
    r_stocks <- rstocks / 100
    betalningar <- amortering / 100 * skuld
    husprisökning <- deltaPris  / 100
    renoveringar_2 <- renovering/100 * pris * tid
    hyresökning <- deltaHyra / 100 
    amortering_2 <- amortering / 100
    skuldsättning_slut <- (1-ki) - tid*amortering_2
    
    # Initiala kostnader 
    intiala_köpa <- pris*ki

    # Återkommande kostnader
    if(skuldsättning_slut>0) {
    räntekostnad <- ränta*(skuld*tid - skuld*(amortering_2*(tid-1)*tid/2))
    }
    else {
    tid_alt <- skuld/(amortering_2*skuld)
    räntekostnad <- ränta*(skuld*tid_alt - skuld*(amortering_2*(tid_alt-1)*tid_alt/2))
    }

    totala_återkommande <- räntekostnad +renoveringar_2 + (forsakring+andrakopa+avgift)*12*tid

    # Alternativkostnad
    alternativkostnad <- pris*ki*(1+r_stocks)^tid-pris*ki+(betalningar*(((1 + r_stocks)^(tid) - 1) / (r_stocks)))-betalningar*tid

    # Husprisvinster
    prisökning <- pris*(1+husprisökning)^input$boxTid-pris
    husprisvinst <- prisökning - (prisökning - renoveringar_2)*22/30*0.3

    totala_kostnader <- intiala_köpa +totala_återkommande + alternativkostnad - husprisvinst #Funkar! 

    # Beräkning av hyra
    round(totala_kostnader  / ((1-(1+hyresökning)^tid)/(1-(1+hyresökning)))/12 
          , digit=0)
    }

  ###############################
  # tabell med värden för pris
  ###############################
  
   tablePris <- reactive({
   seq <-  seq(from=10000, to=10000000, by=10000)
  
   # Första värdet 
    row <- hyraFunction(
     pris=10000, 
     tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
     amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
     rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
     forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=  input$boxAndraHyra
   )     
   
   # alla värden 
    for (i in seq) {
     bal <- hyraFunction(
        pris=i, 
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=  input$boxAndraHyra
      )     
     row <- rbind(row,bal)
    }
   print(row[,1])
  })
   
   # värdet för punkten
   
    output$grafPris <- renderPlot({

    # All data 
    xValue <- 1:1001
    yValue <- tablePris()
    data <- data.frame(xValue,yValue)
    
    punktPris  <- hyraFunction(
      pris=input$boxPris, 
      tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
      amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
      rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
      forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=  input$boxAndraHyra
    )     
    
     xValue2 <- input$boxPris/10000
     data2 <- data.frame(xValue2,punktPris)

  #   # Define the plot
  p <-   ggplot() +
    geom_bar(data=data, aes(x=xValue, y = yValue), stat="identity", alpha=0.2, width=0.7, color="gray83") +
    theme_bw(base_size = 12) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      panel.border = element_blank(),
      axis.ticks = element_blank(),
      axis.title.y.right = element_text(angle = 0, vjust=1, face = "bold"),
      axis.title.x = element_text(face = "bold") 
    ) +
    scale_y_continuous("Motsvarande hyra", position = "right") +
    scale_x_continuous("Bostadspris",breaks= pretty_breaks(), labels=function(x)x*1000) + 
    # Andra delen   
    geom_bar(data=data2, aes(x=xValue2,y=punktPris), stat="identity", color="red") + 
    geom_label(data=data2,  aes(x=xValue2,y=punktPris), label= paste(punktPris, "kr", sep=" ") , position = position_stack(vjust = 1.2))

    # geom_text(data=data2, aes(x=xValue2,y=punktPris), label= punktPris, size = 10,  stat="identity")
  
    # Print the plot
  print(p)
    })
#
  # 
  # output$plot1 <- renderPlot({
  #   # X axis values
  #   tid <- input$boxTid
  #   if(tid<10) {
  #     tid_x <- tid
  #   }
  #   if(tid>=10){
  #     tid_x <- tid / 2
  #   }
  #   if(tid>=25){
  #     tid_x <- tid / 3
  #   }
  #   # Values for graph
  #   xValue <- 1:tid 
  #   yValue <- utgiftTable()
  #   data <- data.frame(xValue,yValue)
  #   
  #   # Define the plot 
  #   p <-   ggplot(data, aes(x=xValue, y = yValue))+ 
  #     geom_point( color="#69b3a2", size=4, alpha=0.9) +
  #     geom_line() + 
  #     labs(y=expression(atop("Utgifter", paste("per månad" )))) + 
  #     theme_bw(base_size = 12) + 
  #     theme(
  #       panel.grid.major = element_blank(), 
  #       panel.border = element_blank(),
  #       axis.ticks = element_blank(),
  #       axis.title.y.right = element_text(angle = 0, vjust=1, face = "bold"),
  #       axis.title.x = element_text(face = "bold")
  #     ) +
  #     scale_y_continuous(position = "right") + 
  #     scale_x_continuous("År",breaks= pretty_breaks(n = tid_x)) 
  #   
  #   # Print the plot 
  #   print(p)
  # })
  # 
  # 
  
  
        ################################
        # initiala kostnader köpa 
        ################################
  
        formel_intiala_köpa <-  reactive({
                pris <- input$boxPris
                ki <- input$boxKI / 100    
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
          ki <- input$boxKI / 100 
          amortering <- input$BoxAmort / 100 
          skuld <- pris*(1-ki)
          ränta <- input$boxR*0.7  / 100 # ränte gånger 1-skatteavdrag 
          df  <- skuld*ränta
          for (i in 1:tid2) {
            bal <- skuld*(1-amortering*i)*ränta
            df <- rbind(df,bal)
          }
          print(df[,1])
        })

        ## Totala räntekostnader
        räntaSum <- reactive({
          skuld <- input$boxPris*(1-input$boxKI/100)
          tid <- input$boxTid
          amortering <- input$BoxAmort/ 100
          ränta <- input$boxR * 0.7 / 100
          skuldsättning_slut <- (1-input$boxKI/100) - tid*amortering 
          if(skuldsättning_slut>0) {
            round(ränta*(skuld*tid - skuld*(amortering*(tid-1)*tid/2)), digit=0)
          }
          else {
            tid <- skuld/(amortering*skuld)
            round(ränta*(skuld*tid - skuld*(amortering*(tid-1)*tid/2)), digit=0)
          }
        })

        
        # # Totala räntekostnader 
        # räntaSum <- reactive({
        #   sum(räntaTable())
        # })
        
        # Renoveringar - procent av tidigare års bostadspris
          # renoveringar <- reactive({
          #   renovering <-  input$boxReno * input$boxPris / 100 
          #   tid <- input$boxTid
          #   ökning <- 1+input$boxDeltaP / 100 
          #   round(renovering * (1-ökning^tid)/(1-ökning))
          # })
          # 
        
        # Renoveringar - procent av ursprunliga priset gånger priset  gånger tid 
        renoveringar <- reactive({
          input$boxReno/100 * input$boxPris * input$boxTid
        })
        
        # totala återkommande kostnader 
        formel_totala_återkommande <- reactive({
          avgifter <- input$boxAvgift * 12
          försäkring <- input$boxFörsäkring * 12
          tid <- input$boxTid
          andra <- input$boxAndraKöpa * 12
          round(räntaSum() + renoveringar() + (försäkring+andra+avgifter)*tid
                ,digit=0)
        })
        
        ################################
        # Alternativkostnader
        ################################
        
        # kontantinsats + amorteringar investerat istället
          # Nu räknas det som att betalningar sparas i slutet av året.
        
        formel_alternativkostnad <- reactive({
          kontantinsats_kr <- input$boxKI / 100  *input$boxPris
          skuld <- input$boxPris - kontantinsats_kr
          r_stocks <- input$boxDeltaSM / 100 
          tid <- input$boxTid
          betalningar <- input$BoxAmort * skuld / 100  #+ input$boxReno + input$boxAndraKöpa + input$boxFörsäkring + input$boxAvgift
          round(kontantinsats_kr*(1+r_stocks)^tid-kontantinsats_kr+(betalningar*(((1 + r_stocks)^(tid) - 1) / (r_stocks)))-betalningar*tid, digit=0)
          })
        
        ################################
        # Vinster för investeringar
        ################################
        
        formel_husprisvinst <-  reactive({
                pris <- input$boxPris
                husprisökning <- input$boxDeltaP   / 100 
                prisökning <- pris*(1+husprisökning)^input$boxTid-pris 
                round(prisökning - (prisökning - renoveringar())*22/30*0.3, digits=0)
        })
    
        # totala amorteringar - amorteringsprocent gånger antal år
        totamortering <- reactive({
                pris <- input$boxPris
                amortering <- input$BoxAmort / 100 
                ki <- input$boxKI / 100 
                år <- input$boxTid
                round(pris*amortering*år, digits=2)
                
        })
        
        ################################
        # Sammanställning kostnader för att köpa
        ################################
        
        formel_totala_kostnader <- reactive({
          formel_intiala_köpa() + formel_totala_återkommande() + formel_alternativkostnad() - formel_husprisvinst()
        })
        
        
        ################################
        # Hyra - 
        ################################
        
        # 
        minHyra <- reactive({
          hyresökning <- (1+input$boxDeltaRent/100)
          tid <- input$boxTid
          AndraHyra <- input$boxAndraHyra
          round(formel_totala_kostnader() / ((1-hyresökning^tid)/(1-hyresökning))/12, digit=0)
        })
  
        # Totala hyreskostader
        sumHyra <- reactive({
          hyresökning <- (1+input$boxDeltaRent/100)
          tid <- input$boxTid
          AndraHyra <- input$boxAndraHyra
          # Formel 
          round(minHyra()*12 * ((1-hyresökning^tid)/(1-hyresökning)), digit=0)
        })

        
        ################################ 
        # Grafer
        ################################
        
        # bostadspriser och motsvarande hyra 
       
        
        
        # Totala kostnader 
        kostnadTable <- reactive({
          tid <- input$boxTid
          tid2 <- tid-1
          pris <- input$boxPris
          deltaP <- (1+input$boxDeltaP / 100 )
          ki <- input$boxKI / 100 
          amortering <- input$BoxAmort / 100 
          ränta <- input$boxR*0.7 / 100  # ränte gånger 1-skatteavdrag 
          skuld <- pris*(1-ki)
          reno <- input$boxReno / 100 
          andrasammanlagda <- input$boxAvgift * 12 +input$boxFörsäkring * 12 + input$boxAndraKöpa  * 12
          df2  <- (skuld*ränta + reno*pris + andrasammanlagda)/12
          
          # Räntekostnader + renoveringar + sammanställda 
            for (i in 1:tid2) {
              bal2 <- (skuld*(1-amortering*i)*ränta + reno*pris + andrasammanlagda)/12
              df2 <- rbind(df2,bal2)
            }
          print(df2[,1])
        })

        # Totala utgifter
        utgiftTable <- reactive({
          tid <- input$boxTid
          tid2 <- tid-1
          pris <- input$boxPris
          deltaP <- (1+input$boxDeltaP / 100 )
          ki <- input$boxKI / 100 
          amortering <- input$BoxAmort / 100 
          ränta <- input$boxR*0.7 / 100  # ränte gånger 1-skatteavdrag 
          skuld <- pris*(1-ki)
          reno <- input$boxReno / 100 
          andrasammanlagda <- input$boxAvgift * 12+input$boxFörsäkring * 12+ input$boxAndraKöpa * 12
          df3  <- (skuld*ränta + reno*pris + andrasammanlagda + amortering*pris*(1-ki))/12
            for (i in 1:tid2) {
              bal3 <- (skuld*(1-amortering*i)*ränta + reno*pris + andrasammanlagda + amortering*pris*(1-ki))/12
              df3 <- rbind(df3,bal3)
            }
          print(df3[,1])
        })
        
          #Graf med kostnader per månad och utgifter per månad
          output$plot1 <- renderPlot({
                  # X axis values
                  tid <- input$boxTid
                  if(tid<10) {
                    tid_x <- tid
                  }
                  if(tid>=10){
                    tid_x <- tid / 2
                  }
                  if(tid>=25){
                    tid_x <- tid / 3
                  }
                  # Values for graph
                  xValue <- 1:tid 
                  yValue <- utgiftTable()
                  data <- data.frame(xValue,yValue)

                # Define the plot 
                p <-   ggplot(data, aes(x=xValue, y = yValue))+ 
                              geom_point( color="#69b3a2", size=4, alpha=0.9) +
                  geom_line() + 
                  labs(y=expression(atop("Utgifter", paste("per månad" )))) + 
                  theme_bw(base_size = 12) + 
                  theme(
                        panel.grid.major = element_blank(), 
                        panel.border = element_blank(),
                        axis.ticks = element_blank(),
                        axis.title.y.right = element_text(angle = 0, vjust=1, face = "bold"),
                        axis.title.x = element_text(face = "bold")
                        ) +
                  scale_y_continuous(position = "right") + 
                  scale_x_continuous("År",breaks= pretty_breaks(n = tid_x)) 
                
                # Print the plot 
                print(p)
           })

        
         
          utgiftTable <- reactive({
            tid <- input$boxTid
            tid2 <- tid-1
            pris <- input$boxPris
            deltaP <- (1+input$boxDeltaP / 100 )
            ki <- input$boxKI / 100 
            amortering <- input$BoxAmort / 100 
            ränta <- input$boxR*0.7 / 100  # ränte gånger 1-skatteavdrag 
            skuld <- pris*(1-ki)
            reno <- input$boxReno / 100 
            andrasammanlagda <- input$boxAvgift * 12+input$boxFörsäkring * 12+ input$boxAndraKöpa * 12
            df3  <- (skuld*ränta + reno*pris + andrasammanlagda + amortering*pris*(1-ki))/12
            for (i in 1:tid2) {
              bal3 <- (skuld*(1-amortering*i)*ränta + reno*pris + andrasammanlagda + amortering*pris*(1-ki))/12
              df3 <- rbind(df3,bal3)
            }
            print(df3[,1])
          })
          
          
          output$plotPris <- renderPlot({
            
            # definera boxarna 
            tid <- input$boxTid
            pris <- input$boxPris
            deltaP <- (1+input$boxDeltaP / 100 )
            ki <- input$boxKI / 100
            amortering <- input$BoxAmort / 100
            ränta <- input$boxR*0.7 / 100  # ränte gånger 1-skatteavdrag 
            skuld <- pris*(1-ki)
            reno <- input$boxReno / 100 
            andrasammanlagda <- input$boxAvgift * 12+input$boxFörsäkring * 12+ input$boxAndraKöpa * 12
            
            # X axis values
            pris <- input$boxPris
          
            # Values for graph
            xValue <- 1000000
            yValue <-minHyra()

            data <- data.frame(xValue,yValue)
            
            # Define the plot 
            p <-   ggplot(data, aes(x=xValue, y = yValue))+ 
              geom_point(color="#69b3a2", size=4, alpha=0.9) +
              geom_line() + 
              labs(y=expression(atop("Utgifter", paste("per månad" )))) + 
              theme_bw(base_size = 12) + 
              theme(
                panel.grid.major = element_blank(), 
                panel.border = element_blank(),
                axis.ticks = element_blank(),
                axis.title.y.right = element_text(angle = 0, vjust=1, face = "bold"),
                axis.title.x = element_text(face = "bold")
              ) +
              scale_y_continuous(position = "right") + 
              scale_x_continuous("År",breaks= pretty_breaks()) 
            
            # Print the plot 
            print(p)
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
                        paste("Initiala kostnader: ", formel_initiala_köpa(), "kr")
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
        
        output$totala_kostnader_hyra <- renderText({
          paste("Kostnad för att hyra:", sumHyra(), "kr")
        })
        
        output$månadskostnader_hyra <- renderText({
          paste("Kostnad per månad för att hyra:", minHyra(), "kr")
        })
        
        output$grafpris <- renderText({
          paste("Funktion pris:", grafPris(), "kr")
        })
        
        
        
        
        
        
        
        ################################ 
        # texter 
        ################################ 
        
        # header
        
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
                "<h4> Hur mycket kostar bostaden? </h4> En väldigt viktig del är hur din nya bostad kostar. 
                Här räknar vi med både priset för att köpa och eventuella avgifter till en bostadsrättsförening. "        })
      
        
        # Tid i bostaden 
        output$text_tid <- renderText({
                "<h4> Hur länge planerar du att bo kvar? </h4> Ju längre du planerar att stanna, desto bättre är det att köpa. 
                              Det är för att fasta kostnader sprids över fler år. "
        })
        
        # Bolån
        output$text_bolån <- renderText({
          "<h4> Hur ser ditt bolån ut?</h4> Vad din ränta och kontantinsats är har stor påverkan på din månadskostnad."
        })
        
        belåningsgrad <- reactive({
          100-input$boxKI
                  })
        
        output$text_belåningsgrad <- renderText({
          paste("<b> Med en kontantinsats på", input$boxKI," blir din belåningsgrad", belåningsgrad(), "procent. </b>")
        })
      
        #inkomst
        output$text_amortering <- renderText({
                "<h4> Hur mycket ska du amortera? </h4> Dina amorteringsbetalningar beror på din belåningsgrad och hur stor 
          din inkomst är relativt ditt lån. Du kan också välja att amortera mer, då minskar dina lånekostnad snabbare över tid."
        })
        
        output$text_framtiden <- renderText({
          "<h4> Vad händer i framtiden? </h4> Vad som händer i framtiden har stor påverkan, men det är tyvärr
          väldigt svårt att veta vad som kommer hända i framtiden. 
          Vi har fyllt i värden för husprisökningar, hyresökningar, avkastningar på investeringar, och inflation.
          Om du själv vill fylla i värden för framtida huspriser, hyresökningar, avkastning på investeringar 
          och inflation så klicka i boxen nedan. " 
        })
        output$text_renoveringar <- renderText({
          "<h4> Avgifter, renoveringar, försäkringar och andra kostnader.</h4> Här kan du fylla i andra kostnader
            som kommer när du köper. " 
        })
        
        # output$text_hyra <- renderText({
        #   "<b> Kostnader för att hyra.</b> " 
        # })
        
        
        
})




