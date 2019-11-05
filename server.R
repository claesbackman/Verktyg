library(shiny)
library(ggplot2)
library(tidyverse)
library(scales)

formatnummer <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)

shinyServer(function(input, output) {
    
    ################################ 
    # Att göra 
    ################################ 

        # uppdatera procent i amorteringar enligt andra amorteringskravet
        # Skatt på alternativkostnader  - hur gör vi det?  
        # Topplån
        # Ändra så att amorteringar/ränta är noll om skulden är noll
  
        # Just nu: renoveringar uppdateras inte när priset går upp. Så du renoverar en procent av det ursprungliga beloppet
        # Annars så ökar renoveringarna när huspriserna ökar, vilket känns konstigt 
  
        # om kontantinsatsen är 100 procent så funkar inte grafen
        # se till så att x-axeln aldrig blir mindre än 0
  
        # Amorteringar räknas inte till alternativkostnaden 
  
  
  ################################ 
  # Function för att beräkna kostnader 
  ################################ 

  hyraFunction<- function(pris,tid,ranta,kontantinsats,inkomst,amortering,deltaHyra,deltaPris,
                          rstocks,avgift,renovering,forsakring,andrakopa,andrahyra)
                          {

    # Definera variabler
    ki <- kontantinsats/ 100
    skuld <- pris*(1-ki)
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
    alternativkostnad <- pris*ki*(1+r_stocks)^tid-pris*ki # +(betalningar*(((1 + r_stocks)^(tid) - 1) / (r_stocks)))-betalningar*tid

    # Husprisvinster
    prisökning <- pris*(1+husprisökning)^tid-pris
    husprisvinst <- prisökning - (prisökning - renoveringar_2)*22/30*0.3

    totala_kostnader <- intiala_köpa +totala_återkommande + alternativkostnad - husprisvinst #Funkar! 

    # Beräkning av hyra
    round(totala_kostnader/ ((1-(1+hyresökning)^tid)/(1-(1+hyresökning)))/12
          , digit=0)
    }

  ###############################
  # Bostadspris - graf 
  ###############################
  
   tablePris <- reactive({
    min <- input$boxPris - 1000000
    max <- input$boxPris + 1000000
    seq <-  seq(from=min, to=max-50000, by=50000)
  
   # Första värdet 
    rowPris <- hyraFunction(
     pris=min, 
     tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
     amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
     rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
     forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=  input$boxAndraHyra
   )     
    
   # alla värden 
    for (i in seq) {
     balPris <- hyraFunction(
        pris=i, 
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=  input$boxAndraHyra
      )     
     rowPris <- rbind(rowPris,balPris)
    }
    print(rowPris[,1])
  })
   
   # värdet för punkten
    output$grafPris <- renderPlot({
      
     min <- input$boxPris - 1000000
     max <- input$boxPris + 1000000
     seq <-  seq(from=min, to=max, by=50000)

    # All data 
    xValue <- seq
    yValue <- tablePris()
    data <- data.frame(xValue,yValue)
    
    # Det valda värdet 
    punktPris  <- hyraFunction(
      pris=input$boxPris,
      tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
      amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
      rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
      forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra
    )     
    
    data <- data %>% mutate(ToHighlight = ifelse(xValue == input$boxPris, "yes", "no"))
    
    print(data)
    
    # max value for y-axis
    ymax <- punktPris*2
    
    # se till att axeln aldrig går under 0 
    if (min < 0) {
      xaxel <- c(0,max+1)
    }
    else {
      xaxel <- c(min,max)
    }
  #   # Define the plot fill = highlight_flag
     p <-   ggplot(data=data, aes(x=xValue, y = yValue, fill = ToHighlight)) +
            geom_bar(stat="identity") +
            theme_bw(base_size = 12) +
            theme(
              # panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              # axis.ticks = element_blank(),
              axis.title.y.right = element_text(angle = 0, vjust=1, face = "bold"),
              axis.title.x = element_text(face = "bold") 
            ) +
            scale_y_continuous("Motsvarande hyra", position = "right", limits=c(0,ymax)) +
            scale_x_continuous("Bostadspris",limits=xaxel, labels = format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)) + 
            scale_fill_manual(values = c( "yes"="red", "no"="gray83"), guide = FALSE) +
            geom_label(data=data %>% filter(ToHighlight=="yes"), aes(x=xValue,y=yValue),
                        label=paste(punktPris, "kr", sep=" ") , 
                        position = position_stack(vjust = 1.2), 
                        color = "black", 
                        fill="white")
    
    # Print the plot
    print(p)
    })
    
    
    ###############################
    # Tid - graf 
    ###############################
    
    tableTid <- reactive({

      # Första värdet 
      rowTid <- hyraFunction(
        pris=input$boxPris, 
        tid=1, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra
      )     
      
      # alla värden 
      for (i in 2:50) {
        balTid <- hyraFunction(
          pris=input$boxPris,
          tid= i, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
          rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra
        )     
        rowTid <- rbind(rowTid,balTid)
      }
      print(rowTid[,1])
    })
    
    # värdet för punkten
    output$grafTid <- renderPlot({
      
      # All data 
      xTid <- 1:50
      yTid <- tableTid()
      dataTid <- data.frame(xTid,yTid)
      
      # Det valda värdet 
      punkt <- hyraFunction(
        pris=input$boxPris,
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra
      )     
      
      dataTid <- dataTid %>% mutate(ToHighlight = ifelse(xTid == input$boxTid, "yes", "no"))
      
      print(data)
      
      # max value for y-axis
      # ymax <- punkt*2
      
      #   # Define the plot fill = highlight_flag
      p <-   ggplot(data=dataTid, aes(x=xTid, y = yTid, fill = ToHighlight)) +
        geom_bar(stat="identity") +
        theme_bw(base_size = 12) +
        theme(
          # panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          # axis.ticks = element_blank(),
          axis.title.y.right = element_text(angle = 0, vjust=1, face = "bold"),
          axis.title.x = element_text(face = "bold") 
        ) +
        scale_y_continuous("Motsvarande hyra", position = "right") +
        scale_x_continuous("Tid", limits=c(0,51), labels = format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)) + 
        scale_fill_manual(values = c( "yes"="red", "no"="gray83"), guide = FALSE) +
        geom_label(data=dataTid %>% filter(ToHighlight=="yes"), aes(x=xTid,y=yTid),
                   label=paste(punkt, "kr", sep=" ") , 
                   position = position_stack(vjust = 1.2), 
                   color = "black", 
                   fill="white")
      
      # Print the plot
      print(p)
    })
    
    
    ###############################
    # Ränta - graf 
    ###############################
    
    tableRanta <- reactive({
      
      # Sequence 
      min <- input$boxR - 3 
      max <- input$boxR + 3
      seq <-  seq(from=min, to=max-0.1, by=0.1)

      # Första värdet 
      rowR <- hyraFunction(
        pris=input$boxPris, 
        tid=input$boxTid, ranta=min, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra
      )     
      
      # alla värden 
      for (i in seq) {
        balR <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=i, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
          rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra
        )     
        rowR <- rbind(rowR,balR)
      }
      print(rowR[,1])
    })
    
    # värdet för punkten
    output$grafRanta <- renderPlot({
      min <- input$boxR - 3 
      max <- input$boxR + 3
      seq <-  seq(from=min, to=max, by=0.1)
      # seq <-  seq(from=0, to=15, by=0.1)
      # All data 
      xR <- seq
      yR <- tableRanta()
      dataR <- data.frame(xR,yR)
      
      # Det valda värdet 
      punkt <- hyraFunction(
        pris=input$boxPris,
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra
      )     
          dataR <- dataR %>% mutate(ToHighlight = ifelse(xR == input$boxR, "yes", "no"))
      
      print(dataR)
      
      # X axeln stannar på 0 
      if (min < 0) {
        xaxel <- c(0,max+1)
      }
      else {
        xaxel <- c(min,max)
      }
      # limits=xaxel

        # Graf
        p <-   ggplot(data=dataR, aes(x=xR, y = yR, fill = ToHighlight)) +
        geom_bar(stat="identity") +
        theme_bw(base_size = 12) +
        theme(
          # panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          # axis.ticks = element_blank(),
          axis.title.y.right = element_text(angle = 0, vjust=1, face = "bold"),
          axis.title.x = element_text(face = "bold") 
        ) +
        scale_y_continuous("Motsvarande hyra", position = "right", labels = format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)) +
        scale_x_continuous("Bolåneränta",  labels = format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)) + 
        scale_fill_manual(values = c( "yes"="red", "no"="gray83"), guide = FALSE) +
        geom_label(data=dataR %>% filter(ToHighlight=="yes"), aes(x=xR,y=yR),
                   label=paste(punkt, "kr", sep=" ") , 
                   position = position_stack(vjust = 1.2), 
                   color = "black", 
                   fill="white")
      
      # Print the plot
      print(p)
    })
    
    
    
    
  
  
        ################################
        # initiala kostnader köpa 
        ################################
  
        formel_initiala_köpa <-  reactive({
                pris <- input$boxPris
                ki <- input$boxKI / 100    
                pris*ki
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
          tid <- input$boxTid
          avgifter <- input$boxAvgift * 12
          försäkring <- input$boxFörsäkring * 12
          andra <- input$boxAndraKöpa * 12
          round(räntaSum() + renoveringar() + (försäkring+andra+avgifter)*tid
                ,digit=0)
        })
        
        ################################
        # Alternativkostnader
        ################################
        
        # kontantinsats + amorteringar investerat istället
          # Nu räknas det som att betalningar sparas i slutet av året.
        
        # compound interest formula with contributions 
        
        formel_alternativkostnad <- reactive({
          kontantinsats_kr <- input$boxKI / 100  *input$boxPris
          skuld <- input$boxPris - kontantinsats_kr
          r_stocks <- input$boxDeltaSM / 100
          tid <- input$boxTid
          betalningar <- input$BoxAmort * skuld / 100  #+ input$boxReno + input$boxAndraKöpa + input$boxFörsäkring + input$boxAvgift
          round(
            kontantinsats_kr*(1+r_stocks)^tid-kontantinsats_kr 
                # + (betalningar*(((1 + r_stocks)^(tid) - 1) / (r_stocks)))-betalningar*tid
                , digit=0)
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
          formel_initiala_köpa() + formel_totala_återkommande() + formel_alternativkostnad() - formel_husprisvinst()
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
          
          
       hyra1år <- reactive({
            hyraFunction(
              pris=input$boxPris,
              tid= 1, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
              amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
              rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
              forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=  input$boxAndraHyra
            )     
          })

        # Titeltext
        output$sumkostnader <- renderText({
                paste("<b>Kostnader efter ", input$boxTid, "år </b>")
        })
          
        # Initiala kostnader köpa 
        output$initiala_köpa <- renderText({
                        paste("Initiala kostnader: ", formatnummer(formel_initiala_köpa()), "kr")
                })
        
        # alternativkostnader 
        output$alternativkostnader <- renderText({
          paste("Alternativkostnader: ", formatnummer(formel_alternativkostnad()), "kr")
        })
        
        # Återkommande kostnader
        output$återkommande <- renderText({
                paste("Återkommande kostnader: ", formatnummer(formel_totala_återkommande()), "kr")
        })
        
        # Vinster
        output$vinster <- renderText({
                paste("Vinster från investeringar: -", formatnummer(formel_husprisvinst()), "kr")
        })
        
        # Initiala kostnader 
        output$intiala_köpa <- renderText({
                paste("Intiala kostnader: ", formatnummer(formel_intiala_köpa()), "kr")
        })
        
        # totala kostnader 
        output$totala_kostnader_köpa <- renderText({
          paste("Totala kostnader:", formatnummer(formel_totala_kostnader()), "kr")
        })
        
        output$totala_kostnader_hyra <- renderText({
          paste("Kostnad för att hyra:", formatnummer(sumHyra()), "kr")
        })
        
        output$månadskostnader_hyra <- renderText({
          paste("Kostnad per månad för att hyra:", formatnummer(minHyra()), "kr")
        })
        
        output$grafpris <- renderText({
          paste("Funktion pris:", grafPris(), "kr")
        })
      
        
        
        ################################ 
        # texter 
        ################################ 
        
        # Bättre att hyra om: 
        output$hyrabättre <- renderText({
          paste("<h3><b>Om du kan hyra för mindre än:</b></h3>")
        })
        
      
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




