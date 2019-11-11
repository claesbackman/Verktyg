library(shiny)
library(ggplot2)
library(tidyverse)
library(scales)
library(tableHTML)
library(shinydashboard)
library(rsconnect)


formatnummer <- format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE, digit=0)

shinyServer(function(input, output) {
    
    ################################ 
    # Att göra 
    ################################ 
  
        # Varför är hyran ökande i tid?  

        # uppdatera procent i amorteringar enligt andra amorteringskravet
        # Skatt på alternativkostnader  - hur gör vi det?  
        # Topplån
        # Ändra så att amorteringar/ränta är noll om skulden är noll
  
     
        # se till så att x-axeln aldrig blir mindre än 0
  
        # Amorteringar räknas inte till alternativkostnaden 
        # Grafen för amorteringar ändras inte när vi ändrar kontantinsatsen
  
  ################################ 
  # att se över 
  ################################ 
  
  # Just nu: renoveringar uppdateras inte när priset går upp. Så du renoverar en procent av det ursprungliga beloppet
  # Annars så ökar renoveringarna när huspriserna ökar, vilket känns konstigt 
  
  # Amorteringar och kontantinsatsen gör nu att vinster blir större. Är det korrekt? 
    # Amorteringar ingår nu i : 
      # 1. alternativkostnad (kunde investerat dom)
      # 2. löpande kostnader: minskar räntekostnader
      # 3. Vinster: ökar vinsten
  
  
  ################################ 
  # Function för att beräkna kostnader 
  ################################ 

  hyraFunction<- function(pris,tid,ranta,kontantinsats,inkomst,amortering,deltaHyra,deltaPris,
                          rstocks,avgift,renovering,forsakring,andrakopa,flyttkostnader,andrahyra,deposition)
                          {
    
    # Definera variabler
    ki <- kontantinsats/ 100
    skuld <- pris*(1-ki)
    ränta <- ranta* 0.7 / 100
    r_stocks <- rstocks / 100
    betalningar <- amortering / 100 * skuld
    totalabetalningar <- input$BoxAmort * skuld / 100+
                        (input$boxAndraKöpa + input$boxFörsäkring + input$boxAvgift)*12+
                        input$boxReno*input$boxPris/100 
    husprisökning <- deltaPris  / 100
    renoveringar_2 <- renovering/100 * pris * tid
    hyresökning <- deltaHyra / 100
    amortering_2 <- amortering / 100
    skuldsättning_slut <- (1-ki) - tid*amortering_2
    totandrahyra <- andrahyra*12*tid
    # deposition <- 1
    
    # Initiala kostnader 
    intiala_köpa <- pris*ki + flyttkostnader

    # Löpande kostnader
    if(kontantinsats == 100) {
      räntekostnad <- 0
    }
    else {
    if(skuldsättning_slut>0) {
    räntekostnad <- ränta*(skuld*tid - skuld*(amortering_2*(tid-1)*tid/2))
    }
    else {
    tid_alt <- skuld/(amortering_2*skuld)
    räntekostnad <- ränta*(skuld*tid_alt - skuld*(amortering_2*(tid_alt-1)*tid_alt/2))
    }
    }
   
    totala_återkommande <- räntekostnad +renoveringar_2 + (forsakring+andrakopa+avgift)*12*tid

    # Alternativkostnad
    alternativkostnad <- (pris*ki+flyttkostnader)*(1+r_stocks)^tid-pris*ki -flyttkostnader + # husprisvinst 
              (totalabetalningar*(((1 + r_stocks)^(tid) - 1) / (r_stocks)))-totalabetalningar*tid # betalningar (amorteringar + kostnader)

    # Husprisvinster
    # if (amortering_2*tid>(1-ki)){
    #   totalamortering <- skuld*amortering*tid
    # }
    # else {
    #   totalamortering <- skuld
    # }
    totalamortering <- skuld*amortering_2*tid
    prisökning <- pris*(1+husprisökning)^tid-pris
    husprisvinst <- prisökning - (prisökning - renoveringar_2)*22/30*0.3 + ki*pris
    
    # Sammanlagt 
    totala_kostnader <- intiala_köpa +totala_återkommande + alternativkostnad - husprisvinst #Funkar! 


  # Motsvarande hyra
  round((totala_kostnader- totandrahyra)*(r_stocks - hyresökning)/
      ((1+r_stocks)*((1+r_stocks)^tid-(1+hyresökning)^tid)+(r_stocks-hyresökning)*deposition/12*(1+r_stocks)^tid-(r_stocks-hyresökning)*deposition/12)/12
  ,digit=0)
    # round(totala_kostnader/((1-(1+hyresökning)^tid)/(1-(1+hyresökning)))/12
    #       , digit=0)
    
    # if(hyresökning == 0) {
    #   round(totala_kostnader/tid/12, digit=0)
    # }
    # else {
    # round(totala_kostnader/((1-(1+hyresökning)^tid)/(1-(1+hyresökning)))/12
    #       , digit=0)
    # }

    }

  ###############################
  # Bostadspris - graf 
  ###############################
  
   tablePris <- reactive({
    min <- input$boxPris - 1000000
    max <- input$boxPris + 1000000
    seq <-  seq(from=min+50000, to=max, by=50000)
  
   # Första värdet 
    rowPris <- hyraFunction(
     pris=min, 
     tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
     amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
     rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
     forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=  input$boxAndraHyra,
     flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
   )     
    
   # alla värden 
    for (i in seq) {
     balPris <- hyraFunction(
        pris=i, 
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=  input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
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
      forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
      flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
    )     
    
    data <- data %>% mutate(ToHighlight = ifelse(xValue == input$boxPris, "yes", "no"))
    
    print(data)
    
    # max value for y-axis

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
            scale_y_continuous("Motsvarande hyra", position = "right") +
            scale_x_continuous("Bostadspris",limits=xaxel, labels = format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)) + 
            scale_fill_manual(values = c( "yes"="#E66100", "no"="gray83"), guide = FALSE) +
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
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      
      # alla värden 
      for (i in 2:50) {
        balTid <- hyraFunction(
          pris=input$boxPris,
          tid= i, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
          rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
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
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
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
        scale_fill_manual(values = c( "yes"="#E66100", "no"="gray83"), guide = FALSE) +
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
      min <- input$boxR - 2
      max <- input$boxR + 2
      seq <-  seq(from=min+0.1, to=max, by=0.1)

      # Första värdet 
      rowR <- hyraFunction(
        pris=input$boxPris, 
        tid=input$boxTid, ranta=min, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      
      # alla värden 
      for (i in seq) {
        balR <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=i, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
          rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowR <- rbind(rowR,balR)
      }
      print(rowR[,1])
    })
    
    # värdet för punkten
    output$grafRanta <- renderPlot({
      min <- input$boxR - 2 
      max <- input$boxR + 2
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
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
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
        scale_fill_manual(values = c( "yes"="#E66100", "no"="gray83"), guide = FALSE) +
        geom_label(data=dataR %>% filter(ToHighlight=="yes"), aes(x=xR,y=yR),
                   label=paste(punkt, "kr", sep=" ") , 
                   position = position_stack(vjust = 1.2), 
                   color = "black", 
                   fill="white")
      
      # Print the plot
      print(p)
    })
  
    
    
    
    ###############################
    # Amortering - graf 
    ###############################
   
    
    tableAmort <- reactive({
      
      # Sequence 
      min <- input$BoxAmort - 2
      max <- input$BoxAmort + 2
      seq <-  seq(from=min+0.1, to=max, by=0.1)
      
      # Första värdet 
      rowAmort <- hyraFunction(
        pris=input$boxPris, 
        tid=input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= min, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      
      # alla värden 
      for (i in seq) {
        balAmort <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= i, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
          rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowAmort <- rbind(rowAmort,balAmort)
      }
      print(rowAmort[,1])
    })
    
    # värdet för punkten
    output$grafAmort <- renderPlot({
      min <- input$BoxAmort - 2
      max <- input$BoxAmort + 2
      seq <-  seq(from=min, to=max, by=0.1)
      # seq <-  seq(from=0, to=15, by=0.1)
      # All data 
      xAmort <- seq
      yAmort <- tableAmort()
      dataAmort <- data.frame(xAmort,yAmort)
      
      # Det valda värdet 
      punkt <- hyraFunction(
        pris=input$boxPris,
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      dataAmort <- dataAmort %>% mutate(ToHighlight = ifelse(xAmort == input$BoxAmort, "yes", "no"))
      
      print(dataAmort)
      
      # X axeln stannar på 0 
      if (min < 0) {
        xaxel <- c(0,max+1)
      }
      else {
        xaxel <- c(min,max)
      }
      # limits=xaxel
      
      # Graf
      p <-   ggplot(data=dataAmort, aes(x=xAmort, y = yAmort, fill = ToHighlight)) +
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
        scale_x_continuous("Amortering per år",  labels = format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)) + 
        scale_fill_manual(values = c( "yes"="#E66100", "no"="gray83"), guide = FALSE) +
        geom_label(data=dataAmort %>% filter(ToHighlight=="yes"), aes(x=xAmort,y=yAmort),
                   label=paste(punkt, "kr", sep=" ") , 
                   position = position_stack(vjust = 1.2), 
                   color = "black", 
                   fill="white")
      
      # Print the plot
      print(p)
    })
    
    
    
    ###############################
    # Kontantinsats - graf 
    ###############################
    
    
    tableKI <- reactive({
      input$boxKI
      # Sequence 
      min <- input$boxKI - 15
      max <- input$boxKI + 20
      seq <-  seq(from=min, to=max-1, by=1)
      
      # Första värdet 
      rowKI <- hyraFunction(
        pris=input$boxPris, 
        tid=input$boxTid, ranta=input$boxR, kontantinsats= min, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      
      # alla värden 
      for (i in seq) {
        balKI <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=input$boxR, kontantinsats= i, inkomst= input$boxInc,
          amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
          rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowKI <- rbind(rowKI,balKI)
      }
      print(rowKI[,1])
    })
    
    # värdet för punkten
    output$grafKI<- renderPlot({
      min <- input$boxKI - 15
      max <- input$boxKI + 20
      seq <-  seq(from=min, to=max, by=1)
      # seq <-  seq(from=0, to=15, by=0.1)
      # All data 
      xKI <- seq
      yKI <- tableKI()
      dataKI <- data.frame(xKI,yKI)
      
      # Det valda värdet 
      punkt <- hyraFunction(
        pris=input$boxPris,
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP ,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      dataKI <- dataKI %>% mutate(ToHighlight = ifelse(xKI == input$boxKI, "yes", "no"))
      
      print(dataKI)
      
      # X axeln stannar på 0 
      if (min < 0) {
        xaxel <- c(0,max+1)
      }
      else {
        xaxel <- c(min,max)
      }
      # limits=xaxel
      
      # Graf
      p <-   ggplot(data=dataKI, aes(x=xKI, y = yKI, fill = ToHighlight)) +
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
        scale_x_continuous("Kontantinsats",  labels = format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)) + 
        scale_fill_manual(values = c( "yes"="#E66100", "no"="gray83"), guide = FALSE) +
        geom_label(data=dataKI %>% filter(ToHighlight=="yes"), aes(x=xKI,y=yKI),
                   label=paste(punkt, "kr", sep=" ") , 
                   position = position_stack(vjust = 1.2), 
                   color = "black", 
                   fill="white")
      
      # Print the plot
      print(p)
    })
    
    
    #########################
    # Husprisökning
    #########################
    
    tableDeltaP <- reactive({
      # Sequence 
      min <- input$boxDeltaP - 10
      max <- input$boxDeltaP + 10
      seq <-  seq(from=min+0.5, to=max, by=0.5)
      
      # Första värdet 
      rowDeltaP <- hyraFunction(
        pris=input$boxPris, 
        tid=input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=min,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      
      # alla värden 
      for (i in seq) {
        balDeltaP <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=i,
          rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowDeltaP <- rbind(rowDeltaP,balDeltaP)
      }
      print(rowDeltaP[,1])
    })
    
    # värdet för punkten
    output$grafDeltaP<- renderPlot({
      min <- input$boxDeltaP - 10
      max <- input$boxDeltaP + 10
      seq <-  seq(from=min, to=max, by=0.5)
      # seq <-  seq(from=0, to=15, by=0.1)
      # All data 
      xDeltaP <- seq
      yDeltaP <- tableDeltaP()
      dataDeltaP <- data.frame(xDeltaP,yDeltaP)
      
      # Det valda värdet 
      punkt <- hyraFunction(
        pris=input$boxPris,
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      dataDeltaP <- dataDeltaP %>% mutate(ToHighlight = ifelse(xDeltaP == input$boxDeltaP, "yes", "no"))
      
      print(dataDeltaP)
      
      # X axeln stannar på 0 
      if (min < 0) {
        xaxel <- c(0,max+1)
      }
      else {
        xaxel <- c(min,max)
      }
      # limits=xaxel
      
      # Graf
      p <-   ggplot(data=dataDeltaP, aes(x=xDeltaP, y = yDeltaP, fill = ToHighlight)) +
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
        scale_x_continuous("Husprisökning per år",  labels = format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)) + 
        scale_fill_manual(values = c( "yes"="#E66100", "no"="gray83"), guide = FALSE) +
        geom_label(data=dataDeltaP %>% filter(ToHighlight=="yes"), aes(x=xDeltaP,y=yDeltaP),
                   label=paste(punkt, "kr", sep=" ") , 
                   position = position_stack(vjust = 1.2), 
                   color = "black", 
                   fill="white")
      
      # Print the plot
      print(p)
    })
    
    
    #########################
    # Hyresökning
    #########################
    
    tableDeltaHyra <- reactive({
      # Sequence 
      min <- input$boxDeltaRent - 10 
      max <- input$boxDeltaRent + 10
      seq <-  seq(from=min+0.5, to=max, by=0.5)
      
      # Första värdet 
      rowDeltaHyra <- hyraFunction(
        pris=input$boxPris, 
        tid=input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=min, deltaPris=input$boxDeltaP,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      
      # alla värden 
      for (i in seq) {
        balDeltaHyra <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= input$BoxAmort, deltaHyra=i, deltaPris=input$boxDeltaP,
          rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowDeltaHyra <- rbind(rowDeltaHyra,balDeltaHyra)
      }
      print(rowDeltaHyra[,1])
    })
    
    # värdet för punkten
    output$grafDeltaHyra<- renderPlot({
      min <- input$boxDeltaRent - 10 
      max <- input$boxDeltaRent + 10
      seq <-  seq(from=min, to=max, by=0.5)

            # All data 
      xDeltaHyra <- seq
      yDeltaHyra <- tableDeltaHyra()
      dataDeltaHyra <- data.frame(xDeltaHyra,yDeltaHyra)
      
      # Det valda värdet 
      punkt <- hyraFunction(
        pris=input$boxPris,
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      dataDeltaHyra <- dataDeltaHyra %>% mutate(ToHighlight = ifelse(xDeltaHyra == input$boxDeltaRent, "yes", "no"))
      
      print(dataDeltaHyra)
      
      # X axeln stannar på 0 
      if (min < 0) {
        xaxel <- c(0,max+1)
      }
      else {
        xaxel <- c(min,max)
      }

      # Graf
      p <-   ggplot(data=dataDeltaHyra, aes(x=xDeltaHyra, y = yDeltaHyra, fill = ToHighlight)) +
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
        scale_x_continuous("Hyresökning per år",  labels = format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)) + 
        scale_fill_manual(values = c( "yes"="#E66100", "no"="gray83"), guide = FALSE) +
        geom_label(data=dataDeltaHyra %>% filter(ToHighlight=="yes"), aes(x=xDeltaHyra,y=yDeltaHyra),
                   label=paste(punkt, "kr", sep=" ") , 
                   position = position_stack(vjust = 1.2), 
                   color = "black", 
                   fill="white")
      
      # Print the plot
      print(p)
    })
    
    
    #########################
    # Avkastning investeringar 
    #########################
    
    tableDeltaSM <- reactive({
      # Sequence 
      min <- input$boxDeltaSM -10
      max <- input$boxDeltaSM +10
      seq <-  seq(from=min+0.5, to=max, by=0.5)
      
      # Första värdet 
      rowDeltaSM  <- hyraFunction(
        pris=input$boxPris,
        tid=input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP,
        rstocks=min, avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      
      # alla värden 
      for (i in seq) {
        balDeltaSM  <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP,
          rstocks=i, avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowDeltaSM  <- rbind(rowDeltaSM ,balDeltaSM )
      }
      print(rowDeltaSM[,1])
    })
    
    # värdet för punkten
    output$grafDeltaSM <- renderPlot({
      min <- input$boxDeltaSM -10
      max <- input$boxDeltaSM +10
      seq <-  seq(from=min, to=max, by=0.5)
      # seq <-  seq(from=0, to=15, by=0.1)
      # All data 
      xDeltaSM <- seq
      yDeltaSM <- tableDeltaSM()
      dataDeltaSM <- data.frame(xDeltaSM,yDeltaSM)
      
      # Det valda värdet 
      punkt <- hyraFunction(
        pris=input$boxPris,
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      dataDeltaSM <- dataDeltaSM %>% mutate(ToHighlight = ifelse(xDeltaSM == input$boxDeltaSM, "yes", "no"))
      
      print(dataDeltaSM)
      
      
      # Graf
      p <-   ggplot(data=dataDeltaSM, aes(x=xDeltaSM, y = yDeltaSM, fill = ToHighlight)) +
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
        scale_x_continuous("Avkastning på investeringar",  labels = format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)) + 
        scale_fill_manual(values = c( "yes"="#E66100", "no"="gray83"), guide = FALSE) +
        geom_label(data=dataDeltaSM %>% filter(ToHighlight=="yes"), aes(x=xDeltaSM,y=yDeltaSM),
                   label=paste(punkt, "kr", sep=" ") , 
                   position = position_stack(vjust = 1.2), 
                   color = "black", 
                   fill="white")
      
      # Print the plot
      print(p)
    })
    
    ###########################
    # Sammanlagda kostnader för avgifter, försäkring och andra kostnader 
      # Renoveringar påverkar också skatten på vinsten, så den är inte med här
    ###########################
    
    tableKostnader <- reactive({
      
    sammanlagt <- input$boxAvgift + input$boxFörsäkring + input$boxAndraKöpa 
      
      # Sequence Kostnader
      min <- sammanlagt -  7500 
      max <- sammanlagt +  7500
      seq <-  seq(from=min+500, to=max, by=500)
      
      # Första värdet 
      rowKostnader  <- hyraFunction(
        pris=input$boxPris,
        tid=input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP,
        rstocks=input$boxDeltaSM, avgift= min, renovering=input$boxReno,
        forsakring= 0, andrakopa= 0, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      
      # alla värden 
      for (i in seq) {
        balKostnader  <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP,
          rstocks=input$boxDeltaSM, avgift= i, renovering=input$boxReno ,
          forsakring= 0, andrakopa= 0, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowKostnader  <- rbind(rowKostnader,balKostnader)
      }
      print(rowKostnader[,1])
    })
    
    # värdet för punkten
    output$grafKostnader <- renderPlot({
      
      
      sammanlagt <- input$boxAvgift + input$boxFörsäkring + input$boxAndraKöpa
      
      min <- sammanlagt -  7500 
      max <- sammanlagt +  7500
      seq <-  seq(from=min, to=max, by=500)
      # seq <-  seq(from=0, to=15, by=0.1)
      # All data 
      xKostnader <- seq
      yKostnader <- tableKostnader()
      dataKostnader <- data.frame(xKostnader,yKostnader)
      
      # Det valda värdet 
      punkt <- hyraFunction(
        pris=input$boxPris,
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      dataKostnader <- dataKostnader %>% mutate(ToHighlight = ifelse(xKostnader == sammanlagt, "yes", "no"))
      
      print(dataKostnader)
      
      
      # Graf
      p <-   ggplot(data=dataKostnader, aes(x=xKostnader, y = yKostnader, fill = ToHighlight)) +
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
        scale_x_continuous("Sammanlagda kostnader",  labels = format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)) + 
        scale_fill_manual(values = c( "yes"="#E66100", "no"="gray83"), guide = FALSE) +
        geom_label(data=dataKostnader %>% filter(ToHighlight=="yes"), aes(x=xKostnader,y=yKostnader),
                   label=paste(punkt, "kr", sep=" ") , 
                   position = position_stack(vjust = 1.2), 
                   color = "black", 
                   fill="white")
      
      # Print the plot
      print(p)
    })
    

    
    ######################
    # Renoveringar 
    #####################
    
    tableReno <- reactive({
    
      # Sequence
      min <- 0
      max <- 20
      seq <-  seq(from=min+1, to=max, by=1)
      
      # Första värdet 
      rowReno <- hyraFunction(
        pris=input$boxPris,
        tid=input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=min,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      
      # alla värden 
      for (i in seq) {
        balReno <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP,
          rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=i,
          forsakring=  input$boxFörsäkring, andrakopa=  input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowReno  <- rbind(rowReno,balReno)
      }
      print(rowReno[,1])
    })
    
    # värdet för punkten
    output$grafReno <- renderPlot({

      min <- 0
      max <- 20
      seq <-  seq(from=min, to=max, by=1)
      
      # All data 
      xReno <- seq
      yReno <- tableReno()
      dataReno <- data.frame(xReno,yReno)
      
      # Det valda värdet 
      punkt <- hyraFunction(
        pris=input$boxPris,
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= input$BoxAmort, deltaHyra=input$boxDeltaRent, deltaPris=input$boxDeltaP,
        rstocks=input$boxDeltaSM, avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      dataReno <- dataReno %>% mutate(ToHighlight = ifelse(xReno == input$boxReno, "yes", "no"))
      
      print(dataReno)
      
      
      # Graf
      p <-   ggplot(data=dataReno, aes(x=xReno, y = yReno, fill = ToHighlight)) +
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
        scale_x_continuous("Renoveringar",  labels = format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)) + 
        scale_fill_manual(values = c( "yes"="#E66100", "no"="gray83"), guide = FALSE) +
        geom_label(data=dataReno %>% filter(ToHighlight=="yes"), aes(x=xReno,y=yReno),
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
                pris*ki + input$boxFlytt  
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
          if(input$boxKI == 100) {
           round(0, digit=0)
          }
          else {
          if(skuldsättning_slut>0) {
            ränta*(skuld*tid - skuld*(amortering*(tid-1)*tid/2))
          }
          else {
            tid <- skuld/(amortering*skuld)
            ränta*(skuld*tid - skuld*(amortering*(tid-1)*tid/2))
          }
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
          räntaSum() + renoveringar() + (försäkring+andra+avgifter)*tid
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
          betalningar <- input$BoxAmort * skuld / 100+
                      (input$boxAndraKöpa + input$boxFörsäkring + input$boxAvgift)*12+
                      input$boxReno*input$boxPris/100
           
            (kontantinsats_kr+input$boxFlytt)*(1+r_stocks)^tid-kontantinsats_kr -input$boxFlytt+ (betalningar*(((1 + r_stocks)^(tid) - 1) / (r_stocks))) - betalningar*tid
          })
        
        ################################
        # Vinster för investeringar
        ################################
        
        formel_husprisvinst <-  reactive({
          tid <- input$boxTid
          pris <- input$boxPris
          ki <- input$boxKI /100
          skuld <-pris *(1-ki)
          husprisökning <- input$boxDeltaP / 100 
          prisökning <- pris*(1+husprisökning)^tid-pris
       -1*(prisökning - (prisökning - renoveringar())*22/30*0.3 + ki*pris)
        })
    
        
        
        #########################################
        # Sammanställning kostnader för att köpa
        #########################################
        
        formel_totala_kostnader <- reactive({
          formel_initiala_köpa() + formel_totala_återkommande() + formel_alternativkostnad() + formel_husprisvinst()
        })
        
        
        ################################
        # Hyra - 
        ################################
        
        # Hyreskostnader som ger samma totalt kostnad 
        minHyra <- reactive({
          r_stocks <- input$boxDeltaSM / 100
          hyresökning <- (input$boxDeltaRent/100)
          tid <- input$boxTid
          deposition <- input$boxDeposition
          AndraHyra <- input$boxAndraHyra * tid *12 
          (formel_totala_kostnader() - AndraHyra)*(r_stocks - hyresökning)/((1+r_stocks)*((1+r_stocks)^tid-(1+hyresökning)^tid)+(r_stocks-hyresökning)*deposition/12*(1+r_stocks)^tid-(r_stocks-hyresökning)*deposition/12)/12
        })
  
        # Totala hyreskostader
        sumHyra <- reactive({
          hyresökning <- (1+input$boxDeltaRent/100)
          tid <- input$boxTid
          AndraHyra <- input$boxAndraHyra
          # Formel 
          minHyra()*12 * ((1-hyresökning^tid)/(1-hyresökning)) + AndraHyra*tid*12
        })
        
        # initiala hyra 
        initialaHyra <- reactive({
          minHyra() * input$boxDeposition
        })
        
        # Alternativkostnader för att hyra 
        alternativHyra <- reactive({
          hyresökning <- input$boxDeltaRent/100
          tid <- input$boxTid
          avkastning <- input$boxDeltaSM / 100
          hyra <- 12* minHyra()
          depositionstid <- input$boxDeposition
          (hyra * (1 + avkastning) * (-1 + (1 + avkastning))* ((1 + avkastning)^tid - (1 + hyresökning)^tid))/ + 
            (avkastning*(-1+(1+avkastning)-hyresökning))- hyra * ((1-(1+hyresökning)^tid)/(1-(1+hyresökning)))  + 
            depositionstid*minHyra()*(1+avkastning)^tid - minHyra()
            # 
        })
        
        # Vinster för att hyra 
        vinsterHyra <- reactive({
          -1*initialaHyra()
        })
        
        # Totalt för att hyra 
        totaltHyra <- reactive({
          initialaHyra() +sumHyra() + alternativHyra() + vinsterHyra()
        })
        
        
        ################################ 
        # Grafer
        ################################
        
        # bostadspriser och motsvarande hyra 
       
        
        
          
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
          
          
      ################################ 
      # Jämförelse hyra/köpa 
      ################################ 
      table <- reactive({
      
      # Data 
      tabledata <-matrix(cbind(
           # Köpa 
           formatnummer(formel_initiala_köpa()),
           formatnummer(formel_totala_återkommande()),
           formatnummer(formel_alternativkostnad()), 
           formatnummer(formel_husprisvinst()),
           formatnummer(formel_totala_kostnader()),
           # Hyra 
           formatnummer(initialaHyra()),
           formatnummer(sumHyra()),
           formatnummer(alternativHyra()), 
           formatnummer(vinsterHyra()),
           formatnummer(totaltHyra())), 
           ncol=2) 
      
      # Namn på matris
      rownames(tabledata) <- c("Initiala kostnader   ", "Löpande kostnader   ", "Alternativkostnader   ", 
                               "Vinster   ","<b>Totala kostnader</b>   ")
      colnames(tabledata) <- c("Köpa","Hyra")
      
      HTML(
        tableHTML(tabledata, 
                  widths = rep(100, 3),
                  collapse = 'separate_shiny', 
                  spacing = '4px', 
                  border=0
                  )
        
              )
            })
        
          
          output$filetable <- renderUI({table()
          })

        # Titeltext
        output$sumkostnader <- renderText({
                paste("<b>Kostnader efter ", input$boxTid, "år")
        })
          
      
        
        
        ################################ 
        # texter 
        ################################ 
        
      
        
        # Bättre att hyra om: 
        output$hyrabättre1 <- renderText({
          paste("<h4><b>Om du kan hyra en liknande bostad för mindre än:</h4><p>",
                "<boxed><center><h3><font color='#E66100'>",formatnummer(minHyra())," kr per månad </h4></font></center></boxed><p>",
                "<h4>är det bättre att hyra</b></h4>"
                )
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
    
        
})




