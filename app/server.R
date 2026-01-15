# Format function that rounds to integers and formats with Swedish number format
formatnummer <- function(x) {
  format_format(big.mark = " ", decimal.mark = ",", scientific = FALSE)(round(x, digits = 0))
}

shinyServer(function(input, output, session) {

  ################################
  # Reactive defaults for conditional inputs
  ################################
  # These inputs only exist when framtiden checkbox is checked
  # Provide default values when they are NULL
  
  deltaPris_val <- reactive({
    if (is.null(input$boxDeltaP)) 3 else input$boxDeltaP
  })
  
  deltaHyra_val <- reactive({
    if (is.null(input$boxDeltaRent)) 2 else input$boxDeltaRent
  })
  
  deltaSM_val <- reactive({
    if (is.null(input$boxDeltaSM)) 7 else input$boxDeltaSM
  })
  
  # BoxAmort has conditional panels based on down payment (boxKI)
  # Provide default values when switching between panels
  BoxAmort_val <- reactive({
    # If BoxAmort input exists and is not NULL, use it
    if (!is.null(input$BoxAmort)) {
      return(input$BoxAmort)
    }

    # BoxAmort is NULL, provide default based on boxKI
    ki <- input$boxKI

    # If boxKI is also NULL (app just starting), return default for 15% down payment
    if (is.null(ki)) {
      return(2)  # Default matches boxKI default of 15% (which is < 30)
    }

    # Return appropriate default based on actual boxKI value
    if (ki < 30) {
      return(2)
    } else if (ki < 50) {
      return(1)
    } else {
      return(0)
    }
  })

  # Render amortization slider dynamically based on boxKI
  output$amortSlider <- renderUI({
    ki <- input$boxKI

    # Set slider parameters based on down payment
    if (is.null(ki) || ki < 30) {
      # High LTV (>70%): min 2% amortization
      tagList(
        sliderInput("BoxAmort", "Amortering", value = 2, min = 2, max = 15,
                    step = 0.5, ticks = FALSE, width = "95%", post = " %"),
        numericInput("BoxAmort_num", "Skriv amortering (%)", value = 2, min = 2, max = 15, step = 0.5)
      )
    } else if (ki < 50) {
      # Medium LTV (50-70%): min 1% amortization
      tagList(
        sliderInput("BoxAmort", "Amortering", value = 1, min = 1, max = 15,
                    step = 0.5, ticks = FALSE, width = "95%", post = " %"),
        numericInput("BoxAmort_num", "Skriv amortering (%)", value = 1, min = 1, max = 15, step = 0.5)
      )
    } else {
      # Low LTV (<50%): no minimum amortization
      tagList(
        sliderInput("BoxAmort", "Amortering", value = 0, min = 0, max = 15,
                    step = 0.5, ticks = FALSE, width = "95%", post = " %"),
        numericInput("BoxAmort_num", "Skriv amortering (%)", value = 0, min = 0, max = 15, step = 0.5)
      )
    }
  })

  syncNumericSlider <- function(slider_id, numeric_id) {
    observeEvent(input[[slider_id]], {
      if (!is.null(input[[numeric_id]]) && !identical(input[[numeric_id]], input[[slider_id]])) {
        updateNumericInput(session, numeric_id, value = input[[slider_id]])
      }
    }, ignoreInit = TRUE)

    observeEvent(input[[numeric_id]], {
      if (!is.null(input[[numeric_id]]) && !identical(input[[numeric_id]], input[[slider_id]])) {
        updateSliderInput(session, slider_id, value = input[[numeric_id]])
      }
    }, ignoreInit = TRUE)
  }

  syncNumericSlider("boxPris", "boxPris_num")
  syncNumericSlider("boxTid", "boxTid_num")
  syncNumericSlider("boxR", "boxR_num")
  syncNumericSlider("boxKI", "boxKI_num")
  syncNumericSlider("boxInc", "boxInc_num")
  syncNumericSlider("BoxAmort", "BoxAmort_num")
  syncNumericSlider("boxDeltaP", "boxDeltaP_num")
  syncNumericSlider("boxDeltaRent", "boxDeltaRent_num")
  syncNumericSlider("boxDeltaSM", "boxDeltaSM_num")
  syncNumericSlider("boxAvgift", "boxAvgift_num")
  syncNumericSlider("boxFörsäkring", "boxFörsäkring_num")
  syncNumericSlider("boxAndraKöpa", "boxAndraKöpa_num")
  syncNumericSlider("boxReno", "boxReno_num")
  syncNumericSlider("boxFlytt", "boxFlytt_num")
  syncNumericSlider("boxAndraHyra", "boxAndraHyra_num")
  syncNumericSlider("boxDeposition", "boxDeposition_num")

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
    # NOTE: 'inkomst' parameter is currently unused but reserved for future calculations
    # such as loan approval rules or income-based amortization requirements (andra amorteringskravet)

    # Definera variabler
    ki <- kontantinsats/ 100
    skuld <- pris*(1-ki)
    ränta <- ranta* 0.7 / 100
    r_stocks <- rstocks / 100
    betalningar <- amortering / 100 * skuld
    totalabetalningar <- amortering * skuld / 100+
                        (andrakopa + forsakring + avgift)*12+
                        renovering*pris/100 
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
    # Capital gains from house price appreciation
    totalamortering <- skuld*amortering_2*tid
    prisökning <- pris*(1+husprisökning)^tid-pris
    # Tax calculation: 22/30 of gain is taxable at 30% rate
    # Renovations reduce the taxable gain (improvements add to cost basis)
    # Down payment is recovered tax-free
    husprisvinst <- prisökning - (prisökning - renoveringar_2)*22/30*0.3 + ki*pris
    
    # Sammanlagt 
    totala_kostnader <- intiala_köpa + totala_återkommande + alternativkostnad - husprisvinst #Funkar! 


  # Motsvarande hyra (Equivalent monthly rent)
  # This calculates what monthly rent would result in the same total cost as buying
  # The formula accounts for:
  #   - Different growth rates between rent and investments
  #   - Deposit opportunity cost
  #   - Time value of money
  #
  # Check for division by zero when investment return equals rent increase
  if (abs(r_stocks - hyresökning) < 0.0001) {
    # When rates are equal, use simplified formula (average cost per month)
    round((totala_kostnader - totandrahyra) / (tid * 12), digits=0)
  } else {
    # Standard formula when rates differ
    # Denominator includes: rent FV terms + deposit opportunity cost
    round((totala_kostnader- totandrahyra)*(r_stocks - hyresökning)/
        (12*(1+r_stocks)*((1+r_stocks)^tid-(1+hyresökning)^tid) + deposition*((1+r_stocks)^tid - 1)*(r_stocks - hyresökning))
    ,digits=0)
  }

    }

  ###############################
  # Bostadspris - graf 
  ###############################
  
   tablePris <- reactive({
    min <- input$boxPris - 1000000
    max <- input$boxPris + 1000000
    seq <-  seq(from=min+50000, to=max, by=50000)
  
   # Start with first value from sequence (not min)
    rowPris <- c()
    for (i in seq) {
     balPris <- hyraFunction(
        pris=i, 
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val() ,
        rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno ,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=  input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
     rowPris <- c(rowPris, balPris)
    }
    rowPris  # Return as vector
  })
   
   # värdet för punkten
    output$grafPris <- renderPlot({

     min <- input$boxPris - 1000000
     max <- input$boxPris + 1000000
     seq <-  seq(from=min+50000, to=max, by=50000)  # Match tablePris

    # All data 
    xValue <- seq
    yValue <- tablePris()
    data <- data.frame(xValue,yValue)
    
    # Det valda värdet 
    punktPris  <- hyraFunction(
      pris=input$boxPris,
      tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
      amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val() ,
      rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno ,
      forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
      flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
    )     
    
    data <- data %>% mutate(ToHighlight = ifelse(xValue == input$boxPris, "yes", "no"))
    
    # print(data)  # Debug output
    
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
      # Build vector for all values 1:50
      rowTid <- c()
      for (i in 1:50) {
        balTid <- hyraFunction(
          pris=input$boxPris,
          tid= i, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val() ,
          rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowTid <- c(rowTid, balTid)
      }
      rowTid  # Return as vector
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
        amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val() ,
        rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      
      dataTid <- dataTid %>% mutate(ToHighlight = ifelse(xTid == input$boxTid, "yes", "no"))
      
      # print(data)  # Debug output
      
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

      # Start with empty vector and build from sequence
      rowR <- c()
      for (i in seq) {
        balR <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=i, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val() ,
          rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowR <- c(rowR, balR)
      }
      rowR  # Return as vector
    })
    
    # värdet för punkten
    output$grafRanta <- renderPlot({
      min <- input$boxR - 2
      max <- input$boxR + 2
      seq <-  seq(from=min+0.1, to=max, by=0.1)  # Match tableRanta
      # seq <-  seq(from=0, to=15, by=0.1)
      # All data 
      xR <- seq
      yR <- tableRanta()
      dataR <- data.frame(xR,yR)
      
      # Det valda värdet 
      punkt <- hyraFunction(
        pris=input$boxPris,
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val() ,
        rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
          dataR <- dataR %>% mutate(ToHighlight = ifelse(xR == input$boxR, "yes", "no"))
      
      # print(dataR)  # Debug output
      
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
      min <- BoxAmort_val() - 2
      max <- BoxAmort_val() + 2
      seq <-  seq(from=min+0.1, to=max, by=0.1)
      
      # Start with empty vector and build from sequence
      rowAmort <- c()
      for (i in seq) {
        balAmort <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= i, deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val() ,
          rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowAmort <- c(rowAmort, balAmort)
      }
      rowAmort  # Return as vector
    })
    
    # värdet för punkten
    output$grafAmort <- renderPlot({
      min <- BoxAmort_val() - 2
      max <- BoxAmort_val() + 2
      seq <-  seq(from=min+0.1, to=max, by=0.1)  # Match the table sequence
      # seq <-  seq(from=0, to=15, by=0.1)
      # All data 
      xAmort <- seq
      yAmort <- tableAmort()
      dataAmort <- data.frame(xAmort,yAmort)
      
      # Det valda värdet 
      punkt <- hyraFunction(
        pris=input$boxPris,
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val() ,
        rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      dataAmort <- dataAmort %>% mutate(ToHighlight = ifelse(xAmort == BoxAmort_val(), "yes", "no"))
      
      # print(dataAmort)  # Debug output
      
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
      # Sequence 
      min <- input$boxKI - 15
      max <- input$boxKI + 20
      seq <-  seq(from=min, to=max-1, by=1)
      
      # Start with empty vector and build from sequence
      rowKI <- c()
      for (i in seq) {
        balKI <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=input$boxR, kontantinsats= i, inkomst= input$boxInc,
          amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val() ,
          rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowKI <- c(rowKI, balKI)
      }
      rowKI  # Return as vector
    })
    
    # värdet för punkten
    output$grafKI<- renderPlot({
      min <- input$boxKI - 15
      max <- input$boxKI + 20
      seq <-  seq(from=min, to=max-1, by=1)  # Match the table sequence
      # seq <-  seq(from=0, to=15, by=0.1)
      # All data 
      xKI <- seq
      yKI <- tableKI()
      dataKI <- data.frame(xKI,yKI)
      
      # Det valda värdet 
      punkt <- hyraFunction(
        pris=input$boxPris,
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val() ,
        rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      dataKI <- dataKI %>% mutate(ToHighlight = ifelse(xKI == input$boxKI, "yes", "no"))
      
      # print(dataKI)  # Debug output
      
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
      min <- max(0, deltaPris_val() - 10)  # Don't allow negative percentages
      max <- deltaPris_val() + 10
      seq <-  seq(from=min, to=max, by=0.5)
      
      # Start with empty vector and build from sequence
      rowDeltaP <- c()
      for (i in seq) {
        balDeltaP <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=i,
          rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowDeltaP <- c(rowDeltaP, balDeltaP)
      }
      rowDeltaP  # Return as vector
    })
    
    # värdet för punkten
    output$grafDeltaP<- renderPlot({
      min <- max(0, deltaPris_val() - 10)  # Match the table sequence
      max <- deltaPris_val() + 10
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
        amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val(),
        rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      dataDeltaP <- dataDeltaP %>% mutate(ToHighlight = ifelse(xDeltaP == deltaPris_val(), "yes", "no"))
      
      # print(dataDeltaP)  # Debug output
      
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
      min <- max(0, deltaHyra_val() - 10)  # Don't allow negative percentages
      max <- deltaHyra_val() + 10
      seq <-  seq(from=min, to=max, by=0.5)
      
      # Start with empty vector and build from sequence
      rowDeltaHyra <- c()
      for (i in seq) {
        balDeltaHyra <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= BoxAmort_val(), deltaHyra=i, deltaPris=deltaPris_val(),
          rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowDeltaHyra <- c(rowDeltaHyra, balDeltaHyra)
      }
      rowDeltaHyra  # Return as vector
    })
    
    # värdet för punkten
    output$grafDeltaHyra<- renderPlot({
      min <- max(0, deltaHyra_val() - 10)  # Match the table sequence
      max <- deltaHyra_val() + 10
      seq <-  seq(from=min, to=max, by=0.5)

            # All data 
      xDeltaHyra <- seq
      yDeltaHyra <- tableDeltaHyra()
      dataDeltaHyra <- data.frame(xDeltaHyra,yDeltaHyra)
      
      # Det valda värdet 
      punkt <- hyraFunction(
        pris=input$boxPris,
        tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
        amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val(),
        rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      dataDeltaHyra <- dataDeltaHyra %>% mutate(ToHighlight = ifelse(xDeltaHyra == deltaHyra_val(), "yes", "no"))
      
      # print(dataDeltaHyra)  # Debug output
      
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
      min <- max(0, deltaSM_val() - 10)  # Don't allow negative percentages
      max <- deltaSM_val() + 10
      seq <-  seq(from=min, to=max, by=0.5)
      
      # Start with empty vector and build from sequence
      rowDeltaSM <- c()
      for (i in seq) {
        balDeltaSM  <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val(),
          rstocks=i, avgift= input$boxAvgift, renovering=input$boxReno ,
          forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowDeltaSM <- c(rowDeltaSM, balDeltaSM)
      }
      rowDeltaSM  # Return as vector
    })
    
    # värdet för punkten
    output$grafDeltaSM <- renderPlot({
      min <- max(0, deltaSM_val() - 10)  # Match the table sequence
      max <- deltaSM_val() + 10
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
        amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val(),
        rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      dataDeltaSM <- dataDeltaSM %>% mutate(ToHighlight = ifelse(xDeltaSM == deltaSM_val(), "yes", "no"))
      
      # print(dataDeltaSM)  # Debug output
      
      
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
      min <- max(0, sammanlagt - 7500)  # Don't allow negative costs
      max <- sammanlagt + 7500
      seq <-  seq(from=min, to=max, by=500)
      
      # Start with empty vector and build from sequence
      rowKostnader <- c()
      for (i in seq) {
        balKostnader  <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val(),
          rstocks=deltaSM_val(), avgift= i, renovering=input$boxReno ,
          forsakring= 0, andrakopa= 0, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowKostnader <- c(rowKostnader, balKostnader)
      }
      rowKostnader  # Return as vector
    })
    
    # värdet för punkten
    output$grafKostnader <- renderPlot({
      
      
      sammanlagt <- input$boxAvgift + input$boxFörsäkring + input$boxAndraKöpa

      min <- max(0, sammanlagt - 7500)  # Match the table sequence
      max <- sammanlagt + 7500
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
        amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val(),
        rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      dataKostnader <- dataKostnader %>% mutate(ToHighlight = ifelse(xKostnader == sammanlagt, "yes", "no"))
      
      # print(dataKostnader)  # Debug output
      
      
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
      seq <-  seq(from=min, to=max, by=1)
      
      # Start with empty vector and build from sequence
      rowReno <- c()
      for (i in seq) {
        balReno <- hyraFunction(
          pris=input$boxPris,
          tid= input$boxTid, ranta=input$boxR, kontantinsats= input$boxKI, inkomst= input$boxInc,
          amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val(),
          rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=i,
          forsakring=  input$boxFörsäkring, andrakopa=  input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
          flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
        )     
        rowReno <- c(rowReno, balReno)
      }
      rowReno  # Return as vector
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
        amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val(),
        rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno,
        forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=input$boxAndraHyra,
        flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
      )     
      dataReno <- dataReno %>% mutate(ToHighlight = ifelse(xReno == input$boxReno, "yes", "no"))
      
      # print(dataReno)  # Debug output
      
      
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
          amortering <- BoxAmort_val() / 100 
          skuld <- pris*(1-ki)
          ränta <- input$boxR*0.7  / 100 # ränte gånger 1-skatteavdrag 
          df  <- skuld*ränta
          for (i in 1:tid2) {
            bal <- skuld*(1-amortering*i)*ränta
            df <- rbind(df,bal)
          }
          # print(df[,1])  # Debug output
        })

        ## Totala räntekostnader
      räntaSum <- reactive({
        skuld <- input$boxPris*(1-input$boxKI/100)
        tid <- input$boxTid
        amortering <- BoxAmort_val()/ 100
        ränta <- input$boxR * 0.7 / 100
        skuldsättning_slut <- (1-input$boxKI/100) - tid*amortering 
        
        if(input$boxKI == 100) {
          round(0, digits=0)
        }
        else {
          if(amortering == 0) {
            # No amortization: interest on full loan amount for entire period
            ränta * skuld * tid
          }
          else if(skuldsättning_slut > 0) {
            # Loan not fully paid off: calculate interest with amortization
            ränta*(skuld*tid - skuld*(amortering*(tid-1)*tid/2))
          }
          else {
            # Loan fully paid off before end of period
            tid_alt <- skuld/(amortering*skuld)  # = 1/amortering
            ränta*(skuld*tid_alt - skuld*(amortering*(tid_alt-1)*tid_alt/2))
          }
        }
      })


        # Renoveringar - procent av ursprunliga priset gånger priset  gånger tid 
        renoveringar <- reactive({
          input$boxReno/100 * input$boxPris * input$boxTid
        })
        
        # totala återkommande kostnader 
        formel_totala_återkommande <- reactive({
          tid <- input$boxTid
          försäkring <- input$boxFörsäkring * 12
          avgifter <- input$boxAvgift * 12
          andra <- input$boxAndraKöpa * 12
          räntaSum() + renoveringar() + (försäkring+avgifter+andra)*tid
        })
        
        ################################
        # Alternativkostnader
        ################################
        
        # kontantinsats + amorteringar investerat istället
          # Nu räknas det som att betalningar sparas i slutet av året.
        
        # compound interest formula with contributions 
        formel_alternativkostnad <- reactive({
          kontantinsats_kr <- input$boxKI / 100*input$boxPris
          skuld <- input$boxPris - kontantinsats_kr
          r_stocks <- deltaSM_val() / 100
          tid <- input$boxTid
          betalningar <- BoxAmort_val() * skuld / 100+
                      (input$boxAndraKöpa + input$boxFörsäkring + input$boxAvgift)*12+
                      input$boxReno*input$boxPris/100
           
            (kontantinsats_kr+input$boxFlytt)*(1+r_stocks)^tid-kontantinsats_kr -input$boxFlytt + (betalningar*(((1 + r_stocks)^(tid) - 1) / (r_stocks))) - betalningar*tid
          })
        
        ################################
        # Vinster för investeringar
        ################################
        
        formel_husprisvinst <-  reactive({
          tid <- input$boxTid
          pris <- input$boxPris
          ki <- input$boxKI /100
          skuld <-pris *(1-ki)
          husprisökning <- deltaPris_val() / 100 
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
          r_stocks <- deltaSM_val() / 100
          hyresökning <- (deltaHyra_val()/100)
          tid <- input$boxTid
          deposition <- input$boxDeposition
          AndraHyra <- input$boxAndraHyra * tid *12

          # Check for division by zero when investment return equals rent increase
          if (abs(r_stocks - hyresökning) < 0.0001) {
            # When rates are equal, use simplified formula (average cost per month)
            round((formel_totala_kostnader() - AndraHyra) / (tid * 12), digits=0)
          } else {
            # Standard formula when rates differ
            # Denominator includes: rent FV terms + deposit opportunity cost
            (formel_totala_kostnader() - AndraHyra)*(r_stocks - hyresökning)/((1+r_stocks)*((1+r_stocks)^tid-(1+hyresökning)^tid)+(r_stocks-hyresökning)*deposition/12*(1+r_stocks)^tid-(r_stocks-hyresökning)*deposition/12)/12
          }
        })
  
        # Totala hyreskostader
        sumHyra <- reactive({
          hyresökning <- (1+deltaHyra_val()/100)
          tid <- input$boxTid
          AndraHyra <- input$boxAndraHyra
          # Formel - check for division by zero when hyresökning == 1
          if (abs(hyresökning - 1) < 0.0001) {
            # When rent increase is 0%, use simple formula
            minHyra()*12*tid + AndraHyra*tid*12
          } else {
            minHyra()*12 * ((1-hyresökning^tid)/(1-hyresökning)) + AndraHyra*tid*12
          }
        })
        
        # initiala hyra 
        initialaHyra <- reactive({
          minHyra() * input$boxDeposition
        })
        
        # Alternativkostnader för att hyra
        alternativHyra <- reactive({
          hyresökning <- deltaHyra_val()/100
          tid <- input$boxTid
          avkastning <- deltaSM_val() / 100
          hyra <- 12* minHyra()
          depositionstid <- input$boxDeposition

          # Check for division by zero when rates are equal or when hyresökning == 0
          if (abs(avkastning - hyresökning) < 0.0001) {
            # When rates are equal, use simplified formula
            hyra * tid * avkastning + depositionstid*minHyra()*((1+avkastning)^tid - 1)
          } else if (abs(hyresökning) < 0.0001) {
            # When rent increase is 0%, use simplified formula
            (hyra * (1 + avkastning) * avkastning * ((1 + avkastning)^tid - 1)) / avkastning - hyra * tid +
              depositionstid*minHyra()*((1+avkastning)^tid - 1)
          } else {
            # Standard formula
            (hyra * (1 + avkastning) * ((1 + avkastning)^tid - (1 + hyresökning)^tid)) / 
              (avkastning - hyresökning) - 
              hyra * ((1-(1+hyresökning)^tid)/(1-(1+hyresökning))) + 
              depositionstid*minHyra()*((1+avkastning)^tid - 1)
          }
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
              amortering= BoxAmort_val(), deltaHyra=deltaHyra_val(), deltaPris=deltaPris_val() ,
              rstocks=deltaSM_val(), avgift= input$boxAvgift, renovering=input$boxReno ,
              forsakring= input$boxFörsäkring, andrakopa= input$boxAndraKöpa, andrahyra=  input$boxAndraHyra,
              flyttkostnader = input$boxFlytt, deposition = input$boxDeposition
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
                               "Vinster   ","<b>Totala kostnader</b>" )
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
        # output$räntekostnader <- renderText({
        #         paste("Räntekostnader",räntaSum(),"kr" )
        # })

        # amorteringar
        # output$amorteringar <- renderText({
        #         paste("Amorteringar ", totamortering(), "kr ")
        # })

        # Fasta kostnader
        # output$fasta <- renderText({
        #         paste("Fasta kostnader ", renoveringar(), "kr ")
        # })
    
        
})




