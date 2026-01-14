# Format function for Swedish number format
formatnummer <- function(x) {
  format(round(x, 0), big.mark = " ", decimal.mark = ",", scientific = FALSE)
}

shinyServer(function(input, output) {

  # Load the calculation function from main app
  source("hyraFunction.R")

  ################################
  # Calculate equivalent rent for a range of amortization levels
  ################################

  amortData <- reactive({
    # Create sequence of amortization levels (fixed: 0% to 10% in 0.5% steps)
    amort_seq <- seq(0, 10, by = 0.5)

    # Calculate equivalent rent for each level
    results <- data.frame(
      amort = amort_seq,
      motsvarande_hyra = sapply(amort_seq, function(a) {
        hyraFunction(
          pris = input$pris,
          tid = input$tid,
          ranta = input$ranta,
          kontantinsats = input$ki,
          inkomst = 0,  # Not used
          amortering = a,
          deltaHyra = input$hyresok,
          deltaPris = input$huspris,
          rstocks = input$avkastning,
          avgift = input$avgift,
          renovering = input$renovering,
          forsakring = input$forsakring,
          andrakopa = input$andra,
          flyttkostnader = input$flytt,
          andrahyra = input$andraHyra,
          deposition = input$deposition
        )
      })
    )

    results
  })

  ################################
  # Calculate wealth for buy vs rent at different amortization levels
  ################################

  wealthData <- reactive({
    # Fixed amortization sequence: 0% to 10% in 0.5% steps
    amort_seq <- seq(0, 10, by = 0.5)

    results <- data.frame(
      amort = amort_seq,
      wealth_buy = numeric(length(amort_seq)),
      wealth_rent = numeric(length(amort_seq))
    )

    for (i in seq_along(amort_seq)) {
      a <- amort_seq[i]

      # Calculate for buying
      ki <- input$ki / 100
      skuld_start <- input$pris * (1 - ki)
      amort_total <- skuld_start * (a / 100) * input$tid
      skuld_slut <- max(0, skuld_start - amort_total)

      # House value after appreciation and tax
      huspris_slut <- input$pris * (1 + input$huspris / 100)^input$tid
      prisökning <- huspris_slut - input$pris
      renovering_total <- input$pris * input$renovering / 100 * input$tid
      skatt <- (prisökning - renovering_total) * 22/30 * 0.3
      hus_netto <- huspris_slut - skatt

      # Buying wealth = net home equity (house value - remaining debt)
      results$wealth_buy[i] <- hus_netto - skuld_slut

      # Variables for rent calculation
      r_stocks <- input$avkastning / 100
      initial_invested <- (input$pris * ki + input$flytt) * (1 + r_stocks)^input$tid

      # Calculate equivalent rent for this amortization
      equiv_rent <- hyraFunction(
        pris = input$pris, tid = input$tid, ranta = input$ranta,
        kontantinsats = input$ki, inkomst = 0, amortering = a,
        deltaHyra = input$hyresok, deltaPris = input$huspris,
        rstocks = input$avkastning, avgift = input$avgift,
        renovering = input$renovering, forsakring = input$forsakring,
        andrakopa = input$andra, flyttkostnader = input$flytt,
        andrahyra = input$andraHyra, deposition = input$deposition
      )

      # When renting, wealth comes from:
      # 1. Down payment that would have gone to buying, invested instead
      # 2. Each year: (buying cash flow - renting cash flow) gets invested

      # Start with down payment invested and grown to final value
      wealth_accumulated <- initial_invested

      # Calculate year-by-year costs and invest the difference
      hyresök_rate <- input$hyresok / 100
      rante_after_tax <- input$ranta * 0.7 / 100
      skuld_remaining <- skuld_start
      amort_yearly <- skuld_start * (a / 100)
      andra_kostnader_yearly <- (input$avgift + input$forsakring + input$andra) * 12
      renovering_yearly <- input$pris * input$renovering / 100

      for (year in 1:input$tid) {
        # Buying cash flow this year
        if (skuld_remaining > 0) {
          interest_this_year <- skuld_remaining * rante_after_tax
          amort_payment <- min(amort_yearly, skuld_remaining)
          skuld_remaining <- skuld_remaining - amort_payment
        } else {
          interest_this_year <- 0
          amort_payment <- 0
        }
        buying_cash_flow <- amort_payment + interest_this_year + andra_kostnader_yearly + renovering_yearly

        # Renting cash flow this year
        rent_this_year <- equiv_rent * 12 * (1 + hyresök_rate)^(year - 1)
        rent_cash_flow <- rent_this_year + input$andraHyra * 12

        # Savings this year (positive if buying costs more, negative if renting costs more)
        savings_this_year <- buying_cash_flow - rent_cash_flow

        # This savings grows for the remaining years
        years_remaining <- input$tid - year
        wealth_accumulated <- wealth_accumulated + savings_this_year * (1 + r_stocks)^years_remaining
      }

      results$wealth_rent[i] <- wealth_accumulated
    }

    results
  })

  # Helper function to calculate interest cost
  calculateInterestCost <- function(pris, tid, ranta, ki, amort) {
    skuld <- pris * (1 - ki / 100)
    rante <- ranta * 0.7 / 100
    amort_dec <- amort / 100

    skuld_slut <- (1 - ki / 100) - tid * amort_dec

    if (ki == 100) {
      return(0)
    } else if (skuld_slut > 0) {
      return(rante * (skuld * tid - skuld * (amort_dec * (tid - 1) * tid / 2)))
    } else {
      tid_alt <- 1 / amort_dec
      return(rante * (skuld * tid_alt - skuld * (amort_dec * (tid_alt - 1) * tid_alt / 2)))
    }
  }

  ################################
  # Detailed calculation for specific amortization level
  ################################

  detailData <- reactive({
    a <- input$amortVald

    # Buy costs
    ki <- input$ki / 100
    initial <- input$pris * ki + input$flytt

    rante <- calculateInterestCost(input$pris, input$tid, input$ranta, input$ki, a)
    renovering <- input$pris * input$renovering / 100 * input$tid
    lopande <- rante + renovering +
      (input$avgift + input$forsakring + input$andra) * 12 * input$tid

    r_stocks <- input$avkastning / 100
    skuld <- input$pris * (1 - ki)
    betalningar_ar <- skuld * (a / 100) +
      (input$avgift + input$forsakring + input$andra) * 12 +
      input$pris * input$renovering / 100

    alternativ_buy <- (initial * (1 + r_stocks)^input$tid - initial) +
      (betalningar_ar * ((1 + r_stocks)^input$tid - 1) / r_stocks - betalningar_ar * input$tid)

    huspris_slut <- input$pris * (1 + input$huspris / 100)^input$tid
    prisökning <- huspris_slut - input$pris
    skatt <- (prisökning - renovering) * 22/30 * 0.3
    vinst <- prisökning - skatt + input$pris * ki

    total_buy <- initial + lopande + alternativ_buy - vinst

    # Rent costs
    equiv_rent <- hyraFunction(
      pris = input$pris, tid = input$tid, ranta = input$ranta,
      kontantinsats = input$ki, inkomst = 0, amortering = a,
      deltaHyra = input$hyresok, deltaPris = input$huspris,
      rstocks = input$avkastning, avgift = input$avgift,
      renovering = input$renovering, forsakring = input$forsakring,
      andrakopa = input$andra, flyttkostnader = input$flytt,
      andrahyra = input$andraHyra, deposition = input$deposition
    )

    initial_rent <- equiv_rent * input$deposition

    hyra_ar <- equiv_rent * 12
    if (abs(input$hyresok) < 0.001) {
      hyra_total <- hyra_ar * input$tid
    } else {
      hyresök_factor <- 1 + input$hyresok / 100
      hyra_total <- hyra_ar * (hyresök_factor^input$tid - 1) / (hyresök_factor - 1)
    }
    lopande_rent <- hyra_total + input$andraHyra * 12 * input$tid

    # Alternative cost for rent (simplified approximation)
    alternativ_rent <- alternativ_buy  # Roughly similar opportunity cost

    total_rent <- initial_rent + lopande_rent + alternativ_rent - initial_rent

    list(
      equiv_rent = equiv_rent,
      buy = list(initial = initial, lopande = lopande,
                 alternativ = alternativ_buy, vinst = vinst, total = total_buy),
      rent = list(initial = initial_rent, lopande = lopande_rent,
                  alternativ = alternativ_rent, vinst = initial_rent, total = total_rent)
    )
  })

  ################################
  # Plots
  ################################

  output$plotAmortHyra <- renderPlot({
    data <- amortData()

    ggplot(data, aes(x = amort, y = motsvarande_hyra)) +
      geom_line(color = "#E66100", size = 1.5) +
      geom_point(color = "#E66100", size = 3) +
      labs(
        x = "Amortering (% av skuld per år)",
        y = "Motsvarande månadshyra (kr)",
        title = ""
      ) +
      scale_y_continuous(labels = function(x) formatnummer(x)) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        panel.grid.minor = element_blank()
      )
  })

  output$plotAmortWealth <- renderPlot({
    data <- wealthData()

    data_long <- data.frame(
      amort = rep(data$amort, 2),
      wealth = c(data$wealth_buy, data$wealth_rent),
      scenario = rep(c("Köpa", "Hyra"), each = nrow(data))
    )

    ggplot(data_long, aes(x = amort, y = wealth, color = scenario, group = scenario)) +
      geom_line(size = 1.5) +
      geom_point(size = 3) +
      labs(
        x = "Amortering (% av skuld per år)",
        y = "Förmögenhet efter vald period (kr)",
        title = "",
        color = "Scenario"
      ) +
      scale_color_manual(values = c("Köpa" = "#E66100", "Hyra" = "#5D3A9B")) +
      scale_y_continuous(labels = function(x) formatnummer(x)) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 16, face = "bold"),
        axis.title = element_text(size = 12),
        legend.position = "top",
        legend.title = element_blank(),
        panel.grid.minor = element_blank()
      )
  })

  ################################
  # Text outputs
  ################################

  output$tolkningHyra <- renderText({
    data <- amortData()
    min_idx <- which.min(data$motsvarande_hyra)
    max_idx <- which.max(data$motsvarande_hyra)

    paste0(
      "<p><strong>Minsta motsvarande hyra:</strong> ",
      formatnummer(data$motsvarande_hyra[min_idx]), " kr/månad ",
      "vid ", data$amort[min_idx], "% amortering</p>",
      "<p><strong>Största motsvarande hyra:</strong> ",
      formatnummer(data$motsvarande_hyra[max_idx]), " kr/månad ",
      "vid ", data$amort[max_idx], "% amortering</p>",
      "<p><strong>Skillnad:</strong> ",
      formatnummer(data$motsvarande_hyra[max_idx] - data$motsvarande_hyra[min_idx]),
      " kr/månad</p>"
    )
  })

  output$tolkningWealth <- renderText({
    data <- wealthData()

    buy_best_idx <- which.max(data$wealth_buy)
    rent_best_idx <- which.max(data$wealth_rent)

    paste0(
      "<p><strong>Köpa - Bäst förmögenhet:</strong> ",
      formatnummer(data$wealth_buy[buy_best_idx]), " kr ",
      "vid ", data$amort[buy_best_idx], "% amortering</p>",
      "<p><strong>Hyra - Bäst förmögenhet:</strong> ",
      formatnummer(data$wealth_rent[rent_best_idx]), " kr ",
      "vid ", data$amort[rent_best_idx], "% amortering</p>",
      "<p><strong>Slutsats:</strong> ",
      if (max(data$wealth_buy) > max(data$wealth_rent)) {
        paste0("Köpa ger högre förmögenhet (+",
               formatnummer(max(data$wealth_buy) - max(data$wealth_rent)), " kr)")
      } else {
        paste0("Hyra ger högre förmögenhet (+",
               formatnummer(max(data$wealth_rent) - max(data$wealth_buy)), " kr)")
      },
      "</p>"
    )
  })

  output$motsvarandeHyra <- renderText({
    detail <- detailData()
    formatnummer(detail$equiv_rent)
  })

  output$tidText <- renderText({
    as.character(input$tid)
  })

  ################################
  # Tables
  ################################

  output$tableKopa <- renderTable({
    detail <- detailData()
    data.frame(
      Komponent = c("Initiala kostnader", "Löpande kostnader",
                    "Alternativkostnader", "Vinster", "Total kostnad"),
      Belopp = c(
        formatnummer(detail$buy$initial),
        formatnummer(detail$buy$lopande),
        formatnummer(detail$buy$alternativ),
        paste0("-", formatnummer(detail$buy$vinst)),
        formatnummer(detail$buy$total)
      )
    )
  }, striped = TRUE, hover = TRUE)

  output$tableHyra <- renderTable({
    detail <- detailData()
    data.frame(
      Komponent = c("Initiala kostnader", "Löpande kostnader",
                    "Alternativkostnader", "Vinster", "Total kostnad"),
      Belopp = c(
        formatnummer(detail$rent$initial),
        formatnummer(detail$rent$lopande),
        formatnummer(detail$rent$alternativ),
        paste0("-", formatnummer(detail$rent$vinst)),
        formatnummer(detail$rent$total)
      )
    )
  }, striped = TRUE, hover = TRUE)

  output$tableWealth <- renderTable({
    wealth <- wealthData()
    idx <- which.min(abs(wealth$amort - input$amortVald))

    data.frame(
      Scenario = c("Köpa", "Hyra", "Skillnad"),
      Förmögenhet = c(
        formatnummer(wealth$wealth_buy[idx]),
        formatnummer(wealth$wealth_rent[idx]),
        formatnummer(wealth$wealth_buy[idx] - wealth$wealth_rent[idx])
      )
    )
  }, striped = TRUE, hover = TRUE)

})
