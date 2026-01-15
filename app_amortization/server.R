# Format function for Swedish number format
formatnummer <- function(x) {
  format(round(x, 0), big.mark = " ", decimal.mark = ",", scientific = FALSE)
}

shinyServer(function(input, output) {

  ################################
  # Calculate year-by-year wealth for both scenarios (deterministic)
  ################################

  yearlyWealthData <- reactive({
    # Input variables
    pris <- input$pris
    ki_pct <- input$ki / 100
    ranta_pct <- input$ranta / 100
    amort_pct <- input$amort / 100
    huspris_pct <- input$huspris / 100
    hyresok_pct <- input$hyresok / 100
    avkastning_pct <- input$avkastning / 100
    tid <- input$tid

    # Initial values
    kontantinsats <- pris * ki_pct
    skuld_start <- pris * (1 - ki_pct)
    flytt <- input$flytt

    # Monthly costs
    avgift <- input$avgift
    forsakring <- input$forsakring
    andra <- input$andra
    renovering_pct <- input$renovering / 100

    # Rent
    hyra <- input$hyra
    andraHyra <- input$andraHyra

    # Effective interest rate after tax deduction (30% deduction on 70% of interest)
    ranta_efter_skatt <- ranta_pct * 0.7

    # Initialize data frame
    years <- 0:tid
    n_years <- length(years)

    buy_wealth <- numeric(n_years)
    rent_wealth <- numeric(n_years)

    # Track for buying
    skuld <- skuld_start
    buy_investerat <- 0  # Track invested interest savings

    for (i in seq_along(years)) {
      year <- years[i]

      if (year == 0) {
        # Year 0: Starting point
        buy_wealth[i] <- pris - skuld_start - flytt
        rent_wealth[i] <- kontantinsats + flytt
      } else {
        # === BUYING CALCULATIONS ===
        hus_varde <- pris * (1 + huspris_pct)^year

        # Interest savings from amortization:
        # Compare interest on current debt vs interest if no amortization
        ranta_utan_amort <- skuld_start * ranta_efter_skatt
        ranta_med_amort <- skuld * ranta_efter_skatt
        rante_besparing <- ranta_utan_amort - ranta_med_amort

        # Grow previous investments and add this year's savings
        if (avkastning_pct > 0) {
          buy_investerat <- buy_investerat * (1 + avkastning_pct)
        }
        buy_investerat <- buy_investerat + rante_besparing

        # Amortization reduces debt
        amort_belopp <- min(skuld, skuld_start * amort_pct)
        skuld <- max(0, skuld - amort_belopp)

        total_renovering <- pris * renovering_pct * year
        prisökning <- hus_varde - pris
        skattepliktig_vinst <- max(0, prisökning - total_renovering)
        skatt <- skattepliktig_vinst * 22/30 * 0.3
        hus_netto <- hus_varde - skatt

        # Total buying wealth = house equity + invested interest savings
        buy_wealth[i] <- hus_netto - skuld + buy_investerat
      }
    }

    # Calculate rent wealth properly
    investerat <- kontantinsats + flytt

    for (i in seq_along(years)) {
      year <- years[i]

      if (year == 0) {
        rent_wealth[i] <- investerat
      } else {
        # Grow previous year's wealth
        if (avkastning_pct > 0) {
          investerat <- investerat * (1 + avkastning_pct)
        }

        # Calculate this year's cash flow difference
        skuld_year_start <- max(0, skuld_start - skuld_start * amort_pct * (year - 1))
        amort_belopp <- min(skuld_year_start, skuld_start * amort_pct)

        # Buying costs this year
        ranta_kostnad <- skuld_year_start * ranta_efter_skatt
        arliga_kostnader <- (avgift + forsakring + andra) * 12
        renovering_ar <- pris * renovering_pct
        kopa_cash_ut <- amort_belopp + ranta_kostnad + arliga_kostnader + renovering_ar

        # Renting costs this year
        hyra_ar <- hyra * 12 * (1 + hyresok_pct)^(year - 1)
        andra_hyra_ar <- andraHyra * 12
        hyra_cash_ut <- hyra_ar + andra_hyra_ar

        # Savings (positive = buying costs more, so renting saves money)
        besparingar <- kopa_cash_ut - hyra_cash_ut
        investerat <- investerat + besparingar

        rent_wealth[i] <- investerat
      }
    }

    data.frame(
      year = years,
      buy = buy_wealth,
      rent = rent_wealth
    )
  })

  ################################
  # Wealth breakdown analysis
  ################################

  wealthBreakdown <- reactive({
    pris <- input$pris
    ki_pct <- input$ki / 100
    ranta_pct <- input$ranta / 100
    amort_pct <- input$amort / 100
    huspris_pct <- input$huspris / 100
    hyresok_pct <- input$hyresok / 100
    avkastning_pct <- input$avkastning / 100
    tid <- input$tid

    kontantinsats <- pris * ki_pct
    skuld_start <- pris * (1 - ki_pct)
    flytt <- input$flytt
    ranta_efter_skatt <- ranta_pct * 0.7

    avgift <- input$avgift
    forsakring <- input$forsakring
    andra <- input$andra
    renovering_pct <- input$renovering / 100
    hyra <- input$hyra
    andraHyra <- input$andraHyra

    # === BUYING BREAKDOWN ===

    # 1. Starting equity (down payment)
    buy_kontantinsats <- kontantinsats

    # 2. House price gains (after tax)
    hus_varde_slut <- pris * (1 + huspris_pct)^tid
    prisökning <- hus_varde_slut - pris
    total_renovering <- pris * renovering_pct * tid
    skattepliktig_vinst <- max(0, prisökning - total_renovering)
    skatt <- skattepliktig_vinst * 22/30 * 0.3
    buy_huspris_vinst <- prisökning - skatt

    # 3. Equity from amortization
    buy_amort <- min(skuld_start, skuld_start * amort_pct * tid)

    # 4. Foregone investment returns (opportunity cost)
    if (avkastning_pct > 0) {
      buy_foregone <- kontantinsats * ((1 + avkastning_pct)^tid - 1)
    } else {
      buy_foregone <- 0
    }

    # 5. Interest savings from amortization (invested)
    # Each year, compare interest with vs without amortization
    skuld_temp <- skuld_start
    total_interest_savings <- 0
    interest_savings_with_returns <- 0

    for (year in 1:tid) {
      ranta_utan_amort <- skuld_start * ranta_efter_skatt
      ranta_med_amort <- skuld_temp * ranta_efter_skatt
      yearly_interest_savings <- ranta_utan_amort - ranta_med_amort
      total_interest_savings <- total_interest_savings + yearly_interest_savings

      # This year's savings grow for remaining years
      years_to_grow <- tid - year
      if (avkastning_pct > 0 && years_to_grow > 0) {
        interest_savings_with_returns <- interest_savings_with_returns +
          yearly_interest_savings * (1 + avkastning_pct)^years_to_grow
      } else {
        interest_savings_with_returns <- interest_savings_with_returns + yearly_interest_savings
      }

      # Reduce debt for next year
      amort_belopp <- min(skuld_temp, skuld_start * amort_pct)
      skuld_temp <- max(0, skuld_temp - amort_belopp)
    }

    buy_interest_savings_principal <- total_interest_savings
    buy_interest_savings_returns <- interest_savings_with_returns - total_interest_savings

    # Total buying wealth
    buy_total <- buy_kontantinsats + buy_huspris_vinst + buy_amort - buy_foregone + interest_savings_with_returns

    # === RENTING BREAKDOWN ===

    # 1. Initial capital
    rent_initial <- kontantinsats + flytt

    # 2. Investment returns on initial capital
    if (avkastning_pct > 0) {
      rent_initial_returns <- rent_initial * ((1 + avkastning_pct)^tid - 1)
    } else {
      rent_initial_returns <- 0
    }

    # 3. Accumulated savings and their returns
    # Need to track year by year
    total_savings <- 0
    savings_with_returns <- 0

    for (year in 1:tid) {
      skuld_year_start <- max(0, skuld_start - skuld_start * amort_pct * (year - 1))
      amort_belopp <- min(skuld_year_start, skuld_start * amort_pct)
      ranta_kostnad <- skuld_year_start * ranta_efter_skatt
      arliga_kostnader <- (avgift + forsakring + andra) * 12
      renovering_ar <- pris * renovering_pct
      kopa_cash_ut <- amort_belopp + ranta_kostnad + arliga_kostnader + renovering_ar

      hyra_ar <- hyra * 12 * (1 + hyresok_pct)^(year - 1)
      andra_hyra_ar <- andraHyra * 12
      hyra_cash_ut <- hyra_ar + andra_hyra_ar

      yearly_savings <- kopa_cash_ut - hyra_cash_ut
      total_savings <- total_savings + yearly_savings

      # This year's savings grow for remaining years
      years_to_grow <- tid - year
      if (avkastning_pct > 0 && years_to_grow > 0) {
        savings_with_returns <- savings_with_returns + yearly_savings * (1 + avkastning_pct)^years_to_grow
      } else {
        savings_with_returns <- savings_with_returns + yearly_savings
      }
    }

    rent_savings_principal <- total_savings
    rent_savings_returns <- savings_with_returns - total_savings

    # Total renting wealth
    rent_total <- rent_initial + rent_initial_returns + savings_with_returns

    list(
      buy = list(
        kontantinsats = buy_kontantinsats,
        huspris_vinst = buy_huspris_vinst,
        amortering = buy_amort,
        foregone_returns = buy_foregone,
        interest_savings_principal = buy_interest_savings_principal,
        interest_savings_returns = buy_interest_savings_returns,
        total = buy_total
      ),
      rent = list(
        initial_kapital = rent_initial,
        initial_returns = rent_initial_returns,
        savings_principal = rent_savings_principal,
        savings_returns = rent_savings_returns,
        total = rent_total
      )
    )
  })

  ################################
  # Summary data
  ################################

  summaryData <- reactive({
    wealth <- yearlyWealthData()
    final <- tail(wealth, 1)

    list(
      buy_final = final$buy,
      rent_final = final$rent,
      difference = final$buy - final$rent,
      winner = if(final$buy > final$rent) "Köpa" else "Hyra"
    )
  })

  ################################
  # Cost breakdown (existing)
  ################################

  costBreakdown <- reactive({
    pris <- input$pris
    ki_pct <- input$ki / 100
    ranta_pct <- input$ranta / 100
    amort_pct <- input$amort / 100
    huspris_pct <- input$huspris / 100
    hyresok_pct <- input$hyresok / 100
    tid <- input$tid

    kontantinsats <- pris * ki_pct
    skuld_start <- pris * (1 - ki_pct)
    flytt <- input$flytt
    ranta_efter_skatt <- ranta_pct * 0.7

    kopa_initial <- kontantinsats + flytt
    total_amort <- min(skuld_start, skuld_start * amort_pct * tid)
    avg_skuld <- skuld_start - total_amort / 2
    kopa_ranta <- avg_skuld * ranta_efter_skatt * tid
    kopa_avgifter <- (input$avgift + input$forsakring + input$andra) * 12 * tid
    kopa_renovering <- pris * input$renovering / 100 * tid

    hus_varde_slut <- pris * (1 + huspris_pct)^tid
    prisökning <- hus_varde_slut - pris
    skattepliktig_vinst <- max(0, prisökning - kopa_renovering)
    kopa_skatt <- skattepliktig_vinst * 22/30 * 0.3

    if (abs(hyresok_pct) < 0.001) {
      hyra_total <- input$hyra * 12 * tid
    } else {
      hyra_total <- input$hyra * 12 * ((1 + hyresok_pct)^tid - 1) / hyresok_pct
    }
    hyra_andra <- input$andraHyra * 12 * tid

    list(
      kopa = list(
        initial = kopa_initial,
        ranta = kopa_ranta,
        avgifter = kopa_avgifter,
        renovering = kopa_renovering,
        skatt = kopa_skatt
      ),
      hyra = list(
        hyra = hyra_total,
        andra = hyra_andra
      )
    )
  })

  ################################
  # Plots
  ################################

  output$plotWealthOverTime <- renderPlot({
    data <- yearlyWealthData()

    data_long <- data.frame(
      year = rep(data$year, 2),
      wealth = c(data$buy, data$rent),
      scenario = rep(c("Köpa", "Hyra"), each = nrow(data))
    )

    ggplot(data_long, aes(x = year, y = wealth, color = scenario, group = scenario)) +
      geom_line(linewidth = 2, alpha = 0.9) +
      geom_point(size = 4, alpha = 0.9) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#adb5bd", linewidth = 0.8) +
      labs(x = "År", y = "Förmögenhet (kr)", color = "") +
      scale_color_manual(values = c("Köpa" = "#D4763A", "Hyra" = "#2E86AB")) +
      scale_y_continuous(labels = function(x) formatnummer(x)) +
      scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)), by = max(1, round(diff(range(x))/10)))) +
      theme_minimal(base_size = 15) +
      theme(
        plot.background = element_rect(fill = "#fafbfc", color = NA),
        panel.background = element_rect(fill = "#fafbfc", color = NA),
        panel.grid.major = element_line(color = "#e9ecef", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13, color = "#495057", face = "bold"),
        axis.text = element_text(size = 11, color = "#6c757d"),
        legend.position = "top",
        legend.text = element_text(size = 12, face = "bold"),
        legend.key = element_rect(fill = "transparent"),
        legend.background = element_rect(fill = "transparent"),
        plot.margin = margin(20, 25, 20, 20)
      )
  })

  output$plotFinalWealth <- renderPlot({
    summary <- summaryData()

    data <- data.frame(
      scenario = factor(c("Köpa", "Hyra"), levels = c("Köpa", "Hyra")),
      wealth = c(summary$buy_final, summary$rent_final)
    )

    ggplot(data, aes(x = scenario, y = wealth, fill = scenario)) +
      geom_col(width = 0.65, alpha = 0.9) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#adb5bd", linewidth = 0.8) +
      labs(x = "", y = "Slutlig förmögenhet (kr)") +
      scale_fill_manual(values = c("Köpa" = "#D4763A", "Hyra" = "#2E86AB")) +
      scale_y_continuous(labels = function(x) formatnummer(x)) +
      theme_minimal(base_size = 15) +
      theme(
        plot.background = element_rect(fill = "#fafbfc", color = NA),
        panel.background = element_rect(fill = "#fafbfc", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#e9ecef", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13, color = "#495057", face = "bold"),
        axis.text = element_text(size = 12, color = "#6c757d", face = "bold"),
        legend.position = "none",
        plot.margin = margin(20, 25, 20, 20)
      )
  })

  output$plotBreakdownBuy <- renderPlot({
    breakdown <- wealthBreakdown()

    # Create data for waterfall-style stacked bar
    data <- data.frame(
      component = factor(
        c("Kontantinsats", "Husprisvinst", "Amortering", "Räntebesparing", "Avkastning räntebesparing", "Alternativkostnad"),
        levels = rev(c("Kontantinsats", "Husprisvinst", "Amortering", "Räntebesparing", "Avkastning räntebesparing", "Alternativkostnad"))
      ),
      value = c(
        breakdown$buy$kontantinsats,
        breakdown$buy$huspris_vinst,
        breakdown$buy$amortering,
        breakdown$buy$interest_savings_principal,
        breakdown$buy$interest_savings_returns,
        -breakdown$buy$foregone_returns
      ),
      type = c("positiv", "positiv", "positiv", "positiv", "positiv", "negativ")
    )

    ggplot(data, aes(x = component, y = value, fill = type)) +
      geom_col(width = 0.75, alpha = 0.9) +
      geom_hline(yintercept = 0, color = "#6c757d", linewidth = 0.6) +
      labs(x = "", y = "Belopp (kr)") +
      scale_fill_manual(values = c("positiv" = "#D4763A", "negativ" = "#8B4726")) +
      scale_y_continuous(labels = function(x) formatnummer(x)) +
      coord_flip() +
      theme_minimal(base_size = 12) +
      theme(
        plot.background = element_rect(fill = "#fafbfc", color = NA),
        panel.background = element_rect(fill = "#fafbfc", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#e9ecef", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 11, color = "#495057", face = "bold"),
        axis.text.y = element_text(size = 10, color = "#495057"),
        axis.text.x = element_text(size = 9, color = "#6c757d"),
        legend.position = "none",
        plot.margin = margin(10, 15, 10, 10)
      )
  })

  output$plotBreakdownRent <- renderPlot({
    breakdown <- wealthBreakdown()

    data <- data.frame(
      component = factor(
        c("Startkapital", "Avkastning startkapital", "Sparande", "Avkastning sparande"),
        levels = rev(c("Startkapital", "Avkastning startkapital", "Sparande", "Avkastning sparande"))
      ),
      value = c(
        breakdown$rent$initial_kapital,
        breakdown$rent$initial_returns,
        breakdown$rent$savings_principal,
        breakdown$rent$savings_returns
      ),
      type = ifelse(c(
        breakdown$rent$initial_kapital,
        breakdown$rent$initial_returns,
        breakdown$rent$savings_principal,
        breakdown$rent$savings_returns
      ) >= 0, "positiv", "negativ")
    )

    ggplot(data, aes(x = component, y = value, fill = type)) +
      geom_col(width = 0.75, alpha = 0.9) +
      geom_hline(yintercept = 0, color = "#6c757d", linewidth = 0.6) +
      labs(x = "", y = "Belopp (kr)") +
      scale_fill_manual(values = c("positiv" = "#2E86AB", "negativ" = "#1B5E7E")) +
      scale_y_continuous(labels = function(x) formatnummer(x)) +
      coord_flip() +
      theme_minimal(base_size = 12) +
      theme(
        plot.background = element_rect(fill = "#fafbfc", color = NA),
        panel.background = element_rect(fill = "#fafbfc", color = NA),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(color = "#e9ecef", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        axis.title.x = element_text(size = 11, color = "#495057", face = "bold"),
        axis.text.y = element_text(size = 10, color = "#495057"),
        axis.text.x = element_text(size = 9, color = "#6c757d"),
        legend.position = "none",
        plot.margin = margin(10, 15, 10, 10)
      )
  })

  ################################
  # Text outputs
  ################################

  output$tolkningWealth <- renderText({
    summary <- summaryData()
    wealth <- yearlyWealthData()

    crossover <- NA
    for (i in 2:nrow(wealth)) {
      if ((wealth$buy[i-1] < wealth$rent[i-1] && wealth$buy[i] >= wealth$rent[i]) ||
          (wealth$buy[i-1] > wealth$rent[i-1] && wealth$buy[i] <= wealth$rent[i])) {
        crossover <- wealth$year[i]
        break
      }
    }

    crossover_text <- if (!is.na(crossover)) {
      paste0("<p><strong>Brytpunkt:</strong> Köpa och hyra ger liknande förmögenhet runt år ", crossover, ".</p>")
    } else {
      ""
    }

    paste0(
      "<p><strong>Efter ", input$tid, " år:</strong></p>",
      "<ul>",
      "<li>Köpa: ", formatnummer(summary$buy_final), " kr</li>",
      "<li>Hyra: ", formatnummer(summary$rent_final), " kr</li>",
      "</ul>",
      "<p><strong>Skillnad:</strong> ",
      if (summary$difference > 0) {
        paste0("Köpa ger ", formatnummer(abs(summary$difference)), " kr mer")
      } else if (summary$difference < 0) {
        paste0("Hyra ger ", formatnummer(abs(summary$difference)), " kr mer")
      } else {
        "Lika"
      },
      "</p>",
      crossover_text
    )
  })

  output$vinnarText <- renderText({
    summary <- summaryData()

    if (abs(summary$difference) < 10000) {
      "<p style='font-size: 18px; color: #495057;'><strong>Resultat:</strong> Ungefär lika!</p>"
    } else if (summary$difference > 0) {
      paste0(
        "<p style='font-size: 18px; color: #D4763A;'><strong>Vinnare: Köpa</strong></p>",
        "<p>Med valda antaganden ger köp ", formatnummer(summary$difference),
        " kr mer i förmögenhet efter ", input$tid, " år.</p>"
      )
    } else {
      paste0(
        "<p style='font-size: 18px; color: #2E86AB;'><strong>Vinnare: Hyra</strong></p>",
        "<p>Med valda antaganden ger hyra ", formatnummer(abs(summary$difference)),
        " kr mer i förmögenhet efter ", input$tid, " år.</p>"
      )
    }
  })

  ################################
  # Tables
  ################################

  output$tableSummary <- renderTable({
    summary <- summaryData()

    data.frame(
      Scenario = c("Köpa", "Hyra", "Skillnad"),
      Förmögenhet = c(
        paste(formatnummer(summary$buy_final), "kr"),
        paste(formatnummer(summary$rent_final), "kr"),
        paste(formatnummer(summary$difference), "kr")
      )
    )
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$tableKopaCosts <- renderTable({
    costs <- costBreakdown()

    data.frame(
      Kostnadspost = c(
        "Kontantinsats + flytt",
        "Räntekostnader (efter avdrag)",
        "Avgifter & försäkring",
        "Renovering",
        "Reavinstskatt vid försäljning"
      ),
      Belopp = c(
        paste(formatnummer(costs$kopa$initial), "kr"),
        paste(formatnummer(costs$kopa$ranta), "kr"),
        paste(formatnummer(costs$kopa$avgifter), "kr"),
        paste(formatnummer(costs$kopa$renovering), "kr"),
        paste(formatnummer(costs$kopa$skatt), "kr")
      )
    )
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$tableHyraCosts <- renderTable({
    costs <- costBreakdown()

    data.frame(
      Kostnadspost = c(
        "Total hyra",
        "Andra kostnader"
      ),
      Belopp = c(
        paste(formatnummer(costs$hyra$hyra), "kr"),
        paste(formatnummer(costs$hyra$andra), "kr")
      )
    )
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$tableBreakdownBuy <- renderTable({
    breakdown <- wealthBreakdown()

    data.frame(
      Komponent = c(
        "Kontantinsats (eget kapital)",
        "Husprisvinst (efter skatt)",
        "Amortering (eget kapital)",
        "Räntebesparing (investerad)",
        "Avkastning på räntebesparing",
        "Alternativkostnad (utebliven avkastning)",
        "SUMMA"
      ),
      Belopp = c(
        paste(formatnummer(breakdown$buy$kontantinsats), "kr"),
        paste(formatnummer(breakdown$buy$huspris_vinst), "kr"),
        paste(formatnummer(breakdown$buy$amortering), "kr"),
        paste(formatnummer(breakdown$buy$interest_savings_principal), "kr"),
        paste(formatnummer(breakdown$buy$interest_savings_returns), "kr"),
        paste("-", formatnummer(breakdown$buy$foregone_returns), "kr"),
        paste(formatnummer(breakdown$buy$total), "kr")
      )
    )
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$tableBreakdownRent <- renderTable({
    breakdown <- wealthBreakdown()

    data.frame(
      Komponent = c(
        "Startkapital (investerat)",
        "Avkastning på startkapital",
        "Ackumulerat sparande",
        "Avkastning på sparande",
        "SUMMA"
      ),
      Belopp = c(
        paste(formatnummer(breakdown$rent$initial_kapital), "kr"),
        paste(formatnummer(breakdown$rent$initial_returns), "kr"),
        paste(formatnummer(breakdown$rent$savings_principal), "kr"),
        paste(formatnummer(breakdown$rent$savings_returns), "kr"),
        paste(formatnummer(breakdown$rent$total), "kr")
      )
    )
  }, striped = TRUE, hover = TRUE, width = "100%")

})
