# Format function for Swedish number format
formatnummer <- function(x) {
  format(round(x, 0), big.mark = " ", decimal.mark = ",", scientific = FALSE)
}

shinyServer(function(input, output, session) {

  ################################
  # Enforce Scenario A <= Scenario B
  ################################
  observeEvent(input$amort_a, {
    if (!is.null(input$amort_b) && input$amort_a > input$amort_b) {
      updateSliderInput(session, "amort_b", value = input$amort_a)
    }
  })

  observeEvent(input$amort_b, {
    if (!is.null(input$amort_a) && input$amort_b < input$amort_a) {
      updateSliderInput(session, "amort_b", value = input$amort_a)
    }
  })

  ################################
  # Core calculation: Year-by-year wealth for both scenarios
  ################################

  yearlyWealthData <- reactive({
    # Input variables
    lan <- input$lan
    ranta_pct <- input$ranta / 100
    amort_a_pct <- input$amort_a / 100
    amort_b_pct <- input$amort_b / 100
    avkastning_pct <- input$avkastning / 100
    tid <- input$tid

    # Effective interest rate after tax deduction
    if (input$skatteavdrag) {
      ranta_effektiv <- ranta_pct * 0.7  # 30% deduction on 70% of interest
    } else {
      ranta_effektiv <- ranta_pct
    }

    # Initialize data frame
    years <- 0:tid
    n_years <- length(years)

    # Scenario A (lower amortization)
    skuld_a <- lan
    amort_ackum_a <- 0
    invest_a <- 0  # Extra money invested (difference in amortization)

    # Scenario B (higher amortization)
    skuld_b <- lan
    amort_ackum_b <- 0
    invest_b <- 0  # Interest savings invested

    # Store yearly values
    wealth_a <- numeric(n_years)
    wealth_b <- numeric(n_years)

    for (i in seq_along(years)) {
      year <- years[i]

      if (year == 0) {
        # Year 0: Starting point - no equity, no investments
        wealth_a[i] <- 0
        wealth_b[i] <- 0
      } else {
        # === Calculate interest paid this year (before amortization) ===
        ranta_a <- skuld_a * ranta_effektiv
        ranta_b <- skuld_b * ranta_effektiv

        # === SCENARIO A: Lower amortization ===
        amort_belopp_a <- min(skuld_a, lan * amort_a_pct)
        skuld_a <- max(0, skuld_a - amort_belopp_a)
        amort_ackum_a <- amort_ackum_a + amort_belopp_a

        # === SCENARIO B: Higher amortization ===
        amort_belopp_b <- min(skuld_b, lan * amort_b_pct)
        skuld_b <- max(0, skuld_b - amort_belopp_b)
        amort_ackum_b <- amort_ackum_b + amort_belopp_b

        # === Investments ===
        # Scenario A: Invests the difference in amortization (B pays more amort, A invests that)
        invest_diff_a <- max(0, amort_belopp_b - amort_belopp_a)
        invest_a <- invest_a * (1 + avkastning_pct) + invest_diff_a

        # Scenario B: Invests the interest savings (B pays less interest than A)
        interest_savings_b <- max(0, ranta_a - ranta_b)
        invest_b <- invest_b * (1 + avkastning_pct) + interest_savings_b

        # Total wealth:
        # - Equity (accumulated amortization) = real ownership in home
        # - Scenario A: + investments (the amortization difference)
        # - Scenario B: + investments (the interest savings from lower debt)
        wealth_a[i] <- amort_ackum_a + invest_a
        wealth_b[i] <- amort_ackum_b + invest_b
      }
    }

    data.frame(
      year = years,
      wealth_a = wealth_a,
      wealth_b = wealth_b
    )
  })

  ################################
  # Detailed breakdown for each scenario
  ################################

  wealthBreakdown <- reactive({
    lan <- input$lan
    ranta_pct <- input$ranta / 100
    amort_a_pct <- input$amort_a / 100
    amort_b_pct <- input$amort_b / 100
    avkastning_pct <- input$avkastning / 100
    tid <- input$tid

    if (input$skatteavdrag) {
      ranta_effektiv <- ranta_pct * 0.7
    } else {
      ranta_effektiv <- ranta_pct
    }

    # === SCENARIO A ===
    skuld_a <- lan
    total_amort_a <- 0
    total_interest_paid_a <- 0
    total_invested_a <- 0
    invested_with_returns_a <- 0

    # === SCENARIO B ===
    skuld_b <- lan
    total_amort_b <- 0
    total_interest_paid_b <- 0
    total_interest_savings_b <- 0
    interest_savings_with_returns_b <- 0

    for (year in 1:tid) {
      # Interest paid this year (before amortization)
      ranta_kostnad_a <- skuld_a * ranta_effektiv
      ranta_kostnad_b <- skuld_b * ranta_effektiv
      total_interest_paid_a <- total_interest_paid_a + ranta_kostnad_a
      total_interest_paid_b <- total_interest_paid_b + ranta_kostnad_b

      # Amortization
      amort_a <- min(skuld_a, lan * amort_a_pct)
      amort_b <- min(skuld_b, lan * amort_b_pct)
      skuld_a <- max(0, skuld_a - amort_a)
      skuld_b <- max(0, skuld_b - amort_b)
      total_amort_a <- total_amort_a + amort_a
      total_amort_b <- total_amort_b + amort_b

      # Scenario A: Investment (difference in amortization)
      invest_diff_a <- max(0, amort_b - amort_a)
      total_invested_a <- total_invested_a + invest_diff_a

      # Scenario B: Interest savings (B pays less interest than A)
      interest_savings <- max(0, ranta_kostnad_a - ranta_kostnad_b)
      total_interest_savings_b <- total_interest_savings_b + interest_savings

      # Calculate with compound growth
      years_to_grow <- tid - year
      if (avkastning_pct > 0 && years_to_grow > 0) {
        invested_with_returns_a <- invested_with_returns_a +
          invest_diff_a * (1 + avkastning_pct)^years_to_grow
        interest_savings_with_returns_b <- interest_savings_with_returns_b +
          interest_savings * (1 + avkastning_pct)^years_to_grow
      } else {
        invested_with_returns_a <- invested_with_returns_a + invest_diff_a
        interest_savings_with_returns_b <- interest_savings_with_returns_b + interest_savings
      }
    }

    investment_returns_a <- invested_with_returns_a - total_invested_a
    interest_savings_returns_b <- interest_savings_with_returns_b - total_interest_savings_b

    list(
      a = list(
        amortering = total_amort_a,
        investering = total_invested_a,
        investering_avkastning = investment_returns_a,
        total = total_amort_a + invested_with_returns_a,
        total_ranta_betald = total_interest_paid_a
      ),
      b = list(
        amortering = total_amort_b,
        rante_besparing = total_interest_savings_b,
        rante_besparing_avkastning = interest_savings_returns_b,
        total = total_amort_b + interest_savings_with_returns_b,
        total_ranta_betald = total_interest_paid_b
      )
    )
  })

  ################################
  # Cash flow data
  ################################

  cashFlowData <- reactive({
    lan <- input$lan
    ranta_pct <- input$ranta / 100
    amort_a_pct <- input$amort_a / 100
    amort_b_pct <- input$amort_b / 100

    if (input$skatteavdrag) {
      ranta_effektiv <- ranta_pct * 0.7
    } else {
      ranta_effektiv <- ranta_pct
    }

    # Monthly payments at start
    ranta_manadsbelopp <- (lan * ranta_effektiv) / 12
    amort_a_manadsbelopp <- (lan * amort_a_pct) / 12
    amort_b_manadsbelopp <- (lan * amort_b_pct) / 12

    list(
      ranta = ranta_manadsbelopp,
      amort_a = amort_a_manadsbelopp,
      amort_b = amort_b_manadsbelopp,
      total_a = ranta_manadsbelopp + amort_a_manadsbelopp,
      total_b = ranta_manadsbelopp + amort_b_manadsbelopp,
      skillnad = amort_b_manadsbelopp - amort_a_manadsbelopp
    )
  })

  ################################
  # Sensitivity analysis
  ################################

  sensitivityData <- reactive({
    lan <- input$lan
    ranta_pct <- input$ranta / 100
    amort_a_pct <- input$amort_a / 100
    amort_b_pct <- input$amort_b / 100
    tid <- input$tid

    if (input$skatteavdrag) {
      ranta_effektiv <- ranta_pct * 0.7
    } else {
      ranta_effektiv <- ranta_pct
    }

    avkastningar <- seq(0, 15, by = 0.5)
    skillnad <- numeric(length(avkastningar))

    for (j in seq_along(avkastningar)) {
      avk <- avkastningar[j] / 100

      # Scenario A
      skuld_a <- lan
      amort_ackum_a <- 0
      invest_a <- 0

      # Scenario B
      skuld_b <- lan
      amort_ackum_b <- 0
      invest_b <- 0

      for (year in 1:tid) {
        # Interest this year
        ranta_a <- skuld_a * ranta_effektiv
        ranta_b <- skuld_b * ranta_effektiv

        # Scenario A
        amort_a <- min(skuld_a, lan * amort_a_pct)
        skuld_a <- max(0, skuld_a - amort_a)
        amort_ackum_a <- amort_ackum_a + amort_a

        # Scenario B
        amort_b <- min(skuld_b, lan * amort_b_pct)
        skuld_b <- max(0, skuld_b - amort_b)
        amort_ackum_b <- amort_ackum_b + amort_b

        # Scenario A: Investment (amortization difference)
        invest_diff_a <- max(0, amort_b - amort_a)
        invest_a <- invest_a * (1 + avk) + invest_diff_a

        # Scenario B: Investment (interest savings)
        interest_savings <- max(0, ranta_a - ranta_b)
        invest_b <- invest_b * (1 + avk) + interest_savings
      }

      wealth_a <- amort_ackum_a + invest_a
      wealth_b <- amort_ackum_b + invest_b
      skillnad[j] <- wealth_a - wealth_b
    }

    data.frame(
      avkastning = avkastningar,
      skillnad = skillnad
    )
  })

  ################################
  # Plots
  ################################

  output$plotWealthOverTime <- renderPlot({
    data <- yearlyWealthData()

    data_long <- data.frame(
      year = rep(data$year, 2),
      wealth = c(data$wealth_a, data$wealth_b),
      scenario = rep(c(paste0("Scenario A (", input$amort_a, "%)"),
                       paste0("Scenario B (", input$amort_b, "%)")),
                     each = nrow(data))
    )

    ggplot(data_long, aes(x = year, y = wealth, color = scenario, group = scenario)) +
      geom_line(linewidth = 2, alpha = 0.9) +
      geom_point(size = 4, alpha = 0.9) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#adb5bd", linewidth = 0.8) +
      labs(x = "År", y = "Total förmögenhet (kr)", color = "") +
      scale_color_manual(values = c("#2E86AB", "#D4763A")) +
      scale_y_continuous(labels = function(x) formatnummer(x)) +
      scale_x_continuous(breaks = function(x) seq(floor(min(x)), ceiling(max(x)),
                         by = max(1, round(diff(range(x))/10)))) +
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

  output$plotBreakdownA <- renderPlot({
    breakdown <- wealthBreakdown()

    data <- data.frame(
      component = factor(
        c("Amortering (eget kapital)", "Investerat belopp", "Avkastning på investering"),
        levels = rev(c("Amortering (eget kapital)", "Investerat belopp", "Avkastning på investering"))
      ),
      value = c(
        breakdown$a$amortering,
        breakdown$a$investering,
        breakdown$a$investering_avkastning
      )
    )

    ggplot(data, aes(x = component, y = value)) +
      geom_col(width = 0.75, alpha = 0.9, fill = "#2E86AB") +
      geom_hline(yintercept = 0, color = "#6c757d", linewidth = 0.6) +
      labs(x = "", y = "Belopp (kr)") +
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
        plot.margin = margin(10, 15, 10, 10)
      )
  })

  output$plotBreakdownB <- renderPlot({
    breakdown <- wealthBreakdown()

    data <- data.frame(
      component = factor(
        c("Amortering (eget kapital)", "Räntebesparing (investerad)", "Avkastning på räntebesparing"),
        levels = rev(c("Amortering (eget kapital)", "Räntebesparing (investerad)", "Avkastning på räntebesparing"))
      ),
      value = c(
        breakdown$b$amortering,
        breakdown$b$rante_besparing,
        breakdown$b$rante_besparing_avkastning
      )
    )

    ggplot(data, aes(x = component, y = value)) +
      geom_col(width = 0.75, alpha = 0.9, fill = "#D4763A") +
      geom_hline(yintercept = 0, color = "#6c757d", linewidth = 0.6) +
      labs(x = "", y = "Belopp (kr)") +
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
        plot.margin = margin(10, 15, 10, 10)
      )
  })

  output$plotCashFlow <- renderPlot({
    cf <- cashFlowData()

    data <- data.frame(
      scenario = factor(c("Scenario A", "Scenario A", "Scenario B", "Scenario B"),
                        levels = c("Scenario A", "Scenario B")),
      component = factor(c("Ränta", "Amortering", "Ränta", "Amortering"),
                         levels = c("Amortering", "Ränta")),
      value = c(cf$ranta, cf$amort_a, cf$ranta, cf$amort_b)
    )

    ggplot(data, aes(x = scenario, y = value, fill = component)) +
      geom_col(width = 0.6, alpha = 0.9) +
      labs(x = "", y = "Månadskostnad (kr)", fill = "") +
      scale_fill_manual(values = c("Amortering" = "#2E86AB", "Ränta" = "#6c757d")) +
      scale_y_continuous(labels = function(x) formatnummer(x)) +
      theme_minimal(base_size = 15) +
      theme(
        plot.background = element_rect(fill = "#fafbfc", color = NA),
        panel.background = element_rect(fill = "#fafbfc", color = NA),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(color = "#e9ecef", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13, color = "#495057", face = "bold"),
        axis.text = element_text(size = 12, color = "#6c757d"),
        legend.position = "top",
        legend.text = element_text(size = 12),
        plot.margin = margin(20, 25, 20, 20)
      )
  })

  output$plotInterestPaid <- renderPlot({
    lan <- input$lan
    ranta_pct <- input$ranta / 100
    amort_a_pct <- input$amort_a / 100
    amort_b_pct <- input$amort_b / 100
    tid <- input$tid

    if (input$skatteavdrag) {
      ranta_effektiv <- ranta_pct * 0.7
    } else {
      ranta_effektiv <- ranta_pct
    }

    years <- 0:tid
    interest_a <- numeric(length(years))
    interest_b <- numeric(length(years))

    skuld_a <- lan
    skuld_b <- lan
    ackum_a <- 0
    ackum_b <- 0

    for (i in seq_along(years)) {
      if (years[i] == 0) {
        interest_a[i] <- 0
        interest_b[i] <- 0
      } else {
        ackum_a <- ackum_a + skuld_a * ranta_effektiv
        ackum_b <- ackum_b + skuld_b * ranta_effektiv
        interest_a[i] <- ackum_a
        interest_b[i] <- ackum_b

        skuld_a <- max(0, skuld_a - lan * amort_a_pct)
        skuld_b <- max(0, skuld_b - lan * amort_b_pct)
      }
    }

    data_long <- data.frame(
      year = rep(years, 2),
      interest = c(interest_a, interest_b),
      scenario = rep(c(paste0("Scenario A (", input$amort_a, "%)"),
                       paste0("Scenario B (", input$amort_b, "%)")),
                     each = length(years))
    )

    ggplot(data_long, aes(x = year, y = interest, color = scenario, group = scenario)) +
      geom_line(linewidth = 2, alpha = 0.9) +
      geom_point(size = 3, alpha = 0.9) +
      labs(x = "År", y = "Ackumulerad räntekostnad (kr)", color = "") +
      scale_color_manual(values = c("#2E86AB", "#D4763A")) +
      scale_y_continuous(labels = function(x) formatnummer(x)) +
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
        plot.margin = margin(20, 25, 20, 20)
      )
  })

  output$plotSensitivity <- renderPlot({
    data <- sensitivityData()
    current_avk <- input$avkastning

    ggplot(data, aes(x = avkastning, y = skillnad)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#dc3545", linewidth = 1) +
      geom_line(linewidth = 2, color = "#2E86AB", alpha = 0.9) +
      geom_point(data = data[data$avkastning == current_avk, ],
                 aes(x = avkastning, y = skillnad),
                 size = 6, color = "#D4763A") +
      geom_area(alpha = 0.2, fill = "#2E86AB") +
      annotate("text", x = 1, y = max(data$skillnad) * 0.9,
               label = "Scenario A bättre", hjust = 0, color = "#2E86AB", fontface = "bold") +
      annotate("text", x = 1, y = min(data$skillnad) * 0.9,
               label = "Scenario B bättre", hjust = 0, color = "#D4763A", fontface = "bold") +
      labs(x = "Aktieavkastning (%)", y = "Skillnad i förmögenhet (A - B)") +
      scale_y_continuous(labels = function(x) formatnummer(x)) +
      theme_minimal(base_size = 15) +
      theme(
        plot.background = element_rect(fill = "#fafbfc", color = NA),
        panel.background = element_rect(fill = "#fafbfc", color = NA),
        panel.grid.major = element_line(color = "#e9ecef", linewidth = 0.5),
        panel.grid.minor = element_blank(),
        axis.title = element_text(size = 13, color = "#495057", face = "bold"),
        axis.text = element_text(size = 11, color = "#6c757d"),
        plot.margin = margin(20, 25, 20, 20)
      )
  })

  ################################
  # Summary cards
  ################################

  output$summaryCards <- renderUI({
    data <- yearlyWealthData()
    final <- tail(data, 1)
    skillnad <- final$wealth_a - final$wealth_b

    skillnad_class <- if (skillnad >= 0) "positive" else "negative"
    vinnare <- if (skillnad >= 0) "Scenario A" else "Scenario B"

    HTML(paste0(
      "<div style='display: flex; gap: 20px; flex-wrap: wrap;'>",
      "<div class='summary-card' style='flex: 1; min-width: 200px;'>",
      "<h5>Scenario A (", input$amort_a, "%)</h5>",
      "<div class='value scenario-a'>", formatnummer(final$wealth_a), " kr</div>",
      "</div>",
      "<div class='summary-card' style='flex: 1; min-width: 200px;'>",
      "<h5>Scenario B (", input$amort_b, "%)</h5>",
      "<div class='value scenario-b'>", formatnummer(final$wealth_b), " kr</div>",
      "</div>",
      "<div class='summary-card' style='flex: 1; min-width: 200px;'>",
      "<h5>Skillnad (A - B)</h5>",
      "<div class='value ", skillnad_class, "'>", formatnummer(skillnad), " kr</div>",
      "</div>",
      "<div class='summary-card' style='flex: 1; min-width: 200px;'>",
      "<h5>Bäst efter ", input$tid, " år</h5>",
      "<div class='value'>", vinnare, "</div>",
      "</div>",
      "</div>"
    ))
  })

  ################################
  # Text outputs
  ################################

  output$tidText <- renderText({
    input$tid
  })

  output$tolkningWealth <- renderText({
    data <- yearlyWealthData()
    final <- tail(data, 1)
    skillnad <- final$wealth_a - final$wealth_b

    if (abs(skillnad) < 10000) {
      paste0(
        "<p><strong>Resultat:</strong> Med dina valda antaganden ger båda strategierna ungefär samma förmögenhet efter ",
        input$tid, " år.</p>"
      )
    } else if (skillnad > 0) {
      paste0(
        "<p><strong>Resultat:</strong> Med ", input$avkastning, "% aktieavkastning ger ",
        "<span class='scenario-label-a'>Scenario A</span> (lägre amortering) ",
        formatnummer(skillnad), " kr mer i förmögenhet efter ", input$tid, " år.</p>",
        "<p>Detta beror på att avkastningen på investeringarna kompenserar för det lägre egna kapitalet i bostaden.</p>"
      )
    } else {
      paste0(
        "<p><strong>Resultat:</strong> Med ", input$avkastning, "% aktieavkastning ger ",
        "<span class='scenario-label-b'>Scenario B</span> (högre amortering) ",
        formatnummer(abs(skillnad)), " kr mer i förmögenhet efter ", input$tid, " år.</p>",
        "<p>Detta beror på att det egna kapitalet i bostaden växer snabbare än investeringarna.</p>"
      )
    }
  })

  output$interestSummary <- renderText({
    breakdown <- wealthBreakdown()
    skillnad <- breakdown$a$total_ranta_betald - breakdown$b$total_ranta_betald

    paste0(
      "<div style='display: flex; gap: 20px; flex-wrap: wrap;'>",
      "<div class='summary-card' style='flex: 1;'>",
      "<h5>Total ränta Scenario A</h5>",
      "<div class='value'>", formatnummer(breakdown$a$total_ranta_betald), " kr</div>",
      "</div>",
      "<div class='summary-card' style='flex: 1;'>",
      "<h5>Total ränta Scenario B</h5>",
      "<div class='value'>", formatnummer(breakdown$b$total_ranta_betald), " kr</div>",
      "</div>",
      "<div class='summary-card' style='flex: 1;'>",
      "<h5>Räntebesparing (B vs A)</h5>",
      "<div class='value positive'>", formatnummer(skillnad), " kr</div>",
      "</div>",
      "</div>",
      "<p style='margin-top: 15px;'>Genom att amortera mer (Scenario B) sparar du ",
      formatnummer(skillnad), " kr i räntekostnader över ", input$tid, " år.</p>"
    )
  })

  output$sensitivityText <- renderText({
    data <- sensitivityData()
    current <- data[data$avkastning == input$avkastning, ]

    paste0(
      "<p>Den orangea punkten visar din valda avkastning (", input$avkastning, "%).</p>",
      "<p>Vid denna avkastning är skillnaden ", formatnummer(current$skillnad), " kr till fördel för ",
      if (current$skillnad >= 0) "Scenario A (lägre amortering)." else "Scenario B (högre amortering).",
      "</p>"
    )
  })

  output$breakEvenText <- renderText({
    data <- sensitivityData()

    # Find break-even point
    break_even <- NA
    for (i in 2:nrow(data)) {
      if ((data$skillnad[i-1] < 0 && data$skillnad[i] >= 0) ||
          (data$skillnad[i-1] > 0 && data$skillnad[i] <= 0)) {
        # Linear interpolation
        break_even <- data$avkastning[i-1] +
          (0 - data$skillnad[i-1]) / (data$skillnad[i] - data$skillnad[i-1]) *
          (data$avkastning[i] - data$avkastning[i-1])
        break
      }
    }

    if (!is.na(break_even)) {
      paste0(
        "<div class='summary-card'>",
        "<h5>Brytpunkt för aktieavkastning</h5>",
        "<div class='value'>", round(break_even, 1), " %</div>",
        "<p style='margin-top: 10px; font-size: 14px;'>",
        "Om aktieavkastningen är högre än ", round(break_even, 1), "% lönar sig Scenario A (lägre amortering).",
        "<br>Om den är lägre lönar sig Scenario B (högre amortering).</p>",
        "</div>"
      )
    } else if (all(data$skillnad >= 0)) {
      "<div class='summary-card'><p>Scenario A är bättre oavsett aktieavkastning (inom 0-15%).</p></div>"
    } else {
      "<div class='summary-card'><p>Scenario B är bättre oavsett aktieavkastning (inom 0-15%).</p></div>"
    }
  })

  ################################
  # Tables
  ################################

  output$tableBreakdownA <- renderTable({
    breakdown <- wealthBreakdown()

    data.frame(
      Komponent = c(
        "Amortering (eget kapital)",
        "Investerat belopp",
        "Avkastning på investering",
        "SUMMA"
      ),
      Belopp = c(
        paste(formatnummer(breakdown$a$amortering), "kr"),
        paste(formatnummer(breakdown$a$investering), "kr"),
        paste(formatnummer(breakdown$a$investering_avkastning), "kr"),
        paste(formatnummer(breakdown$a$total), "kr")
      )
    )
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$tableBreakdownB <- renderTable({
    breakdown <- wealthBreakdown()

    data.frame(
      Komponent = c(
        "Amortering (eget kapital)",
        "Räntebesparing (investerad)",
        "Avkastning på räntebesparing",
        "SUMMA"
      ),
      Belopp = c(
        paste(formatnummer(breakdown$b$amortering), "kr"),
        paste(formatnummer(breakdown$b$rante_besparing), "kr"),
        paste(formatnummer(breakdown$b$rante_besparing_avkastning), "kr"),
        paste(formatnummer(breakdown$b$total), "kr")
      )
    )
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$tableCashFlow <- renderTable({
    cf <- cashFlowData()

    data.frame(
      ` ` = c("Ränta/månad", "Amortering/månad", "Total/månad", "Skillnad/månad"),
      `Scenario A` = c(
        paste(formatnummer(cf$ranta), "kr"),
        paste(formatnummer(cf$amort_a), "kr"),
        paste(formatnummer(cf$total_a), "kr"),
        "-"
      ),
      `Scenario B` = c(
        paste(formatnummer(cf$ranta), "kr"),
        paste(formatnummer(cf$amort_b), "kr"),
        paste(formatnummer(cf$total_b), "kr"),
        paste("+", formatnummer(cf$skillnad), "kr")
      ),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%")

})
