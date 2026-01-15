formatnummer <- function(x) {
  format(round(x, 0), big.mark = " ", decimal.mark = ",", scientific = FALSE)
}

shinyServer(function(input, output, session) {
  effectiveInterest <- reactive({
    if (isTRUE(input$taxDeduction)) {
      input$interest * 0.7
    } else {
      input$interest
    }
  })

  monthlyFactor <- reactive({
    (effectiveInterest() + input$amort) / 1200
  })

  incomeBasedCap <- reactive({
    monthly_income <- input$householdIncome / 12
    monthly_income * input$incomeShare / 100
  })

  maxLoanFromPayment <- reactive({
    factor <- monthlyFactor()
    if (factor <= 0) {
      Inf
    } else {
      incomeBasedCap() / factor
    }
  })

  scenarioForDownpayment <- function(downpaymentPercent) {
    rate <- downpaymentPercent / 100
    if (rate <= 0) {
      price_from_downpayment <- Inf
      loan_from_downpayment <- Inf
    } else {
      price_from_downpayment <- input$downpayment / rate
      loan_from_downpayment <- max(0, price_from_downpayment - input$downpayment)
    }

    loan_from_payment <- maxLoanFromPayment()
    max_loan <- min(loan_from_downpayment, loan_from_payment)
    max_price <- max_loan + input$downpayment

    list(
      downpayment_rate = downpaymentPercent,
      price_from_downpayment = price_from_downpayment,
      loan_from_downpayment = loan_from_downpayment,
      loan_from_payment = loan_from_payment,
      max_loan = max_loan,
      max_price = max_price
    )
  }

  summaryData <- reactive({
    scenario_15 <- scenarioForDownpayment(15)
    scenario_10 <- scenarioForDownpayment(10)

    list(
      s15 = scenario_15,
      s10 = scenario_10,
      extra_loan = scenario_10$max_loan - scenario_15$max_loan,
      extra_price = scenario_10$max_price - scenario_15$max_price
    )
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

  syncNumericSlider("downpayment", "downpayment_num")
  syncNumericSlider("householdIncome", "householdIncome_num")
  syncNumericSlider("incomeShare", "incomeShare_num")
  syncNumericSlider("interest", "interest_num")
  syncNumericSlider("amort", "amort_num")

  output$summaryLoan <- renderUI({
    data <- summaryData()
    max_loan <- data$s15$max_loan
    if (!is.finite(max_loan)) {
      value <- "Ingen gräns"
    } else {
      value <- paste(formatnummer(max_loan), "kr")
    }

    HTML(paste0(
      "<div class='summary-card'>",
      "<h5>Max lån (15% krav)</h5>",
      "<div class='value'>", value, "</div>",
      "</div>"
    ))
  })

  output$summaryPrice <- renderUI({
    data <- summaryData()
    max_price <- data$s15$max_price
    if (!is.finite(max_price)) {
      value <- "Ingen gräns"
    } else {
      value <- paste(formatnummer(max_price), "kr")
    }

    HTML(paste0(
      "<div class='summary-card'>",
      "<h5>Max bostadspris (15% krav)</h5>",
      "<div class='value'>", value, "</div>",
      "</div>"
    ))
  })

  output$summaryGap <- renderUI({
    data <- summaryData()
    if (!is.finite(data$extra_loan)) {
      value <- "Ingen gräns"
    } else {
      value <- paste(formatnummer(data$extra_loan), "kr")
    }

    HTML(paste0(
      "<div class='summary-card'>",
      "<h5>Extra lån vid 10% krav</h5>",
      "<div class='value'>", value, "</div>",
      "</div>"
    ))
  })

  output$summaryNote <- renderUI({
    data <- summaryData()
    payment <- incomeBasedCap()
    factor <- monthlyFactor()

    if (factor <= 0) {
      note <- "Med 0% ränta och 0% amortering begränsar bara kontantinsatsen."
    } else {
      note <- paste0(
        "Månadstaket på ", formatnummer(payment), " kr bygger på ",
        format(round(input$incomeShare, 1), nsmall = 1), "% av inkomsten. Maxlånet beräknas med en ",
        "effektiv ränta på ", format(round(effectiveInterest(), 2), nsmall = 2), "% ",
        "och amortering ", format(round(input$amort, 2), nsmall = 2), "%."
      )
    }

    HTML(paste0("<p>", note, "</p>"))
  })

  output$computedCap <- renderUI({
    income_cap <- incomeBasedCap()

    HTML(paste0(
      "<p class='help-block'>",
      "Beräknat inkomsttak: <strong>", formatnummer(income_cap), " kr</strong> ",
      "(", format(round(input$incomeShare, 1), nsmall = 1), "% av inkomsten)</p>"
    ))
  })

  output$summaryTable <- renderTable({
    data <- summaryData()

    data.frame(
      Scenario = c("15% kontantinsatskrav", "10% kontantinsatskrav"),
      `Max bostadspris` = c(
        paste(formatnummer(data$s15$max_price), "kr"),
        paste(formatnummer(data$s10$max_price), "kr")
      ),
      `Max lån` = c(
        paste(formatnummer(data$s15$max_loan), "kr"),
        paste(formatnummer(data$s10$max_loan), "kr")
      ),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$downpaymentTable <- renderTable({
    data <- summaryData()

    data.frame(
      Krav = c("15% kontantinsats", "10% kontantinsats"),
      `Max bostadspris` = c(
        paste(formatnummer(data$s15$max_price), "kr"),
        paste(formatnummer(data$s10$max_price), "kr")
      ),
      `Max lån` = c(
        paste(formatnummer(data$s15$max_loan), "kr"),
        paste(formatnummer(data$s10$max_loan), "kr")
      ),
      `Lån från kontantinsats` = c(
        paste(formatnummer(data$s15$loan_from_downpayment), "kr"),
        paste(formatnummer(data$s10$loan_from_downpayment), "kr")
      ),
      `Lån från inkomsttak` = c(
        ifelse(is.finite(data$s15$loan_from_payment), paste(formatnummer(data$s15$loan_from_payment), "kr"), "Ingen gräns"),
        ifelse(is.finite(data$s10$loan_from_payment), paste(formatnummer(data$s10$loan_from_payment), "kr"), "Ingen gräns")
      ),
      check.names = FALSE
    )
  }, striped = TRUE, hover = TRUE, width = "100%")

  output$downpaymentInsight <- renderUI({
    data <- summaryData()
    extra_loan <- data$extra_loan
    extra_price <- data$extra_price

    text <- if (!is.finite(extra_loan)) {
      "När inkomsttaket inte begränsar kan sänkt krav öka låneutrymmet utan övre gräns."
    } else {
      paste0(
        "Med dina antaganden ökar maxlånet med ",
        formatnummer(extra_loan), " kr och maxpriset med ",
        formatnummer(extra_price), " kr när kravet går från 15% till 10%."
      )
    }

    HTML(paste0("<p><strong>", text, "</strong></p>"))
  })

  output$interestPlot <- renderPlot({
    rates <- seq(0, 10, by = 0.25)
    amort <- input$amort
    max_payment <- incomeBasedCap()

    effective_rates <- if (isTRUE(input$taxDeduction)) rates * 0.7 else rates
    factors <- (effective_rates + amort) / 1200
    max_loan <- ifelse(factors <= 0, NA, max_payment / factors)

    data <- data.frame(
      rate = rates,
      loan = max_loan
    )

    ggplot(data, aes(x = rate, y = loan)) +
      geom_line(linewidth = 2, alpha = 0.9, color = "#2E86AB") +
      geom_point(size = 4, alpha = 0.9, color = "#2E86AB") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#adb5bd", linewidth = 0.8) +
      labs(x = "Bolåneränta (%)", y = "Max lån (kr)") +
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

  output$amortPlot <- renderPlot({
    amort_rates <- seq(0, 5, by = 0.25)
    rate <- input$interest
    max_payment <- incomeBasedCap()
    eff_rate <- if (isTRUE(input$taxDeduction)) rate * 0.7 else rate

    factors <- (eff_rate + amort_rates) / 1200
    max_loan <- ifelse(factors <= 0, NA, max_payment / factors)

    data <- data.frame(
      amort = amort_rates,
      loan = max_loan
    )

    ggplot(data, aes(x = amort, y = loan)) +
      geom_line(linewidth = 2, alpha = 0.9, color = "#D4763A") +
      geom_point(size = 4, alpha = 0.9, color = "#D4763A") +
      geom_hline(yintercept = 0, linetype = "dashed", color = "#adb5bd", linewidth = 0.8) +
      labs(x = "Amortering per år (%)", y = "Max lån (kr)") +
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
})
