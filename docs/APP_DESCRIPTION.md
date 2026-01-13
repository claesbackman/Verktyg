# Rent vs. Buy Housing Calculator - Shiny App Documentation

## Overview

This Shiny application is a comprehensive financial decision-making tool designed to help users determine whether it's more economically advantageous to buy or rent a home. The app is written in Swedish and titled "Ska du köpa eller hyra?" (Should you buy or rent?).

## Purpose

The application calculates an "equivalent rent" - the monthly rent amount that would result in the same total cost as purchasing a home over a specified time period. If you can find a rental property for less than this equivalent rent, it would be financially better to rent rather than buy.

## Core Functionality

### Main Calculation Engine

The heart of the application is the `hyraFunction()` which takes 17 parameters and calculates the equivalent monthly rent by considering:

1. **Initial Costs**
   - Home purchase price
   - Down payment (kontantinsats)
   - Moving costs (flyttkostnader)

2. **Recurring Costs**
   - Mortgage interest payments (after 30% tax deduction on 70% of interest)
   - HOA/Condo fees (avgift)
   - Home insurance (försäkring)
   - Other recurring costs (andrakopa)
   - Renovation costs as percentage of purchase price

3. **Opportunity Costs**
   - Returns foregone on down payment if it were invested instead
   - Returns foregone on mortgage payments and other costs if invested
   - Calculated using compound interest with regular contributions

4. **Capital Gains/Losses**
   - Home price appreciation over time
   - Capital gains tax (22/30 of gain taxed at 30%)
   - Renovations reduce taxable gains (added to cost basis)
   - Down payment recovered tax-free

### Interactive Visualizations

The app generates **10 interactive bar charts** that show how the equivalent rent changes based on:

1. **Home Price** - Shows sensitivity to property price (±1M SEK range)
2. **Time Horizon** - Years planning to stay (1-50 years)
3. **Mortgage Interest Rate** - Current mortgage rate (±2% range)
4. **Amortization Rate** - Annual principal repayment percentage
5. **Down Payment** - Kontantinsats percentage (±15-20% range)
6. **Home Price Appreciation** - Expected annual price growth
7. **Rent Increase Rate** - Expected annual rent inflation
8. **Investment Returns** - Expected return on alternative investments
9. **Total Recurring Costs** - Combined fees, insurance, and other costs
10. **Renovation Budget** - Annual renovation as % of home price

Each chart highlights the user's selected value in orange and displays the calculated equivalent rent for that specific parameter value.

## User Interface Structure

### Input Parameters

The UI is organized as a step-by-step questionnaire:

1. **Property Price** - Slider from 0 to 10M SEK
2. **Time Horizon** - How long planning to stay (1-50 years)
3. **Mortgage Details**:
   - Interest rate (0-15%)
   - Down payment percentage (15-100%)
   - Gross income (for amortization requirements)
   - Amortization rate (conditional based on down payment)
4. **Future Assumptions** (optional advanced section):
   - Home price appreciation rate
   - Rent increase rate
   - Investment return rate
5. **Other Costs**:
   - Monthly HOA/condo fees
   - Monthly insurance
   - Other monthly costs
   - Renovation budget (% of price)
   - Moving costs
6. **Rental Comparison Costs**:
   - Other rental costs per month
   - Security deposit (in months)

### Output Display

The right-side panel shows:

1. **Primary Result** - The equivalent monthly rent in large orange text
2. **Cost Breakdown Table** - Comparing buy vs. rent across:
   - Initial costs
   - Recurring costs
   - Opportunity costs
   - Capital gains/losses
   - Total costs

The table uses Swedish number formatting (space as thousands separator, comma as decimal).

## Key Financial Calculations

### Opportunity Cost Formula

The app calculates opportunity costs using a growing annuity formula that accounts for:
- Compound interest on the down payment and moving costs
- Regular contributions (mortgage payments, fees, renovations) that could have been invested
- Different growth rates between investments and rent/home prices

### Equivalent Rent Calculation

The equivalent rent formula is a present value calculation for a growing annuity:

```
equivalent_rent = (total_buy_cost - rental_costs) × (r - g) /
                  [(1+r)×((1+r)^t - (1+g)^t) + (r-g)×d/12×(1+r)^t - (r-g)×d/12] / 12
```

Where:
- `r` = investment return rate
- `g` = rent increase rate
- `t` = time horizon in years
- `d` = security deposit in months

Special handling when `r ≈ g` to avoid division by zero.

### Tax Treatment

- **Mortgage interest**: 30% tax deduction applied (interest multiplied by 0.7)
- **Capital gains**: 22/30 of gain is taxable at 30% rate
- **Renovations**: Added to cost basis, reducing taxable gains

## Technical Features

### Reactive Programming

The app uses Shiny's reactive programming model extensively:

- **Reactive values** for conditional inputs that only exist when the "future assumptions" checkbox is selected
- **Reactive tables** (e.g., `tablePris()`, `tableTid()`) that pre-calculate data for all chart points
- **Reactive formulas** for individual cost components

### Conditional UI

The UI adapts based on:
- Down payment percentage determines minimum amortization requirements
- "Choose values yourself" checkbox reveals advanced future assumption inputs

### Data Visualization

Charts are created using ggplot2 with:
- Bar charts with the selected value highlighted in orange
- Labels showing the exact equivalent rent value
- Swedish number formatting
- Right-side y-axis positioning
- Gray bars for non-selected values

## Assumptions and Limitations

### Built-in Assumptions

1. **Swedish Tax Rules**:
   - 30% tax deduction on 70% of mortgage interest
   - 22/30 of capital gains taxable at 30%
2. **Amortization Requirements**: UI suggests minimum rates based on loan-to-value ratio
3. **Default Future Values** (when not manually set):
   - Home price appreciation: 3% annually
   - Rent increase: 2% annually
   - Investment returns: 7% annually

### Known Issues (from code comments)

The code includes TODO comments noting:
- Rent shouldn't increase over time in the current model (noted as issue)
- Need to update amortization percentages per "andra amorteringskravet" (second amortization requirement)
- Tax on opportunity costs needs consideration
- "Topplån" (top loan) feature not implemented
- Renovations calculated as % of original price (not updated with home value)

## Technologies Used

- **R Shiny** - Web application framework
- **ggplot2** - Data visualization
- **tidyverse** - Data manipulation
- **scales** - Number formatting
- **tableHTML** - Cost comparison table
- **shinythemes** - "Sandstone" theme applied

## Author

Designed by Claes Bäckman

## Summary

This is a sophisticated financial modeling tool that goes beyond simple mortgage calculators by incorporating:
- Opportunity costs of capital
- Tax implications
- Future growth assumptions
- Complete cost comparison
- Sensitivity analysis through interactive charts

The app provides users with a data-driven answer to one of life's biggest financial decisions: whether to buy or rent a home.
