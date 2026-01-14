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
  alternativkostnad <- (pris*ki+flyttkostnader)*(1+r_stocks)^tid-pris*ki -flyttkostnader +
    (totalabetalningar*(((1 + r_stocks)^(tid) - 1) / (r_stocks)))-totalabetalningar*tid

  # Husprisvinster
  # Capital gains from house price appreciation
  totalamortering <- skuld*amortering_2*tid
  prisökning <- pris*(1+husprisökning)^tid-pris
  # Tax calculation: 22/30 of gain is taxable at 30% rate
  # Renovations reduce the taxable gain (improvements add to cost basis)
  # Down payment is recovered tax-free
  husprisvinst <- prisökning - (prisökning - renoveringar_2)*22/30*0.3 + ki*pris

  # Sammanlagt
  totala_kostnader <- intiala_köpa + totala_återkommande + alternativkostnad - husprisvinst

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
            (12*(1+r_stocks)*((1+r_stocks)^tid-(1+hyresökning)^tid) + deposition*((1+r_stocks)^tid - 1)*(r_stocks - hyresökning))/12
          ,digits=0)
  }

}
