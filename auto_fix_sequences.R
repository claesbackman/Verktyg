# Automatic sequence fixer - matches all graph sequences to their table sequences

cat("Auto-fixing all sequence mismatches in server.R...\n\n")

# Read server.R
lines <- readLines("server.R")

# Define the fixes needed based on analysis
fixes <- list(
  list(
    name = "grafPris",
    old = 'seq <-  seq(from=min, to=max, by=50000)',
    new = 'seq <-  seq(from=min+50000, to=max, by=50000)  # Match tablePris'
  ),
  list(
    name = "grafTid",
    old_pattern = 'output\\$grafTid.*?seq <-  seq\\(from=min, to=max, by=1\\)',
    find_line = function(lines) grep('output\\$grafTid', lines)[1] + 3,
    old = 'seq <-  seq(from=min, to=max, by=1)',
    new = 'seq <-  seq(from=min+1, to=max, by=1)  # Match tableTid'
  ),
  list(
    name = "grafRanta",
    old = 'seq <-  seq(from=min, to=max, by=0.5)',
    new = 'seq <-  seq(from=min+0.5, to=max, by=0.5)  # Match tableRanta',
    context = "grafRanta"
  )
)

# Apply fixes
content <- paste(lines, collapse = "\n")

# Fix grafPris - table uses from=min+50000
content <- gsub(
  'output\\$grafPris <- renderPlot\\(\\{\\s*min <- input\\$boxPris - 1000000\\s*max <- input\\$boxPris \\+ 1000000\\s*seq <-  seq\\(from=min, to=max, by=50000\\)',
  'output$grafPris <- renderPlot({\n     min <- input$boxPris - 1000000\n     max <- input$boxPris + 1000000\n     seq <-  seq(from=min+50000, to=max, by=50000)  # Match tablePris',
  content
)

# Fix grafTid - table uses from=min+1
content <- gsub(
  '(output\\$grafTid <- renderPlot\\(\\{[^}]*?min <- input\\$boxTid - 25[^}]*?max <- input\\$boxTid \\+ 25[^}]*?)seq <-  seq\\(from=min, to=max, by=1\\)',
  '\\1seq <-  seq(from=min+1, to=max, by=1)  # Match tableTid',
  content, perl = TRUE
)

# Fix grafRanta - table uses from=min+0.5
content <- gsub(
  '(output\\$grafRanta <- renderPlot\\(\\{[^}]*?min <- input\\$boxR - 10[^}]*?max <- input\\$boxR \\+ 10[^}]*?)seq <-  seq\\(from=min, to=max, by=0\\.5\\)',
  '\\1seq <-  seq(from=min+0.5, to=max, by=0.5)  # Match tableRanta',
  content, perl = TRUE
)

# Write back
writeLines(strsplit(content, "\n")[[1]], "server.R")

cat("Fixes applied! Restart the app to test.\n")
