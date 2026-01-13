# R script to analyze error log and identify which graph/table pairs need fixing

cat("Analyzing errors from full_output.log...\n\n")

if (!file.exists("full_output.log")) {
  cat("Error: full_output.log not found. Run the app first.\n")
  quit(save = "no")
}

# Read the log file
log_lines <- readLines("full_output.log")

# Find all "arguments imply" errors
error_indices <- grep("arguments imply differing number of rows", log_lines)

if (length(error_indices) == 0) {
  cat("SUCCESS! No sequence mismatch errors found!\n")
  quit(save = "no")
}

cat(sprintf("Found %d sequence mismatch errors:\n\n", length(error_indices)))

# Extract details for each error
errors_df <- data.frame(
  rows_expected = integer(),
  rows_actual = integer(),
  line_number = integer(),
  graph_name = character(),
  stringsAsFactors = FALSE
)

for (idx in error_indices) {
  # Get context lines after the error
  context_start <- idx
  context_end <- min(idx + 15, length(log_lines))
  context <- log_lines[context_start:context_end]

  # Extract row counts
  error_line <- log_lines[idx]
  row_match <- regexpr("rows: (\\d+), (\\d+)", error_line, perl = TRUE)
  if (row_match > 0) {
    matched <- regmatches(error_line, row_match)
    nums <- as.numeric(unlist(regmatches(matched, gregexpr("\\d+", matched, perl = TRUE))))
    rows_expected <- nums[1]
    rows_actual <- nums[2]
  } else {
    next
  }

  # Extract line number from context
  line_match <- grep("renderPlot.*server.R#(\\d+)", context, value = TRUE)
  if (length(line_match) > 0) {
    line_num <- as.numeric(sub(".*server.R#(\\d+).*", "\\1", line_match[1]))
  } else {
    line_num <- NA
  }

  # Extract graph name
  graph_match <- grep("output\\$(\\w+)", context, value = TRUE)
  if (length(graph_match) > 0) {
    graph_name <- sub(".*output\\$(\\w+).*", "\\1", graph_match[1])
  } else {
    graph_name <- "unknown"
  }

  errors_df <- rbind(errors_df, data.frame(
    rows_expected = rows_expected,
    rows_actual = rows_actual,
    line_number = line_num,
    graph_name = graph_name,
    stringsAsFactors = FALSE
  ))
}

# Print summary
cat("Error Summary:\n")
cat("==============\n\n")

for (i in 1:nrow(errors_df)) {
  cat(sprintf("%d. %s (line %d):\n", i, errors_df$graph_name[i], errors_df$line_number[i]))
  cat(sprintf("   Graph sequence: %d values\n", errors_df$rows_expected[i]))
  cat(sprintf("   Table data: %d values\n", errors_df$rows_actual[i]))
  cat(sprintf("   Mismatch: %d values\n\n", errors_df$rows_expected[i] - errors_df$rows_actual[i]))
}

# Identify the specific graphs that need fixing
unique_graphs <- unique(errors_df$graph_name)
cat("\nGraphs that need sequence fixes:\n")
cat("================================\n")
for (graph in unique_graphs) {
  cat(sprintf("  - %s\n", graph))
}

cat("\n\nNext steps:\n")
cat("1. For each graph above, find the corresponding table function (e.g., grafPris -> tablePris)\n")
cat("2. Ensure the graph's seq() call exactly matches the table's seq() call\n")
cat("3. Common issues:\n")
cat("   - Graph uses 'from=min' but table uses 'from=min+offset'\n")
cat("   - Graph uses 'to=max' but table uses 'to=max-1'\n")
cat("   - Graph allows negative values but table uses 'max(0, min)'\n")
