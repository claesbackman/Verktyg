# Test hyraFunction with default values

cat("Testing hyraFunction...\n\n")

# Load required libraries
source("global.R")

# Load the function
source("server.R", local = TRUE)

# Test with default values from the UI
result <- tryCatch({
  hyraFunction(
    pris = 1000000,
    tid = 10,
    ranta = 3,
    kontantinsats = 15,
    inkomst = 200000,
    amortering = 2,
    deltaHyra = 2,
    deltaPris = 3,
    rstocks = 7,
    avgift = 2000,
    renovering = 1,
    forsakring = 1000,
    andrakopa = 1000,
    flyttkostnader = 10000,
    andrahyra = 1000,
    deposition = 1
  )
}, error = function(e) {
  cat("ERROR in hyraFunction:\n")
  cat(e$message, "\n\n")
  print(e)
  return(NULL)
})

if (is.null(result)) {
  cat("\nhyraFunction returned NULL or failed!\n")
} else {
  cat(sprintf("\nSUCCESS! hyraFunction returned: %s\n", result))
}
