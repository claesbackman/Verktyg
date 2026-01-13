# Comprehensive Sequence Fix Guide

## Problem
All graphs are showing "arguments imply differing number of rows" errors because the graph sequences don't match the table sequences.

## Root Cause
Each graph has a corresponding table that generates data. The graph creates an x-axis sequence and the table returns y-values. If these don't match in length, data.frame() fails.

## Systematic Fix Required

For EACH graph/table pair, the sequence definitions must be IDENTICAL.

### Pairs to Fix:

| Graph Name | Table Name | Current Status |
|------------|------------|----------------|
| grafPris | tablePris | ✅ FIXED (from=min+50000) |
| grafTid | tableTid | ❌ NEEDS CHECK |
| grafRanta | tableRanta | ❌ NEEDS FIX (from=min vs from=min+0.5) |
| grafAmort | tableAmort | ✅ FIXED (from=min+0.1) |
| grafKI | tableKI | ✅ FIXED (to=max-1) |
| grafDeltaP | tableDeltaP | ✅ FIXED (max(0, min)) |
| grafDeltaHyra | tableDeltaHyra | ✅ FIXED (max(0, min)) |
| grafDeltaSM | tableDeltaSM | ✅ FIXED (max(0, min)) |
| grafKostnader | tableKostnader | ✅ FIXED (max(0, min)) |
| grafReno | tableReno | ❌ NEEDS CHECK |

### Remaining Fixes Needed:

#### 1. grafTid (line ~268)
**Table:** for loop 2:50 starting with tid=1 → 50 values
**Graph:** xTid <- 1:50 → 50 values
**Status:** Should match, but returns 0 values - hyraFunction may be failing

#### 2. grafRanta (line ~359)
**Table:** seq(from=min+0.5, to=max, by=0.5)
**Graph:** seq(from=min, to=max, by=0.5)
**FIX:** Change graph to `seq(from=min+0.5, to=max, by=0.5)`

## Implementation Script

```r
# Fix grafRanta
lines <- readLines("server.R")
# Find grafRanta section and update sequence

# For grafTid and grafReno, the sequences match but data is empty
# This suggests hyraFunction is returning NULL
# Need to debug why hyraFunction fails
```

## Testing
After each fix:
1. Restart R completely
2. Run analyze_errors.R to check remaining issues
3. Iterate until no errors remain
