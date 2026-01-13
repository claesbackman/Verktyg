# Test Plan for Fixed R Shiny Application

## Fixes Applied

### High Priority Fixes
1. ✅ Fixed missing parameters (flyttkostnader, deposition) in hyra1år function
2. ✅ Added reactive defaults for conditional inputs (boxDeltaP, boxDeltaRent, boxDeltaSM)
3. ✅ Fixed division by zero when investment return equals rent increase
4. ✅ Removed undefined totamortering() reference

### Medium Priority Fixes
5. ✅ Commented out all debug print() statements
6. ✅ Documented inkomst parameter as unused but reserved
7. ✅ Fixed intro.txt encoding for Swedish characters
8. ✅ Removed commented code blocks
9. ✅ Added documentation comments for complex formulas

## Manual Testing Checklist

### 1. Application Startup
- [ ] Application launches without errors
- [ ] No console warnings or errors displayed
- [ ] All UI elements render correctly

### 2. Basic Functionality Tests
- [ ] All sliders move smoothly
- [ ] All 11 graphs render correctly:
  - [ ] Bostadspris (Housing price)
  - [ ] Tid (Time)
  - [ ] Bolåneränta (Mortgage rate)
  - [ ] Amortering (Amortization)
  - [ ] Kontantinsats (Down payment)
  - [ ] Husprisökning (House price increase)
  - [ ] Hyresökning (Rent increase)
  - [ ] Avkastning på investeringar (Investment return)
  - [ ] Sammanlagda kostnader (Total costs)
  - [ ] Renoveringar (Renovations)
- [ ] Motsvarande hyra calculation displays correctly
- [ ] Cost breakdown table displays correctly

### 3. Edge Case Tests

#### Test Case 1: 100% Down Payment
- Set boxKI to 100%
- Expected: No interest costs, calculation should work without errors
- [ ] Passes

#### Test Case 2: 1 Year Time Period
- Set boxTid to 1 year
- Expected: All calculations work, graphs display correctly
- [ ] Passes

#### Test Case 3: Equal Investment Return and Rent Increase (Division by Zero Fix)
- Set boxDeltaSM (Investment return) to 7%
- Set boxDeltaRent (Rent increase) to 7%
- Expected: Uses simplified formula, no division by zero error
- [ ] Passes

#### Test Case 4: Framtiden Checkbox Unchecked (Conditional Inputs Test)
- Uncheck "Välj värden själv" checkbox
- Expected: Application uses default values (3%, 2%, 7%) without errors
- [ ] Passes

#### Test Case 5: Framtiden Checkbox Checked
- Check "Välj värden själv" checkbox
- Move boxDeltaP, boxDeltaRent, boxDeltaSM sliders
- Expected: Graphs update correctly with chosen values
- [ ] Passes

### 4. Swedish Character Tests
- [ ] intro.txt displays correctly (no garbled characters)
- [ ] All Swedish text displays properly throughout the app
- [ ] Special characters (å, ä, ö) render correctly

### 5. Calculation Verification

#### Test with known values:
- Bostadspris: 2,000,000 kr
- Tid: 10 years
- Bolåneränta: 3%
- Kontantinsats: 15%
- Amortering: 2%
- Expected behavior:
  - [ ] Motsvarande hyra shows a reasonable monthly rent value
  - [ ] Cost breakdown table shows positive values for all categories
  - [ ] Graphs respond to parameter changes

### 6. Console Output
- [ ] No debug print() statements appear in console
- [ ] No error messages displayed
- [ ] No warning messages (or only expected warnings)

### 7. Responsiveness Tests
- [ ] Move each slider rapidly
- [ ] Change multiple sliders in quick succession
- [ ] Expected: No lag, crashes, or calculation errors

## How to Run Tests

1. Start R or RStudio
2. Set working directory:
   ```r
   setwd("c:/Github/Verktyg")
   ```
3. Load the application:
   ```r
   library(shiny)
   runApp()
   ```
4. Follow the checklist above
5. Document any issues found

## Known Issues (Pre-Fix)
These should now be resolved:
- ❌ hyra1år was missing flyttkostnader and deposition parameters
- ❌ Division by zero when investment return equals rent increase
- ❌ Application crashed when framtiden checkbox was unchecked
- ❌ Undefined totamortering() caused errors
- ❌ Debug print statements cluttered console

## Success Criteria
All checklist items pass ✅
No runtime errors occur
All graphs display correctly
Calculations produce sensible results for various input combinations
