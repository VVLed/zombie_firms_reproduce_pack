library(stringr); library(stringi)
library(data.table)
library(fst)
library(ggplot2)
library(forcats)
library(lubridate)
library(arrow)
options(scipen = 999)

# Declare working directory beforehand in an environment variable
# STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("STUDY_PATH")
setwd(path)
path_d <- Sys.getenv("DRIVE_D")

# Load the data ====

## Financials ====
# Read RFSD metadata from local file
RFSD <- open_dataset(paste0(path_d, "_datasets/RFSD"))

# Load only pre-2023 data into memory
scan_builder <- RFSD$NewScan()
# scan_builder$Filter()

scan_builder$Project(cols = c(
  "inn", "year", "region", "filed", "imputed", "simplified" ,"outlier", "okved", "okved_section", "okopf", "financial",
  "line_1150", "line_1200", "line_1210", "line_1250", "line_1230", 
  "line_1300", "line_1370", # Approximate retained earnings as capital and reserves 
  "line_1400", "line_1410", "line_1450", 
  "line_1500", "line_1510", "line_1520", "line_1550",
  "line_1600",
  "line_2110", "line_2120", "line_2210", "line_2220", 
  "line_2300", "line_2310", "line_2320", "line_2330", "line_2340", "line_2350",
  "line_2400",
  "age", "dissolution_date"))

scanner <- scan_builder$Finish()
financials <- as.data.table(scanner$ToTable())

financials[, .N] # 60127983

# Delete inapplicable observations ====

# Limit the sample to commercial corporate organisations 
financials <- financials[str_sub(okopf, start = 1, end = 2) == "12", ]

# Filter the outliers out 
financials <- financials[outlier == 0, ]

financials[, .N] # 53091164

# Filter financial organisations out 
financials <- financials[!(okved_section == "K" | is.na(okved_section) | financial == 1), ]

financials[, .N] # 48765112

# Omit variables if no further use
financials[, c("outlier", "okopf") := NULL] 

# Rename the financial columns ====

old_names <- c(
  "line_1150", "line_1200", "line_1210", "line_1250", "line_1230", 
  "line_1300", "line_1370",
  "line_1400", "line_1410", "line_1450", 
  "line_1500", "line_1510", "line_1520", "line_1550",
  "line_1600",
  "line_2110", "line_2120", "line_2210", "line_2220", 
  "line_2300", "line_2310", "line_2320", "line_2330", "line_2340", "line_2350",
  "line_2400")

new_names <- c(
  "capital", "working_capital", "reserves", "cash_and_equivalents", "debt_receivable",
  "equity_and_reserves", "retained_earnings",
  "long_liabilities", "borrowed_loans_long", "misc_liabilities_long", 
  "short_liabilities", "borrowed_loans_short", "debt_payable", "misc_liabilities_short",
  "total_assets",
  "revenue", "cost_of_price", "expenses_comm", "expenses_admin", 
  "income_bef_tax","income_shareholding", "interest_receivable", "interest_payable", "income_misc", "expenses_misc",
  "net_income")

setnames(financials, old = old_names, new = new_names)

financial_cols <- new_names

# Replace NAs in the statements with zeroes ====
financials[, has_statements := fifelse(filed == 1 | imputed == 1, 1, 0)]

# Since we work with financial variables from the balance and the profit and loss account only we can safely assume that NAs are zeroes
financials[has_statements == 1, (financial_cols) := lapply(.SD, function (x) fifelse(is.na(x), 0, x)), .SDcols = financial_cols]

## Deflators ====
deflators <- fread(paste0(path_d, "some_folder/zombie_firms/data/ipc_deflators_2002_2025.csv"), data.table = TRUE)

financials <- merge(financials, deflators, by = c("year", "region"), all.x = TRUE, all.y = FALSE)

# (Gal, 2013) suggests to compute investments as difference  capital(t) - capital(t-1), both adjusted for inflation in t 
setorderv(financials, c("inn", "year"), c(1, 1))
financials[, inn_b1 := shift(inn, type = "lag", n = 1)]
financials[, capital_b1 := shift(capital, type = "lag", n = 1)]
financials[inn != inn_b1, capital_b1 := NA]

financial_cols <- c(financial_cols, "capital_b1")

financials[, (financial_cols) := lapply(.SD, function (x) x/deflator_2021), .SDcols = financial_cols]

## Clean up
rm(deflators); gc()

# Confirm that financial columns are clean
# financials[has_statements == 1, lapply(.SD, function (x) sum(is.na(x))), .SDcols = financial_cols]

# Clean up wrongfully positive/negative values ====

# Positive columns which should not be accidentally negaive
columns_positive_strict <- c("capital", "working_capital", "reserves", "cash_and_equivalents", "debt_receivable", 
                   "total_assets",
                   "revenue",
                   "income_shareholding", "interest_receivable", "income_misc")

# Positive columns which could be reasonably and accidentally considered negative 
columns_positive_lax <- c("long_liabilities", "borrowed_loans_long", "misc_liabilities_long",
                             "short_liabilities", "borrowed_loans_short", "debt_payable", "misc_liabilities_short")

columns_negative <- c("cost_of_price", "expenses_comm", "expenses_admin", "interest_payable", "expenses_misc")

columns_both <- c("equity_and_reserves", "retained_earnings", "income_bef_tax", "net_income")

# Check all columns are considered
# setdiff(financial_cols, c(columns_positive_strict, columns_positive_lax, columns_negative, columns_both)) # 0

financials[has_statements == 1, (columns_positive_strict) := lapply(.SD, function (x) fifelse(x < 0, NA, x) ), .SDcols = columns_positive_strict]
financials[has_statements == 1, (columns_positive_lax) := lapply(.SD, function (x) fifelse(x < 0, abs(x), x) ), .SDcols = columns_positive_lax]

financials[has_statements == 1, (columns_negative) := lapply(.SD, function (x) fifelse(x > 0, NA, x) ), .SDcols = columns_negative]

# rm(total_n, shares_negative, shares_positive, columns_positive, columns_negative); gc()

# Winsorize financial variables ====
winsorize <- function(x, low = 1, high = 99) {
  low_limit <- quantile(x, probs = seq(0, 1, 0.01), na.rm = TRUE)[low+1]
  high_limit <- quantile(x, probs = seq(0, 1, 0.01), na.rm = TRUE)[high+1]
  new_x <- fifelse(x < low_limit, low_limit, 
                   fifelse(x > high_limit, high_limit, x))
  return(new_x)
}

# View(financials[, lapply(.SD, function (x) quantile(x, seq(0, 1, 0.001), na.rm = TRUE)), .SDcols = columns_positive])
# View(financials[, lapply(.SD, function (x) quantile(x, seq(0, 1, 0.001), na.rm = TRUE)), .SDcols = columns_negative])
# View(financials[, lapply(.SD, function (x) quantile(x, seq(0, 1, 0.001), na.rm = TRUE)), .SDcols = columns_both])

columns_positive <- c(columns_positive_strict, columns_positive_lax) 

financials[, (columns_positive) := lapply(.SD, function (x) winsorize(x, low = 0, high = 99)), .SDcols = columns_positive]
financials[, (columns_negative) := lapply(.SD, function (x) winsorize(x, low = 1, high = 100)), .SDcols = columns_negative]
financials[, (columns_both) := lapply(.SD, function (x) winsorize(x, low = 1, high = 99)), .SDcols = columns_both]

## Define "market" ====
financials[, okved_2dig := str_extract(okved, "^.{2}")]

# Mark exit years
financials[, exit := fifelse(year(dissolution_date) == year, 1, 0) ]

# Compute financial variables ====

## Investments 

# Note that capital_b1 deflated by deflator in t, not t-1 
# 10% drop is "mean" depreciation for a "mean" fixed asset given clause 3 of article 258 of Tax Code with linear depreciation
financials[, investments := (capital - (capital_b1 * 0.9))]

# Winosorize investments more
columns_to_winz <- c("investments")
financials[, (columns_to_winz) := lapply(.SD, function (x) winsorize(x, low = 2, high = 98)), .SDcols = columns_to_winz]

financials[, investments_percent := round((capital - capital_b1)/capital_b1 * 100, 2)]

# if there is capital_b1 == 0 (inf) or capital and capital_b1 == 0 in the data assume zero net investments
financials[is.infinite(investments_percent) | is.nan(investments_percent), investments_percent := 0] 

financials[, negative_investments := fifelse(investments_percent < 0, 1, 0)]

# Check
# View(financials[inn == "0105044069", .(inn, year, investments_percent, investments, capital, capital_b1, negative_investments)])

## EBIT 

# 2300 + abs(-2330)
# There is no line_2300 that is income_bef_tax for simplified statements => compute it
# line_2300 = 2200 + 2340 + (-2330)
# line_2200 = 2110 + (-2120) (2120 for simplified is costs of sale + (-comm. expenses) + (-adm. expenses))
# => for simplified income_bef_tax = 2110 + (-2120) + 2340 + (-2330) + (-2350)
financials[simplified == 1, income_bef_tax := revenue + cost_of_price + income_misc + interest_payable + expenses_misc]

# To calculate EBIT "return" paid interest 
financials[, ebit := income_bef_tax + abs(interest_payable)]

## Long liabilities for simplified statements
financials[simplified == 1, long_liabilities :=  borrowed_loans_long + misc_liabilities_long]

## Short liabilities for simplified statements
financials[simplified == 1, short_liabilities := borrowed_loans_short + debt_payable + misc_liabilities_short]

## ROA 
financials[total_assets != 0, roa_net_inc := net_income/total_assets*100]
financials[total_assets != 0, roa_ebit := ebit/total_assets*100]

## Winsorize ROA more ====
columns_both <- c("roa_net_inc", "roa_ebit")
financials[, (columns_both) := lapply(.SD, function (x) winsorize(x, low = 3, high = 97)), .SDcols = columns_both]

# if total_assets are zero => roa is NA
# if interest_payable are zero => icr is NA
financials[is.infinite(roa_net_inc) | is.nan(roa_net_inc), roa_net_inc := NA]
financials[is.infinite(roa_ebit) | is.nan(roa_ebit), roa_ebit := NA]

## ICR 
# ICR = EBIT/Interest payable

# Firm is a zombie if it has EBIT insufficient to cover interest payments
# financials[, icr := NULL]
# financials[, icr_below_one := NULL]
# financials[, icr_imp := NULL]

financials[interest_payable != 0, icr := ebit/abs(interest_payable)]

# If there is no debt assign 0 to icr_below_one (no debts to cover means debt is covered)
financials[, icr_below_one := fifelse(icr >= 1 | interest_payable == 0, 0, 1)]
# financials[is.na(icr), .N, .(has_statements, interest_payable == 0)]
#    has_statements interest_payable        N
#             <num>           <lgcl>    <int>
# 1:              0               NA 22498072
# 2:              1             TRUE 23553575
# 3:              1            FALSE     2125

# Mean ICR over three years

# assign extreme value of ICR to compute mean ICR so that firms without debt are considered firms with "good" ICR 
financials[has_statements == 1, icr_imp := fifelse(interest_payable == 0, 10e9, icr)]

# financials[!is.na(icr_imp), .N, .(has_statements, interest_payable == 0, icr_imp == 10e9)]
#    has_statements interest_payable        N
#             <num>           <lgcl>    <int>
# 1:              0               NA 22498072
# 2:              1            FALSE     2125

setorderv(financials, c("inn", "year"), c(1, 1))
financials[, inn_b2 := shift(inn, type = "lag", n = 2)]
financials[, icr_imp_b1 := shift(icr_imp, type = "lag", n = 1)]
financials[, icr_imp_b2 := shift(icr_imp, type = "lag", n = 2)]
financials[inn != inn_b1, icr_imp_b1 := NA]
financials[inn != inn_b2, icr_imp_b2 := NA]

financials[, mean_icr_3yrs := rowMeans(.SD), .SDcols = c("icr_imp", "icr_imp_b1", "icr_imp_b2")]

financials[, mean_icr_3yrs_below_25 := fifelse(mean_icr_3yrs < 2.5, 1, 0)]

## Leverage

# Compute long and short liabilities variables for the firms with simplified statements
financials[simplified == 1, long_liabilities :=  borrowed_loans_long + misc_liabilities_long]
financials[simplified == 1, short_liabilities := borrowed_loans_short + debt_payable + misc_liabilities_short]

financials[total_assets != 0, leverage := (long_liabilities + short_liabilities)/total_assets]

# financials[, .N, .(is.infinite(leverage), is.nan(leverage))]
# if total_assets are zero => leverage is NA
financials[is.infinite(leverage) | is.nan(leverage), leverage := NA]

# leverage below 50%
financials[, leverage_below_05 := fifelse(leverage < 0.5, 1, 0)]

# Albuquerque, Iyer, 2024	compute a leverage ratio above the median firm in the country-industry pair
# I compute median leverage by industry-year

financials[, median_leverage_ind_year := median(leverage, na.rm = TRUE), by = .(okved_2dig, year)]
financials[, leverage_higher_median_ind_year := fifelse(leverage > median_leverage_ind_year, 1, 0)]

# Carreira et al., 2022 use the following criteria:
# "leverage is higher than the industry-median (at the two-digit NACE Rev.2 level) of the low return-on-assets exiting group"
# This approach leads to large number of NAs and is inpplicable for the recent year unavailable in the data => omit it

financials[, median_roa_ind_year := median(roa_ebit, na.rm = TRUE), by = .(okved_2dig, year)]
financials[, roa_below_median_ind_year := fifelse(roa_ebit < median_roa_ind_year, 1, 0)]
financials[, median_leverage_ind_year_low_roa := median(leverage[roa_below_median_ind_year == 1], na.rm = TRUE), by = .(okved_2dig, year)]

financials[, leverage_higher_median_ind_year_low_roa := fifelse(leverage > median_leverage_ind_year_low_roa, 1, 0)]

## Negative growth

setorderv(financials, c("inn", "year"), c(1, 1))
financials[, revenue_b1 := shift(revenue, type = "lag", n = 1)]
financials[inn != inn_b1, revenue_b1 := NA]

financials[, negative_growth := fifelse( (revenue - revenue_b1) < 0, 1, 0)]

financials[, revenue_b1 := NULL]

## Debt service capacity

financials[, total_loans := borrowed_loans_short + borrowed_loans_long]

financials[, ebit_to_debt := ebit/total_loans]

# If there is no debt than we assume the firm has debt capacity
financials[borrowed_loans_short == 0 & borrowed_loans_long == 0, ebit_to_debt := 1]

financials[, hasnt_debt_service_capacity_20 := fifelse(ebit_to_debt < 0.2, 1, 0)]

financials[, hasnt_debt_service_capacity_5 := fifelse(ebit_to_debt < 0.05, 1, 0)]

## Negative ROA
financials[, negative_roa := fifelse(roa_net_inc < 0, 1, 0)]

## Growth of debt 
financials[, total_liabilities := long_liabilities + short_liabilities]

financials[, total_loans_b1 := shift(total_loans, type = "lag", n = 1)]
financials[inn != inn_b1, total_loans_b1 := NA]

financials[, debt_growth := fifelse( (total_loans - total_loans_b1) > 0, 1, 0)]
                                      
## Age > 5 or 10 years
financials[, age_above_10 := fifelse(age > 10, 1, 0)]
financials[, age_above_5 := fifelse(age > 5, 1, 0)]

## ROA above baseline interest rate

# Load and attach data about weighted-mean yearly refinanincing rates of the Central Bank ====
base_interest_yearly <- readxl::read_xlsx(paste0(path_d, "some_folder/zombie_firms/data/key_interest_rate_yearly.xlsx"))
setDT(base_interest_yearly)
base_interest_yearly$year <- as.numeric(base_interest_yearly$year)
base_interest_yearly <- base_interest_yearly[, .(year, base_rate = refin_rate)]

financials <- merge(financials, base_interest_yearly, all.x = TRUE, all.y = FALSE, by = "year")

# If firm pays no interest, that is has no debt => roa covers interest
financials[, roa_covers_interest_refin := fifelse(roa_net_inc > base_rate | interest_payable == 0, 1, 0)]

financials[, base_rate := NULL]

# Repeat for yearly ruonia ====
base_interest_yearly <- readxl::read_xlsx(paste0(path_d, "some_folder/zombie_firms/data/ruonia_rate_yearly.xlsx"))
setDT(base_interest_yearly)
base_interest_yearly$year <- as.numeric(base_interest_yearly$year)
base_interest_yearly <- base_interest_yearly[, .(year, base_rate = ruonia_rate)]

financials <- merge(financials, base_interest_yearly, all.x = TRUE, all.y = FALSE, by = "year")

# If firm pays no interest, that is has no debt => roa covers interest
financials[, roa_covers_interest_ruonia := fifelse(roa_net_inc > base_rate | interest_payable == 0, 1, 0)]

# Altman Z ====

# We do not have retained earnings (line 1370) for firms filed simplified statements.

# We approximately recompute them by taking equity_and_reserves (line 1300) and substracting the chapter capital avaiable from the EGRUL
# Number of participants ====
# Load a panel with equity interest in limited liability corporations

load(paste0(path_d, "/egrul/pseudo_panels/egrul_init_23may26.rdata"))

egrul_init <- egrul_init[!is.na(inn), ]

egrul_init <- egrul_init[, .(inn, datedump, init_equity)]

egrul_init[, year := fcase(
  datedump == "2015-08-29", 2014,
  datedump == "2016-01-01", 2015,
  datedump == "2017-01-01", 2016,
  datedump == "2018-01-01", 2017,
  datedump == "2019-01-01", 2018,
  datedump == "2020-01-01", 2019,
  datedump == "2021-01-01", 2020,
  datedump == "2022-01-01", 2021,
  datedump == "2023-01-01", 2022,
  datedump == "2024-01-01", 2023,
  datedump == "2025-01-01", 2024,
  default = NA
)]

egrul_init[, init_equity := round(init_equity/1000, 0)]

egrul_init <- unique(egrul_init[, .(inn, year, share_capital = init_equity)])

financials <- merge(financials, egrul_init, by = c("inn", "year"), all.x = TRUE, all.y = FALSE)

financials[total_assets != 0, x1 := working_capital/total_assets]

financials[total_assets != 0 & simplified == 0, x2 := retained_earnings/total_assets]
financials[total_assets != 0 & simplified == 1, x2 := (equity_and_reserves - share_capital)/total_assets]

# Estimate changes 
quantile(financials[total_assets != 0 & simplified == 0, retained_earnings - (equity_and_reserves - share_capital)], seq(0, 0.99, 0.05), na.rm = TRUE)
#             0%             5%            10%            15%            20%            25%            30%            35%            40%            45%            50%            55%            60%            65% 
# -382951.843713   -5754.829670    -220.376536     -27.609820      -7.284621      -4.955052      -3.619117      -3.081750      -2.607171      -2.360654      -1.996050      -1.713474      -1.419393      -0.920000 
#            70%            75%            80%            85%            90%            95% 
#       0.000000       0.000000       1.236526       2.226816       4.981875      17.638916 

financials[total_assets != 0, x3 := ebit/total_assets]
financials[total_assets != 0, x4 := (long_liabilities + short_liabilities)/total_assets]

financials[, altman_z := 3.25 + (6.56 * x1) + (3.26 * x2) + (6.72 * x3) + (1.05 * x4)]

financials[, alt_z_below_threshold := fifelse(altman_z < 1.1, 1, 0)]

financials[, c("x1", "x2", "x3", "x4") := NULL]

setorderv(financials, c("inn", "year"), c(1, 1))
financials[, altman_z_b1 := shift(altman_z, type = "lag", n = 1)]
financials[, altman_z_b2 := shift(altman_z, type = "lag", n = 2)]
financials[inn != inn_b1, altman_z_b1 := NA]
financials[inn != inn_b2, altman_z_b2 := NA]

financials[, mean_altman_z_3yrs := rowMeans(.SD), .SDcols = c("altman_z", "altman_z_b1", "altman_z_b2")]

financials[, mean_altman_z_3yrs_below_11 := fifelse(mean_altman_z_3yrs < 1.1, 1, 0)]

# Create technical variables ====
financials[, zero_interest := fifelse(interest_payable == 0, 1, 0)]
financials[, zero_assets := fifelse(total_assets == 0 | (is.na(total_assets) & has_statements == 1), 1, 0)] # There are apprx 12.5k of is.na(total_assets => it is quite safe to consider them zero
financials[, zero_loans := fifelse(total_loans == 0, 1, 0)] # There are apprx 12.5k of is.na(total_assets => it is quite safe to consider them zero

# Save file ====

financials <- financials[, .(inn, year, region, okved_section, okved_2dig, has_statements, zero_interest, zero_assets, zero_loans, age, # demography
               capital, total_liabilities, total_loans, leverage, total_assets, revenue, ebit, roa_net_inc, roa_ebit, investments_percent, investments, icr, altman_z, interest_payable, # financials
               debt_growth, negative_investments, icr_below_one, mean_icr_3yrs_below_25, leverage_below_05, leverage_higher_median_ind_year, leverage_higher_median_ind_year_low_roa, negative_growth,  
               age_above_10, age_above_5, roa_covers_interest_refin, roa_covers_interest_ruonia,
               alt_z_below_threshold, mean_altman_z_3yrs_below_11, negative_roa, 
               hasnt_debt_service_capacity_20, hasnt_debt_service_capacity_5 # zombie criteria
               )]

write_fst(financials, paste0(path_d, "some_folder/zombie_firms/data/financial_panel.fst"))
