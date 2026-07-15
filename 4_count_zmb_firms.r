library(stringr); library(stringi)
library(data.table)
library(fst)
library(ggplot2)
library(forcats)
library(lubridate)
library(corrplot)
options(scipen = 999)

# Declare working directory beforehand in an environment variable
# STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("STUDY_PATH")
setwd(path)
path_d <- Sys.getenv("DRIVE_D")

# Load the data ====
financials <- read_fst(paste0(path_d, "some_folder/zombie_firms/data/financial_panel.fst"), as.data.table = TRUE)
setorderv(financials, c("inn", "year"), c(1, 1))

financials[, .N, keyby = .(has_statements, zero_interest, zero_assets, zero_loans)]
#    has_statements zero_interest zero_assets zero_loans        N
# 1:              0          NA          NA         NA 22500558
# 2:              1           0           0          0  2340491
# 3:              1           0           0          1   370314
# 4:              1           0           1          0     1829
# 5:              1           0           1          1     1740
# 6:              1           1           0          0  8534890
# 7:              1           1           0          1 13855873
# 8:              1           1           1          0   107018
# 9:              1           1           1          1  1060362

orig_columns <- copy(colnames(financials))

# financials <- financials[has_statements == 1, ]

# lag inn
financials[, inn_b1 := shift(inn, type = "lag", n = 1)]
financials[, inn_b2 := shift(inn, type = "lag", n = 2)]
financials[, inn_b3 := shift(inn, type = "lag", n = 3)]

# Albuquerque, Iyer, 2024	 ====
# "firms that have for at least two consecutive years: 
# (i) an ICR below one, 
# (ii) a leverage ratio above the median firm in the country-industry pair, and 
# (iii) negative real sales growth. 
# 
# ICR, computed as the ratio of EBIT (earnings before interest, and taxes) to interest expenses
# leverage ratio <...>, computed as total debt (short-term and long-term) divided by total assets"

# "To exit the zombie status, we require a zombie firm to record for two consecutive years an ICR above one, or a leverage
# ratio below the median firm in the country-industry pair, or positive sales growth. "

albuquerque_cols <- c("icr_below_one", "leverage_higher_median_ind_year", "negative_growth")

financials[, paste0(albuquerque_cols, "_b1") := lapply(.SD, function(x) shift(x, type = "lag", n = 1)), .SDcols = albuquerque_cols]
financials[, paste0(albuquerque_cols, "_b2") := lapply(.SD, function(x) shift(x, type = "lag", n = 2)), .SDcols = albuquerque_cols]

financials[inn != inn_b1, paste0(albuquerque_cols, "_b1") := NA]
financials[inn != inn_b1, paste0(albuquerque_cols, "_b2") := NA]

albuquerque_cols <- c(albuquerque_cols, paste0(albuquerque_cols, "_b1"), paste0(albuquerque_cols, "_b2"))
financials[, zmb_albuquerque_has_data := as.integer(complete.cases(.SD)), .SDcols = albuquerque_cols]

financials[has_statements == 1, .N, keyby = .(zmb_albuquerque_has_data, zero_assets)]
#    zmb_albuquerque_has_data zero_assets        N
#                       <int>       <num>    <int>
# 1:                        0           0 11607293
# 2:                        0           1  1170949
# 3:                        1           0 13494275

financials[zmb_albuquerque_has_data == 1, zmb_albuquerque_bin := fifelse(
  icr_below_one == 1 & icr_below_one_b1 == 1 & 
    leverage_higher_median_ind_year == 1 & leverage_higher_median_ind_year_b1 == 1 & 
    negative_growth == 1 & negative_growth_b1 == 1, 
  1, 0)]

financials[zmb_albuquerque_has_data == 1, zmb_albuquerque_exit := fifelse(
  (icr_below_one == 0 & icr_below_one_b1 == 0) | 
    (leverage_higher_median_ind_year == 0 & leverage_higher_median_ind_year_b1 == 0) | 
    (negative_growth == 0 & negative_growth_b1 == 0), 
  1, 0)]

# Takes a few minutes
financials[, zmb_albuquerque := {
  
  is_zombie <- 0
  out <- integer(.N)
  
  for (i in seq_len(.N)) {
    
    if ((is_zombie != 1 | is.na(is_zombie)) & zmb_albuquerque_bin[i] == 0 & !is.na(zmb_albuquerque_bin[i]) & !is.na(zmb_albuquerque_exit[i]) ) {is_zombie <- 0}
    
    if ( zmb_albuquerque_bin[i] == 1 & !is.na(zmb_albuquerque_bin[i]) & !is.na(zmb_albuquerque_exit[i]) ) {is_zombie <- 1}
    
    if (is.na(zmb_albuquerque_bin[i]) | is.na(zmb_albuquerque_exit[i])) {is_zombie <- NA_real_}
    
    if ( zmb_albuquerque_exit[i] == 1 & !is.na(zmb_albuquerque_bin[i]) & !is.na(zmb_albuquerque_exit[i]) ) {is_zombie <- 0}
    
    out[i] <- is_zombie
    
  }
  
  out
  
}, by = inn]

financials[, c("zmb_albuquerque_bin", "zmb_albuquerque_exit") := NULL]

# Yamada et al., 2025	 ====
# "zombie firms as firms meeting the above three requirements for three consecutive years:
# - Interest rate requirement: Rate of interest paid < Average contracted interest rate on loans (stock base)* 
#   or 
#   Current term borrowings > Previous term borrowings.
# - Solvency requirement: ICR <1. (EBIT/interest expense)
# - Growth potential requirement: Founded at least ten years before."

yamada_cols <- c("icr_below_one", "debt_growth", "age_above_10")

financials[, paste0(yamada_cols, "_b1") := lapply(.SD, function(x) shift(x, type = "lag", n = 1)), .SDcols = yamada_cols]
financials[, paste0(yamada_cols, "_b2") := lapply(.SD, function(x) shift(x, type = "lag", n = 2)), .SDcols = yamada_cols]

financials[inn != inn_b1, paste0(yamada_cols, "_b1") := NA]
financials[inn != inn_b1, paste0(yamada_cols, "_b2") := NA]

yamada_cols <- c(yamada_cols, paste0(yamada_cols, "_b1"), paste0(yamada_cols, "_b2"))
financials[, zmb_yamada_has_data := as.integer(complete.cases(.SD)), .SDcols = yamada_cols]

financials[has_statements == 1, .N, keyby = .(zmb_yamada_has_data)]
# 1:                   0 12076351
# 2:                   1 14196166

financials[zmb_yamada_has_data == 1, zmb_yamada := fifelse( 
  icr_below_one == 1 & icr_below_one_b1 == 1 & icr_below_one_b2 == 1 &
    debt_growth == 1 & debt_growth_b1 == 1 & debt_growth_b2 == 1 &
    age_above_10 == 1 , 
  1, 0)]


# McGowan et al., 2018	 ====
# "a firm is defined as a zombie firm in 2013 if:
# - it is aged 10 years or older in 2013 and 
# - it had an interest coverage ratio less than one for three consecutive years (2011-2013)."
mcgowan_cols <- c("icr_below_one", "icr_below_one_b1", "icr_below_one_b2", "age_above_10")
financials[, zmb_mcgowan_has_data := as.integer(complete.cases(.SD)), .SDcols = mcgowan_cols]

financials[has_statements == 1, .N, keyby = .(zmb_mcgowan_has_data)]
# 1:                    0  7884760
# 2:                    1 18387757

financials[zmb_mcgowan_has_data == 1, zmb_mcgowan := fifelse( 
  icr_below_one == 1 & icr_below_one_b1 == 1 & icr_below_one_b2 == 1 &
    age_above_10 == 1 , 
  1, 0)]

# Andrews, Petroulakis, 2019	 ====
# "In both cases age > 10
# register an interest coverage ratio (the ratio of profit to interest payments) below 1 for three years in a row.
# or
# + - i) low debt service capacity for three years in a row; and 
# - ii) either negative return on assets or negative investment for three years in a row.
# >we set the limit for low debt service capacity as a ratio of EBIT to financial debt (sum of loans and longterm debt) lower than 20%."

andrews_cols <- c("hasnt_debt_service_capacity_20", "negative_investments", "negative_roa")

financials[, paste0(andrews_cols, "_b1") := lapply(.SD, function(x) shift(x, type = "lag", n = 1)), .SDcols = andrews_cols]
financials[, paste0(andrews_cols, "_b2") := lapply(.SD, function(x) shift(x, type = "lag", n = 2)), .SDcols = andrews_cols]

financials[inn != inn_b1, paste0(andrews_cols, "_b1") := NA]
financials[inn != inn_b1, paste0(andrews_cols, "_b2") := NA]

andrews_cols <- c(andrews_cols, paste0(andrews_cols, "_b1"), paste0(andrews_cols, "_b2"), "age_above_10")
financials[, zmb_andrews_has_data := as.integer(complete.cases(.SD)), .SDcols = andrews_cols]

financials[has_statements == 1, .N, keyby = .(zmb_andrews_has_data, zero_assets)]
# 1:                    0           0 11625659
# 2:                    0           1  1170949
# 3:                    1           0 13475909

financials[zmb_andrews_has_data == 1, zmb_andrews := fifelse( 
  hasnt_debt_service_capacity_20 == 1 & hasnt_debt_service_capacity_20_b1 == 1 & hasnt_debt_service_capacity_20_b2 == 1 &
    ( (negative_investments == 1 & negative_investments_b1 == 1 & negative_investments_b2 == 1) | (negative_roa == 1 & negative_roa_b1 == 1 & negative_roa_b2 == 1) ) &
    age_above_10 == 1,  
  1, 0)]

# Acharya et al., 2022	 ====
# "- Low-Quality1: Three-year average ICR implied rating of BB (ICR cutoff: 2.5) or lower.

acharya_cols <- c("mean_icr_3yrs_below_25")
financials[, zmb_acharya_has_data := as.integer(complete.cases(.SD)), .SDcols = acharya_cols]

financials[has_statements == 1, .N, keyby = .(zmb_acharya_has_data)]
# 1:                    0  8760868
# 2:                    1 17511649

financials[zmb_acharya_has_data == 1, zmb_acharya := fifelse(mean_icr_3yrs_below_25 == 1, 1, 0)] 

# Carreira et al., 2022	 ====
# "Following Schivardi et al. (2017), in this study, a firm is classified as a zombie whenever: 
# - (i) its return-on assets is lower than the low-risk interest rate at least for a period of three consecutive years, 
# - (ii) its leverage is higher than the industry-median (at the two-digit NACE Rev.2 level) of the low return-on-assets exiting group and 
# - (iii) it is older than 5 years.
# >The return-on-assets is defined as EBITDA over total assets.
# > We compare return-on-assets with the annual average Euribor 12-month interest rate, the reference interest rate commonly used for loans by the Portuguese banking sector. 
# > The leverage is defined as the ratio of the sum of debt in current liabilities and longterm debt to total assets"

# "we screen the zombie identification by excluding “one-shot zombie” firms from the zombie group (i.e. one-off zombies or false zombies). 
# Conversely, we include “one-shot restructuring” firms, that is, zombie firms that become non-zombies in t + 1 and zombies again in t + 2 (i.e. false restructurings)."

carreira_cols <- c("roa_covers_interest_refin", "roa_covers_interest_ruonia", "leverage_higher_median_ind_year_low_roa")
financials[, paste0(carreira_cols, "_b1") := lapply(.SD, function(x) shift(x, type = "lag", n = 1)), .SDcols = carreira_cols]
financials[, paste0(carreira_cols, "_b2") := lapply(.SD, function(x) shift(x, type = "lag", n = 2)), .SDcols = carreira_cols]
financials[inn != inn_b1, paste0(carreira_cols, "_b1") := NA]
financials[inn != inn_b1, paste0(carreira_cols, "_b2") := NA]

carreira_cols <- c("roa_covers_interest_refin", "roa_covers_interest_refin_b1", "roa_covers_interest_refin_b2", 
                   "roa_covers_interest_refin", "roa_covers_interest_refin_b1", "roa_covers_interest_refin_b2", 
                   "leverage_higher_median_ind_year_low_roa", "age_above_5")

financials[, zmb_carreira_has_data := as.integer(complete.cases(.SD)), .SDcols = carreira_cols]

financials[has_statements == 1, .N, keyby = .(zmb_carreira_has_data, zero_assets)]
#    zmb_carreira_has_data zero_assets        N
#                    <int>       <num>    <int>
# 1:                     0           0  7379165
# 2:                     0           1  1170949
# 3:                     1           0 17722403

financials[zmb_carreira_has_data == 1, zmb_carreira_refin := fifelse(
  roa_covers_interest_refin == 0 & roa_covers_interest_refin_b1 == 0 & roa_covers_interest_refin_b2 == 0 &
    leverage_higher_median_ind_year_low_roa == 1 &
  age_above_5 == 1,
  1, 0
)]

financials[zmb_carreira_has_data == 1, zmb_carreira_ruonia := fifelse(
  roa_covers_interest_ruonia == 0 & roa_covers_interest_ruonia_b1 == 0 & roa_covers_interest_ruonia_b2 == 0 &
    leverage_higher_median_ind_year_low_roa == 1 &
    age_above_5 == 1,
  1, 0
)]

setorderv(financials, c("inn", "year"), c(1, 1))
financials[, inn_f1 := shift(inn, type = "lead", n = 1)]
financials[, inn_f2 := shift(inn, type = "lead", n = 2)]
financials[, zmb_carreira_refin_f1 := shift(zmb_carreira_refin, type = "lead", n = 1)]
financials[, zmb_carreira_refin_f2 := shift(zmb_carreira_refin, type = "lead", n = 2)]
financials[, zmb_carreira_ruonia_f1 := shift(zmb_carreira_ruonia, type = "lead", n = 1)]
financials[, zmb_carreira_ruonia_f2 := shift(zmb_carreira_ruonia, type = "lead", n = 2)]
financials[inn != inn_f1, c("zmb_carreira_refin_f1", "zmb_carreira_ruonia_f1") := NA]
financials[inn != inn_f2, c("zmb_carreira_refin_f2", "zmb_carreira_ruonia_f2") := NA]

# For Albuquerque, Iyer, 2024 criterion firms tend to de-zombify rarely => for firms with _f1 and _f2 observations we do not assume de-zombification.
financials[zmb_carreira_has_data == 1, zmb_carreira_refin := fifelse(!is.na(zmb_carreira_refin_f1) & !is.na(zmb_carreira_refin_f2) & zmb_carreira_refin_f1 == 0 & zmb_carreira_refin_f2 == 0, 0, zmb_carreira_refin)]
financials[zmb_carreira_has_data == 1, zmb_carreira_ruonia := fifelse(!is.na(zmb_carreira_ruonia_f1) & !is.na(zmb_carreira_ruonia_f2) & zmb_carreira_ruonia_f1 == 0 & zmb_carreira_ruonia_f2 == 0, 0, zmb_carreira_ruonia)]

financials[, c("zmb_carreira_refin_f1", "zmb_carreira_refin_f2", "zmb_carreira_ruonia_f1", "zmb_carreira_ruonia_f2") := NULL]

# Storz et al., 2017	 ====
# "we identify zombie firms in this paper as follows: A company is considered a zombie, whenever 
# - (i) its return on assets is negative, 
# - (ii) its net investments are negative, and 
# - (iii) its debt servicing capacity – measured as EBITDA over total financial debt – is lower than 5% for 
# - (iv) at least two consecutive years.
# Investment Net change in total fixed assets relative to previous year"

storz_cols <- c("hasnt_debt_service_capacity_5", "negative_investments", "negative_roa")
financials[, paste0(storz_cols, "_b1") := lapply(.SD, function(x) shift(x, type = "lag", n = 1)), .SDcols = storz_cols]
financials[, paste0(storz_cols, "_b2") := lapply(.SD, function(x) shift(x, type = "lag", n = 2)), .SDcols = storz_cols]

financials[inn != inn_b1, paste0(storz_cols, "_b1") := NA]
financials[inn != inn_b1, paste0(storz_cols, "_b2") := NA]

storz_cols <- c(storz_cols, paste0(storz_cols, "_b1"), paste0(storz_cols, "_b2"))

financials[, zmb_storz_has_data := as.integer(complete.cases(.SD)), .SDcols = storz_cols]
financials[has_statements == 1, .N, keyby = .(zmb_storz_has_data, zero_assets)]
#    zmb_storz_has_data zero_assets        N
#                 <int>       <num>    <int>
# 1:                  0           0 11625659
# 2:                  0           1  1170949
# 3:                  1           0 13475909

financials[zmb_storz_has_data == 1, zmb_storz := fifelse( 
  hasnt_debt_service_capacity_5 == 1 & hasnt_debt_service_capacity_5_b1 == 1 &
  negative_investments == 1 & negative_investments_b1 == 1 &
  negative_roa == 1 & negative_roa_b1 == 1,
  1, 0
)]

# Schivardi et al., 2020	 ====
# Altman z-score >= 8 is a nonsense. The larger z-score the better financial condition of a firm must be. It is unclear which Z-score Schivardi et al., 2020 uses.
# Basically, Z''-score was suggested by Altman, 1983 for non-public firms in developing countries with Z''-score < 1.1 indicating distressed firm
# Later Altman et al., 2024 for unclear reasons use Z′′-score (Z′′) below 0 for public corporations
# I stick to the original <1.1 criterion

financials[, zmb_altman_has_data := as.integer(complete.cases(.SD)), .SDcols = c("mean_altman_z_3yrs_below_11", "mean_icr_3yrs_below_1")]
financials[has_statements == 1, .N, keyby = .(zmb_altman_has_data, zero_assets)]
#    has_zombie_cols zero_assets        N
#              <int>       <num>    <int>
# 1:               0           0 10555193
# 2:               0           1  1170949
# 3:               1           0 14546375

financials[zmb_altman_has_data == 1, zmb_altman := fifelse(mean_altman_z_3yrs_below_11 == 1 & mean_icr_3yrs_below_1 == 1, 1, 0)]

# NCR, 2022 ====
# ICR below 1 for two consecutive years

financials[, zmb_ncr_has_data := as.integer(complete.cases(.SD)), .SDcols = c("icr_below_one", "icr_below_one_b1")]
financials[has_statements == 1, .N, keyby = .(zmb_ncr_has_data)]
#    zmb_ncr_has_data        N
#               <int>    <int>
# 1:                0  4692708
# 2:                1 21579809

financials[zmb_ncr_has_data == 1, zmb_ncr := fifelse(icr_below_one == 1 & icr_below_one_b1 == 1, 1, 0)]

# If we do not have data to classify firm as a zombie consider it non-zombie
zombie_cols <- grep("zmb", colnames(financials), value = TRUE)
has_zombie_cols <- grep("has_data$", zombie_cols, value = TRUE)
zombie_cols <- setdiff(zombie_cols, has_zombie_cols)

financials[, (zombie_cols) := lapply(.SD, function (x) fifelse(is.na(x), 0, x)), .SDcols = zombie_cols]


# Mark firms which have all necessary data (filed statements, has assets, interest etc.)
n_criteria <- length(has_zombie_cols)
financials[, has_all_data := fifelse(rowSums(.SD) ==  n_criteria, 1, 0), .SDcols = has_zombie_cols]
financials[, .N, keyby = .(has_statements, has_all_data)]
# 1:              0            0 22500558
# 2:              1            0 13790086
# 3:              1            1 12482431

cols_to_save <- c(orig_columns, zombie_cols, has_zombie_cols, "has_all_data")

financials <- financials[, ..cols_to_save]

write_fst(financials, paste0(path_d, "some_folder/zombie_firms/data/financial_panel_zmb_definitions.fst"))



