library(stringr); library(stringi)
library(data.table)
library(fst)
library(ggplot2)
library(forcats)
library(lubridate)
library(corrplot)
library(psych)
library(mltools)
options(scipen = 999)

# Declare working directory beforehand in an environment variable
# STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("STUDY_PATH")
setwd(path)
path_d <- Sys.getenv("DRIVE_D")

# Load the data ====
financials <- read_fst(paste0(path_d, "some_folder/zombie_firms/data/financial_panel_zmb_definitions.fst"), as.data.table = TRUE)
setorderv(financials, c("inn", "year"), c(1, 1))

# N and share in EGRUL ====
zombie_cols <- grep("zmb", colnames(financials), value = TRUE)
has_zombie_cols <- grep("has_data$", zombie_cols, value = TRUE)
zombie_cols <- setdiff(zombie_cols, has_zombie_cols)

n_zombies_yearly <- financials[, lapply(.SD, function (x) sum(x)), .SDcols = zombie_cols, keyby = year]
n_zombies_yearly[, (zombie_cols) := lapply(.SD, function (x) fifelse(x == 0, NA_real_, x)), .SDcols = zombie_cols]
#colnames(n_zombies_yearly)[colnames(n_zombies_yearly) %in% zombie_cols] <- paste0(zombie_cols, "_N")
  
share_zombies_yearly <- financials[has_statements == 1, lapply(.SD, function (x) round(mean(x)*100, 2)), .SDcols = zombie_cols, keyby = year]
share_zombies_yearly[, (zombie_cols) := lapply(.SD, function (x) fifelse(x == 0, NA_real_, x)), .SDcols = zombie_cols]
#colnames(share_zombies_yearly)[colnames(share_zombies_yearly) %in% zombie_cols] <- paste0(zombie_cols, "_share")

n_zombies_yearly <- dcast(
  melt(n_zombies_yearly, id.vars = "year", variable.name = "zombie_measure"),
  zombie_measure ~ year,
  value.var = "value"
)

year_cols <- as.character(2011:2024)
colnames(n_zombies_yearly)[colnames(n_zombies_yearly) %in% year_cols] <- paste0(year_cols, "_N")

share_zombies_yearly <- dcast(
  melt(share_zombies_yearly, id.vars = "year", variable.name = "zombie_measure"),
  zombie_measure ~ year,
  value.var = "value"
)

colnames(share_zombies_yearly)[colnames(share_zombies_yearly) %in% year_cols] <- paste0(year_cols, "_share")

zmb_n_share <- merge(n_zombies_yearly, share_zombies_yearly, by = "zombie_measure")

setcolorder(zmb_n_share, neworder = c("zombie_measure", "2011_N", "2011_share", "2012_N", "2012_share", "2013_N", "2013_share", "2014_N", "2014_share", "2015_N", "2015_share",
                                      "2016_N", "2016_share","2017_N", "2017_share", "2018_N", "2018_share","2019_N", "2019_share","2020_N", "2020_share",
                                      "2021_N", "2021_share","2022_N", "2022_share", "2023_N", "2023_share", "2024_N", "2024_share"))

writexl::write_xlsx(zmb_n_share, paste0(path_d, "some_folder/zombie_firms/plots_tables/tables/zmb_n_share_yearly.xlsx"))


# Share by industry ====

zombie_cols <- grep("zmb", colnames(financials), value = TRUE)
has_zombie_cols <- grep("has_data$", zombie_cols, value = TRUE)
zombie_cols <- setdiff(zombie_cols, has_zombie_cols)


n_by_year_industry <- rbindlist(
  lapply(zombie_cols, function(zmb) {
    financials[has_statements == 1,  {
      total_fin <- .N # [!is.na(get(zmb))]
      zombie_fin <- sum(get(zmb), na.rm = TRUE)
      nfirms <- zombie_fin / total_fin * 100
      as.list(nfirms)
    }, 
    by = .(year, okved_section)][, zmb_indicator := zmb]
  })
)

setnames(n_by_year_industry, "V1", "nfirms")

selected_zombies <- c("zmb_carreira_ruonia") 

n_by_year_industry_long <- melt(n_by_year_industry, id.vars = c("year", "zmb_indicator", "okved_section"))

n_by_year_industry_long[value == 0, value := NA_real_]

irrelevant_sections <- c("U", "T", "O")

n_by_year_industry_long <- n_by_year_industry_long[!okved_section %chin% irrelevant_sections & zmb_indicator %chin% selected_zombies, ]

n_by_year_industry_long[, okved_section_full:= fcase(
  okved_section == "A", "А. Хозяйство", 
  okved_section == "B", "B. Полезные ископаемые", 
  okved_section == "C",  "C. Обраб. пр-ва", 
  okved_section == "D",  "D. Энергия", 
  okved_section == "E",  "E. Водоснабжение, отходы", 
  okved_section == "F",  "F. Строительство",
  okved_section == "G",  "G. Торговля",
  okved_section == "H",  "H. Транспорт., хранение",
  okved_section == "I",  "I. Гостиницы, общ. питание",
  okved_section == "J",  "J. Информация и связь",
  okved_section == "L",  "L. Недвижимость",
  okved_section == "M",  "M. Профес. деятельность",
  okved_section == "N",  "N. Админ. деятельность",
  okved_section == "P",  "P. Образование",
  okved_section == "Q",  "Q. Здравоохранение",
  okved_section == "R",  "R. Культура и спорт",
  okved_section == "S",  "S. Прочие услуги",
  default = NA
)]

n_by_year_industry_long <- n_by_year_industry_long[year %chin% c("2014", "2018", "2024"), ]
n_by_year_industry_long$value <- round(n_by_year_industry_long$value, 2)
shares_by_year_industry <- dcast(n_by_year_industry_long, okved_section_full ~ year, value.var = "value")



writexl::write_xlsx(shares_by_year_industry, paste0(path_d, "some_folder/zombie_firms/plots_tables/tables/shares_by_year_industry.xlsx"))



# Data availability ====
# Mark firms which have all necessary data (filed statements, has assets, interest etc.)
n_criteria <- length(has_zombie_cols)
financials[, has_all_data := fifelse(rowSums(.SD) ==  n_criteria, 1, 0), .SDcols = has_zombie_cols]
financials[, .N, keyby = .(has_statements, has_all_data)]
# 1:              0            0 22500558
# 2:              1            0 13790086
# 3:              1            1 12482431

data_availability_full_n <- financials[, lapply(.SD, function (x) round(sum(x)/1000, 1)), .SDcols = has_zombie_cols, keyby = year]
data_availability_full_n
#      year zmb_albuquerque_has_data zmb_yamada_has_data zmb_mcgowan_has_data zmb_andrews_has_data zmb_acharya_has_data zmb_carreira_has_data zmb_storz_has_data zmb_altman_has_data zmb_ncr_has_data
#     <int>                    <num>               <num>                <num>                <num>                <num>                 <num>              <num>               <num>            <num>
#  1:  2011                      0.0                 0.0                  0.0                  0.0                  0.0                   0.0                0.0                 0.0              0.0
#  2:  2012                      0.0                 0.0                119.1                  0.0                  0.0                 118.5                0.0                 0.0            574.0
#  3:  2013                      0.0                 0.0                522.8                  0.0                488.7                 519.4                0.0                76.3           1116.4
#  4:  2014                    425.9               430.2               1009.5                425.7                963.1                 996.6              425.7               277.9           1445.7
#  5:  2015                    837.9               854.3               1317.5                837.4               1260.2                1297.1              837.4               651.9           1664.6
#  6:  2016                   1077.6              1101.5               1476.9               1076.7               1414.7                1452.7             1076.7              1341.1           1738.5
#  7:  2017                   1226.8              1258.0               1577.6               1225.6               1521.8                1546.8             1225.6              1444.0           1800.3
#  8:  2018                   1312.2              1351.5               1645.2               1310.6               1579.4                1606.6             1310.6              1496.7           1933.3
#  9:  2019                   1317.4              1382.4               1758.7               1315.6               1695.2                1682.1             1315.6              1549.3           1923.2
# 10:  2020                   1440.6              1529.0               1800.9               1438.4               1729.3                1717.8             1438.4              1563.0           1949.3
# 11:  2021                   1458.7              1562.7               1816.9               1456.1               1749.1                1715.0             1456.1              1557.7           1898.1
# 12:  2022                   1471.1              1586.4               1790.8               1468.3               1715.0                1693.8             1468.3              1535.4           1869.3
# 13:  2023                   1467.7              1579.0               1787.3               1465.0               1715.7                1695.0             1465.0              1536.0           1854.2
# 14:  2024                   1458.6              1561.3               1764.5               1456.5               1679.5                1681.0             1456.5              1517.0           1813.0

data_availability_full_share <- financials[, lapply(.SD, function (x) round(sum(x)/.N*100, 2)), .SDcols = has_zombie_cols, keyby = year]
data_availability_full_share
#      year zmb_albuquerque_has_data zmb_yamada_has_data zmb_mcgowan_has_data zmb_andrews_has_data zmb_acharya_has_data zmb_carreira_has_data zmb_storz_has_data zmb_altman_has_data zmb_ncr_has_data
#     <int>                    <num>               <num>                <num>                <num>                <num>                 <num>              <num>               <num>            <num>
#  1:  2011                     0.00                0.00                 0.00                 0.00                 0.00                  0.00               0.00                0.00             0.00
#  2:  2012                     0.00                0.00                 3.32                 0.00                 0.00                  3.31               0.00                0.00            16.01
#  3:  2013                     0.00                0.00                14.09                 0.00                13.17                 14.00               0.00                2.06            30.08
#  4:  2014                    11.02               11.13                26.13                11.02                24.92                 25.79              11.02                7.19            37.41
#  5:  2015                    20.85               21.26                32.79                20.84                31.36                 32.28              20.84               16.22            41.43
#  6:  2016                    25.67               26.24                35.19                25.65                33.71                 34.61              25.65               31.95            41.42
#  7:  2017                    30.42               31.19                39.12                30.39                37.73                 38.35              30.39               35.80            44.64
#  8:  2018                    33.94               34.96                42.55                33.90                40.85                 41.55              33.90               38.71            50.01
#  9:  2019                    36.49               38.29                48.71                36.44                46.95                 46.59              36.44               42.91            53.27
# 10:  2020                    44.65               47.39                55.82                44.58                53.60                 53.24              44.58               48.45            60.42
# 11:  2021                    49.52               53.05                61.68                49.44                59.38                 58.23              49.44               52.89            64.44
# 12:  2022                    52.39               56.49                63.77                52.29                61.07                 60.32              52.29               54.68            66.57
# 13:  2023                    52.95               56.96                64.48                52.85                61.89                 61.15              52.85               55.41            66.89
# 14:  2024                    53.50               57.27                64.72                53.42                61.60                 61.66              53.42               55.64            66.50

data_availability_hasstatements_share <- financials[has_statements == 1, lapply(.SD, function (x) round(sum(x)/.N*100, 2)), .SDcols = has_zombie_cols, keyby = year]
data_availability_hasstatements_share
#      year zmb_albuquerque_has_data zmb_yamada_has_data zmb_mcgowan_has_data zmb_andrews_has_data zmb_acharya_has_data zmb_carreira_has_data zmb_storz_has_data zmb_altman_has_data zmb_ncr_has_data
#     <int>                    <num>               <num>                <num>                <num>                <num>                 <num>              <num>               <num>            <num>
#  1:  2011                     0.00                0.00                 0.00                 0.00                 0.00                  0.00               0.00                0.00             0.00
#  2:  2012                     0.00                0.00                 9.74                 0.00                 0.00                  9.69               0.00                0.00            46.94
#  3:  2013                     0.00                0.00                31.54                 0.00                29.48                 31.33               0.00                4.60            67.34
#  4:  2014                    22.44               22.67                53.21                22.43                50.76                 52.53              22.43               14.65            76.19
#  5:  2015                    40.61               41.40                63.86                40.59                61.08                 62.87              40.59               31.60            80.68
#  6:  2016                    51.81               52.97                71.02                51.77                68.03                 69.85              51.77               64.49            83.59
#  7:  2017                    55.46               56.87                71.32                55.40                68.80                 69.92              55.40               65.28            81.38
#  8:  2018                    59.66               61.44                74.80                59.58                71.80                 73.04              59.58               68.04            87.89
#  9:  2019                    60.36               63.34                80.58                60.28                77.67                 77.08              60.28               70.99            88.12
# 10:  2020                    67.80               71.97                84.76                67.70                81.40                 80.85              67.70               73.57            91.75
# 11:  2021                    70.18               75.19                87.42                70.06                84.16                 82.52              70.06               74.95            91.33
# 12:  2022                    72.27               77.94                87.98                72.14                84.26                 83.22              72.14               75.43            91.84
# 13:  2023                    72.94               78.47                88.83                72.81                85.27                 84.24              72.81               76.34            92.15
# 14:  2024                    75.51               80.83                91.35                75.41                86.95                 87.03              75.41               78.54            93.86
data_availability <- rbindlist(list(full_n = data_availability_full_n, share_in_all_firms = data_availability_full_share, share_in_all_filing_firms = data_availability_hasstatements_share), idcol = c("type"))

# Zero-interest loans ====
financials[has_statements == 1, mean(zero_interest), keyby = zero_loans]
#   zero_loans        V1
# 1:          0 0.7867561
# 2:          1 0.9756641
