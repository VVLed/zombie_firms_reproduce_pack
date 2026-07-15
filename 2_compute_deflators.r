library(data.table)
library(ggplot2)
library(stringr); library(stringi)
library(readxl)
library(arrow)
options(scipen = 999)

# Declare working directory beforehand in an environment variable
# STUDY_PATH = "path_to_your_folder"
# with the aid of usethis::edit_r_environ()
# Restart R session for the changes to take effect
path <- Sys.getenv("STUDY_PATH")
setwd(path)
path_d <- Sys.getenv("DRIVE_D")

## Financials ====
# Read RFSD metadata from local file
RFSD <- open_dataset(paste0(path_d, "_datasets/RFSD"))

# Load only pre-2023 data into memory
scan_builder <- RFSD$NewScan()
# scan_builder$Filter()

scan_builder$Project(cols = c("region"))

scanner <- scan_builder$Finish()
financials <- as.data.table(scanner$ToTable())

rfsd_region_names <- sort(unique(financials$region))     

# Load the data ====
column_names <- c("region", "type", paste0("year_", as.character(2002:2025)))
year_cols <- paste0("year_", as.character(2002:2025))
deflators <- as.data.table(read_xls("data/deflators/ipc_2002_2025.xls", skip = 5, col_names = column_names))
deflators[, type := NULL]

# Rename regions 

deflators[, region := str_to_lower(region)]

# NB! I use all-Russian deflators for regions without region-specific deflators
# Move each all-Russian deflator into a single row
deflators[, (year_cols) := lapply(.SD, function(x) fifelse(region == "российская федерация" & is.na(x), x[region == "российская федерация без учета новых субъектов (с 01.01.2023)"], x) ), .SDcols = year_cols]

# Delete unnecessary deflators
pattern_to_delete <- 'новых|едеральный|ентраль|апад|жный|авказ|риволжс|ральск|ибирски|альневосточн|экономическ'
deflators <- deflators[!str_detect(string = region, pattern = pattern_to_delete), ]

pattern_to_delete <- 'таймыр|усть|агин|пермяц|эвенк|коряк'
deflators <- deflators[!str_detect(string = region, pattern = pattern_to_delete), ]

# Oblast's with autonomous regions
deflators <- deflators[!region %chin% c("тюменская область", "архангельская область"), ]

detect_region <- function(col_region, pattern_reg) {
  stringi::stri_detect_regex(str = col_region, pattern = pattern_reg, case_insensitive = TRUE)
}

deflators[, region_rfsd := fcase(
  detect_region(region, "Санкт"), "sankt-petersburg",
  
  # NB! Autonomous regions and regions with simillar names
  detect_region(region, "Тюменск.*(?=автоном|АО)"), "tyumen",
  detect_region(region, "Хант|Югр|ХМАО"), "khanty-mansijsk",
  detect_region(region, "Ямал|ЯНАО"), "yamal-nenets ao",
#  detect_region(region, "Тюмен"), "Тюменская область",
  
  detect_region(region, "Архангел.*(?=автоном|АО)"), "arkhangelsk",
  detect_region(region, "(?<!Ямало[-– ])Ненец"), "nenets ao", 
#  detect_region(region, "Архангельск"), "Архангельская область",
  
  
  detect_region(region, "Алтайск"), "altai terr.",
  detect_region(region, "Алтай(?!ск)"), "altai rep.",
  
  detect_region(region, "^Омск"), "omsk",
  detect_region(region, "Томск"), "tomsk",
  
  detect_region(region, "Якут|Саха(?!л)"), "yakutia",
  detect_region(region, "Сахалин"), "sakhalin",
  
  detect_region(region, "Москв"), "moscow city",
  detect_region(region, "Москов"), "moscow reg.",
  detect_region(region, "Севаст"), "sevastopol",
  
  detect_region(region, "Белгород"), "belgorod",
  detect_region(region, "Брянск"), "bryansk",
  detect_region(region, "Владимир"), "vladimir",
  detect_region(region, "Воронеж"), "voronezh",
  detect_region(region, "Иванов"), "ivanovo",
  detect_region(region, "Калуж"), "kaluga",
  detect_region(region, "Костром"), "kostroma",
  detect_region(region, "Курск"), "kursk",
  detect_region(region, "Липец"), "lipetzk",
  detect_region(region, "Орлов"), "oryol",
  detect_region(region, "Рязан"), "ryazan",
  detect_region(region, "Смолен"), "smolensk",
  detect_region(region, "Тамбов"), "tambov",
  detect_region(region, "Твер"), "tver",
  detect_region(region, "Туль"), "tula",
  detect_region(region, "Ярослав"), "yaroslavl",
  detect_region(region, "Карел"), "karelia",
  detect_region(region, "Коми"), "komi",
  detect_region(region, "Вологод"), "vologda",
  detect_region(region, "Калинин"), "kaliningrad",
  detect_region(region, "Ленинград"), "leningrad",
  detect_region(region, "Мурман"), "murmansk",
  detect_region(region, "Новгород"), "novgorod",
  detect_region(region, "Псков"), "pskov",
  detect_region(region, "Адыг"), "adygeya",
  detect_region(region, "Калм"), "kalmykia",
  detect_region(region, "Крым"), "crimea",
  detect_region(region, "Краснодар"), "krasnodar",
  detect_region(region, "Астрахан"), "astrakhan",
  detect_region(region, "Волгоград"), "volgograd",
  detect_region(region, "Ростов"), "rostov",
  detect_region(region, "Дагест"), "dagestan",
  detect_region(region, "Ингуш"), "ingushetia",
  detect_region(region, "Кабардин"), "kabardino-balkaria",
  detect_region(region, "Карачаев"), "karachaevo-chercessia",
  detect_region(region, "Осет|Алан"), "north ossetia",
  detect_region(region, "Чечен|Чечн"), "chechnya",
  detect_region(region, "Ставр"), "stavropol",
  detect_region(region, "Башк"), "bashkortostan",
  detect_region(region, "Марий"), "marij el",
  detect_region(region, "Мордов"), "mordovia",
  detect_region(region, "Татарстан"), "tatarstan",
  detect_region(region, "Удмурт"), "udmurtia",
  detect_region(region, "Чуваш"), "chuvashia",
  detect_region(region, "^Перм"), "perm",
  detect_region(region, "Киров"), "kirov",
  detect_region(region, "Нижегород"), "nizhni novgorod",
  detect_region(region, "Оренбург"), "orenburg",
  detect_region(region, "Пенз"), "penza",
  detect_region(region, "Самар"), "samara",
  detect_region(region, "Сарат"), "saratov",
  detect_region(region, "Ульянов"), "ulyanovsk",
  detect_region(region, "Курган"), "kurgan",
  detect_region(region, "Свердлов"), "sverdlovsk",
  detect_region(region, "Челябин"), "chelyabinsk",
  detect_region(region, "Тыва|Тува"), "tuva",
  detect_region(region, "Хакас"), "khakasia",
  detect_region(region, "Краснояр"), "krasnoyarsk",
  detect_region(region, "Иркут"), "irkutsk",
  detect_region(region, "Кемер|Кузб"), "kemerovo",
  detect_region(region, "Новосиб"), "novosibirsk",
  detect_region(region, "Бурят"), "buryatia",
  detect_region(region, "Забайкал"), "zabaikalsk terr.",
  detect_region(region, "Камчат"), "kamchatka",
  detect_region(region, "Примор"), "primorsky terr.",
  detect_region(region, "Хабаров"), "khabarovsk",
  detect_region(region, "Амур"), "amur",
  detect_region(region, "Магад"), "magadan",
  detect_region(region, "Еврей"), "jewish ao",
  detect_region(region, "Чукот"), "chukotka",
  default = "unclear"
)]

# View(unique(deflators[, .(region, region_test)]))

# For regions available in the RFSD, but without region-specific deflators assign all-Russian deflators 
df <- data.frame()
for (reg in setdiff(rfsd_region_names, deflators$region_rfsd)) {
  row <- cbind(data.frame(region_rfsd = reg), deflators[region == "российская федерация", ..year_cols])
  df <- rbind(df, row)
}

deflators <- rbindlist(list(deflators, df), use.names = TRUE, fill = TRUE)

# For regions without region-specific deflators assign all-Russian deflators 
all_rus_defl <- deflators[region == "российская федерация", ]

for (col in year_cols) {
  deflators[is.na(get(col)), (col) := all_rus_defl[[col]]]
}

# Convert to long
deflators_long <- melt(deflators, id.vars = "region_rfsd", measure.vars = year_cols, variable.name = "year", value.name = "deflator")

deflators_long[, year := str_sub(year, start = 6)]

setorder(deflators_long, region_rfsd, year)

deflators_long[, ipc_defl_base := NA_real_]
deflators_long[year == 2021, ipc_defl_base := 100]

for(yr in 2020:min(deflators_long$year) ) {
  deflators_long[year == yr, ipc_defl_base := deflators_long[year == (yr + 1), ipc_defl_base] / deflators_long[year == (yr + 1), deflator] * 100]
}

for(yr in 2022:max(deflators_long$year) ) {
  deflators_long[year == yr, ipc_defl_base := deflators_long[year == (yr - 1), ipc_defl_base] * deflators_long[year == yr, deflator]  / 100 ]
}

deflators <- deflators_long[, .(year, region = region_rfsd, deflator_2021 = ipc_defl_base / 100)]

fwrite(x = deflators, "data/deflators/ipc_deflators_2002_2025.csv")
