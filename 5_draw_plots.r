library(stringr); library(stringi)
library(data.table)
library(fst)
library(ggplot2)
library(forcats)
library(lubridate)
library(corrplot)
library(psych)
library(mltools)
library(ggpubr)
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

# Count ====
zombie_cols <- grep("zmb", colnames(financials), value = TRUE)
has_zombie_cols <- grep("has_data$", zombie_cols, value = TRUE)
zombie_cols <- setdiff(zombie_cols, has_zombie_cols)

financial_cols <- c(
  "total_assets", # results for "capital" are essentially the same as for the total assets
  "revenue",
  "total_loans", # "total_liabilities" are essentially the same, 
  "investments",
  "interest_payable"
)

## Share in the economy by years ====

shares_by_year <- rbindlist(
  lapply(zombie_cols, function(zmb) {
    financials[,  {
      total_fin <- sapply(.SD, function(x) sum(x, na.rm = TRUE)) # [!is.na(get(zmb))]
      zombie_fin <- sapply(.SD, function(x) sum(x[get(zmb) == 1], na.rm = TRUE))
      shares <- zombie_fin / total_fin * 100
      as.list(shares)
    }, 
    by = year, .SDcols = financial_cols][, zmb_indicator := zmb]
  })
)

n_by_year <- rbindlist(
  lapply(zombie_cols, function(zmb) {
    financials[,  {
      total_fin <- .N # [!is.na(get(zmb))]
      zombie_fin <- sum(get(zmb), na.rm = TRUE)
      nfirms <- zombie_fin / total_fin * 100
      as.list(nfirms)
    }, 
    by = year][, zmb_indicator := zmb]
  })
)

setnames(n_by_year, "V1", "nfirms")
shares_by_year <- merge(shares_by_year, n_by_year, by = c("year", "zmb_indicator"))

selected_zombies <- c("zmb_albuquerque", "zmb_carreira_ruonia", "zmb_mcgowan", "zmb_altman", "zmb_ncr") 

shares_by_year_long <- melt(shares_by_year, id.vars = c("year", "zmb_indicator"))

shares_by_year_long[value == 0, value := NA_real_]

shares_by_year_long$variable <- forcats::fct_relevel(as.factor(shares_by_year_long$variable), 
                                                                               "total_assets", "investments", "revenue",
                                                                               "total_loans", "interest_payable", "nfirms")

# Yearly share assets ====

yearly_share_assets <- ggplot(shares_by_year_long[year >= 2013 & zmb_indicator %chin% selected_zombies & variable == "total_assets", ], #  
       aes(x = year, y = value, group = zmb_indicator, linetype = zmb_indicator, shape = zmb_indicator)) +
  geom_line() +
  geom_point(size = 5) +
  scale_x_continuous(breaks = 2013:2024, labels = 2013:2024, expand = c(0.01, 0.01)) + 
  # scale_color_manual(
  #   values = c("zmb_albuquerque" = "red", "zmb_carreira_ruonia" = "orange", "zmb_mcgowan" = "green", "zmb_altman" = "blue", "zmb_ncr" = "darkgrey"),
  #   breaks = selected_zombies,
  #   labels = c("Albuquerque, Iyer, 2024", "Carreira et al., 2022", "McGowan et al., 2018", "Altman et al., 2024", "Тайкетаев, Доронкин, 2022"), name = "Определение зомби-фирмы") +
  scale_linetype_manual(
    values = c("zmb_albuquerque" = "solid", "zmb_carreira_ruonia" = "solid", "zmb_mcgowan" = "dashed", "zmb_altman" = "solid", "zmb_ncr" = "dashed"),
    breaks = selected_zombies,
    labels = c("Albuquerque, Iyer, 2024", "Carreira et al., 2022", "McGowan et al., 2018", "Altman et al., 2024", "Тайкетаев, Доронкин, 2022"), name = "Определение зомби-фирмы") +
  scale_shape_manual(
    values = c(zmb_albuquerque = 1, zmb_carreira_ruonia = 2, zmb_mcgowan = 3, zmb_altman = 4, zmb_ncr = 5),
    breaks = selected_zombies,
    labels = c("Albuquerque, Iyer, 2024", "Carreira et al., 2022", "McGowan et al., 2018", "Altman et al., 2024", "Тайкетаев, Доронкин, 2022"), name = "Определение зомби-фирмы") +
  scale_y_continuous(breaks = seq(0, 30, 2), labels = seq(0, 30, 2)) +
  theme_bw() +
  theme(
    legend.position = "bottom", 
    legend.key.width = unit(1.8, "cm"),
    text = element_text(size = 14, family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    legend.box.spacing = unit(0, "cm"),
    strip.text = element_text(face = "bold", size = 14),
    strip.background = element_blank()) +
  labs(x = "", y = "%", title = "") + # Погодовая доля (%) зомби-фирм в активах
  guides(shape = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) 

ggsave(yearly_share_assets, file = paste0(path_d, "some_folder/zombie_firms/plots_tables/plots/zmb_assets_share_by_years.png"), device = png, height = 7, width = 10)
ggsave(yearly_share_assets, file = paste0(path_d, "some_folder/zombie_firms/plots_tables/plots/zmb_assets_share_by_years.pdf"), device = cairo_pdf, height = 7, width = 10)

# Yearly share other financials =====

vars_of_interest <- c("investments", "revenue", "total_loans", "interest_payable")

yearly_share_plots <- ggplot(shares_by_year_long[year >= 2013 & zmb_indicator %chin% selected_zombies & variable %chin% vars_of_interest, ], #  
                             aes(x = year, y = value, group = zmb_indicator, linetype = zmb_indicator, shape = zmb_indicator)) +
  geom_line() +
  geom_point(size = 5) +
  scale_x_continuous(breaks = 2013:2024, labels = 2013:2024, expand = c(0.01, 0.01)) + 
  # scale_color_manual(
  #   values = c("zmb_albuquerque" = "red", "zmb_carreira_ruonia" = "orange", "zmb_mcgowan" = "green", "zmb_altman" = "blue", "zmb_ncr" = "darkgrey"),
  #   breaks = selected_zombies,
  #   labels = c("Albuquerque, Iyer, 2024", "Carreira et al., 2022", "McGowan et al., 2018", "Altman et al., 2024", "Тайкетаев, Доронкин, 2022"), name = "Определение зомби-фирмы") +
  scale_linetype_manual(
    values = c("zmb_albuquerque" = "solid", "zmb_carreira_ruonia" = "solid", "zmb_mcgowan" = "dashed", "zmb_altman" = "solid", "zmb_ncr" = "dashed"),
    breaks = selected_zombies,
    labels = c("Albuquerque, Iyer, 2024", "Carreira et al., 2022", "McGowan et al., 2018", "Altman et al., 2024", "Тайкетаев, Доронкин, 2022"), name = "Определение зомби-фирмы") +
  scale_shape_manual(
    values = c(zmb_albuquerque = 1, zmb_carreira_ruonia = 2, zmb_mcgowan = 3, zmb_altman = 4, zmb_ncr = 5),
    breaks = selected_zombies,
    labels = c("Albuquerque, Iyer, 2024", "Carreira et al., 2022", "McGowan et al., 2018", "Altman et al., 2024", "Тайкетаев, Доронкин, 2022"), name = "Определение зомби-фирмы") +
  scale_y_continuous(breaks = seq(0, 30, 2), labels = seq(0, 30, 2)) +
  theme_bw() +
  theme(
    legend.position = "bottom", 
    legend.key.width = unit(1.8, "cm"),
    text = element_text(size = 13, family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    legend.box.spacing = unit(0, "cm"),
    strip.text = element_text(face = "bold", size = 13),
    strip.background = element_blank()) +
  labs(x = "", y = "%", title = "") + # Погодовая доля (%) зомби-фирм в финансовых показателях
  facet_wrap(~ variable, 
             labeller = labeller(variable = c(investments = "Инвестиции", revenue = "Выручка", total_loans = "Займы и кредиты", interest_payable = "% к уплате")), # , nfirms = "Число фирм"; total_assets = "Активы", 
             ncol = 2, scales = "free_y") +
  guides(shape = guide_legend(nrow = 2), linetype = guide_legend(nrow = 2)) 

ggsave(yearly_share_plots, file = paste0(path_d, "some_folder/zombie_firms/plots_tables/plots/zmb_share_by_years.png"), device = png, height = 7, width = 10)
ggsave(yearly_share_plots, file = paste0(path_d, "some_folder/zombie_firms/plots_tables/plots/zmb_share_by_years.pdf"), device = cairo_pdf, height = 7, width = 10)

## Share in the economy by industry ====

shares_by_year_industry <- rbindlist(
  lapply(zombie_cols, function(zmb) {
    financials[,  {
      total_fin <- sapply(.SD, function(x) sum(x, na.rm = TRUE)) # [!is.na(get(zmb))]
      zombie_fin <- sapply(.SD, function(x) sum(x[get(zmb) == 1], na.rm = TRUE))
      shares <- zombie_fin / total_fin * 100
      as.list(shares)
    }, 
    by = .(year, okved_section), .SDcols = financial_cols][, zmb_indicator := zmb]
  })
)

n_by_year_industry <- rbindlist(
  lapply(zombie_cols, function(zmb) {
    financials[,  {
      total_fin <- .N # [!is.na(get(zmb))]
      zombie_fin <- sum(get(zmb), na.rm = TRUE)
      nfirms <- zombie_fin / total_fin * 100
      as.list(nfirms)
    }, 
    by = .(year, okved_section)][, zmb_indicator := zmb]
  })
)

setnames(n_by_year_industry, "V1", "nfirms")
shares_by_year_industry <- merge(shares_by_year_industry, n_by_year_industry, by = c("year", "okved_section", "zmb_indicator"))

selected_zombies <- c("zmb_albuquerque", "zmb_carreira_ruonia", "zmb_mcgowan") 

shares_by_year_industry_long <- melt(shares_by_year_industry, id.vars = c("year", "zmb_indicator"))

shares_by_year_industry_long[value == 0, value := NA_real_]

shares_by_year_industry_long$variable <- forcats::fct_relevel(as.factor(shares_by_year_industry_long$variable), 
                                                     "total_assets", "investments", "revenue",
                                                     "total_loans", "interest_payable", "nfirms")

irrelevant_sections <- c("U", "T", "O")

## N firms ====
n_firms_by_year_ind <- ggplot(shares_by_year_industry[year >= 2013 & !okved_section %chin% irrelevant_sections, ], #  & zmb_indicator %chin% selected_zombies
       aes(x = year, y = nfirms, group = zmb_indicator, linetype = zmb_indicator)) + #
  geom_line() +
  scale_x_continuous(breaks = 2013:2024, labels = 2013:2024, expand = c(0.01, 0.01)) + 
  scale_linetype_manual(
    values = c("zmb_albuquerque" = "solid", "zmb_carreira_ruonia" = "dashed", "zmb_mcgowan" = "dotdash"),
    breaks = selected_zombies,
    labels = c("Albuquerque, Iyer, 2024", "Carreira et al., 2022", "McGowan et al., 2018"), name = "Определение зомби-фирмы") +
  scale_y_continuous(breaks = seq(0, 2, 0.5), labels = seq(0, 2, 0.5), limits = c(0, 2.2), expand = c(0, 0)) +
  theme_bw() +
  theme(
    legend.position = "bottom", 
    legend.key.width = unit(1.8, "cm"),
    text = element_text(size = 14, family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    legend.box.spacing = unit(0, "cm"),
    strip.text = element_text(face = "bold", size = 7),
    strip.background = element_blank()) +
  labs(x = "", y = "", title = "Погодовая доля (%) зомби-фирм по индустриям") +
  facet_wrap(~ okved_section, nrow = 6, labeller = labeller(okved_section = c(
    A = "А. Хозяйство", 
    B = "B. Полезные ископаемые", 
    C = "C. Обраб. пр-ва", 
    D = "D. Энергия", 
    E = "E. Водоснабжение, отходы", 
    F = "F. Строительство",
    G = "G. Торговля",
    H = "H. Транспорт., хранение",
    I = "I. Гостиницы, общ. питание",
    J = "J. Информация и связь",
    L = "L. Недвижимость",
    M = "M. Профес. деятельность",
    N = "N. Админ. деятельность",
    P = "P. Образование",
    Q = "Q. Здравоохранение",
    R = "R. Культура и спорт",
    S = "S. Прочие услуги"))) 

ggsave(n_firms_by_year_ind, file = paste0(path_d, "some_folder/zombie_firms/plots_tables/plots/n_firms_by_year_ind.pdf"), device = cairo_pdf, height = 7, width = 10)
ggsave(n_firms_by_year_ind, file = paste0(path_d, "some_folder/zombie_firms/plots_tables/plots/n_firms_by_year_ind.png"), device = png, height = 7, width = 10)


## Assets ====
assets_by_year_ind <- ggplot(shares_by_year_industry[year >= 2013 & !okved_section %chin% irrelevant_sections, ], #  & zmb_indicator %chin% selected_zombies
                              aes(x = year, y = total_assets, group = zmb_indicator, linetype = zmb_indicator)) + #
  geom_line() +
  scale_x_continuous(breaks = 2013:2024, labels = 2013:2024, expand = c(0.01, 0.01)) + 
  scale_linetype_manual(
    values = c("zmb_albuquerque" = "solid", "zmb_carreira_ruonia" = "dashed", "zmb_mcgowan" = "dotdash"),
    breaks = selected_zombies,
    labels = c("Albuquerque, Iyer, 2024", "Carreira et al., 2022", "McGowan et al., 2018"), name = "Определение зомби-фирмы") +
  scale_y_continuous(breaks = seq(0, 5, 1), labels = seq(0, 5, 1), limits = c(0, 5.5), expand = c(0, 0)) +
  theme_bw() +
  theme(
    legend.position = "bottom", 
    legend.key.width = unit(1.8, "cm"),
    text = element_text(size = 14, family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    legend.box.spacing = unit(0, "cm"),
    strip.text = element_text(face = "bold", size = 7),
    strip.background = element_blank()) +
  labs(x = "", y = "", title = "Погодовая доля (%) зомби-фирм в активах по индустриям") +
  facet_wrap(~ okved_section, nrow = 6, labeller = labeller(okved_section = c(
    A = "А. Хозяйство", 
    B = "B. Полезные ископаемые", 
    C = "C. Обраб. пр-ва", 
    D = "D. Энергия", 
    E = "E. Водоснабжение, отходы", 
    F = "F. Строительство",
    G = "G. Торговля",
    H = "H. Транспорт., хранение",
    I = "I. Гостиницы, общ. питание",
    J = "J. Информация и связь",
    L = "L. Недвижимость",
    M = "M. Профес. деятельность",
    N = "N. Админ. деятельность",
    P = "P. Образование",
    Q = "Q. Здравоохранение",
    R = "R. Культура и спорт",
    S = "S. Прочие услуги"))) 

ggsave(assets_by_year_ind, file = paste0(path_d, "some_folder/zombie_firms/plots_tables/plots/assets_by_year_ind.pdf"), device = cairo_pdf, height = 7, width = 10)
ggsave(assets_by_year_ind, file = paste0(path_d, "some_folder/zombie_firms/plots_tables/plots/assets_by_year_ind.png"), device = png, height = 7, width = 10)

## Revenue ====

revenue_by_year_ind <- ggplot(shares_by_year_industry[year >= 2013 & !okved_section %chin% irrelevant_sections, ], #  & zmb_indicator %chin% selected_zombies
                             aes(x = year, y = revenue, group = zmb_indicator, linetype = zmb_indicator)) + #
  geom_line() +
  scale_x_continuous(breaks = 2013:2024, labels = 2013:2024, expand = c(0.01, 0.01)) + 
  scale_linetype_manual(
    values = c("zmb_albuquerque" = "solid", "zmb_carreira_ruonia" = "dashed", "zmb_mcgowan" = "dotdash"),
    breaks = selected_zombies,
    labels = c("Albuquerque, Iyer, 2024", "Carreira et al., 2022", "McGowan et al., 2018"), name = "Определение зомби-фирмы") +
  scale_y_continuous(breaks = seq(0, 5, 1), labels = seq(0, 5, 1), limits = c(0, 5.5), expand = c(0, 0)) +
  theme_bw() +
  theme(
    legend.position = "bottom", 
    legend.key.width = unit(1.8, "cm"),
    text = element_text(size = 14, family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    legend.box.spacing = unit(0, "cm"),
    strip.text = element_text(face = "bold", size = 7),
    strip.background = element_blank()) +
  labs(x = "", y = "", title = "Погодовая доля (%) зомби-фирм в выручке по индустриям") +
  facet_wrap(~ okved_section, nrow = 6, labeller = labeller(okved_section = c(
    A = "А. Хозяйство", 
    B = "B. Полезные ископаемые", 
    C = "C. Обраб. пр-ва", 
    D = "D. Энергия", 
    E = "E. Водоснабжение, отходы", 
    F = "F. Строительство",
    G = "G. Торговля",
    H = "H. Транспорт., хранение",
    I = "I. Гостиницы, общ. питание",
    J = "J. Информация и связь",
    L = "L. Недвижимость",
    M = "M. Профес. деятельность",
    N = "N. Админ. деятельность",
    P = "P. Образование",
    Q = "Q. Здравоохранение",
    R = "R. Культура и спорт",
    S = "S. Прочие услуги"))) 

ggsave(revenue_by_year_ind, file = paste0(path_d, "some_folder/zombie_firms/plots_tables/plots/revenue_by_year_ind.pdf"), device = cairo_pdf, height = 7, width = 10)
ggsave(revenue_by_year_ind, file = paste0(path_d, "some_folder/zombie_firms/plots_tables/plots/revenue_by_year_ind.png"), device = png, height = 7, width = 10)

## Investments ====
revenue_by_year_ind <- ggplot(shares_by_year_industry[year >= 2013 & !okved_section %chin% irrelevant_sections, ], #  & zmb_indicator %chin% selected_zombies
                              aes(x = year, y = revenue, group = zmb_indicator, linetype = zmb_indicator)) + #
  geom_line() +
  scale_x_continuous(breaks = 2013:2024, labels = 2013:2024, expand = c(0.01, 0.01)) + 
  scale_linetype_manual(
    values = c("zmb_albuquerque" = "solid", "zmb_carreira_ruonia" = "dashed", "zmb_mcgowan" = "dotdash"),
    breaks = selected_zombies,
    labels = c("Albuquerque, Iyer, 2024", "Carreira et al., 2022", "McGowan et al., 2018"), name = "Определение зомби-фирмы") +
  scale_y_continuous(breaks = seq(0, 5, 1), labels = seq(0, 5, 1), limits = c(0, 5.5), expand = c(0, 0)) +
  theme_bw() +
  theme(
    legend.position = "bottom", 
    legend.key.width = unit(1.8, "cm"),
    text = element_text(size = 14, family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5), 
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(), 
    panel.grid.major = element_blank(),
    legend.box.spacing = unit(0, "cm"),
    strip.text = element_text(face = "bold", size = 7),
    strip.background = element_blank()) +
  labs(x = "", y = "", title = "Погодовая доля (%) зомби-фирм в инвестициях по индустриям") +
  facet_wrap(~ okved_section, nrow = 6, labeller = labeller(okved_section = c(
    A = "А. Хозяйство", 
    B = "B. Полезные ископаемые", 
    C = "C. Обраб. пр-ва", 
    D = "D. Энергия", 
    E = "E. Водоснабжение, отходы", 
    F = "F. Строительство",
    G = "G. Торговля",
    H = "H. Транспорт., хранение",
    I = "I. Гостиницы, общ. питание",
    J = "J. Информация и связь",
    L = "L. Недвижимость",
    M = "M. Профес. деятельность",
    N = "N. Админ. деятельность",
    P = "P. Образование",
    Q = "Q. Здравоохранение",
    R = "R. Культура и спорт",
    S = "S. Прочие услуги"))) 

ggsave(revenue_by_year_ind, file = paste0(path_d, "some_folder/zombie_firms/plots_tables/plots/revenue_by_year_ind.pdf"), device = cairo_pdf, height = 7, width = 10)
ggsave(revenue_by_year_ind, file = paste0(path_d, "some_folder/zombie_firms/plots_tables/plots/revenue_by_year_ind.png"), device = png, height = 7, width = 10)


# Distribution of financials ====

## Combined plot ====

### assets ====
financials[total_assets > 0, total_assets_log := log(total_assets)]

assets_by_zmb_plot <- ggplot() +
  geom_density(data = financials[has_statements == 1 & total_assets_log >= 0, ], aes(x = total_assets_log, linetype = "Все фирмы"), linewidth = 1.5) +
  geom_density(data = financials[zmb_carreira_ruonia == 1 & has_statements == 1 & total_assets_log >= 0], aes(x = total_assets_log, linetype = "Carreira et al., 2022"), linewidth = 1.5) +
  geom_density(data = financials[zmb_altman == 1 & has_statements == 1 & total_assets_log >= 0], aes(x = total_assets_log, linetype = "Altman et al., 2024"), linewidth = 1.5) +
  scale_linetype_manual(
    values = c(
      "Все фирмы" = "solid",
      "Carreira et al., 2022" = "dotted", 
      "Altman et al., 2024" = "dashed"),
    breaks = c("Все фирмы", "Carreira et al., 2022", "Altman et al., 2024"),
    name = "") + # Определение зомби-фирмы
  scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 16, 2), labels = seq(0, 16, 2), expand = c(0, 0)) +
  labs(title = "", x = "", y = "") + # Натуральный логарифм активов\nВсе фирмы и зомби-фирмы
  theme_bw() + 
  theme(
    legend.position = "bottom", 
    legend.key.width = unit(2.2, "cm"),
    legend.text = element_text(margin = margin(r = 10, unit = "pt")),
    text = element_text(size = 14, family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5), 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), 
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black")) +
  guides(
    linetype = guide_legend(nrow = 1),
    override.aes = list(linewidth = 3))

### revenue ====
financials[revenue > 0, revenue_log := log(revenue)]

revenue_by_zmb_plot <- ggplot() +
  geom_density(data = financials[has_statements == 1 & revenue_log >= 0, ], aes(x = revenue_log, linetype = "Все фирмы"), linewidth = 1.5) +
  geom_density(data = financials[zmb_carreira_ruonia == 1 & has_statements == 1 & revenue_log >= 0], aes(x = revenue_log, linetype = "Carreira et al., 2022"), linewidth = 1.5) +
  geom_density(data = financials[zmb_altman == 1 & has_statements == 1 & revenue_log >= 0], aes(x = revenue_log, linetype = "Altman et al., 2024"), linewidth = 1.5) +
  scale_linetype_manual(
    values = c(
      "Все фирмы" = "solid",
      "Carreira et al., 2022" = "dotted", 
      "Altman et al., 2024" = "dashed"),
    breaks = c("Все фирмы", "Carreira et al., 2022", "Altman et al., 2024"),
    name = "") + # Определение зомби-фирмы
  scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 16, 2), labels = seq(0, 16, 2), expand = c(0, 0)) +
  labs(title = "", x = "", y = "") + # Натуральный логарифм выручки\nВсе фирмы и зомби-фирмы
  theme_bw() + 
  theme(
    legend.position = "bottom", 
    legend.key.width = unit(2.2, "cm"),
    legend.text = element_text(margin = margin(r = 10, unit = "pt")),
    text = element_text(size = 14, family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5), 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), 
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black")) +
  guides(
    linetype = guide_legend(nrow = 1),
    override.aes = list(linewidth = 3))

### investments asinh ====
financials[, investments_asinh := asinh(investments)]

investments_asinh_by_zmb_plot <- ggplot() +
  geom_density(data = financials[has_statements == 1 & investments_asinh != 0, ], aes(x = investments_asinh, linetype = "Все фирмы"), linewidth = 1.5) +
  geom_density(data = financials[zmb_carreira_ruonia == 1 & has_statements == 1 & investments_asinh != 0], aes(x = investments_asinh, linetype = "Carreira et al., 2022"), linewidth = 1.5) +
  geom_density(data = financials[zmb_altman == 1 & has_statements == 1 & investments_asinh != 0], aes(x = investments_asinh, linetype = "Altman et al., 2024"), linewidth = 1.5) +
  scale_linetype_manual(
    values = c(
      "Все фирмы" = "solid",
      "Carreira et al., 2022" = "dotted", 
      "Altman et al., 2024" = "dashed"),
    breaks = c("Все фирмы", "Carreira et al., 2022", "Altman et al., 2024"),
    name = "") + # Определение зомби-фирмы
  scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(-11, 10, 2), labels = seq(-10, 10, 2), expand = c(0, 0)) +
  labs(title = "", x = "", y = "") + # Обратный гиперболический синус ненулевых инвестиций\nВсе фирмы и зомби-фирмы
  theme_bw() + 
  theme(
    legend.position = "bottom", 
    legend.key.width = unit(2.2, "cm"),
    legend.text = element_text(margin = margin(r = 10, unit = "pt")),
    text = element_text(size = 14, family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5), 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), 
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black")) +
  guides(
    linetype = guide_legend(nrow = 1),
    override.aes = list(linewidth = 3))

### roa +-25 =====
roa_by_zmb_plot <- ggplot() +
  geom_density(data = financials[has_statements == 1 & roa_net_inc != 0 & roa_net_inc %between% c(-25, 25), ], aes(x = roa_net_inc, linetype = "Все фирмы"), linewidth = 1.2) +
  geom_density(data = financials[zmb_carreira_ruonia == 1 & has_statements == 1 & roa_net_inc != 0 & roa_net_inc %between% c(-25, 25), ], aes(x = roa_net_inc, linetype = "Carreira et al., 2022"), linewidth = 1.2) +
  geom_density(data = financials[zmb_altman == 1 & has_statements == 1 & roa_net_inc != 0 & roa_net_inc %between% c(-25, 25), ], aes(x = roa_net_inc, linetype = "Altman et al., 2024"), linewidth = 1.2) +
  scale_linetype_manual(values = c(
    "Все фирмы" = "solid",
    "Carreira et al., 2022" = "dotted", 
    "Altman et al., 2024" = "dashed"
  ),
  breaks = c("Все фирмы", "Carreira et al., 2022", "Altman et al., 2024"),
  name = "") + # Определение зомби-фирмы
  scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(-25, 25, 5), labels = seq(-25, 25, 5), expand = c(0, 0)) +
  labs(title = "", x = "", y = "") + # Ненулевая рентабельность активов от - 25% до 25%\nВсе фирмы и зомби-фирмы
  theme_bw() + 
  theme(
    legend.position = "bottom", 
    legend.key.width = unit(2.2, "cm"),
    legend.text = element_text(margin = margin(r = 10, unit = "pt")),
    text = element_text(size = 14, family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5), 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), 
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black")) +
  guides(
    linetype = guide_legend(nrow = 1),
    override.aes = list(linewidth = 3))

### loans ====
financials[, total_loans_log := log(total_loans)]

total_loans_by_zmb_plot <- ggplot() +
  geom_density(data = financials[has_statements == 1 & total_loans > 1, ], aes(x = total_loans_log, linetype = "Все фирмы"), linewidth = 1.5) +
  geom_density(data = financials[zmb_carreira_ruonia == 1 & has_statements == 1 & total_loans > 1, ], aes(x = total_loans_log, linetype = "Carreira et al., 2022"), linewidth = 1.5) +
  geom_density(data = financials[zmb_altman == 1 & has_statements == 1 & total_loans > 1, ], aes(x = total_loans_log, linetype = "Altman et al., 2024"), linewidth = 1.5) +
  scale_linetype_manual(values = c(
    "Все фирмы" = "solid",
    "Carreira et al., 2022" = "dotted", 
    "Altman et al., 2024" = "dashed"
  ),
  breaks = c("Все фирмы", "Carreira et al., 2022", "Altman et al., 2024"),
  name = "") + # Определение зомби-фирмы
  scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 16, 2), labels = seq(0, 16, 2), expand = c(0, 0)) +
  labs(title = "", x = "", y = "") + # Натуральный логарифм займов и кредитов\nВсе фирмы и зомби-фирмы
  theme_bw() + 
  theme(
    legend.position = "bottom", 
    legend.key.width = unit(2.2, "cm"),
    legend.text = element_text(margin = margin(r = 10, unit = "pt")),
    text = element_text(size = 14, family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5), 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), 
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black")) +
  guides(
    linetype = guide_legend(nrow = 1),
    override.aes = list(linewidth = 3))

### interest ====
financials[interest_payable != 0, interest_payable_log := log(abs(interest_payable))]

interest_by_zmb_plot <- ggplot() +
  geom_density(data = financials[has_statements == 1 & abs(interest_payable) > 1, ], aes(x = interest_payable_log, linetype = "Все фирмы"), linewidth = 1.2) +
  geom_density(data = financials[zmb_carreira_ruonia == 1 & has_statements == 1 & abs(interest_payable) > 1, ], aes(x = interest_payable_log, linetype = "Carreira et al., 2022"), linewidth = 1.2) +
  geom_density(data = financials[zmb_altman == 1 & has_statements == 1 & abs(interest_payable) > 1, ], aes(x = interest_payable_log, linetype = "Altman et al., 2024"), linewidth = 1.2) +
  scale_linetype_manual(values = c(
    "Все фирмы" = "solid",
    "Carreira et al., 2022" = "dotted", 
    "Altman et al., 2024" = "dashed"
  ),
  breaks = c("Все фирмы", "Carreira et al., 2022", "Altman et al., 2024"),
  name = "") + # Определение зомби-фирмы
  scale_y_continuous(breaks = NULL, expand = c(0, 0)) +
  scale_x_continuous(breaks = seq(0, 20, 1), labels = seq(0, 20, 1), expand = c(0, 0)) +
  labs(title = "", x = "", y = "") + # Натуральный логарифм % к уплате\nВсе фирмы и зомби-фирмы
  theme_bw() + 
  theme(
    legend.position = "bottom", 
    legend.key.width = unit(2.2, "cm"),
    legend.text = element_text(margin = margin(r = 10, unit = "pt")),
    text = element_text(size = 14, family = "Times New Roman"),
    plot.title = element_text(hjust = 0.5), 
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(), 
    panel.border = element_blank(),
    axis.line.x = element_line(color = "black")) +
  guides(
    linetype = guide_legend(nrow = 1),
    override.aes = list(linewidth = 3))

zmb_distr_plots <- ggarrange(assets_by_zmb_plot, revenue_by_zmb_plot, investments_asinh_by_zmb_plot, roa_by_zmb_plot, total_loans_by_zmb_plot, interest_by_zmb_plot,  
                             labels = c("a", "b", "c", "d", "e", "f"), font.label = list(size = 10, color = "black", face = "bold"),
                             nrow = 3, ncol = 2, legend = "bottom", common.legend = TRUE, 
                             heights = c(0.9, 0.9, 0.9))
ggsave(zmb_distr_plots, file = paste0(path_d, "some_folder/zombie_firms/plots_tables/plots/zmb_distr_plots.png"), device = png, height = 10, width = 9)
ggsave(zmb_distr_plots, file = paste0(path_d, "some_folder/zombie_firms/plots_tables/plots/zmb_distr_plots.pdf"), device = cairo_pdf, height = 10, width = 9)


# Correlation between different definitions ====

# zombie_cols <- zombie_cols[which(zombie_cols != "zmb_carreira_refin")]

zombie_cols <- c("zmb_storz", "zmb_mcgowan", "zmb_andrews", "zmb_carreira_ruonia", "zmb_acharya", "zmb_ncr", "zmb_albuquerque", "zmb_altman", "zmb_yamada")

names_names <- c(
  "Storz et al., 2017  [1]",
  "McGowan et al., 2018 [2]",
  "Andrews, Petroulakis, 2019 [3]",
  "Carreira et al., 2022 [4]",
  "Acharya et al., 2022 [5]",
  "Тайкетаев, Доронкин, 2022   [6]",
  "Albuquerque, Iyer, 2024 [7]",
  "Altman et al., 2024 [8]",
  "Yamada et al., 2025 [9]")

variables_names <- c(
  "Непокрытие долга & ROA<0 & инвестиции<0\n[1]",
  "ICR & 10+ лет\n[2]",
  "Непокрытие долга & (ROA<0 / инвестиции<0)\n[3]",
  "ROA>ключевой ставки & Леверидж & 5+ лет \n[4]",
  "Средний ICR\n[5]",
  "ICR\n[6]",
  "ICR & левередж & Δвыручки<0\n[7]",
  "Z''Альтмана & Средний ICR\n[8]",
  "Δдолга>0 & ICR & 10+ лет \n[9]")

## MCC ====

# Initialize correlation matrix
n_cols <- length(zombie_cols)
zombie_corr_mat <- matrix(NA, nrow = n_cols, ncol = n_cols)
rownames(zombie_corr_mat) <- names_names
colnames(zombie_corr_mat) <- variables_names

# Compute MCC for each pair
for (i in 1:n_cols) {
  for (j in i:n_cols) {
    if (i == j) {
      zombie_corr_mat[i, j] <- 1
    } else {
      mcc_val <- mcc(financials[has_statements == 1, ][[zombie_cols[i]]], 
                              financials[has_statements == 1, ][[zombie_cols[j]]])
      zombie_corr_mat[i, j] <- mcc_val
      zombie_corr_mat[j, i] <- mcc_val
    }
  }
}


zombie_melted <- melt(zombie_corr_mat, varnames = c("Var1", "Var2"), value.name = "Overlap")

# Get the order of rows/columns
row_order <- rownames(zombie_corr_mat)
zombie_melted$Var1 <- factor(zombie_melted$Var1, levels = row_order)

col_order <- colnames(zombie_corr_mat)
zombie_melted$Var2 <- factor(zombie_melted$Var2, levels = col_order)

setDT(zombie_melted)

# Keep only lower triangle (including diagonal)
zombie_melted_lower <- zombie_melted[as.numeric(Var1) <= as.numeric(Var2)]

zombie_melted_lower$Overlap <- round(zombie_melted_lower$Overlap, 2)

zmb_mcc_plot <- ggplot(zombie_melted_lower, aes(x = Var1, y = Var2, fill = Overlap)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "white", 
    mid = "grey75", 
    high = "black", 
    midpoint = 0.5,
    limits = c(0, 1),
    na.value = "grey90",
    name = ""
  ) +
  geom_text(aes(label = Overlap), 
            color = "black", 
            size = 3.5) +
  # Mirror the matrix by reversing the y-axis
  scale_y_discrete(limits = rev(levels(zombie_melted_lower$Var2))) +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 35, hjust = 1, size = 14),
    axis.text.y = element_text(size = 14),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    legend.position = "none",
    panel.border = element_blank()
  ) +
  labs(title = "") + # Попарные корреляции Мэттьюса определений зомби-фирм
  coord_fixed()

ggsave(zmb_mcc_plot, file = paste0(path_d, "some_folder/zombie_firms/plots_tables/plots/zmb_mcc_plot.png"), device = png, height = 7, width = 10)
ggsave(zmb_mcc_plot, file = paste0(path_d, "ledenev/zombie_firms/plots_tables/plots/zmb_mcc_plot.pdf"), device = cairo_pdf, height = 7, width = 10)
