library(tidyverse)
library(ggplot2)
library(scales)
library(fixest)
library(staggered)



#---------------- Load data -------------------
# Load data (all)
data_daily <- read.csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESGmateriality/Data/panel_jmp_day.csv")


# Select main regression variables to make working with data more manageable
data_daily = data_daily %>% dplyr::select(cusip, incident_date, YearQuarter, material_flag , severity, reach, novelty, SICS.Codified.Industry, 
                                  post_provisional_standard, car_den, jump, std_1.2, std_1, std_2, roa_abs_p1p99, NonMaterial_manual_earnings_overall_share,
                                  Material_manual_earnings_overall_share,
                                  earnings_over_assets_w, earnings_vol_20q, leverage_p1p99, car_den,
                                  jump, std_1.2, std_1, std_2)

data_daily <- data_daily %>% drop_na()

data_daily <- data_daily %>%
  mutate(
    cusip = as.factor(cusip),
    YearQuarter = as.factor(YearQuarter)
  )

# Filter for denominator values smaller than the cutoff

data_std12_daily <- data_daily %>% filter(abs(car_den) >= `std_1.2`)
data_std1_daily  <- data_daily %>% filter(abs(car_den) >= std_1)
data_std2_daily  <- data_daily %>% filter(abs(car_den) >= std_2)

#---------------- Run Regression (JMP ~ Post ) -------------------

est_11 <- feols(
  jump ~ post_provisional_standard + material_flag + material_flag:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_std12_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_12 <- feols(
  jump ~ post_provisional_standard + material_flag + material_flag:post_provisional_standard +
    roa_abs_p1p99  + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | SICS.Codified.Industry + YearQuarter,
  data = data_std12_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_13 <- feols(
  jump ~ post_provisional_standard + material_flag + material_flag:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_std12_daily,
  vcov = ~ cusip + YearQuarter
)

est_14 <- feols(
  jump ~ post_provisional_standard + material_flag + material_flag:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | cusip + YearQuarter,
  data = data_std12_daily,
  vcov = ~ cusip + YearQuarter
)



etable(est_11, est_12, est_13, est_14, fixef_sizes = TRUE, coefstat = 'tstat')

#---------------- Run Regression (JMP ~ Disclosure ) -------------------


est_21 <- feols(
  jump ~ Material_manual_earnings_overall_share + material_flag + material_flag:Material_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_std12_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_22 <- feols(
  jump ~ Material_manual_earnings_overall_share + material_flag + material_flag:Material_manual_earnings_overall_share +
    roa_abs_p1p99+ earnings_vol_20q + leverage_p1p99  + severity + reach + novelty
  | SICS.Codified.Industry + YearQuarter,
  data = data_std12_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_23 <- feols(
  jump ~ Material_manual_earnings_overall_share + material_flag + material_flag:Material_manual_earnings_overall_share +
    roa_abs_p1p99+ earnings_vol_20q + leverage_p1p99 
  | cusip + YearQuarter,
  data = data_std12_daily,
  vcov = ~ cusip + YearQuarter
)

est_24 <- feols(
  jump ~ Material_manual_earnings_overall_share + material_flag + material_flag:Material_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99  + severity + reach + novelty
  | cusip + YearQuarter,
  data = data_std12_daily,
  vcov = ~ cusip + YearQuarter
)


etable(est_21, est_22, est_23, est_24, fixef_sizes = TRUE, coefstat = 'tstat')

#---------------- Run Regression (JMP ~ Disclosure * Post ) -------------------

est_31 <- feols(
  jump ~ Material_manual_earnings_overall_share + material_flag + post_provisional_standard +
    post_provisional_standard:material_flag +  post_provisional_standard:Material_manual_earnings_overall_share + 
    material_flag:Material_manual_earnings_overall_share + material_flag:Material_manual_earnings_overall_share:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_std12_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_32 <- feols(
  jump ~ Material_manual_earnings_overall_share + material_flag + post_provisional_standard +
    post_provisional_standard:material_flag +  post_provisional_standard:Material_manual_earnings_overall_share + 
    material_flag:Material_manual_earnings_overall_share + material_flag:Material_manual_earnings_overall_share:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | SICS.Codified.Industry + YearQuarter,
  data = data_std12_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)


est_33 <- feols(
  jump ~ Material_manual_earnings_overall_share + material_flag + post_provisional_standard +
    post_provisional_standard:material_flag +  post_provisional_standard:Material_manual_earnings_overall_share + 
    material_flag:Material_manual_earnings_overall_share + material_flag:Material_manual_earnings_overall_share:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_std12_daily,
  vcov = ~ cusip + YearQuarter
)

est_34 <- feols(
  jump ~ Material_manual_earnings_overall_share + material_flag + post_provisional_standard +
    post_provisional_standard:material_flag +  post_provisional_standard:Material_manual_earnings_overall_share + 
    material_flag:Material_manual_earnings_overall_share + material_flag:Material_manual_earnings_overall_share:post_provisional_standard +
    roa_abs_p1p99+ earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | cusip + YearQuarter,
  data = data_std12_daily,
  vcov = ~ cusip + YearQuarter
)



etable(est_31, est_32, est_33, est_34, fixef_sizes = TRUE, coefstat = 'tstat')

# --- Fit models (yours) -------------------------------------------------------
est_14 <- feols(
  jump ~ material_flag + post_provisional_standard  + material_flag:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | cusip + YearQuarter,
  data = data_std12_daily,
  vcov = ~ cusip + YearQuarter
)

est_24 <- feols(
  jump ~ material_flag + Material_manual_earnings_overall_share + material_flag:Material_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99  + severity + reach + novelty
  | cusip + YearQuarter,
  data = data_std12_daily,
  vcov = ~ cusip + YearQuarter
)

est_34 <- feols(
  jump ~ material_flag + post_provisional_standard + Material_manual_earnings_overall_share +
    material_flag:post_provisional_standard + material_flag:Material_manual_earnings_overall_share +
    post_provisional_standard:Material_manual_earnings_overall_share + 
    material_flag:Material_manual_earnings_overall_share:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | cusip + YearQuarter,
  data = data_std12_daily,
  vcov = ~ cusip + YearQuarter
)




# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^material_flag$",
  "^post_provisional_standard$",
  "^Material_manual_earnings_overall_share$",
  "^material_flag:post_provisional_standard$",
  "^material_flag:Material_manual_earnings_overall_share$",
  "^post_provisional_standard:Material_manual_earnings_overall_share$",
  "^material_flag:Material_manual_earnings_overall_share:post_provisional_standard$",
  "^severity$",
  "^reach$",
  "^novelty$",
  "^roa_abs_p1p99$",
  "^earnings_vol_20q$",
  "^leverage_p1p99$"
)
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  post_provisional_standard                                          = "Post",
  material_flag                                                      = "Material incident",
  Material_manual_earnings_overall_share                             = "Material disclosure",
  `material_flag:post_provisional_standard`                          = "Post × Material incident",
  `material_flag:Material_manual_earnings_overall_share`             = "Material incident × Material disclosure",
  `Material_manual_earnings_overall_share:post_provisional_standard` = "Material disclosure × Post",
  `material_flag:Material_manual_earnings_overall_share:post_provisional_standard`
  = "Material disclosure × Material incident × Post",
  roa_abs_p1p99                                                      = "ROA",
  earnings_vol_20q                                                   = "Earnings vol",
  leverage_p1p99                                                     = "Leverage",
  severity                                                           = "Severity",
  reach                                                              = "Reach",
  novelty                                                            = "Novelty",
  "cusip"       = "Firm",
  "YearQuarter" = "Year–Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`        = est_14,
  `(2) Disclosure spec`  = est_24,
  `(3) Full spec`        = est_34,
  file        = "./results/sasb_jump_coef_table.tex",
  replace = TRUE, dict=dict, digits = "r3", digits.stats = "r3",
  float=FALSE, coefstat= "tstat", fitstat = ~ r2 + n,
  keep        = keep_vec,
  order       = order_vec,
  style.tex = style.tex("aer",
                        yesNo = c('Yes', 'No'),
                        fixef.title = "\\midrule",
                        fixef.where = 'var',
                        fixef.suffix       = " FE",
                        stats.title = "\\midrule",
                        tabular = "*"))








# --- 1) Start from your filtered data ------------------------------
df <- data_std12_disclosure %>%
  mutate(
    incident_date = as.Date(incident_date),
    post = as.integer(post_provisional_standard == 1),
    year = as.integer(str_sub(as.character(YearQuarter), 1, 4))
  ) %>%
  arrange(cusip, incident_date)

# --- 2) Keep exactly one PRE (last before treatment) and one POST (first after) per firm
# last pre row per firm
pre  <- df %>%
  filter(post == 0) %>%
  group_by(cusip) %>%
  slice_tail(n = 2) %>%
  ungroup() %>%
  mutate(event_window = "pre", rel_time = -3L)

# first post row per firm
post <- df %>%
  filter(post == 1) %>%
  group_by(cusip) %>%
  slice_head(n = 2) %>%
  ungroup() %>%
  mutate(event_window = "post", rel_time = +3L)

# stack pre & post, keep firms that have both rows
panel_2p <- bind_rows(pre, post) %>%
  group_by(cusip) %>%
  filter(n() == 2) %>%
  ungroup() %>%
  mutate(post = as.integer(event_window == "post"))

# Optional sanity checks
# table(panel_2p$event_window)
length(unique(panel_2p$cusip))           # number of firms in stacked sample
panel_2p %>% count(cusip) %>% summary()  # should be exactly 2 per firm

# (A) Firm FE + Quarter FE
m_q <- feols(
  jump ~ post + Material_manual_earnings_overall_share +
    post:Material_manual_earnings_overall_share |
    cusip + YearQuarter,
  data = panel_2p, cluster = ~ cusip
)

etable(m_q, fixef_sizes = TRUE, coefstat = 'tstat')


# --- 2) Keep exactly TWO PRE (last two before treatment) and TWO POST (first two after) per firm
# last two pre rows per firm
pre  <- df %>%
  filter(post == 0) %>%
  group_by(cusip) %>%
  slice_tail(n = 2) %>%
  arrange(cusip, incident_date) %>%
  mutate(event_window = "pre",
         rel_time = row_number() - 3L) %>%   # gives -2, -1
  ungroup()

# first two post rows per firm
post <- df %>%
  filter(post == 1) %>%
  group_by(cusip) %>%
  slice_head(n = 2) %>%
  arrange(cusip, incident_date) %>%
  mutate(event_window = "post",
         rel_time = row_number()) %>%        # gives +1, +2
  ungroup()

# stack pre & post, keep firms that have BOTH 2 pre AND 2 post (=> 4 rows per firm)
panel_4p <- bind_rows(pre, post) %>%
  group_by(cusip) %>%
  filter(sum(event_window == "pre") == 2 & sum(event_window == "post") == 2) %>%
  ungroup() %>%
  mutate(post = as.integer(event_window == "post"))

# Optional sanity checks
length(unique(panel_4p$cusip))              # number of firms in stacked sample
panel_4p %>% count(cusip) %>% summary()     # should be exactly 4 per firm
table(panel_4p$event_window)

# (A) Firm FE + Quarter FE
m_q <- feols(
  jump ~ post + Material_manual_earnings_overall_share + material_flag +
    post:Material_manual_earnings_overall_share + post:material_flag + material_flag:Material_manual_earnings_overall_share + 
    post:Material_manual_earnings_overall_share:material_flag|
    SICS.Codified.Industry + YearQuarter,
  data = panel_4p, cluster = ~ SICS.Codified.Industry
)

etable(m_q, fixef_sizes = TRUE, coefstat = 'tstat')
