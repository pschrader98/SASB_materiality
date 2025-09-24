library(tidyverse)
library(ggplot2)
library(scales)
library(fixest)
library(staggered)



#---------------- Load data -------------------

data_daily <- read.csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESGmateriality/Data/panel_jmp_day.csv")

data_daily <- data_daily %>% dplyr::select(cusip, incident_date, YearQuarter, SICS.Codified.Industry, overall_material_disclosure = overall_manual_Material_words,
                                                quarters_from_adoption, post_provisional_standard, Material_manual_earnings_overall_share, 
                                                leverage_p1p99, roa_abs_p1p99, mtb_prevq_p1p99, size_ln_mve_prevq,
                                                car_1_material, car_5_material, reach_material, car_den, jump, std_1.2, std_1, std_2
)

wins <- function(x, p = 0.01) {
  qs <- stats::quantile(x, c(p, 1 - p), na.rm = TRUE, names = FALSE)
  pmin(pmax(x, qs[1]), qs[2])
}

# Use the new name in winsorization + factors
data_daily <- data_daily %>%
  dplyr::mutate(
    overall_material_disclosure            = wins(overall_material_disclosure, 0.01),
    Material_manual_earnings_overall_share = wins(Material_manual_earnings_overall_share, 0.01),
    cusip      = as.factor(cusip),
    YearQuarter= as.factor(YearQuarter),
    Industry   = as.factor(SICS.Codified.Industry)
  ) %>%
  # add log1p versions (consistent naming)
  dplyr::mutate(
    overall_material_disclosure_log1p            = log1p(overall_material_disclosure)
  )

data_post2010 <- data_daily %>%
  dplyr::mutate(
    .year_tmp = suppressWarnings(as.integer(substr(as.character(YearQuarter), 1, 4)))
  ) %>%
  dplyr::filter(.year_tmp > 2009) %>%
  dplyr::select(-.year_tmp) %>%
  droplevels()

# Filter for denominator values smaller than the cutoff

data_std12_daily <- data_daily %>% filter(abs(car_den) >= `std_1.2`)
data_std1_daily  <- data_daily %>% filter(abs(car_den) >= std_1)
data_std2_daily  <- data_daily %>% filter(abs(car_den) >= std_2)

  #---------------- Run Regression (JMP ~ Reach ) -------------------
est_11 <- feols(
  jump ~ reach_material +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_std12_daily,
  vcov = ~ cusip + YearQuarter
)

est_12 <- feols(
  jump ~ post_provisional_standard +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_std12_daily,
  vcov = ~ cusip + YearQuarter
)

est_13 <- feols(
  jump ~ reach_material + post_provisional_standard + reach_material:post_provisional_standard +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_std12_daily,
  vcov = ~ cusip + YearQuarter
)



etable(est_11, est_12, est_13, fixef_sizes = TRUE, coefstat = 'tstat')

# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^reach_material$",
  "^post_provisional_standard$",
  "^reach_material:post_provisional_standard$"
)

order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  post_provisional_standard                                          = "Post",
  reach_material                                                      = "High Reach Incident",
  `reach_material:post_provisional_standard`                          = "Post × Material Incident",
  "cusip"       = "Firm",
  "YearQuarter" = "Year–Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`        = est_11,
  `(2) Disclosure spec`  = est_12,
  `(3) Full spec`        = est_13,
  file        = "./results/jump_reach.tex",
  replace = TRUE, dict=dict, digits = "r3", digits.stats = "r3",
  float=FALSE, coefstat= "tstat", fitstat = ~ r2 + n,
  keep        = keep_vec,
  order       = order_vec,
  extralines = list("Controls" = rep("Yes", 3)),
  style.tex = style.tex("aer",
                        yesNo = c('Yes', 'No'),
                        fixef.title = "\\midrule",
                        fixef.where = 'var',
                        fixef.suffix       = " FE",
                        stats.title = "\\midrule",
                        tabular = "*"))



# --- Fit models  -------------------------------------------------------
est_11 <- feols(
  jump ~ car_5_material +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_std12_daily,
  vcov = ~ cusip + YearQuarter
)

est_12 <- feols(
  jump ~ post_provisional_standard +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_std12_daily,
  vcov = ~ cusip + YearQuarter
)

est_13 <- feols(
  jump ~ car_5_material + post_provisional_standard + car_5_material:post_provisional_standard +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_std12_daily,
  vcov = ~ cusip + YearQuarter
)



etable(est_11, est_12, est_13, fixef_sizes = TRUE, coefstat = 'tstat')


# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^car_5_material$",
  "^post_provisional_standard$",
  "^car_5_material:post_provisional_standard$"
)

order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  post_provisional_standard                                          = "Post",
  car_5_material                                                      = "Low CAR incident",
  `car_5_material:post_provisional_standard`                          = "Post × Material Incident",
  "cusip"       = "Firm",
  "YearQuarter" = "Year–Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`        = est_11,
  `(2) Disclosure spec`  = est_12,
  `(3) Full spec`        = est_13,
  file        = "./results/jump_car.tex",
  replace = TRUE, dict=dict, digits = "r3", digits.stats = "r3",
  float=FALSE, coefstat= "tstat", fitstat = ~ r2 + n,
  keep        = keep_vec,
  order       = order_vec,
  extralines = list("Controls" = rep("Yes", 3)),
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
