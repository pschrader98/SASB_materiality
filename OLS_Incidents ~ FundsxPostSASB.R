library(tidyverse)
library(ggplot2)
library(scales)
library(fixest)
library(staggered)
library(zoo)
library(broom)


#-------------------Funds--------------------

data_reprisk <- read.csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESGmateriality/Data/panel_funds_incidents.csv")

# Select main regression variables (rename here)
data_reprisk <- data_reprisk %>% dplyr::select( gvkey, YearQuarter, SICS.Codified.Industry, overall_material_disclosure = overall_manual_Material_words,
                                                quarters_from_adoption, post_provisional_standard, Material_manual_earnings_overall_share, 
                                                leverage_p1p99, roa_abs_p1p99, mtb_prevq_p1p99, size_ln_mve_prevq,
                                                n_car_1_material, n_car_5_material, n_car_30_material, n_reach_material,
                                                n_material_t1, n_nonmaterial_t1, n_car_1_material_t1, n_car_5_material_t1, n_reach_material_t1, n_severity_material_t1, 
                                                n_material_t2, n_nonmaterial_t2, n_car_1_material_t2, n_car_5_material_t2, n_reach_material_t2, n_severity_material_t2, 
                                                n_material_t3, n_nonmaterial_t3, n_car_1_material_t3, n_car_5_material_t3, n_reach_material_t3, n_severity_material_t3,
                                                n_car_30_material_t1, n_car_30_material_t2, n_car_30_material_t3, n_car_1_material_year_t1, n_car_1_material_year_t2, 
                                                n_car_1_material_year_t3, n_car_5_material_year_t1, n_car_5_material_year_t2, 
                                                n_car_5_material_year_t3, n_car_30_material_year_t1, n_car_30_material_year_t2, 
                                                n_car_30_material_year_t3,
                                                ownership_mf_perc_ESG_delta_win, sold_mf
) %>%
  dplyr::mutate(
    n_incidents_t1 = n_material_t1 + n_nonmaterial_t1,
    n_incidents_t2 = n_material_t2 + n_nonmaterial_t2,
    n_incidents_t3 = n_material_t3 + n_nonmaterial_t3
  ) %>%
  # log(x+1) for all *_t1/_t2/_t3 vars except n_material_* and n_nonmaterial_*
  dplyr::mutate(
    dplyr::across(
      .cols = dplyr::matches("(_t1|_t2|_t3)$") &
        !dplyr::starts_with("n_material_") &
        !dplyr::starts_with("n_nonmaterial_"),
      .fns  = ~log1p(.),
      .names = "{.col}_log1p"
    )
  )

wins <- function(x, p = 0.01) {
  qs <- stats::quantile(x, c(p, 1 - p), na.rm = TRUE, names = FALSE)
  pmin(pmax(x, qs[1]), qs[2])
}

# Use the new name in winsorization + factors
data_reprisk <- data_reprisk %>%
  dplyr::mutate(
    overall_material_disclosure            = wins(overall_material_disclosure, 0.01),
    Material_manual_earnings_overall_share = wins(Material_manual_earnings_overall_share, 0.01),
    cusip      = as.factor(gvkey),
    YearQuarter= as.factor(YearQuarter),
    Industry   = as.factor(SICS.Codified.Industry)
  ) %>%
  # add log1p versions (consistent naming)
  dplyr::mutate(
    overall_material_disclosure_log1p            = log1p(overall_material_disclosure)
  )

data_post2010 <- data_reprisk %>%
  dplyr::mutate(
    .year_tmp = suppressWarnings(as.integer(substr(as.character(YearQuarter), 1, 4)))
  ) %>%
  dplyr::filter(.year_tmp > 2009) %>%
  dplyr::select(-.year_tmp) %>%
  droplevels()

#------------------- Plot Fund holdings

#---------------- Plot holding pre/post incident and pre/post SASB  -------------------

df <- data_post2010 %>%
  mutate(
    qtr  = as.yearqtr(gsub("Q", " Q", YearQuarter), format = "%Y Q%q"),
    sold_mf = as.integer(sold_mf),
    post_flag = as.integer(post_provisional_standard == 1)
  ) %>%
  select(gvkey, qtr, n_car_30_material, sold_mf, post_flag)

# --- 2) Identify event quarters (material incident > 0), tagged as Pre/Post by the event quarter ---
events <- df %>%
  filter(n_car_30_material > 0) %>%
  distinct(gvkey, event_qtr = qtr, event_post = post_flag)

# --- 3) Build event windows by firm and event ---
evt_windows <- df %>%
  inner_join(events, by = "gvkey") %>%
  mutate(
    rel_q = as.integer(round((qtr - event_qtr) * 4))  # difference in quarters
  ) %>%
  filter(rel_q >= -4, rel_q <= 4)

# --- 4) Aggregate: share sold_mf by relative quarter and event_post (Pre/Post) ---
plot_df <- evt_windows %>%
  mutate(period = ifelse(event_post == 1, "Post", "Pre")) %>%
  group_by(rel_q, period) %>%
  summarize(
    share_sold = mean(sold_mf == 1, na.rm = TRUE),
    n = sum(!is.na(sold_mf)),
    se = sqrt(share_sold * (1 - share_sold) / pmax(n, 1)),
    lo = pmax(0, share_sold - 1.96 * se),
    hi = pmin(1, share_sold + 1.96 * se),
    .groups = "drop"
  )

# --- 5) Plot: two lines (Pre vs Post) ---
p <- ggplot(plot_df, aes(x = rel_q, y = share_sold,
                         color = period, linetype = period)) +
  geom_line(linewidth = 1) +
  geom_point() +
  # Optional CI ribbon per group (comment out if not desired)
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = period),
              alpha = 0.15, linewidth = 0, colour = NA) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = -4:4, limits = c(-4, 4)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Share of Firms Selling Mutual Fund Holdings Around Material Incidents",
    subtitle = "Split by incident-quarter being Pre vs Post provisional standard",
    x = "Quarters relative to incident (0 = incident quarter)",
    y = "Share of firms with sold_mf = 1",
    color = "Incident period",
    linetype = "Incident period",
    fill = "Incident period"
  ) +
  theme_minimal(base_size = 13)

p

# --- 6) Save (optional) ---
dir.create("Plots", showWarnings = FALSE, recursive = TRUE)
ggsave("Plots/share_sold_mf_car.pdf", plot = p, device = "pdf",
       width = 8, height = 5, units = "in")

#-------------------Ownership Share Plot

df <- data_post2010 %>%
  mutate(
    qtr  = as.yearqtr(gsub("Q", " Q", YearQuarter), format = "%Y Q%q"),
    post_flag = as.integer(post_provisional_standard == 1)
  ) %>%
  select(gvkey, qtr, n_car_30_material, ownership_mf_perc_ESG_delta_win, post_flag)

events <- df %>%
  filter(n_car_30_material > 0) %>%
  distinct(gvkey, event_qtr = qtr, event_post = post_flag)

evt_windows <- df %>%
  inner_join(events, by = "gvkey") %>%
  mutate(rel_q = as.integer(round((qtr - event_qtr) * 4))) %>%
  filter(rel_q >= -4, rel_q <= 4)

# --- Correct CI for mean of a continuous variable ---
plot_df <- evt_windows %>%
  mutate(period = ifelse(event_post == 1, "Post", "Pre")) %>%
  group_by(rel_q, period) %>%
  summarize(
    mean_change = mean(ownership_mf_perc_ESG_delta_win, na.rm = TRUE),
    n  = sum(!is.na(ownership_mf_perc_ESG_delta_win)),
    sd = sd(ownership_mf_perc_ESG_delta_win, na.rm = TRUE),
    se = sd / sqrt(pmax(n, 1)),
    lo = mean_change - 1.96 * se,
    hi = mean_change + 1.96 * se,
    .groups = "drop"
  )

p <- ggplot(plot_df, aes(x = rel_q, y = mean_change,
                         color = period, linetype = period, group = period)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_ribbon(aes(ymin = lo, ymax = hi, fill = period),
              alpha = 0.15, linewidth = 0, colour = NA) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = -4:4, limits = c(-4, 4)) +
  # choose your rounding: 0 decimals, 1 dec, 2 dec
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(
    title = "Change in Mutual Fund Ownership Around Material Incidents",
    subtitle = "Averages by incident-quarter being Pre vs Post provisional standard",
    x = "Quarters relative to incident (0 = incident quarter)",
    y = "Average change in MF ownership",
    color = "Incident period",
    linetype = "Incident period",
    fill = "Incident period"
  ) +
  theme_minimal(base_size = 13)

p

# --- 6) Save (optional) ---
dir.create("Plots", showWarnings = FALSE, recursive = TRUE)
ggsave("Plots/share_mf_car.pdf", plot = p, device = "pdf",
       width = 8, height = 5, units = "in")


#-------------------Incidents_all = Funds--------------------

est_13 <- feols(
  n_incidents_t1_log1p ~ ownership_mf_perc_ESG_delta_win + post_provisional_standard + post_provisional_standard:ownership_mf_perc_ESG_delta_win +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_23 <- feols(
  n_incidents_t2_log1p ~ ownership_mf_perc_ESG_delta_win + post_provisional_standard + post_provisional_standard:ownership_mf_perc_ESG_delta_win +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_33 <- feols(
  n_incidents_t3_log1p ~ ownership_mf_perc_ESG_delta_win + post_provisional_standard + post_provisional_standard:ownership_mf_perc_ESG_delta_win +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)



etable(est_13, est_23, est_33, fixef_sizes = TRUE, coefstat = 'tstat')

# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^ownership_mf_perc_ESG_delta_win$",
  "^post_provisional_standard$",
  "^ownership_mf_perc_ESG_delta_win:post_provisional_standard$"
)
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  n_incidents_t1_log1p                                              = "Ln # Incidents (t+1)",
  n_incidents_t2_log1p                                              = "Ln # Incidents (t+2)",
  n_incidents_t3_log1p                                              = "Ln # Incidents (t+3)",
  post_provisional_standard                                       = "Post SASB",
  "ownership_mf_perc_ESG_delta_win:post_provisional_standard"          = "Post SASB x Fund Ownership",
  ownership_mf_perc_ESG_delta_win                                       = "Fund Ownership Delta",
  "cusip"       = "Firm",
  "Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`   = est_13,
  `(2) Post spec 2` = est_23,
  `(3) Post spec`   = est_33,
  
  # ensure clustered SEs are used in the table computation
  cluster     = ~ cusip + YearQuarter,
  
  # print a row with the SE type (e.g., “Clustered (cusip & YearQuarter)”)
  se.row      = TRUE,           
  
  # other stats you want at the bottom
  fitstat     = ~ r2 + n,
  
  file         = "./results/incidents_funds_post_ols.tex",
  replace      = TRUE,
  dict         = dict,
  digits       = "r3",
  digits.stats = "r3",
  float        = FALSE,
  coefstat     = "tstat",       # Poisson → z-stats
  keep         = keep_vec,
  order        = order_vec,
  extralines = list("Controls" = rep("Yes", 3)),
  style.tex = style.tex(
    "aer",
    yesNo = c("Yes","No"),
    fixef.title  = "\\midrule",
    fixef.where  = "stat",      # optional; keeps FE info in stats block
    fixef.suffix = " FE",
    stats.title  = "\\midrule",
    tabular      = "*"
  )
)

#-------------------Incidents_car_material = Disclosure--------------------


est_13 <- feols(
  n_car_5_material_t1_log1p ~ ownership_mf_perc_ESG_delta_win + post_provisional_standard + post_provisional_standard:ownership_mf_perc_ESG_delta_win +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_23 <- feols(
  n_car_5_material_t2_log1p ~ ownership_mf_perc_ESG_delta_win + post_provisional_standard + post_provisional_standard:ownership_mf_perc_ESG_delta_win +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)


est_33 <- feols(
  n_car_5_material_t3_log1p ~ ownership_mf_perc_ESG_delta_win + post_provisional_standard + post_provisional_standard:ownership_mf_perc_ESG_delta_win +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)


etable(est_13,est_23,  est_33, fixef_sizes = TRUE, coefstat = 'tstat')

# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^ownership_mf_perc_ESG_delta_win$",
  "^post_provisional_standard$",
  "^ownership_mf_perc_ESG_delta_win:post_provisional_standard$"
)
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  n_car_5_material_t1_log1p                                             = "Ln # Low CAR Incidents (t+1)",
  n_car_5_material_t2_log1p                                              = "Ln # Low CAR Incidents (t+2)",
  n_car_5_material_t3_log1p                                              = "Ln # Low CAR Incidents (t+3)",
  post_provisional_standard                                       = "Post SASB",
  "ownership_mf_perc_ESG_delta_win:post_provisional_standard"          = "Post SASB x Fund Ownership",
  ownership_mf_perc_ESG_delta_win                                       = "Fund Ownership Delta",
  "cusip"       = "Firm",
  "Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`   = est_13,
  `(2) Post spec 2` = est_23,
  `(3) Post spec`   = est_33,
  
  # ensure clustered SEs are used in the table computation
  cluster     = ~ cusip + YearQuarter,
  
  # print a row with the SE type (e.g., “Clustered (cusip & YearQuarter)”)
  se.row      = TRUE,           
  
  # other stats you want at the bottom
  fitstat     = ~ r2 + n,
  
  file         = "./results/car_funds_post_ols.tex",
  replace      = TRUE,
  dict         = dict,
  digits       = "r3",
  digits.stats = "r3",
  float        = FALSE,
  coefstat     = "tstat",       # Poisson → z-stats
  keep         = keep_vec,
  order        = order_vec,
  extralines = list("Controls" = rep("Yes", 3)),
  style.tex = style.tex(
    "aer",
    yesNo = c("Yes","No"),
    fixef.title  = "\\midrule",
    fixef.where  = "stat",      # optional; keeps FE info in stats block
    fixef.suffix = " FE",
    stats.title  = "\\midrule",
    tabular      = "*"
  )
)
#-------------------Incidents_reach_material = Disclosure--------------------

est_13 <- feols(
  n_reach_material_t1_log1p ~ ownership_mf_perc_ESG_delta_win + post_provisional_standard + post_provisional_standard:ownership_mf_perc_ESG_delta_win +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_23 <- feols(
  n_reach_material_t2_log1p  ~ ownership_mf_perc_ESG_delta_win + post_provisional_standard + post_provisional_standard:ownership_mf_perc_ESG_delta_win +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)


est_33 <- feols(
  n_reach_material_t3_log1p ~ ownership_mf_perc_ESG_delta_win + post_provisional_standard + post_provisional_standard:ownership_mf_perc_ESG_delta_win +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)


etable(est_13,  est_23,  est_33, fixef_sizes = TRUE, coefstat = 'tstat')

# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^ownership_mf_perc_ESG_delta_win$",
  "^post_provisional_standard$",
  "^ownership_mf_perc_ESG_delta_win:post_provisional_standard$"
)
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  n_reach_material_t1_log1p                                              = "Ln # High Reach Incidents (t+1)",
  n_reach_material_t2_log1p                                              = "Ln # High Reach Incidents (t+2)",
  n_reach_material_t3_log1p                                              = "Ln # High Reach Incidents (t+3)",
  post_provisional_standard                                       = "Post SASB",
  "ownership_mf_perc_ESG_delta_win:post_provisional_standard"          = "Post SASB x Fund Ownership",
  ownership_mf_perc_ESG_delta_win                                       = "Fund Ownership Delta",
  "cusip"       = "Firm",
  "Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`   = est_13,
  `(2) Post spec 2` = est_23,
  `(3) Post spec`   = est_33,
  
  # ensure clustered SEs are used in the table computation
  cluster     = ~ cusip + YearQuarter,
  
  # print a row with the SE type (e.g., “Clustered (cusip & YearQuarter)”)
  se.row      = TRUE,           
  
  # other stats you want at the bottom
  fitstat     = ~ r2 + n,
  
  file         = "./results/reach_funds_post_ols.tex",
  replace      = TRUE,
  dict         = dict,
  digits       = "r3",
  digits.stats = "r3",
  float        = FALSE,
  coefstat     = "tstat",       # Poisson → z-stats
  keep         = keep_vec,
  order        = order_vec,
  extralines = list("Controls" = rep("Yes", 3)),
  style.tex = style.tex(
    "aer",
    yesNo = c("Yes","No"),
    fixef.title  = "\\midrule",
    fixef.where  = "stat",      # optional; keeps FE info in stats block
    fixef.suffix = " FE",
    stats.title  = "\\midrule",
    tabular      = "*"
  )
)


#---------------- Sold ------------
#-------------------Incidents_all = Funds--------------------

est_13 <- feols(
  n_incidents_t1_log1p ~ sold_mf + post_provisional_standard + post_provisional_standard:sold_mf +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_23 <- feols(
  n_incidents_t2_log1p ~ sold_mf + post_provisional_standard + post_provisional_standard:sold_mf +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_33 <- feols(
  n_incidents_t3_log1p ~ sold_mf + post_provisional_standard + post_provisional_standard:sold_mf +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)



etable(est_13, est_23, est_33, fixef_sizes = TRUE, coefstat = 'tstat')

# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^sold_mf$",
  "^post_provisional_standard$",
  "^sold_mf:post_provisional_standard$"
)
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  n_incidents_t1_log1p                                              = "Ln # Incidents (t+1)",
  n_incidents_t2_log1p                                              = "Ln # Incidents (t+2)",
  n_incidents_t3_log1p                                              = "Ln # Incidents (t+3)",
  post_provisional_standard                                       = "Post SASB",
  "sold_mf:post_provisional_standard"          = "Post SASB x Fund Ownership",
  sold_mf                                       = "Fund Sold",
  "cusip"       = "Firm",
  "Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`   = est_13,
  `(2) Post spec 2` = est_23,
  `(3) Post spec`   = est_33,
  
  # ensure clustered SEs are used in the table computation
  cluster     = ~ cusip + YearQuarter,
  
  # print a row with the SE type (e.g., “Clustered (cusip & YearQuarter)”)
  se.row      = TRUE,           
  
  # other stats you want at the bottom
  fitstat     = ~ r2 + n,
  
  file         = "./results/incidents_funds_sold_post_ols.tex",
  replace      = TRUE,
  dict         = dict,
  digits       = "r3",
  digits.stats = "r3",
  float        = FALSE,
  coefstat     = "tstat",       # Poisson → z-stats
  keep         = keep_vec,
  order        = order_vec,
  extralines = list("Controls" = rep("Yes", 3)),
  style.tex = style.tex(
    "aer",
    yesNo = c("Yes","No"),
    fixef.title  = "\\midrule",
    fixef.where  = "stat",      # optional; keeps FE info in stats block
    fixef.suffix = " FE",
    stats.title  = "\\midrule",
    tabular      = "*"
  )
)

#-------------------Incidents_car_material = Disclosure--------------------


est_13 <- feols(
  n_car_5_material_t1_log1p ~ sold_mf + post_provisional_standard + post_provisional_standard:sold_mf +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_23 <- feols(
  n_car_5_material_t2_log1p ~ sold_mf + post_provisional_standard + post_provisional_standard:sold_mf +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)


est_33 <- feols(
  n_car_5_material_t3_log1p ~ sold_mf + post_provisional_standard + post_provisional_standard:sold_mf +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)


etable(est_13,est_23,  est_33, fixef_sizes = TRUE, coefstat = 'tstat')

# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^sold_mf$",
  "^post_provisional_standard$",
  "^sold_mf:post_provisional_standard$"
)
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  n_car_5_material_t1_log1p                                             = "Ln # Low CAR Incidents (t+1)",
  n_car_5_material_t2_log1p                                              = "Ln # Low CAR Incidents (t+2)",
  n_car_5_material_t3_log1p                                              = "Ln # Low CAR Incidents (t+3)",
  post_provisional_standard                                       = "Post SASB",
  "sold_mf:post_provisional_standard"          = "Post SASB x Fund Ownership",
  sold_mf                                       = "Fund Sold",
  "cusip"       = "Firm",
  "Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`   = est_13,
  `(2) Post spec 2` = est_23,
  `(3) Post spec`   = est_33,
  
  # ensure clustered SEs are used in the table computation
  cluster     = ~ cusip + YearQuarter,
  
  # print a row with the SE type (e.g., “Clustered (cusip & YearQuarter)”)
  se.row      = TRUE,           
  
  # other stats you want at the bottom
  fitstat     = ~ r2 + n,
  
  file         = "./results/car_funds_sold_post_ols.tex",
  replace      = TRUE,
  dict         = dict,
  digits       = "r3",
  digits.stats = "r3",
  float        = FALSE,
  coefstat     = "tstat",       # Poisson → z-stats
  keep         = keep_vec,
  order        = order_vec,
  extralines = list("Controls" = rep("Yes", 3)),
  style.tex = style.tex(
    "aer",
    yesNo = c("Yes","No"),
    fixef.title  = "\\midrule",
    fixef.where  = "stat",      # optional; keeps FE info in stats block
    fixef.suffix = " FE",
    stats.title  = "\\midrule",
    tabular      = "*"
  )
)
#-------------------Incidents_reach_material = Disclosure--------------------

est_13 <- feols(
  n_reach_material_t1_log1p ~ sold_mf  + post_provisional_standard + post_provisional_standard:sold_mf  +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_23 <- feols(
  n_reach_material_t2_log1p  ~ sold_mf  + post_provisional_standard + post_provisional_standard:sold_mf  +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)


est_33 <- feols(
  n_reach_material_t3_log1p ~ sold_mf  + post_provisional_standard + post_provisional_standard:sold_mf  +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)


etable(est_13,  est_23,  est_33, fixef_sizes = TRUE, coefstat = 'tstat')

# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^sold_mf$",
  "^post_provisional_standard$",
  "^sold_mf:post_provisional_standard$"
)
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  n_reach_material_t1_log1p                                              = "Ln # High Reach Incidents (t+1)",
  n_reach_material_t2_log1p                                              = "Ln # High Reach Incidents (t+2)",
  n_reach_material_t3_log1p                                              = "Ln # High Reach Incidents (t+3)",
  post_provisional_standard                                       = "Post SASB",
  "sold_mf:post_provisional_standard"          = "Post SASB x Fund Ownership",
  sold_mf                                       = "Fund Sold",
  "cusip"       = "Firm",
  "Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`   = est_13,
  `(2) Post spec 2` = est_23,
  `(3) Post spec`   = est_33,
  
  # ensure clustered SEs are used in the table computation
  cluster     = ~ cusip + YearQuarter,
  
  # print a row with the SE type (e.g., “Clustered (cusip & YearQuarter)”)
  se.row      = TRUE,           
  
  # other stats you want at the bottom
  fitstat     = ~ r2 + n,
  
  file         = "./results/reach_funds_sold_post_ols.tex",
  replace      = TRUE,
  dict         = dict,
  digits       = "r3",
  digits.stats = "r3",
  float        = FALSE,
  coefstat     = "tstat",       # Poisson → z-stats
  keep         = keep_vec,
  order        = order_vec,
  extralines = list("Controls" = rep("Yes", 3)),
  style.tex = style.tex(
    "aer",
    yesNo = c("Yes","No"),
    fixef.title  = "\\midrule",
    fixef.where  = "stat",      # optional; keeps FE info in stats block
    fixef.suffix = " FE",
    stats.title  = "\\midrule",
    tabular      = "*"
  )
)
