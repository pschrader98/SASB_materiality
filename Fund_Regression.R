library(tidyverse)
library(ggplot2)
library(scales)
library(fixest)
library(staggered)
library(zoo)



#---------------- Load data -------------------
# Load data (all)
data_daily <- read.csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESGmateriality/Data/panel_funds_incidents.csv")

# Select main regression variables to make working with data more manageable
data_daily = data_daily %>% dplyr::select(gvkey, datacqtr, n_material, n_nonmaterial, severity_sum, reach_sum, novelty_sum, SICS.Codified.Industry, sold_mf, 
                                          post_provisional_standard, roa_abs_p1p99, NonMaterial_manual_earnings_overall_share, Material_manual_earnings_overall_share,
                                          earnings_vol_20q, leverage_p1p99, ownership_mf_perc_ESG_delta, ownership_mf_perc_ESG_delta_win)

data_daily <- data_daily %>%
  mutate(
    cusip = as.factor(gvkey),
    YearQuarter = as.factor(datacqtr)
  )


data_daily <- data_daily %>% drop_na()

#---------------- Plot holding pre/post incident  -------------------


# --- 1) Parse quarter and keep only needed columns ---
df <- data_daily %>%
  mutate(
    qtr = as.yearqtr(gsub("Q", " Q", datacqtr), format = "%Y Q%q"),
    sold_mf = as.integer(sold_mf)  # ensure 0/1
  ) %>%
  select(gvkey, qtr, n_material, sold_mf)

# --- 2) Identify event quarters (material incident > 0) ---
events <- df %>%
  filter(n_material > 0) %>%
  distinct(gvkey, event_qtr = qtr)

# --- 3) Build event windows by firm: all quarters relative to each event ---
# (Cartesian join within firm, then compute relative quarter)
evt_windows <- df %>%
  inner_join(events, by = "gvkey") %>%
  mutate(
    rel_q = as.integer(round((qtr - event_qtr) * 4))  # difference in quarters
  ) %>%
  filter(rel_q >= -4, rel_q <= 4)

# --- 4) Aggregate: share of firms with sold_mf == 1 by relative quarter ---
plot_df <- evt_windows %>%
  group_by(rel_q) %>%
  summarize(
    share_sold = mean(sold_mf == 1, na.rm = TRUE),
    n = sum(!is.na(sold_mf)),
    # Optional 95% normal-approx CI
    se = sqrt(share_sold * (1 - share_sold) / pmax(n, 1)),
    lo = pmax(0, share_sold - 1.96 * se),
    hi = pmin(1, share_sold + 1.96 * se),
    .groups = "drop"
  )

# --- 5) Plot ---
p <- ggplot(plot_df, aes(x = rel_q, y = share_sold)) +
  geom_line(linewidth = 1) +
  geom_point() +
  # Optional CI ribbon; comment out if you don’t want it
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = -4:4, limits = c(-4, 4)) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  labs(
    title = "Share of Firms Selling Mutual Fund Holdings Around Material Incidents",
    subtitle = "sold_mf == 1; aggregated across firm-event windows (n_material > 0)",
    x = "Quarters relative to incident (0 = incident quarter)",
    y = "Share of firms (sold_mf)"
  ) +
  theme_minimal(base_size = 13)

p

# --- 6) Save (optional) ---
dir.create("Plots", showWarnings = FALSE, recursive = TRUE)
ggsave("Plots/share_sold_mf_event_window.pdf", plot = p, device = "pdf",
       width = 8, height = 5, units = "in")

#---------------- Plot holding pre/post incident and pre/post SASB  -------------------

df <- data_daily %>%
  mutate(
    qtr  = as.yearqtr(gsub("Q", " Q", datacqtr), format = "%Y Q%q"),
    sold_mf = as.integer(sold_mf),
    post_flag = as.integer(post_provisional_standard == 1)
  ) %>%
  select(gvkey, qtr, n_material, sold_mf, post_flag)

# --- 2) Identify event quarters (material incident > 0), tagged as Pre/Post by the event quarter ---
events <- df %>%
  filter(n_material > 0) %>%
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
ggsave("Plots/share_sold_mf_event_window_pre_post.pdf", plot = p, device = "pdf",
       width = 8, height = 5, units = "in")

#---------------- Plot holding pre/post incident and pre/post SASB  -------------------


df <- data_daily %>%
  mutate(
    qtr  = as.yearqtr(gsub("Q", " Q", datacqtr), format = "%Y Q%q"),
    post_flag = as.integer(post_provisional_standard == 1)
  ) %>%
  select(gvkey, qtr, n_material, ownership_mf_perc_ESG_delta_win, post_flag)

events <- df %>%
  filter(n_material > 0) %>%
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
ggsave("Plots/share_mf_event_window_pre_post.pdf", plot = p, device = "pdf",
       width = 8, height = 5, units = "in")


#---------------- Run Regression (Can funds predict incidents pre/post ) -------------------

est_11 <- feols(
  sold_mf ~ post_provisional_standard + n_material + n_nonmaterial + n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_12 <- feols(
  sold_mf ~ post_provisional_standard + n_material + n_nonmaterial + n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_daily,
  vcov = ~ cusip + YearQuarter
)

est_13 <- feols(
  ownership_mf_perc_ESG_delta_win ~ post_provisional_standard + n_material + n_nonmaterial + n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)


est_14 <- feols(
  ownership_mf_perc_ESG_delta_win ~ post_provisional_standard + n_material + n_nonmaterial + n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_daily,
  vcov = ~ cusip + YearQuarter
)



etable(est_11, est_12, est_13, est_14, fixef_sizes = TRUE, coefstat = 'tstat')

#---------------- Run Regression (Can disclosure predict MF activities ) -------------------

est_21 <- feols(
  sold_mf ~ Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share + post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)



est_22 <- feols(
  sold_mf ~ Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share + post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_daily,
  vcov = ~ cusip + YearQuarter
)

est_23 <- feols(
  ownership_mf_perc_ESG_delta_win ~ Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share + post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_24 <- feols(
  ownership_mf_perc_ESG_delta_win ~ Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share + post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_daily,
  vcov = ~ cusip + YearQuarter
)



etable(est_21, est_22, est_23, est_24, fixef_sizes = TRUE, coefstat = 'tstat')

#---------------- Run Regression (Does disclosure prior to an incident predict MF holding prior to incident ) -------------------

est_31 <- feols(
  sold_mf ~ post_provisional_standard + n_material + n_nonmaterial + Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share:n_nonmaterial +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)


est_32 <- feols(
  sold_mf ~ post_provisional_standard + n_material + n_nonmaterial + Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share:n_nonmaterial +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_daily,
  vcov = ~ cusip + YearQuarter
)


est_33 <- feols(
  ownership_mf_perc_ESG_delta ~ post_provisional_standard + n_material + n_nonmaterial + Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share:n_nonmaterial +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)



est_34 <- feols(
  ownership_mf_perc_ESG_delta ~ post_provisional_standard + n_material + n_nonmaterial + Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share:n_nonmaterial +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_daily,
  vcov = ~ cusip + YearQuarter
)



etable(est_31, est_32, est_33, est_34, fixef_sizes = TRUE, coefstat = 'tstat')


#-------------------------Save Latex Table-------------------

est_11 <- feols(
  sold_mf ~ post_provisional_standard + n_material + n_nonmaterial + n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_12 <- feols(
  sold_mf ~ post_provisional_standard + n_material + n_nonmaterial + n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_daily,
  vcov = ~ cusip + YearQuarter
)

est_21 <- feols(
  sold_mf ~ Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share + post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)



est_22 <- feols(
  sold_mf ~ Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share + post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_daily,
  vcov = ~ cusip + YearQuarter
)


est_31 <- feols(
  sold_mf ~ post_provisional_standard + n_material + n_nonmaterial + Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share:n_nonmaterial +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)


est_32 <- feols(
  sold_mf ~ post_provisional_standard + n_material + n_nonmaterial + Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share:n_nonmaterial +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_daily,
  vcov = ~ cusip + YearQuarter
)

# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^post_provisional_standard$",
  "^n_material$",
  "^n_nonmaterial$",
  "^post_provisional_standard:n_material$",
  "^post_provisional_standard:n_nonmaterial$",
  "^Material_manual_earnings_overall_share$",
  "^NonMaterial_manual_earnings_overall_share$",
  "^post_provisional_standard:Material_manual_earnings_overall_share$",
  "^post_provisional_standard:NonMaterial_manual_earnings_overall_share$",
  "^post_provisional_standard:n_material:Material_manual_earnings_overall_share$",
  "^post_provisional_standard:n_nonmaterial:NonMaterial_manual_earnings_overall_share$",
  "^roa_abs_p1p99$",
  "^earnings_vol_20q$",
  "^leverage_p1p99$"
)
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  post_provisional_standard                                          = "Post SASB",
  n_material                                                              = "# Material Incidents",
  n_nonmaterial                                                              = "# Non-Material Incidents",
  Material_manual_earnings_overall_share                             = "Material Disclosure",
  NonMaterial_manual_earnings_overall_share                             = "Non-Material Disclosure",
  `post_provisional_standard:n_material`                          = "Post SASB × # Material Incidents",
  `post_provisional_standard:n_nonmaterial`                          = "Post SASB × # Non-Material Incidents",
  `post_provisional_standard:Material_manual_earnings_overall_share` = "Material disclosure × Post SASB",
  `post_provisional_standard:NonMaterial_manual_earnings_overall_share` = "Non-Material disclosure × Post SASB",
  `post_provisional_standard:n_material:Material_manual_earnings_overall_share`
  = "Material disclosure × # Material Incidents × Post SASB",
  `post_provisional_standard:n_nonmaterial:NonMaterial_manual_earnings_overall_share`
  = "Non-Material disclosure × # Non-Material Incidents × Post SASB",
  roa_abs_p1p99                                                      = "ROA",
  earnings_vol_20q                                                   = "Earnings vol",
  leverage_p1p99                                                     = "Leverage",
  "cusip"       = "Firm",
  "SICS.Codified.Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`        = est_11,
  `(2) Post spec 2`        = est_12,
  `(3) Post spec`        = est_21,
  `(4) Post spec 2`        = est_22,
  `(5) Full spec`        = est_31,
  `(6) Full spec 2`        = est_32,
  file        = "./results/sasb_sold_coef_table.tex",
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



#--------------------Table with continous changes in ESG fund holdings -----------------



est_13 <- feols(
  ownership_mf_perc_ESG_delta_win ~ post_provisional_standard + n_material + n_nonmaterial + n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)


est_14 <- feols(
  ownership_mf_perc_ESG_delta_win ~ post_provisional_standard + n_material + n_nonmaterial + n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_daily,
  vcov = ~ cusip + YearQuarter
)

est_23 <- feols(
  ownership_mf_perc_ESG_delta_win ~ Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share + post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_24 <- feols(
  ownership_mf_perc_ESG_delta_win ~ Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share + post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_daily,
  vcov = ~ cusip + YearQuarter
)

est_33 <- feols(
  ownership_mf_perc_ESG_delta_win ~ post_provisional_standard + n_material + n_nonmaterial + Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share:n_nonmaterial +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_daily,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)



est_34 <- feols(
  ownership_mf_perc_ESG_delta_win ~ post_provisional_standard + n_material + n_nonmaterial + Material_manual_earnings_overall_share + NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard + n_nonmaterial:post_provisional_standard +
    post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share +
    n_material:post_provisional_standard:Material_manual_earnings_overall_share + post_provisional_standard:NonMaterial_manual_earnings_overall_share:n_nonmaterial +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_daily,
  vcov = ~ cusip + YearQuarter
)

# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^post_provisional_standard$",
  "^n_material$",
  "^n_nonmaterial$",
  "^post_provisional_standard:n_material$",
  "^post_provisional_standard:n_nonmaterial$",
  "^Material_manual_earnings_overall_share$",
  "^NonMaterial_manual_earnings_overall_share$",
  "^post_provisional_standard:Material_manual_earnings_overall_share$",
  "^post_provisional_standard:NonMaterial_manual_earnings_overall_share$",
  "^post_provisional_standard:n_material:Material_manual_earnings_overall_share$",
  "^post_provisional_standard:n_nonmaterial:NonMaterial_manual_earnings_overall_share$",
  "^roa_abs_p1p99$",
  "^earnings_vol_20q$",
  "^leverage_p1p99$"
)
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  post_provisional_standard                                          = "Post SASB",
  n_material                                                              = "# Material Incidents",
  n_nonmaterial                                                              = "# Non-Material Incidents",
  Material_manual_earnings_overall_share                             = "Material Disclosure",
  NonMaterial_manual_earnings_overall_share                             = "Non-Material Disclosure",
  `post_provisional_standard:n_material`                          = "Post SASB × # Material Incidents",
  `post_provisional_standard:n_nonmaterial`                          = "Post SASB × # Non-Material Incidents",
  `post_provisional_standard:Material_manual_earnings_overall_share` = "Material disclosure × Post SASB",
  `post_provisional_standard:NonMaterial_manual_earnings_overall_share` = "Non-Material disclosure × Post SASB",
  `post_provisional_standard:n_material:Material_manual_earnings_overall_share`
  = "Material disclosure × # Material Incidents × Post SASB",
  `post_provisional_standard:n_nonmaterial:NonMaterial_manual_earnings_overall_share`
  = "Non-Material disclosure × # Non-Material Incidents × Post SASB",
  roa_abs_p1p99                                                      = "ROA",
  earnings_vol_20q                                                   = "Earnings vol",
  leverage_p1p99                                                     = "Leverage",
  "cusip"       = "Firm",
  "SICS.Codified.Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`        = est_13,
  `(2) Post spec 2`        = est_14,
  `(3) Post spec`        = est_23,
  `(4) Post spec 2`        = est_24,
  `(5) Full spec`        = est_33,
  `(6) Full spec 2`        = est_34,
  file        = "./results/sasb_hold_coef_table.tex",
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


