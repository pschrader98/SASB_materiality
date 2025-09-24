library(tidyverse)
library(ggplot2)
library(scales)
library(fixest)
library(staggered)



#---------------- Load data -------------------
# Load data (all)
data_daily <- read.csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESGmateriality/Data/panel_bid_ask_disclosure.csv")

# Select main regression variables to make working with data more manageable
data_daily = data_daily %>% dplyr::select(gvkey, YearQuarter, SICS.Codified.Industry, overall_material_disclosure = overall_manual_Material_words,
                                          quarters_from_adoption, post_provisional_standard, Material_manual_earnings_overall_share, 
                                          leverage_p1p99, roa_abs_p1p99, mtb_prevq_p1p99, size_ln_mve_prevq,
                                          car_1_material, car_5_material, car_30_material, reach_material, event,
                                          spread, log_spread, spread_winsorized, spread_cs, log_spread_cs, spread_cs_winsorized, rel_day
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
    cusip      = as.factor(gvkey),
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


#---------------- Plot Distribution -------------------


window_min <- -10
window_max <-  2

# 1) Build event ids so negatives are attached to the upcoming zero
ds <- data_post2010 %>%
  arrange(gvkey, rel_day) %>%
  group_by(gvkey) %>%
  mutate(event_id = cumsum(rel_day == 0) + as.integer(rel_day < 0)) %>%  # <-- key fix
  ungroup()

# 2) Keep only windows that truly are CAR(5) incidents (at the anchor day)
ds_events <- ds %>%
  group_by(gvkey, event_id) %>%
  mutate(
    is_car5_incident = any(rel_day == 0 & car_5_material > 0, na.rm = TRUE),
    event_post_flag  = dplyr::first(post_provisional_standard[rel_day == 0]),
    period           = ifelse(event_post_flag == 1, "Post adoption", "Pre adoption")
  ) %>%
  ungroup() %>%
  filter(is_car5_incident, between(rel_day, window_min, window_max))

# 3) Aggregate and plot
plot_df <- ds_events %>%
  group_by(rel_day, period) %>%
  summarise(
    avg_spread = mean(spread_winsorized, na.rm = TRUE),
    n = sum(!is.na(spread_winsorized)),
    .groups = "drop"
  )

ggplot(plot_df, aes(rel_day, avg_spread, color = period, linetype = period)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(breaks = seq(window_min, window_max, 1),
                     limits = c(window_min, window_max)) +
  labs(
    title = "Average Winsorized Spread Around CAR(5) Incidents",
    subtitle = "Pre- vs post-adoption (period determined at event day)",
    x = "Relative day to incident (0 = event day)",
    y = "Average spread (winsorized)",
    color = "Incident period", linetype = "Incident period"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")



ggplot2::ggsave(
  filename = "Plots/spread_plot_car5_pre_post.pdf",
  plot     = p,
  device   = grDevices::cairo_pdf,
  width    = 8, height = 5, units = "in"
)




#---------------- Run Regression (Spread ~ Post + Event ) -------------------

est_11 <- feols(
  spread_winsorized ~ post_provisional_standard +
    leverage_p1p99 + roa_abs_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_12 <- feols(
  spread_winsorized ~ event +
    leverage_p1p99 + roa_abs_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_13 <- feols(
  spread_winsorized ~ car_5_material +
    leverage_p1p99 + roa_abs_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_14 <- feols(
  spread_winsorized ~ post_provisional_standard + event + post_provisional_standard:event +
    leverage_p1p99 + roa_abs_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_15 <- feols(
  spread_winsorized ~ car_5_material + event + car_5_material:event +
    leverage_p1p99 + roa_abs_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_16 <- feols(
  spread_winsorized ~ car_5_material + post_provisional_standard + car_5_material:post_provisional_standard +
    leverage_p1p99 + roa_abs_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_17 <- feols(
  spread_winsorized ~ car_5_material + post_provisional_standard + event +
  car_5_material:post_provisional_standard + event:post_provisional_standard + event:car_5_material +
    car_5_material:post_provisional_standard:event +
    leverage_p1p99 + roa_abs_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)




etable(est_11, est_12, est_13, est_14, est_15, est_16, est_17, fixef_sizes = TRUE, coefstat = 'tstat')



#---------------- Run Regression (Spread ~ Disclosure + Post) -------------------

est_21 <- feols(
  spread ~ Material_manual_earnings_overall_share + post_provisional_standard + post_provisional_standard:Material_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_spread,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_22 <- feols(
  spread ~ Material_manual_earnings_overall_share + post_provisional_standard + post_provisional_standard:Material_manual_earnings_overall_share +
    roa_abs_p1p99  + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | SICS.Codified.Industry + YearQuarter,
  data = data_spread,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_23 <- feols(
  spread ~ Material_manual_earnings_overall_share + post_provisional_standard + post_provisional_standard:Material_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_spread,
  vcov = ~ cusip + YearQuarter
)

est_24 <- feols(
  spread ~ Material_manual_earnings_overall_share + post_provisional_standard + post_provisional_standard:Material_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | cusip + YearQuarter,
  data = data_spread,
  vcov = ~ cusip + YearQuarter
)



etable(est_21, est_22, est_23, est_24, fixef_sizes = TRUE, coefstat = 'tstat')


#---------------- Run Regression (Spread ~ Disclosure + Post) -------------------

est_31 <- feols(
  spread_winsorized ~ Material_manual_earnings_overall_share + post_provisional_standard + event +
    post_provisional_standard:Material_manual_earnings_overall_share + event:post_provisional_standard + event:Material_manual_earnings_overall_share +
    post_provisional_standard:event:Material_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_spread,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_32 <- feols(
  spread_winsorized ~ Material_manual_earnings_overall_share + post_provisional_standard + event +
    post_provisional_standard:Material_manual_earnings_overall_share + event:post_provisional_standard + event:Material_manual_earnings_overall_share +
    post_provisional_standard:event:Material_manual_earnings_overall_share +
    roa_abs_p1p99  + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | SICS.Codified.Industry + YearQuarter,
  data = data_spread,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_33 <- feols(
  spread_winsorized ~ Material_manual_earnings_overall_share + post_provisional_standard + event +
    post_provisional_standard:Material_manual_earnings_overall_share + event:post_provisional_standard + event:Material_manual_earnings_overall_share +
    post_provisional_standard:event:Material_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_spread,
  vcov = ~ cusip + YearQuarter
)

est_34 <- feols(
  spread_winsorized ~ Material_manual_earnings_overall_share + post_provisional_standard + event +
    post_provisional_standard:Material_manual_earnings_overall_share + event:post_provisional_standard + event:Material_manual_earnings_overall_share +
    event:Material_manual_earnings_overall_share:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | cusip + YearQuarter,
  data = data_spread,
  vcov = ~ cusip + YearQuarter
)



etable(est_31, est_32, est_33, est_34, fixef_sizes = TRUE, coefstat = 'tstat')

#-------------------------Save Latex Table-------------------

est_12 <- feols(
  spread_winsorized ~ post_provisional_standard + event + event:post_provisional_standard +
    roa_abs_p1p99  + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | SICS.Codified.Industry + YearQuarter,
  data = data_spread,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_14 <- feols(
  spread_winsorized ~ post_provisional_standard + event + event:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | cusip + YearQuarter,
  data = data_spread,
  vcov = ~ cusip + YearQuarter
)

est_32 <- feols(
  spread_winsorized ~ Material_manual_earnings_overall_share + post_provisional_standard + event +
    post_provisional_standard:Material_manual_earnings_overall_share + event:post_provisional_standard + event:Material_manual_earnings_overall_share +
    post_provisional_standard:event:Material_manual_earnings_overall_share +
    roa_abs_p1p99  + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | SICS.Codified.Industry + YearQuarter,
  data = data_spread,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_34 <- feols(
  spread_winsorized ~ Material_manual_earnings_overall_share + post_provisional_standard + event +
    post_provisional_standard:Material_manual_earnings_overall_share + event:post_provisional_standard + event:Material_manual_earnings_overall_share +
    event:Material_manual_earnings_overall_share:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | cusip + YearQuarter,
  data = data_spread,
  vcov = ~ cusip + YearQuarter
)

etable(est_12, est_14, est_32, est_34, fixef_sizes = TRUE, coefstat = 'tstat')


# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^event$",
  "^post_provisional_standard$",
  "^post_provisional_standard:event$",
  "^Material_manual_earnings_overall_share$",
  "^Material_manual_earnings_overall_share:post_provisional_standard$",
  "^Material_manual_earnings_overall_share:event$",
  "^Material_manual_earnings_overall_share:post_provisional_standard:event$",
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
  post_provisional_standard                                          = "Post SASB",
  event                                                              = "Incident",
  Material_manual_earnings_overall_share                             = "Material disclosure",
  `post_provisional_standard:event`                          = "Post SASB × Incident",
  `Material_manual_earnings_overall_share:event`             = "Incident × Material disclosure",
  `Material_manual_earnings_overall_share:post_provisional_standard` = "Material disclosure × Post SASB",
  `Material_manual_earnings_overall_share:post_provisional_standard:event`
  = "Material disclosure × Incident × Post SASB",
  roa_abs_p1p99                                                      = "ROA",
  earnings_vol_20q                                                   = "Earnings vol",
  leverage_p1p99                                                     = "Leverage",
  severity                                                           = "Severity",
  reach                                                              = "Reach",
  novelty                                                            = "Novelty",
  "cusip"       = "Firm",
  "SICS.Codified.Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`        = est_12,
  `(2) Post spec 2`        = est_14,
  `(3) Full spec`        = est_32,
  `(4) Full spec 2`        = est_34,
  file        = "./results/sasb_spread_coef_table.tex",
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




#---------------- Run Regression (Spread_CS as outcome) -------------------

est_31 <- feols(
  spread_cs_winsorized ~ Material_manual_earnings_overall_share + post_provisional_standard + event +
    post_provisional_standard:Material_manual_earnings_overall_share + event:post_provisional_standard + Material_manual_earnings_overall_share:event +
    post_provisional_standard:event:Material_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_cs,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_32 <- feols(
  spread_cs_winsorized ~ Material_manual_earnings_overall_share + post_provisional_standard + event +
    post_provisional_standard:Material_manual_earnings_overall_share + event:post_provisional_standard + Material_manual_earnings_overall_share:event +
    post_provisional_standard:event:Material_manual_earnings_overall_share +
    roa_abs_p1p99  + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | SICS.Codified.Industry + YearQuarter,
  data = data_cs,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_33 <- feols(
  spread_cs_winsorized ~ Material_manual_earnings_overall_share + post_provisional_standard + event +
    post_provisional_standard:Material_manual_earnings_overall_share + event:post_provisional_standard + Material_manual_earnings_overall_share:event +
    post_provisional_standard:event:Material_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_cs,
  vcov = ~ cusip + YearQuarter
)

est_34 <- feols(
  spread_cs_winsorized ~ Material_manual_earnings_overall_share + post_provisional_standard + event +
    post_provisional_standard:Material_manual_earnings_overall_share + event:post_provisional_standard + Material_manual_earnings_overall_share:event +
    post_provisional_standard:event:Material_manual_earnings_overall_share +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | cusip + YearQuarter,
  data = data_cs,
  vcov = ~ cusip + YearQuarter
)



etable(est_31, est_32, est_33, est_34, fixef_sizes = TRUE, coefstat = 'tstat')
