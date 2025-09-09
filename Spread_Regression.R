library(tidyverse)
library(ggplot2)
library(scales)
library(fixest)
library(staggered)



#---------------- Load data -------------------
# Load data (all)
data_daily <- read.csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESGmateriality/Data/panel_bid_ask_disclosure.csv")

# Select main regression variables to make working with data more manageable
data_daily = data_daily %>% dplyr::select(cusip8, evtdate, rel_day, YearQuarter, material_flag , severity, reach, novelty, SICS.Codified.Industry, event,
                                          post_provisional_standard, spread, log_spread, spread_winsorized, spread_cs, log_spread_cs, spread_cs_winsorized,
                                          roa_abs_p1p99, NonMaterial_manual_earnings_overall_share,
                                          Material_manual_earnings_overall_share,
                                          earnings_vol_20q, leverage_p1p99)

data_daily <- data_daily %>%
  mutate(
    cusip = as.factor(cusip8),
    YearQuarter = as.factor(YearQuarter)
  )


data_cs <- data_daily %>% drop_na()

# 2) Drop rows with NA in all columns EXCEPT these three (more missings for cs values, so we want to keep these observations because we dont use them in spread regression):
#    spread_cs, log_spread_cs, spread_cs_winsorized

cols_check <- setdiff(names(data_daily),
                      c("spread_cs", "log_spread_cs", "spread_cs_winsorized"))

data_spread <- data_daily %>% drop_na(all_of(cols_check))

#---------------- Plot Distribution -------------------


window_min <- -10
window_max <-  2

plot_df <- data_spread %>%
  filter(rel_day >= window_min, rel_day <= window_max) %>%
  group_by(rel_day) %>%
  summarize(
    avg_spread = mean(spread_winsorized, na.rm = TRUE),
    n = sum(!is.na(spread_winsorized)),
    .groups = "drop"
  )

# Create the directory if needed
dir.create("Plots", showWarnings = FALSE, recursive = TRUE)

# Build the plot and assign to `p`
p <- ggplot(plot_df, aes(x = rel_day, y = avg_spread)) +
  geom_line() +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(window_min, window_max, by = 1),
                     limits = c(window_min, window_max)) +
  labs(
    title = "Average Winsorized Spread Around Event (Window: -10 to +2)",
    x = "Relative day to event (rel_day)",
    y = "Average spread_winsorized"
  ) +
  theme_minimal(base_size = 13)

# Save as PDF (with embedded fonts via cairo); fall back to "pdf" if cairo not available
ggplot2::ggsave(
  filename = "Plots/spread_plot.pdf",
  plot     = p,
  device   = grDevices::cairo_pdf,  # or device = "pdf"
  width    = 8, height = 5, units = "in"
)



plot_df <- data_spread %>%
  filter(rel_day >= window_min, rel_day <= window_max) %>%
  mutate(
    period = ifelse(post_provisional_standard == 1, "Post", "Pre")
  ) %>%
  group_by(rel_day, period) %>%
  summarize(
    avg_spread = mean(spread_winsorized, na.rm = TRUE),
    n = sum(!is.na(spread_winsorized)),
    .groups = "drop"
  )

p <- ggplot(plot_df, aes(x = rel_day, y = avg_spread,
                         color = period, linetype = period)) +
  geom_line(linewidth = 1) +
  geom_point() +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(window_min, window_max, by = 1),
                     limits = c(window_min, window_max)) +
  labs(
    title = "Average Winsorized Spread Around Event (Pre vs Post)",
    x = "Relative day to event (rel_day)",
    y = "Average spread_winsorized",
    color = "Period", linetype = "Period"
  ) +
  theme_minimal(base_size = 13)

ggplot2::ggsave(
  filename = "Plots/spread_plot_post.pdf",
  plot     = p,
  device   = grDevices::cairo_pdf,  # or device = "pdf"
  width    = 8, height = 5, units = "in"
)




#---------------- Run Regression (Spread ~ Post + Event ) -------------------

est_11 <- feols(
  spread ~ post_provisional_standard + event + event:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_spread,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_12 <- feols(
  spread ~ post_provisional_standard + event + event:post_provisional_standard +
    roa_abs_p1p99  + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | SICS.Codified.Industry + YearQuarter,
  data = data_spread,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)

est_13 <- feols(
  spread ~ post_provisional_standard + event + event:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99
  | cusip + YearQuarter,
  data = data_spread,
  vcov = ~ cusip + YearQuarter
)

est_14 <- feols(
  spread ~ post_provisional_standard + event + event:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99 + severity + reach + novelty
  | cusip + YearQuarter,
  data = data_spread,
  vcov = ~ cusip + YearQuarter
)



etable(est_11, est_12, est_13, est_14, fixef_sizes = TRUE, coefstat = 'tstat')



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
