library(tidyverse)
library(ggplot2)
library(scales)
library(fixest)
library(staggered)
library(zoo)
library(broom)


#-------------------Funds--------------------

data_reprisk <- read.csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESGmateriality/Data/panel_prices.csv")

# Select main regression variables (rename here)
data_reprisk <- data_reprisk %>% dplyr::select( gvkey, YearQuarter, SICS.Codified.Industry, overall_material_disclosure = overall_manual_Material_words,
                                                 post_provisional_standard, Material_manual_earnings_overall_share, 
                                                leverage_p1p99, roa_abs_p1p99, mtb_prevq_p1p99, size_ln_mve_prevq,
                                                car_1_material, car_5_material, car_30_material, reach_material,
                                                bhar, car
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
    car = wins(car, 0.01),
    bhar = wins(bhar, 0.01),
    cusip      = as.factor(gvkey),
    YearQuarter= as.factor(YearQuarter),
    Industry   = as.factor(SICS.Codified.Industry)
  ) %>%
  # add log1p versions (consistent naming)
  dplyr::mutate(
    overall_material_disclosure_log1p            = log1p(overall_material_disclosure),
    car_log1p            = log1p(car),
    bhar_log1p            = log1p(bhar)
    
  )

data_post2010 <- data_reprisk %>%
  dplyr::mutate(
    .year_tmp = suppressWarnings(as.integer(substr(as.character(YearQuarter), 1, 4)))
  ) %>%
  dplyr::filter(.year_tmp > 2009) %>%
  dplyr::select(-.year_tmp) %>%
  droplevels()

#------------------- Plot Returns

#---------------- Plot holding pre/post incident and pre/post SASB  -------------------

data_prices <- read.csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESGmateriality/Data/panel_returns.csv")

# Select main regression variables (rename here)
data_prices <- data_prices %>% dplyr::select(gvkey, YearQuarter, SICS.Codified.Industry, overall_material_disclosure = overall_manual_Material_words,
                                                quarters_from_adoption, post_provisional_standard, Material_manual_earnings_overall_share, 
                                                leverage_p1p99, roa_abs_p1p99, mtb_prevq_p1p99, size_ln_mve_prevq,
                                                car_1_material, car_5_material, car_30_material, reach_material,
                                                abret, evttime)

window_min <- -5
window_max <-  5

# 1) Build event ids per firm so negatives attach to the upcoming zero
df <- data_prices %>%
  mutate(evttime = as.integer(evttime)) %>%
  arrange(gvkey, evttime) %>%
  group_by(gvkey) %>%
  mutate(
    # base id increases at each anchor (evttime == 0)
    base_id  = cumsum(evttime == 0),
    # attach negatives to the NEXT anchor (so they share the same event id)
    event_id = base_id + as.integer(evttime < 0)
  ) %>%
  ungroup()

# 2) Keep only true CAR(5) incident windows (anchor day has car_5_material > 0)
df_evt <- df %>%
  group_by(gvkey, event_id) %>%
  mutate(
    is_car5_incident = any(evttime == 0 & car_5_material > 0, na.rm = TRUE),
    # period flagged from the anchor day
    event_post_flag  = first(post_provisional_standard[evttime == 0]),
    period           = ifelse(event_post_flag == 1, "Post adoption", "Pre adoption")
  ) %>%
  ungroup() %>%
  filter(is_car5_incident, between(evttime, window_min, window_max))


# 3) Aggregate mean abret and 95% CI by relative day & period
plot_df <- df_evt %>%
  group_by(evttime, period) %>%
  summarise(
    mean_abret = mean(abret, na.rm = TRUE),
    n          = sum(!is.na(abret)),
    se         = sd(abret, na.rm = TRUE) / sqrt(pmax(n, 1)),
    lo         = mean_abret - 1.96 * se,
    hi         = mean_abret + 1.96 * se,
    .groups    = "drop"
  )

# 4) Plot
p <- ggplot(plot_df, aes(x = evttime, y = mean_abret,
                    color = period, linetype = period, fill = period)) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dotted") +
  geom_ribbon(aes(ymin = lo, ymax = hi), alpha = 0.15, linewidth = 0, colour = NA) +
  geom_line(linewidth = 1) +
  geom_point() +
  scale_x_continuous(breaks = window_min:window_max, limits = c(window_min, window_max)) +
  # If abret is a decimal return, keep percent labels; otherwise, drop percent_format
  scale_y_continuous(labels = percent_format(accuracy = 0.01)) +
  labs(
    title    = "Average Abnormal Return (abret) Around CAR(5) Incidents",
    subtitle = "Mean abret by trading day relative to incident; grouped by incident-day adoption status",
    x        = "Trading days relative to incident (0 = incident day)",
    y        = "Average abnormal return",
    color    = "Incident period",
    linetype = "Incident period",
    fill     = "Incident period"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

# High-quality PDF (good for LaTeX); 'cairo_pdf' embeds fonts nicely on Windows too
ggplot2::ggsave(
  filename = "Plots/abret_event_car5_pre_post.pdf",
  plot     = p,
  device   = grDevices::cairo_pdf,
  width    = 8, height = 5, units = "in"
)

                                                

#-------------------Incidents_all = Funds--------------------

est_11 <- feols(
  car ~ car_5_material +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_12 <- feols(
  car ~ post_provisional_standard +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_13 <- feols(
  car ~ car_5_material + post_provisional_standard + post_provisional_standard:car_5_material +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
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
  car                                              = "CAR[-60;-1]",
  post_provisional_standard                                       = "Post SASB",
  car_5_material                                       = "Low CAR Incident",
  "car_5_material:post_provisional_standard"          = "Post SASB x Incident",
  "cusip"       = "Firm",
  "Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`   = est_13,
  `(2) Post spec 2` = est_12,
  `(3) Post spec`   = est_13,
  
  # ensure clustered SEs are used in the table computation
  cluster     = ~ cusip + YearQuarter,
  
  # print a row with the SE type (e.g., “Clustered (cusip & YearQuarter)”)
  se.row      = TRUE,           
  
  # other stats you want at the bottom
  fitstat     = ~ r2 + n,
  
  file         = "./results/prices_car.tex",
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


est_11 <- feols(
  car ~ reach_material +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_12 <- feols(
  car ~ post_provisional_standard +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_13 <- feols(
  car ~ reach_material + post_provisional_standard + post_provisional_standard:reach_material +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
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
  car                                              = "CAR[-60;-1]",
  post_provisional_standard                                       = "Post SASB",
  reach_material                                       = "High Reach Incident",
  "reach_material:post_provisional_standard"          = "Post SASB x Incident",
  "cusip"       = "Firm",
  "Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`   = est_13,
  `(2) Post spec 2` = est_12,
  `(3) Post spec`   = est_13,
  
  # ensure clustered SEs are used in the table computation
  cluster     = ~ cusip + YearQuarter,
  
  # print a row with the SE type (e.g., “Clustered (cusip & YearQuarter)”)
  se.row      = TRUE,           
  
  # other stats you want at the bottom
  fitstat     = ~ r2 + n,
  
  file         = "./results/prices_reach.tex",
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

