library(tidyverse)
library(ggplot2)
library(scales)
library(fixest)
library(staggered)
library(zoo)



#---------------- Sentences -------------------
# Load data (all)
data_sentences <- read.csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESGmateriality/Data/disclosure_pre_post_sentences.csv")

# Select main regression variables to make working with data more manageable
data_sentences = data_sentences %>% dplyr::select(gvkey, YearQuarter, SICS.Codified.Industry, overall_manual_Material_sentences, overall_o3_Material_sentences,
                                                    quarters_from_adoption, post_provisional_standard, Material_manual_earnings_overall_share, Material_o3_earnings_overall_share,
                                                    Material_manual_earnings_presentation_share, Material_manual_earnings_question_share, Material_manual_earnings_answer_share,
                                                    NonMaterial_manual_earnings_overall_share)

data_sentences <- data_sentences %>%
  mutate(
    cusip = as.factor(gvkey),
    YearQuarter = as.factor(YearQuarter)
  )

#-------------------Words--------------------

# Load data (all)
data_words <- read.csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESGmateriality/Data/disclosure_pre_post_words.csv")

# Select main regression variables to make working with data more manageable
data_words = data_words %>% dplyr::select(gvkey, YearQuarter, SICS.Codified.Industry, overall_manual_Material_words, overall_manual_NonMaterial_words,
                                                  quarters_from_adoption, post_provisional_standard, Material_manual_earnings_overall_share,
                                                  Material_manual_earnings_presentation_share, Material_manual_earnings_question_share, Material_manual_earnings_answer_share,
                                                  NonMaterial_manual_earnings_overall_share)

data_words <- data_words %>%
  mutate(
    cusip = as.factor(gvkey),
    YearQuarter = as.factor(YearQuarter)
  )

#-------------------Financials--------------------

data_financials <- read.csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESGmateriality/Data/panel_financials_disclosure_sentences.csv")

# Select main regression variables to make working with data more manageable
data_financials = data_financials %>% dplyr::select(gvkey, YearQuarter, SICS.Codified.Industry, overall_manual_Material_sentences, overall_o3_Material_sentences,
                                          quarters_from_adoption, post_provisional_standard, Material_manual_earnings_overall_share, Material_o3_earnings_overall_share,
                                          Material_manual_earnings_presentation_share, Material_manual_earnings_question_share, Material_manual_earnings_answer_share,
                                          NonMaterial_manual_earnings_overall_share, earnings_vol_20q, leverage_p1p99, roa_abs_p1p99, mtb_prevq_p1p99, altman_z_p1p99)

data_financials <- data_financials %>%
  mutate(
    cusip = as.factor(gvkey),
    YearQuarter = as.factor(YearQuarter)
  )

data_financials <- read.csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESGmateriality/Data/panel_financials_disclosure_words.csv")

# Select main regression variables to make working with data more manageable
data_financials = data_financials %>% dplyr::select(gvkey, YearQuarter, SICS.Codified.Industry, overall_manual_Material_words, overall_manual_NonMaterial_words,
                                                    quarters_from_adoption, post_provisional_standard, Material_manual_earnings_overall_share, size_ln_mve_prevq,
                                                    Material_manual_earnings_presentation_share, Material_manual_earnings_question_share, Material_manual_earnings_answer_share,
                                                    NonMaterial_manual_earnings_overall_share, earnings_vol_20q, leverage_p1p99, roa_abs_p1p99, mtb_prevq_p1p99, altman_z_p1p99)

data_financials <- data_financials %>%
  mutate(
    cusip = as.factor(gvkey),
    YearQuarter = as.factor(YearQuarter)
  )

data_financials <- data_financials %>%
  dplyr::mutate(
    .year_tmp = suppressWarnings(as.integer(substr(as.character(YearQuarter), 1, 4)))
  ) %>%
  dplyr::filter(.year_tmp > 2009) %>%
  dplyr::select(-.year_tmp) %>%
  droplevels()

data_financials <- data_financials %>%
  dplyr::mutate(
    overall_manual_Material_words            = wins(overall_manual_Material_words, 0.01),
    Material_manual_earnings_overall_share = wins(Material_manual_earnings_overall_share, 0.01),
    cusip      = as.factor(gvkey),
    YearQuarter= as.factor(YearQuarter),
    Industry   = as.factor(SICS.Codified.Industry)
  ) %>%
  # add log1p versions (consistent naming)
  dplyr::mutate(
    overall_material_disclosure_log1p            = log1p(overall_manual_Material_words)
  )

#-------------------Reprisk--------------------

#-------------------Reprisk sentences--------------------

data_reprisk <- read.csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESGmateriality/Data/panel_financials_reprisk_incidents_disclosure_sentences.csv")

# Select main regression variables (rename here)
data_reprisk <- data_reprisk %>% dplyr::select( gvkey, YearQuarter, SICS.Codified.Industry, overall_material_disclosure = overall_manual_Material_sentences,
                                                quarters_from_adoption, post_provisional_standard, Material_manual_earnings_overall_share, 
                                                leverage_p1p99, roa_abs_p1p99, mtb_prevq_p1p99, size_ln_mve_prevq, 
                                                n_material_t1, n_nonmaterial_t1, n_car_1_material_t1, n_car_5_material_t1, n_reach_material_t1, n_severity_material_t1, 
                                                n_material_t2, n_nonmaterial_t2, n_car_1_material_t2, n_car_5_material_t2, n_reach_material_t2, n_severity_material_t2, 
                                                n_material_t3, n_nonmaterial_t3, n_car_1_material_t3, n_car_5_material_t3, n_reach_material_t3, n_severity_material_t3
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

# --- subset to observations after 2010 (2011+) -------------------------------
data_post2010 <- data_reprisk %>%
  dplyr::mutate(
    .year_tmp = suppressWarnings(as.integer(substr(as.character(YearQuarter), 1, 4)))
  ) %>%
  dplyr::filter(.year_tmp > 2009) %>%
  dplyr::select(-.year_tmp) %>%
  droplevels()


#-------------------Reprisk words--------------------

data_reprisk <- read.csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESGmateriality/Data/panel_financials_reprisk_incidents_disclosure_words.csv")

# Select main regression variables (rename here)
data_reprisk <- data_reprisk %>% dplyr::select( gvkey, YearQuarter, SICS.Codified.Industry, overall_material_disclosure = overall_manual_Material_words,
                                                quarters_from_adoption, post_provisional_standard, Material_manual_earnings_overall_share, 
                                                leverage_p1p99, roa_abs_p1p99, mtb_prevq_p1p99, size_ln_mve_prevq, 
                                                n_material_t1, n_nonmaterial_t1, n_car_1_material_t1, n_car_5_material_t1, n_reach_material_t1, n_severity_material_t1, 
                                                n_material_t2, n_nonmaterial_t2, n_car_1_material_t2, n_car_5_material_t2, n_reach_material_t2, n_severity_material_t2, 
                                                n_material_t3, n_nonmaterial_t3, n_car_1_material_t3, n_car_5_material_t3, n_reach_material_t3, n_severity_material_t3
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


#---------------- Plot disclosure shares ----------------------------------------

# Reprisk Sample


# ---- window ----
window_min <- -12
window_max <-   32

dw <- data_post2010 %>%
  mutate(
    quarters_from_adoption = as.integer(quarters_from_adoption),
    Material_manual_earnings_overall_share = as.numeric(Material_manual_earnings_overall_share),
    overall_material_disclosure = as.numeric(overall_material_disclosure)
  ) %>%
  filter(quarters_from_adoption >= window_min,
         quarters_from_adoption <= window_max)

# --- summarize share (left axis) ---
share_df <- dw %>%
  group_by(quarters_from_adoption) %>%
  summarize(
    mean_share = mean(Material_manual_earnings_overall_share, na.rm = TRUE),
    n  = sum(!is.na(Material_manual_earnings_overall_share)),
    sd = sd(Material_manual_earnings_overall_share, na.rm = TRUE),
    se = sd / sqrt(pmax(n, 1)),
    lo = pmax(0, mean_share - 1.96 * se),
    hi = pmin(1, mean_share + 1.96 * se),
    .groups = "drop"
  )

# --- summarize words (right axis; will be scaled to left) ---
words_df <- dw %>%
  group_by(quarters_from_adoption) %>%
  summarize(
    mean_words = mean(overall_material_disclosure, na.rm = TRUE),
    n  = sum(!is.na(overall_material_disclosure)),
    sd = sd(overall_material_disclosure, na.rm = TRUE),
    se = sd / sqrt(pmax(n, 1)),
    lo = pmax(0, mean_words - 1.96 * se),
    hi = mean_words + 1.96 * se,
    .groups = "drop"
  )

# Scale factor to overlay words on the share axis
den <- max(words_df$mean_words, na.rm = TRUE)
num <- max(share_df$mean_share, na.rm = TRUE)
s <- if (is.finite(den) && den > 0 && is.finite(num) && num > 0) num / den else 1

# --- plot with secondary y-axis ---
p <- ggplot() +
  # Share (left axis)
  geom_ribbon(data = share_df,
              aes(x = quarters_from_adoption, ymin = lo, ymax = hi, fill = "Share CI"),
              alpha = 0.15, linewidth = 0) +
  geom_line(data = share_df,
            aes(x = quarters_from_adoption, y = mean_share, color = "Share"), linewidth = 1) +
  geom_point(data = share_df,
             aes(x = quarters_from_adoption, y = mean_share, color = "Share")) +
  
  # Words (right axis; scaled to left by s)
  geom_ribbon(data = words_df,
              aes(x = quarters_from_adoption, ymin = lo * s, ymax = hi * s, fill = "Words CI"),
              alpha = 0.15, linewidth = 0) +
  geom_line(data = words_df,
            aes(x = quarters_from_adoption, y = mean_words * s, color = "Counts"),
            linewidth = 1, linetype = "dashed") +
  geom_point(data = words_df,
             aes(x = quarters_from_adoption, y = mean_words * s, color = "Counts"),
             shape = 17) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(window_min, window_max, 1),
                     limits = c(window_min, window_max)) +
  scale_y_continuous(
    name = "Average share of material words",
    labels = label_percent(accuracy = 0.1),
    sec.axis = sec_axis(~ . / s,
                        name = "Average number of material words",
                        labels = label_comma())
  ) +
  scale_color_manual(values = c("Share" = "#1f77b4", "Counts" = "#ff7f0e"), name = NULL) +
  scale_fill_manual(values = c("Share CI" = "#1f77b4", "Words CI" = "#ff7f0e"), guide = "none") +
  labs(
    title = "Material disclosure around SASB adoption",
    x = "Quarters relative to adoption"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

p


ggsave("Plots/material_share_and_words.pdf", plot = p, device = "pdf",
       width = 9, height = 6, units = "in")

#---------------- Words for all ----------------------

# ---- window ----
window_min <- -32
window_max <-   32

dw <- data_words %>%
  mutate(
    quarters_from_adoption = as.integer(quarters_from_adoption),
    Material_manual_earnings_overall_share = as.numeric(Material_manual_earnings_overall_share),
    overall_material_disclosure = as.numeric(overall_material_disclosure)
  ) %>%
  filter(quarters_from_adoption >= window_min,
         quarters_from_adoption <= window_max)

# --- summarize share (left axis) ---
share_df <- dw %>%
  group_by(quarters_from_adoption) %>%
  summarize(
    mean_share = mean(Material_manual_earnings_overall_share, na.rm = TRUE),
    n  = sum(!is.na(Material_manual_earnings_overall_share)),
    sd = sd(Material_manual_earnings_overall_share, na.rm = TRUE),
    se = sd / sqrt(pmax(n, 1)),
    lo = pmax(0, mean_share - 1.96 * se),
    hi = pmin(1, mean_share + 1.96 * se),
    .groups = "drop"
  )

# --- summarize words (right axis; will be scaled to left) ---
words_df <- dw %>%
  group_by(quarters_from_adoption) %>%
  summarize(
    mean_words = mean(overall_manual_Material_words, na.rm = TRUE),
    n  = sum(!is.na(overall_manual_Material_words)),
    sd = sd(overall_manual_Material_words, na.rm = TRUE),
    se = sd / sqrt(pmax(n, 1)),
    lo = pmax(0, mean_words - 1.96 * se),
    hi = mean_words + 1.96 * se,
    .groups = "drop"
  )

# Scale factor to overlay words on the share axis
den <- max(words_df$mean_words, na.rm = TRUE)
num <- max(share_df$mean_share, na.rm = TRUE)
s <- if (is.finite(den) && den > 0 && is.finite(num) && num > 0) num / den else 1

# --- plot with secondary y-axis ---
p <- ggplot() +
  # Share (left axis)
  geom_ribbon(data = share_df,
              aes(x = quarters_from_adoption, ymin = lo, ymax = hi, fill = "Share CI"),
              alpha = 0.15, linewidth = 0) +
  geom_line(data = share_df,
            aes(x = quarters_from_adoption, y = mean_share, color = "Share"), linewidth = 1) +
  geom_point(data = share_df,
             aes(x = quarters_from_adoption, y = mean_share, color = "Share")) +
  
  # Words (right axis; scaled to left by s)
  geom_ribbon(data = words_df,
              aes(x = quarters_from_adoption, ymin = lo * s, ymax = hi * s, fill = "Words CI"),
              alpha = 0.15, linewidth = 0) +
  geom_line(data = words_df,
            aes(x = quarters_from_adoption, y = mean_words * s, color = "Counts"),
            linewidth = 1, linetype = "dashed") +
  geom_point(data = words_df,
             aes(x = quarters_from_adoption, y = mean_words * s, color = "Counts"),
             shape = 17) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(window_min, window_max, 1),
                     limits = c(window_min, window_max)) +
  scale_y_continuous(
    name = "Average share of material words",
    labels = label_percent(accuracy = 0.1),
    sec.axis = sec_axis(~ . / s,
                        name = "Average number of material words",
                        labels = label_comma())
  ) +
  scale_color_manual(values = c("Share" = "#1f77b4", "Counts" = "#ff7f0e"), name = NULL) +
  scale_fill_manual(values = c("Share CI" = "#1f77b4", "Words CI" = "#ff7f0e"), guide = "none") +
  labs(
    title = "Material disclosure around SASB adoption",
    x = "Quarters relative to adoption"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

p


ggsave("Plots/material_share_and_words.pdf", plot = p, device = "pdf",
       width = 9, height = 6, units = "in")

#---------------Sentences ------------

# ---- window ----
window_min <- -32
window_max <-   32

dw <- data_sentences %>%
  mutate(
    quarters_from_adoption = as.integer(quarters_from_adoption),
    Material_manual_earnings_overall_share = as.numeric(Material_manual_earnings_overall_share),
    overall_manual_Material_words = as.numeric(overall_manual_Material_sentences)
  ) %>%
  filter(quarters_from_adoption >= window_min,
         quarters_from_adoption <= window_max)

# --- summarize share (left axis) ---
share_df <- dw %>%
  group_by(quarters_from_adoption) %>%
  summarize(
    mean_share = mean(Material_manual_earnings_overall_share, na.rm = TRUE),
    n  = sum(!is.na(Material_manual_earnings_overall_share)),
    sd = sd(Material_manual_earnings_overall_share, na.rm = TRUE),
    se = sd / sqrt(pmax(n, 1)),
    lo = pmax(0, mean_share - 1.96 * se),
    hi = pmin(1, mean_share + 1.96 * se),
    .groups = "drop"
  )

# --- summarize words (right axis; will be scaled to left) ---
words_df <- dw %>%
  group_by(quarters_from_adoption) %>%
  summarize(
    mean_words = mean(overall_manual_Material_sentences, na.rm = TRUE),
    n  = sum(!is.na(overall_manual_Material_sentences)),
    sd = sd(overall_manual_Material_sentences, na.rm = TRUE),
    se = sd / sqrt(pmax(n, 1)),
    lo = pmax(0, mean_words - 1.96 * se),
    hi = mean_words + 1.96 * se,
    .groups = "drop"
  )

# Scale factor to overlay words on the share axis
den <- max(words_df$mean_words, na.rm = TRUE)
num <- max(share_df$mean_share, na.rm = TRUE)
s <- if (is.finite(den) && den > 0 && is.finite(num) && num > 0) num / den else 1

# --- plot with secondary y-axis ---
p <- ggplot() +
  # Share (left axis)
  geom_ribbon(data = share_df,
              aes(x = quarters_from_adoption, ymin = lo, ymax = hi, fill = "Share CI"),
              alpha = 0.15, linewidth = 0) +
  geom_line(data = share_df,
            aes(x = quarters_from_adoption, y = mean_share, color = "Share"), linewidth = 1) +
  geom_point(data = share_df,
             aes(x = quarters_from_adoption, y = mean_share, color = "Share")) +
  
  # Words (right axis; scaled to left by s)
  geom_ribbon(data = words_df,
              aes(x = quarters_from_adoption, ymin = lo * s, ymax = hi * s, fill = "Words CI"),
              alpha = 0.15, linewidth = 0) +
  geom_line(data = words_df,
            aes(x = quarters_from_adoption, y = mean_words * s, color = "Counts"),
            linewidth = 1, linetype = "dashed") +
  geom_point(data = words_df,
             aes(x = quarters_from_adoption, y = mean_words * s, color = "Counts"),
             shape = 17) +
  
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_x_continuous(breaks = seq(window_min, window_max, 1),
                     limits = c(window_min, window_max)) +
  scale_y_continuous(
    name = "Average share of material sentences",
    labels = label_percent(accuracy = 0.1),
    sec.axis = sec_axis(~ . / s,
                        name = "Average number of material sentences",
                        labels = label_comma())
  ) +
  scale_color_manual(values = c("Share" = "#1f77b4", "Counts" = "#ff7f0e"), name = NULL) +
  scale_fill_manual(values = c("Share CI" = "#1f77b4", "Words CI" = "#ff7f0e"), guide = "none") +
  labs(
    title = "Material disclosure around SASB adoption",
    x = "Quarters relative to adoption"
  ) +
  theme_minimal(base_size = 13) +
  theme(legend.position = "top")

p


ggsave("Plots/material_share_and_sentences.pdf", plot = p, device = "pdf",
       width = 9, height = 6, units = "in")

#---------------- All disclosures --------------------------------


est_11 <- feols(
  overall_manual_Material_words ~ post_provisional_standard 
  | SICS.Codified.Industry + YearQuarter,
  data = data_words,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)


est_12 <- feols(
  overall_manual_Material_words ~ post_provisional_standard 
  | cusip + YearQuarter,
  data = data_words,
  vcov = ~ cusip + YearQuarter
)


etable(est_11, est_12,fixef_sizes = TRUE, coefstat = 'tstat')

#-------------------Disclosure + Financials ----------------

est_21 <- feols(
  overall_material_disclosure_log1p ~ post_provisional_standard 
  | SICS.Codified.Industry + YearQuarter,
  data = data_financials,
  vcov = ~ cusip + YearQuarter
)

est_22 <- feols(
  overall_material_disclosure_log1p ~ post_provisional_standard  +
    roa_abs_p1p99 + size_ln_mve_prevq + leverage_p1p99 + mtb_prevq_p1p99 
  | SICS.Codified.Industry + YearQuarter,
  data = data_financials,
  vcov = ~ cusip + YearQuarter
)


est_23 <- feols(
  overall_material_disclosure_log1p ~ post_provisional_standard 
  | cusip + YearQuarter,
  data = data_financials,
  vcov = ~ cusip + YearQuarter
)

est_24 <- feols(
  overall_material_disclosure_log1p ~ post_provisional_standard  +
    roa_abs_p1p99 + size_ln_mve_prevq + leverage_p1p99 + mtb_prevq_p1p99 
  | cusip + YearQuarter,
  data = data_financials,
  vcov = ~ cusip + YearQuarter
)


etable(est_21, est_22, est_23, est_24, fixef_sizes = TRUE, coefstat = 'tstat')

#-------------------Disclosure + Reprisk ----------------

est_31 <- feols(
  overall_material_disclosure_log1p ~ post_provisional_standard 
  | SICS.Codified.Industry + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_32 <- feols(
  overall_material_disclosure_log1p ~ post_provisional_standard  +
    roa_abs_p1p99 + size_ln_mve_prevq + leverage_p1p99 + mtb_prevq_p1p99 
  | SICS.Codified.Industry + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)


est_33 <- feols(
  overall_material_disclosure_log1p ~ post_provisional_standard 
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_34 <- feols(
  overall_material_disclosure_log1p ~ post_provisional_standard  +
    roa_abs_p1p99 + size_ln_mve_prevq + leverage_p1p99 + mtb_prevq_p1p99 
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)


etable(est_31, est_32, est_33, est_34, fixef_sizes = TRUE, coefstat = 'tstat')

#-------------------------Save Latex Table-------------------

# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^post_provisional_standard$"
)
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  overall_material_disclosure_log1p = "Ln # of Material Words",
  post_provisional_standard                                          = "Post SASB",
  "cusip"       = "Firm",
  "SICS.Codified.Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(3) Post spec`        = est_31,
  `(4) Post spec 2`        = est_32,
  `(5) Full spec`        = est_33,
  `(6) Full spec 2`        = est_34,
  # ensure clustered SEs are used in the table computation
  cluster     = ~ cusip + YearQuarter,
  
  # print a row with the SE type (e.g., “Clustered (cusip & YearQuarter)”)
  se.row      = TRUE,           
  
  # other stats you want at the bottom
  fitstat     = ~ r2 + n,
  
  file         = "./results/sasb_disclosure_coef_table_words.tex",
  replace      = TRUE,
  dict         = dict,
  digits       = "r3",
  digits.stats = "r3",
  float        = FALSE,
  coefstat     = "tstat",       # Poisson → z-stats
  keep         = keep_vec,
  order        = order_vec,
  extralines = list("Controls" = c("No", "Yes", "No", "Yes")),
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



#---------------- Does disclosure predict incidents  -------------------


est_11 <- fepois(
  n_material ~ overall_manual_Material_words  + post_provisional_standard +
    overall_manual_Material_words:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99 + mtb_prevq_p1p99 + altman_z_p1p99
  | SICS.Codified.Industry + YearQuarter,
  data = data_reprisk,
  vcov = ~ SICS.Codified.Industry + YearQuarter
)


est_12 <- fepois(
  n_material ~ overall_manual_Material_words  + post_provisional_standard +
    overall_manual_Material_words:post_provisional_standard +
    roa_abs_p1p99 + earnings_vol_20q + leverage_p1p99 + mtb_prevq_p1p99 + altman_z_p1p99
  | cusip + YearQuarter,
  data = data_reprisk,
  vcov = ~ cusip + YearQuarter
)


etable(est_11, est_12, fixef_sizes = TRUE, coefstat = 'tstat')

# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^post_provisional_standard$",
  "^overall_manual_Material_words$",
  "^overall_manual_Material_wordss:post_provisional_standard$",
  "^roa_abs_p1p99$",
  "^earnings_vol_20q$",
  "^leverage_p1p99$",
  "^mtb_prevq_p1p99$",
  "^altman_z_p1p99$"
)
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  n_material                                                              = "# Material Incidents",
  overall_manual_Material_words                                       = "# of Material Words",
  "overall_manual_Material_sentences:post_provisional_standard"            = "Material disclosure × Post SASB",
  post_provisional_standard                                          = "Post SASB",
  roa_abs_p1p99                                                      = "ROA",
  earnings_vol_20q                                                   = "Earnings vol",
  leverage_p1p99                                                     = "Leverage",
  altman_z_p1p99                                                     = "Altman-Z",
  mtb_prevq_p1p99                                                     = "Market-to-Book",
  "cusip"       = "Firm",
  "SICS.Codified.Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`        = est_11,
  `(2) Post spec 2`        = est_12,
  file        = "./results/sasb_disclosure_incidents_words.tex",
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

