library(tidyverse)
library(ggplot2)
library(scales)
library(fixest)
library(staggered)
library(zoo)



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
    overall_material_disclosure_log1p            = log1p(overall_material_disclosure),
    Material_manual_earnings_overall_share_log1p = log1p(pmax(Material_manual_earnings_overall_share, 0))
  )

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
                                                n_material_t3, n_nonmaterial_t3, n_car_1_material_t3, n_car_5_material_t3, n_reach_material_t3, n_severity_material_t3,
                                                n_car_30_material_t1, n_car_30_material_t2, n_car_30_material_t3, n_car_1_material_year_t1, n_car_1_material_year_t2, 
                                                n_car_1_material_year_t3, n_car_5_material_year_t1, n_car_5_material_year_t2, 
                                                n_car_5_material_year_t3, n_car_30_material_year_t1, n_car_30_material_year_t2, 
                                                n_car_30_material_year_t3,
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

#-------------------Incidents_all = Disclosure x Post--------------------

est_11 <- fepois(
  n_incidents_t1 ~ post_provisional_standard + overall_material_disclosure_log1p + overall_material_disclosure_log1p:post_provisional_standard +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)


est_12 <- fepois(
  n_incidents_t1 ~ post_provisional_standard + overall_material_disclosure_log1p + overall_material_disclosure_log1p:post_provisional_standard +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | Industry + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_13 <- fepois(
  n_incidents_t1 ~ post_provisional_standard + overall_material_disclosure_log1p + overall_material_disclosure_log1p:post_provisional_standard +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)


etable(est_11, est_12, est_13, fixef_sizes = TRUE, coefstat = 'tstat')

# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^post_provisional_standard$",
  "^overall_material_disclosure_log1p$",
  "^post_provisional_standard:overall_material_disclosure_log1p$"
)
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  n_incidents_t1                                               = "# Incidents (t+1)",
  post_provisional_standard                                       = "Post SASB",
  overall_material_disclosure_log1p                               = "Ln # of Material Words",
  "post_provisional_standard:overall_material_disclosure_log1p"          = "Post SASB x Disclosure",
  "cusip"       = "Firm",
  "Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`   = est_11,
  `(2) Post spec 2` = est_12,
  `(3) Post spec`   = est_13,
  
  # ensure clustered SEs are used in the table computation
  cluster     = ~ cusip + YearQuarter,
  
  # print a row with the SE type (e.g., “Clustered (cusip & YearQuarter)”)
  se.row      = TRUE,           
  
  # other stats you want at the bottom
  fitstat     = ~ r2 + n,
  
  file         = "./results/incidents_post_disclosure_words.tex",
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

est_11 <- fepois(
  n_car_5_material_t1 ~ post_provisional_standard + overall_material_disclosure_log1p + overall_material_disclosure_log1p:post_provisional_standard +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq,
  data =  data_post2010,
  vcov = ~ cusip + YearQuarter
)


est_12 <- fepois(
  n_car_5_material_t1 ~ post_provisional_standard + overall_material_disclosure_log1p + overall_material_disclosure_log1p:post_provisional_standard +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | Industry + YearQuarter,
  data =  data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_13 <- fepois(
  n_car_5_material_t1 ~ post_provisional_standard + overall_material_disclosure_log1p + overall_material_disclosure_log1p:post_provisional_standard +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data =  data_post2010,
  vcov = ~ cusip + YearQuarter
)



etable(est_11, est_12, est_13, fixef_sizes = TRUE, coefstat = 'tstat')

# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^post_provisional_standard$",
  "^overall_material_disclosure_log1p$",
  "^post_provisional_standard:overall_material_disclosure_log1p$"
)
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  n_car_5_material_t1                                               = "# Low CAR Incidents (t+1)",
  post_provisional_standard                                       = "Post SASB",
  overall_material_disclosure_log1p                               = "Ln # of Material Words",
  "post_provisional_standard:overall_material_disclosure_log1p"          = "Post SASB x Disclosure",
  "cusip"       = "Firm",
  "Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`   = est_11,
  `(2) Post spec 2` = est_12,
  `(3) Post spec`   = est_13,
  
  # ensure clustered SEs are used in the table computation
  cluster     = ~ cusip + YearQuarter,
  
  # print a row with the SE type (e.g., “Clustered (cusip & YearQuarter)”)
  se.row      = TRUE,           
  
  # other stats you want at the bottom
  fitstat     = ~ r2 + n,
  
  file         = "./results/car_post_disclosure_words.tex",
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

est_11 <- fepois(
  n_reach_material_t1 ~ post_provisional_standard + overall_material_disclosure_log1p + overall_material_disclosure_log1p:post_provisional_standard +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq,
  data =  data_post2010, 
  vcov = ~ cusip + YearQuarter
)


est_12 <- fepois(
  n_reach_material_t1 ~ post_provisional_standard + overall_material_disclosure_log1p + overall_material_disclosure_log1p:post_provisional_standard +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | Industry + YearQuarter,
  data =  data_post2010,
  vcov = ~ cusip + YearQuarter
)

est_13 <- fepois(
  n_reach_material_t1 ~ post_provisional_standard + overall_material_disclosure_log1p + overall_material_disclosure_log1p:post_provisional_standard +
    roa_abs_p1p99 + leverage_p1p99 + mtb_prevq_p1p99 + size_ln_mve_prevq
  | cusip + YearQuarter,
  data = data_post2010,
  vcov = ~ cusip + YearQuarter
)


etable(est_11, est_12, est_13, fixef_sizes = TRUE, coefstat = 'tstat')


# --- exact order on ORIGINAL names (prefix with % so dict doesn't interfere) ---
ord <- c(
  "^post_provisional_standard$",
  "^overall_material_disclosure_log1p$",
  "^post_provisional_standard:overall_material_disclosure_log1p$"
)
order_vec <- paste0("%", ord)
keep_vec  <- order_vec

# --- pretty labels (your dict) ---
dict <- c(
  n_reach_material_t1                                               = "# High Reach Incidents (t+1)",
  post_provisional_standard                                       = "Post SASB",
  overall_material_disclosure_log1p                               = "Ln # of Material Words",
  "post_provisional_standard:overall_material_disclosure_log1p"          = "Post SASB x Disclosure",
  "cusip"       = "Firm",
  "Industry" = "Industry",
  "YearQuarter"            = "Year-Quarter"
)

# --- etable to LaTeX with named columns, replace & float control ---
etable(
  `(1) Post spec`   = est_11,
  `(2) Post spec 2` = est_12,
  `(3) Post spec`   = est_13,
  
  # ensure clustered SEs are used in the table computation
  cluster     = ~ cusip + YearQuarter,
  
  # print a row with the SE type (e.g., “Clustered (cusip & YearQuarter)”)
  se.row      = TRUE,           
  
  # other stats you want at the bottom
  fitstat     = ~ r2 + n,
  
  file         = "./results/reach_post_disclosure_words.tex",
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



