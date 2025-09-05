library(tidyverse)
library(ggplot2)
library(scales)


packageVersion("ggplot2")

#---------------- Load data -------------------
# Load data
data <- read.csv("E:/GermanBusinessPanelTeam/Schrader/Forschung/ESGmateriality/Data/panel_disclosure_jmp.csv")


# Select main regression variables to make working with data more manageable
data = data %>% dplyr::select(cusip, incident_date, YearQuarter, SICS.Codified.Industry, total_incidents, material_incidents,
                              nonmaterial_incidents, severity, reach, novelty, Material_manual_earnings_overall_share, post_provisional_standard, car_den,
                              jump, std_1.2, std_1, std_2)

data <- data %>%
  mutate(jump = as.numeric(jump)) %>%
  filter(is.finite(jump))

# Filter for denominator values smaller than the cutoff

data_std12 <- data %>% filter(abs(car_den) >= `std_1.2`)
data_std1  <- data %>% filter(abs(car_den) >= std_1)
data_std2  <- data %>% filter(abs(car_den) >= std_2)

# Plot distribution to compare with Weller et al. 2018

# --- helpers ---
winsor <- function(x, lo = -1, hi = 2) pmin(pmax(as.numeric(x), lo), hi)
share01 <- function(x) mean(x >= 0 & x <= 1, na.rm = TRUE)

# --- top numbers (your current definition) ---
top2  <- nrow(data_std2)/nrow(data)
top1  <- nrow(data_std1)/nrow(data)
top12 <- nrow(data_std12)/nrow(data)

# --- bottom numbers on kept samples ---
bot2  <- share01(data_std2$jump)
bot1  <- share01(data_std1$jump)
bot12 <- share01(data_std12$jump)

# --- data for plotting (winsorized for display) ---
plot_df <- bind_rows(
  transmute(data_std2,  cutoff = "cutoff = 2\u03C3",   jump_w = winsor(jump)),
  transmute(data_std1,  cutoff = "cutoff = 1\u03C3",   jump_w = winsor(jump)),
  transmute(data_std12, cutoff = "cutoff = 0.5\u03C3", jump_w = winsor(jump))
) %>%
  filter(is.finite(jump_w)) %>%
  mutate(cutoff = factor(cutoff, levels = c("cutoff = 2\u03C3","cutoff = 1\u03C3","cutoff = 0.5\u03C3")))

# --- label heights based on *proportions per bin* ---
bins <- 80
breaks <- seq(-1, 2, length.out = bins + 1)
ymax <- plot_df |>
  group_by(cutoff) |>
  summarise(maxprop = {
    h <- hist(jump_w, breaks = breaks, plot = FALSE, include.lowest = TRUE, right = TRUE)
    max(h$counts / sum(h$counts))
  }, .groups = "drop") |>
  summarise(max(maxprop)) |>
  pull()

labs <- tibble(
  cutoff = factor(c("cutoff = 2\u03C3","cutoff = 1\u03C3","cutoff = 0.5\u03C3"),
                  levels = c("cutoff = 2\u03C3","cutoff = 1\u03C3","cutoff = 0.5\u03C3")),
  x = 0.55,
  y = c(0.90, 0.80, 0.70) * ymax,
  label = c(
    paste0("2\u03C3: ",  percent(top2,  accuracy = 0.1), " (", percent(bot2,  accuracy = 0.1), ")"),
    paste0("1\u03C3: ",  percent(top1,  accuracy = 0.1), " (", percent(bot1,  accuracy = 0.1), ")"),
    paste0("0.5\u03C3: ",percent(top12, accuracy = 0.1), " (", percent(bot12, accuracy = 0.1), ")")
  )
)

# --- PLOT with RELATIVE FREQUENCY (proportion per bin) ---
# replace your geom_histogram(...) with this
y_cap <- 0.03

# axis limits used in your plot
x_lo <- -1; x_hi <- 2

# recompute label positions for top-right placement
new_ymax <- min(y_cap, ymax)             # ymax from your earlier calc
x_pad    <- 0.5 * (x_hi - x_lo)         # 3% padding from right
x_right  <- x_hi - x_pad

labs_cap <- labs %>%
  mutate(
    x = x_right,
    y = new_ymax * c(0.97, 0.87, 0.77)   # top, then step down
  )


p <- ggplot(plot_df, aes(x = jump_w, fill = cutoff)) +
  geom_histogram(
    aes(y = after_stat(count / sum(count)), group = cutoff),
    bins = 80, position = "identity", alpha = 0.5, boundary = 0
  ) +
  geom_vline(xintercept = c(0, 1), linetype = "dashed") +
  geom_text(data = labs_cap, aes(x = x, y = y, label = label), inherit.aes = FALSE) +
  scale_x_continuous(limits = c(-1, 2), breaks = c(-1, 0, 1, 2)) +
  coord_cartesian(ylim = c(0, y_cap)) +  # <-- cap y-axis without dropping data
  labs(x = "jump at impact / total price change",
       y = "relative frequency", fill = "") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

print(p)

ggplot2::ggsave(
  filename = "Plots/jump_hist.pdf",
  plot     = p,
  device   = grDevices::cairo_pdf,  # embeds fonts; good for σ, etc.
  width    = 8, height = 5, units = "in"
)

