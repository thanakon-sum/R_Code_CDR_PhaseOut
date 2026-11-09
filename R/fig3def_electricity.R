library(dplyr)
library(tidyr)
library(ggplot2)
library(cowplot)

csv_path  <- "data/iamc_fig.csv"
out_path  <- "output/panels/SecEle_DAC_2panel_fresh.png"

scenario_map <- tribble(
  ~SCENARIO,                   ~label,
  "SSP2_400C_2025CP_NoCC_No",  "No Phase-out",
  "SSP2_TPCC_Ext_NoCC_No",     "TPCC",
  "SSP2_HCC_2110_NoCC_No",     "HCC_2110",
  "SSP2_HCC_2150_NoCC_No",     "HCC_2150",
  "SSP2_Med_2110_NoCC_No",     "MCC_2110",
  "SSP2_Med_2120_NoCC_No",     "MCC_2120",
  "SSP2_Med_2130_NoCC_No",     "MCC_2130",
  "SSP2_Med_2140_NoCC_No",     "MCC_2140",
  "SSP2_Med_2150_NoCC_No",     "MCC_2150",
  "SSP2_LCC_2110_NoCC_No",     "LCC_2110",
  "SSP2_LCC_2150_NoCC_No",     "LCC_2150"
)

scenario_order <- c("No Phase-out","TPCC",
                    "HCC_2110","HCC_2150",
                    "MCC_2110","MCC_2120","MCC_2130","MCC_2140","MCC_2150",
                    "LCC_2110","LCC_2150")

pal <- c(
  "No Phase-out" = "#E31A1C",
  "TPCC"    = "#A65628",
  "HCC_2110"     = "#377EB8",
  "HCC_2150"     = "#A6CEE3",
  "MCC_2110"     = "#FF7F00",
  "MCC_2120"     = "#FDBF6F",
  "MCC_2130"     = "#B2DF8A",
  "MCC_2140"     = "#33A02C",
  "MCC_2150"     = "#1F6B39",
  "LCC_2110"     = "#6A3D9A",
  "LCC_2150"     = "#CAB2D6"
)

year_cols <- as.character(seq(2010, 2150, by = 5))
raw       <- read.csv(csv_path, check.names = FALSE)

# --- Panel (a): time series of total secondary electricity ---
df_a <- raw |>
  filter(REGION == "World", VARIABLE == "Secondary Energy|Electricity") |>
  inner_join(scenario_map, by = "SCENARIO") |>
  select(label, VARIABLE, all_of(year_cols)) |>
  pivot_longer(all_of(year_cols), names_to = "year", values_to = "value") |>
  mutate(year = as.integer(year), value = as.numeric(value),
         label = factor(label, levels = scenario_order)) |>
  filter(!is.na(value))

# --- Shared DAC electricity data (panels b and c) ---
df_dac <- raw |>
  filter(REGION == "World",
         VARIABLE %in% c("Final Energy|Carbon Management|Direct Air Capture|Electricity",
                         "Secondary Energy|Electricity")) |>
  inner_join(scenario_map, by = "SCENARIO") |>
  select(label, VARIABLE, all_of(year_cols)) |>
  pivot_longer(all_of(year_cols), names_to = "year", values_to = "value") |>
  mutate(year = as.integer(year), value = as.numeric(value),
         label = factor(label, levels = scenario_order)) |>
  filter(!is.na(value)) |>
  pivot_wider(names_from = VARIABLE, values_from = value) |>
  rename(dac_ele = `Final Energy|Carbon Management|Direct Air Capture|Electricity`,
         tot_ele = `Secondary Energy|Electricity`) |>
  mutate(pct = ifelse(tot_ele > 0, dac_ele / tot_ele * 100, NA)) |>
  filter(!is.na(dac_ele))

# Scale factor aligns the two y-axes at their respective maxima
sf <- max(df_dac$pct, na.rm = TRUE) / max(df_dac$dac_ele, na.rm = TRUE)

# --- Panel (c) bar data: peak DACCS share year per scenario ---
# --- Panel (c) bar data: peak DACCS share year per scenario ---
df_peak <- df_dac |>
  group_by(label) |>
  slice_max(pct, n = 1, with_ties = FALSE) |>
  ungroup() |>
  rename(peak_year = year) |>
  mutate(other_ele = tot_ele - dac_ele,
         label     = factor(label, levels = scenario_order))

df_bar <- df_peak |>
  select(label, peak_year, pct, dac_ele, other_ele) |>
  pivot_longer(c(other_ele, dac_ele), names_to = "type", values_to = "value") |>
  mutate(
    type      = factor(type, levels = c("other_ele", "dac_ele")),
    ann_label = if_else(type == "dac_ele", as.character(peak_year), NA_character_)
  )

# Secondary-axis scaling: map DACCS share (%) onto the EJ axis for the dots
bar_max <- max(df_peak$dac_ele + df_peak$other_ele, na.rm = TRUE)
pct_top <- 20
sf2     <- bar_max / pct_top

# --- Theme ---
theme_clean <- function(...) {
  theme_bw(base_size = 15) +
    theme(plot.title       = element_text(size = 15, face = "bold"),
          axis.title.y     = element_text(size = 13),
          axis.text.x      = element_text(angle = 45, hjust = 1, size = 11),
          axis.text.y      = element_text(size = 11),
          panel.grid.minor = element_blank(),
          legend.position  = "none", ...)
}

x_scale <- scale_x_continuous(breaks = seq(2010, 2150, 10))

# --- Panel (a) ---
p_a <- ggplot(df_a, aes(x = year, y = value, color = label)) +
  geom_line(linewidth = 1.0) +
  scale_color_manual(values = pal, breaks = scenario_order, name = NULL) +
  x_scale +
  scale_y_continuous(labels = scales::comma_format(accuracy = 1)) +
  labs(title = "(a) Total Electricity", x = NULL, y = "EJ/yr") +
  theme_clean()

# --- Panel (b): DAC electricity input, dual y-axis ---
p_b <- ggplot(df_dac, aes(x = year, color = label)) +
  geom_line(aes(y = dac_ele), linewidth = 1.0) +
  scale_color_manual(values = pal, breaks = scenario_order, name = NULL) +
  x_scale +
  scale_y_continuous(
    name     = "EJ/yr",
    labels   = scales::comma_format(accuracy = 1),
    sec.axis = sec_axis(
      transform = ~ . * sf,
      name      = "% of total electricity",
      labels    = scales::number_format(accuracy = 0.1, suffix = "%")
    )
  ) +
  labs(title = "(b) Electricity input to DAC", x = NULL) +
  theme_clean(axis.title.y.right = element_text(size = 13))

# --- Panel (c): stacked bar at peak DACCS share year (per scenario) ---
p_c <- ggplot(df_bar, aes(x = label, y = value, fill = type)) +
  geom_bar(stat = "identity", width = 0.7) +
  # the label for LCC (6 %); the white share dots all sit below the top of the bars)
  geom_text(data = df_peak, aes(x = label, y = dac_ele + other_ele - bar_max * 0.015, label = peak_year),
            colour = "grey15", size = 3.6, fontface = "bold", angle = 90, hjust = 1, vjust = 0.5, inherit.aes = FALSE) +
  geom_point(data = df_peak, aes(x = label, y = pct * sf2),
             shape = 21, size = 3.4, colour = "black", fill = "white",
             stroke = 0.8, inherit.aes = FALSE) +
  scale_fill_manual(
    values = c("other_ele" = "grey70", "dac_ele" = "#E31A1C"),
    labels = c("other_ele" = "Other", "dac_ele" = "DACCS"),
    name   = NULL
  ) +
  scale_y_continuous(
    name     = "EJ/yr",
    labels   = scales::comma_format(accuracy = 1),
    sec.axis = sec_axis(~ . / sf2, name = "% of total electricity",
                        breaks = seq(0, pct_top, 5),
                        labels = scales::number_format(accuracy = 1, suffix = "%"))
  ) +
  labs(title = "(c)  Electricity at peak DACCS share", x = NULL) +
  theme_clean() +
  theme(
    legend.position.inside = c(0.8, 0.9),
    legend.position        = "inside",
    legend.background      = element_rect(fill = "white", colour = NA),
    legend.key.size        = unit(0.7, "lines"),
    legend.text            = element_text(size = 11),
    axis.title.y.right     = element_text(size = 13)
  )

# --- Scenario legend (for panels a & b) ---
leg <- get_legend(
  p_a + theme(legend.position = "right",
              legend.key.size = unit(0.5, "lines"),
              legend.text     = element_text(size = 9))
)

# --- Assemble: (a) | (b) | (c) | legend ---
final_fig <- plot_grid(
  plot_grid(p_a, p_b, p_c, ncol = 3, align = "hv", axis = "tb"),
  ncol = 2, rel_widths = c(0.5, 0)
)
print(final_fig)

ggsave(out_path, final_fig, width = 16, height = 3.96, dpi = 200)
cat("Saved:", out_path, "\n")

