library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(patchwork)
update_geom_defaults("text", list(family = "Helvetica"))
# biomass, DAC, fossil, industry); Carbon Sequestration|Direct Air Capture -> Carbon Capture|Geological Storage|Direct Air Capture.
# Panels: b total CDR line; c all CO2 stored underground; d its DACCS part (c and d share the y axis); a is the composition grid.

# --- Data source (unchanged) ---
csv_path <- "data/ccs_iamc.csv"

scenario_map <- tribble(
  ~SCENARIO,                   ~label,
  "SSP2_400C_2025CP_NoCC_No",  "No Phase-out",
  "SSP2_TPCC_Ext_NoCC_No",     "TPCC",
  "SSP2_Med_2110_NoCC_No",     "MCC_2110",
  "SSP2_Med_2120_NoCC_No",     "MCC_2120",
  "SSP2_Med_2130_NoCC_No",     "MCC_2130",
  "SSP2_Med_2140_NoCC_No",     "MCC_2140",
  "SSP2_Med_2150_NoCC_No",     "MCC_2150",
  "SSP2_LCC_2110_NoCC_No",     "LCC_2110",
  "SSP2_LCC_2150_NoCC_No",     "LCC_2150",
  "SSP2_HCC_2110_NoCC_No",     "HCC_2110",
  "SSP2_HCC_2150_NoCC_No",     "HCC_2150"
)

scenario_colors <- c(
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

scenario_order <- c("No Phase-out", "TPCC",
                    "HCC_2110", "HCC_2150",
                    "MCC_2110", "MCC_2120", "MCC_2130", "MCC_2140", "MCC_2150",
                    "LCC_2110", "LCC_2150")

# Legend: single column on the right
leg_order  <- scenario_order
leg_labels <- scenario_order

vars_needed <- c(
  "Carbon Removal",
  "Carbon Capture|Geological Storage",
  "Carbon Capture|Geological Storage|Direct Air Capture"
)

box_titles <- c(
  "Carbon Removal"                          = "",
  "Carbon Capture|Geological Storage"                     = "",
  "Carbon Capture|Geological Storage|Direct Air Capture" = ""
)

# --- Read and reshape (unchanged) ---
raw <- read.csv(csv_path, check.names = FALSE) %>%
  rename(Variable = VARIABLE) %>%
  filter(REGION   == "World",
         Variable %in% vars_needed,
         SCENARIO %in% scenario_map$SCENARIO) %>%
  pivot_longer(cols = matches("^\\d{4}$"),
               names_to = "Year", values_to = "Value_Mt") %>%
  mutate(Year     = as.integer(Year),
         Value_Gt = as.numeric(Value_Mt) / 1000) %>%
  filter(Year >= 2025, Year <= 2150) %>%
  left_join(scenario_map, by = "SCENARIO") %>%
  mutate(label = factor(label, levels = scenario_order))

# ── Row 1: CDR line plot only (full width) ────────────────────────────────────
d_line    <- raw %>% filter(Variable == "Carbon Removal", !is.na(Value_Gt))
y_max_line <- ceiling(max(d_line$Value_Gt, na.rm = TRUE) / 5) * 5

p_line <- ggplot(d_line, aes(x = Year, y = Value_Gt, color = label, group = label)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(values = scenario_colors, name = NULL, guide = "none") +
  scale_x_continuous(breaks = seq(2030, 2150, 10)) +
  scale_y_continuous(limits = c(0, y_max_line), labels = comma,
                     expand = expansion(mult = c(0, 0.02))) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.4) +
  labs(title = "",
       x = NULL,
       y = expression("GtCO"[2]*"/yr")) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid       = element_blank(),
    panel.border     = element_blank(),
    axis.line.x      = element_line(color = "black"),
    axis.line.y      = element_line(color = "black"),
    axis.ticks       = element_line(color = "black"),
    axis.text.x      = element_text(angle = 45, hjust = 1, size = 9),
    panel.background = element_rect(fill = "white", color = NA),
    plot.title       = element_text(face = "bold", size = 11),
    legend.position  = "none"   # collected at bottom by patchwork
  )

# ── Shared y limits for b-d (same scale, zoomed to data range) ───────────────
all_box <- raw %>%
  filter(Variable %in% vars_needed[-1], !is.na(Value_Gt)) %>%   # storage only: the cumulative CDR panel was removed
  arrange(label, Variable, Year) %>%
  group_by(label, Variable) %>%
  summarise(Value_Gt = sum((Value_Gt + dplyr::lead(Value_Gt, default = dplyr::last(Value_Gt)))/2 * c(diff(Year), 0), na.rm = TRUE), .groups = "drop")

box_padding <- 0.05 * diff(range(all_box$Value_Gt, na.rm = TRUE))
shared_y_min <- floor((min(all_box$Value_Gt, na.rm = TRUE) - box_padding) / 50) * 50
shared_y_max <- ceiling((max(all_box$Value_Gt, na.rm = TRUE) + box_padding) / 50) * 50

# ── Row 2: boxplot factory ────────────────────────────────────────────────────
make_box <- function(var, show_legend = FALSE, show_y = TRUE) {
  d_box <- all_box %>%
    filter(Variable == var) %>%
    mutate(label = factor(label, levels = leg_order))

  leg_guide <- if (show_legend)
    guide_legend(ncol = 1, override.aes = list(size = 4))
  else "none"

  ggplot(d_box, aes(x = factor("Cumul.\n2025 to 2150"), y = Value_Gt)) +
    geom_boxplot(fill = "grey80", color = "black", outlier.shape = NA,
                 width = 0.45, alpha = 0.7) +
    geom_jitter(aes(color = label), width = 0.12, size = 3) +
    scale_color_manual(values = scenario_colors, name = NULL,
                       breaks = leg_order, labels = leg_labels, drop = FALSE,
                       guide = leg_guide) +
    scale_y_continuous(limits = c(shared_y_min, shared_y_max), labels = comma,
                       expand = expansion(mult = c(0, 0.02))) +
    labs(title = box_titles[var],
         x = NULL,
         y = expression("GtCO"[2])) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid       = element_blank(),
      panel.border     = element_blank(),
      axis.line.x      = element_line(color = "black"),
      axis.line.y      = element_line(color = "black"),
      axis.ticks.x     = element_blank(),
      axis.text.x      = element_text(size = 10, face = "bold", hjust = 0.5),
      axis.text.y      = element_text(size = 9),
      axis.ticks.y     = element_line(color = "black"),
      panel.background = element_rect(fill = "white", color = NA),
      plot.title       = element_text(face = "bold", size = 11),
      legend.position  = "none"
    ) +
    if (!show_y) theme(axis.text.y = element_blank(), axis.title.y = element_blank(),
                       axis.ticks.y = element_blank()) else theme()
}

# c = all CO2 stored underground, d = its DACCS part; one y scale, labels on c only
p_box_c <- make_box("Carbon Capture|Geological Storage", show_legend = TRUE)
p_box_d <- make_box("Carbon Capture|Geological Storage|Direct Air Capture", show_y = FALSE)

# ── Assemble: line (b) and the two storage boxplots (c, d) in one row ────────
final <- (p_line | p_box_c | p_box_d) +
  plot_layout(widths = c(3.2, 1, 1), guides = "collect") +
  plot_annotation(tag_levels = list(c("b", "c", "d"))) &
  theme(legend.position = "right",
        legend.key.width = unit(0.5, "cm"),
        text = element_text(family = "Helvetica"),
        plot.tag = element_text(face = "bold", size = 20, family = "Helvetica"))

out <- "output/panels/CDR_CCS_revised_fresh.png"
ggsave(out, plot = final, width = 14, height = 5, dpi = 300, bg = "white", device = ragg::agg_png)
cat("Saved:", out, "\n")
print(final)

