# (Carbon Removal|Geological Storage|Biomass), so it is no longer derived as a residual; the six parts add up
library(dplyr); library(tidyr); library(ggplot2); library(scales); library(patchwork)
update_geom_defaults("text", list(family = "Helvetica"))

scen <- c("SSP2_400C_2025CP_NoCC_No" = "No Phase-out", "SSP2_TPCC_Ext_NoCC_No" = "TPCC",
          "SSP2_HCC_2110_NoCC_No" = "HCC_2110", "SSP2_HCC_2150_NoCC_No" = "HCC_2150",
          "SSP2_Med_2110_NoCC_No" = "MCC_2110", "SSP2_Med_2120_NoCC_No" = "MCC_2120",
          "SSP2_Med_2130_NoCC_No" = "MCC_2130", "SSP2_Med_2140_NoCC_No" = "MCC_2140",
          "SSP2_Med_2150_NoCC_No" = "MCC_2150", "SSP2_LCC_2110_NoCC_No" = "LCC_2110",
          "SSP2_LCC_2150_NoCC_No" = "LCC_2150")
tech <- c("Carbon Removal|Land Use|Re/Afforestation"             = "Afforestation",
          "Carbon Removal|Land Use|Biochar"                      = "Biochar",
          "Carbon Removal|Geological Storage|Biomass"            = "BECCS",
          "Carbon Removal|Geological Storage|Direct Air Capture" = "DACCS",
          "Carbon Removal|Enhanced Weathering"                   = "Enhanced Weathering",
          "Carbon Removal|Land Use|Soil Carbon Management"       = "Soil Carbon Seq.")
cdr_colors <- c("Afforestation" = "#7F8C8D", "Biochar" = "#F1C40F", "BECCS" = "#3498DB",
                "DACCS" = "#E74C3C", "Enhanced Weathering" = "#1ABC9C", "Soil Carbon Seq." = "#8E44AD")

raw <- read.csv("data/cdr_iamc.csv", check.names = FALSE) %>%
  filter(SCENARIO %in% names(scen)) %>%
  pivot_longer(matches("^\\d{4}$"), names_to = "Year", values_to = "Mt") %>%
  mutate(Year = as.integer(Year), Gt = as.numeric(Mt) / 1000, label = scen[SCENARIO]) %>%
  filter(Year >= 2025, Year <= 2150)
plot_data <- raw %>% filter(VARIABLE %in% names(tech)) %>%
  mutate(Variable = factor(tech[VARIABLE], levels = names(cdr_colors)))
total_line <- raw %>% filter(VARIABLE == "Carbon Removal") %>% select(label, Year, Total_Gt = Gt)
y_max <- ceiling(max(total_line$Total_Gt, na.rm = TRUE) / 5) * 5

make_panel <- function(sc_label, show_y = TRUE) {
  p <- ggplot() +
    geom_area(data = filter(plot_data, label == sc_label), aes(Year, Gt, fill = Variable), alpha = 0.9) +
    geom_line(data = filter(total_line, label == sc_label), aes(Year, Total_Gt),
              colour = "black", linetype = "dashed", linewidth = 0.8) +
    scale_fill_manual(values = cdr_colors, drop = FALSE, name = NULL) +
    scale_x_continuous(breaks = seq(2025, 2150, 25)) +
    scale_y_continuous(limits = c(0, y_max), labels = comma, expand = expansion(mult = c(0, 0.02))) +
    labs(title = sc_label, x = NULL, y = if (show_y) expression("GtCO"[2]*" yr"^{-1}) else NULL) +
    theme_minimal(base_size = 13, base_family = "Helvetica") +
    theme(panel.grid = element_blank(), panel.border = element_blank(),
          axis.line = element_line(colour = "black"), axis.ticks = element_line(colour = "black"),
          axis.text.x = element_text(angle = 90, size = 9.5, hjust = 1, vjust = 0.5),
          axis.text.y = element_text(size = 9.5),
          panel.background = element_rect(fill = "white", colour = NA),
          plot.title = element_text(face = "bold", size = 12, hjust = 0.5))
  if (!show_y) p <- p + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
  p
}

ord <- unname(scen)
panels <- lapply(seq_along(ord), function(i) make_panel(ord[i], show_y = (i - 1) %% 3 == 0))
final <- wrap_plots(c(panels, list(guide_area())), ncol = 3) +
  plot_layout(guides = "collect") +
  plot_annotation(title = "CDR composition across levels of CDR reliance and phase-out years",
                  theme = theme(plot.title = element_text(face = "bold", size = 13, hjust = 0, family = "Helvetica"))) &
  theme(legend.text = element_text(size = 11, family = "Helvetica"), legend.key.size = unit(0.5, "cm"))

ggsave("output/panels/DACCS_CDR_composition_grid_fresh.png", final, width = 12, height = 9.5, dpi = 300,
       bg = "white", device = ragg::agg_png)
print(final)
cat("Saved output/panels/DACCS_CDR_composition_grid_fresh.png\n")
