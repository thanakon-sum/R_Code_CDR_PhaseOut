library(ggplot2)

# thinner lines and smaller points and labels, scaled for a 19 cm wide page (layers are shared objects)
shrink <- function(p, k_line = 0.55, k_size = 0.6) {
  for (L in p$layers) {
    if (!is.null(L$aes_params$linewidth)) L$aes_params$linewidth <- L$aes_params$linewidth * k_line
    if (!is.null(L$aes_params$size))      L$aes_params$size      <- L$aes_params$size * k_size
  }
  p
}
th <- theme(text = element_text(family = "Helvetica", size = 7),
            axis.title = element_text(size = 7), axis.title.y.right = element_text(size = 7),
            axis.text.x = element_text(size = 5.5), axis.text.y = element_text(size = 5.5),
            axis.text.y.right = element_text(size = 5.5),
            plot.title = element_text(size = 6.5, face = "bold", hjust = 0.5, margin = margin(0, 0, 1, 0)),
            legend.text = element_text(size = 6), legend.key.size = unit(0.28, "cm"),
            plot.tag = element_text(size = 10, face = "bold", family = "Helvetica"),
            plot.margin = margin(2, 3, 2, 3))

# scenarios: one hue per family, darker = earlier deadline
mck_scen <- c("No Phase-out" = "#051C2C", "TPCC" = "#E5546C",
              "HCC_2110" = "#2251FF", "HCC_2150" = "#8FA6FF",
              "MCC_2110" = "#00796B", "MCC_2120" = "#00897B", "MCC_2130" = "#26A69A",
              "MCC_2140" = "#4DB6AC", "MCC_2150" = "#80CBC4",
              "LCC_2110" = "#D98200", "LCC_2150" = "#F5C04A")
mck_cdr <- c("Afforestation" = "#1F9E89", "Biochar" = "#F2B134", "BECCS" = "#7B1FA2",
             "DACCS" = "#F48FB1", "Enhanced Weathering" = "#00A9F4", "Soil Carbon Seq." = "#051C2C")
# power technologies and DACCS (Fig. 3, Fig. S7), keyed by model code; fossil in slate greys. Non-fossil hues are kept away from
# colour and 25 from the greys, and at least 26 from each other (old palette: DACCS = TPCC red, BECCS = HCC_2110 blue)
# darker purple and nuclear the softer one ("darker purple for BECCS and nuclear can be softer")
mck_power <- c(DACCS = "#F48FB1", E_BIN = "#7B1FA2", E_GEO = "#C8A27A", E_WIN = "#C0CA33", E_WIF = "#827717", E_SPV = "#BF360C",
               E_BIO = "#33691E", E_NUC = "#CE93D8", E_HYD = "#6D4C41",
               E_GAS = "#C9CED6", E_OIL = "#8C96A3", E_COL = "#4A5563")
mck_light <- c("DACCS", "E_NUC", "E_GEO", "E_WIN", "E_GAS")  # fills light enough to need dark label text
