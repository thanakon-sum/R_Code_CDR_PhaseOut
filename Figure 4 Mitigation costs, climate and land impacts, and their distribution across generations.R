# ============================================================================
# FIGURE 4 MITIGATION COSTS, CLIMATE AND LAND IMPACTS, AND THEIR DISTRIBUTION ACROSS GENERATIONS
# ============================================================================
# (a) Consumption loss relative to the baseline. (b, c) Cumulative discounted consumption loss. (d) Change in world GDP
# due to climate change. (e) Population at risk of hunger and (f) food price index. (g, h) Consumption loss discounted
# with the Ramsey rule and its relative difference between generations.
# Data: data/fig4_welfare_annual.csv, data/ramsey_liu_weights.csv, data/burke_world_11.csv, data/takakura_world_11.csv,
# data/heatmap_iamc_11.csv
# Output: output/Figure_4.png
# Run: Rscript "Figure 4 Mitigation costs, climate and land impacts, and their distribution across generations.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all four.
# ============================================================================
# ---- run from this folder (Rscript, source() or RStudio) and install missing packages ----
if (!l10n_info()[["UTF-8"]]) for (l in c("C.UTF-8", "en_US.UTF-8", "English_United States.utf8"))  # the scripts hold UTF-8 text
  if (nzchar(suppressWarnings(Sys.setlocale("LC_CTYPE", l)))) break
local({
  f <- sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
  if (!length(f)) f <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
  if (length(f) && nzchar(f[1])) setwd(dirname(normalizePath(gsub("~+~", " ", f[1], fixed = TRUE))))
})
source("R/setup.R")
library(dplyr); library(tidyr); library(ggplot2); library(patchwork)
update_geom_defaults("text", list(family = "Helvetica"))
source("R/style.R")

lab <- c("SSP2_400C_2025CP_NoCC_No" = "No Phase-out", "SSP2_TPCC_Ext_NoCC_No" = "TPCC",
         "SSP2_HCC_2110_NoCC_No" = "HCC_2110", "SSP2_HCC_2150_NoCC_No" = "HCC_2150",
         "SSP2_Med_2110_NoCC_No" = "MCC_2110", "SSP2_Med_2120_NoCC_No" = "MCC_2120",
         "SSP2_Med_2130_NoCC_No" = "MCC_2130", "SSP2_Med_2140_NoCC_No" = "MCC_2140",
         "SSP2_Med_2150_NoCC_No" = "MCC_2150", "SSP2_LCC_2110_NoCC_No" = "LCC_2110",
         "SSP2_LCC_2150_NoCC_No" = "LCC_2150")
ord <- unname(lab)
lab_b <- c(lab, "SSP2_BaU_NoCC_No" = "BaU"); ord_b <- c(ord, "BaU")   # BaU: panels d-g only
pal_b <- c(mck_scen, BaU = "#8C96A3")
d <- read.csv("data/fig4_welfare_annual.csv") %>%
  mutate(label = factor(lab[SCENARIO], levels = ord))
base <- theme_minimal(base_family = "Helvetica") +
  theme(panel.grid = element_blank(), axis.line = element_line(colour = "black", linewidth = 0.3),
        axis.ticks = element_line(colour = "black", linewidth = 0.3)) + th

# a: annual consumption loss rate
rate <- d %>% filter(variable == "Pol_Cos_Cns_Los_rat", year >= 2030, year <= 2150)
# generation boundaries (same split as c): Gen 1 2030-2069, Gen 2 2070-2109, Gen 3 2110-2150
ytop <- ceiling(max(rate$value) / 3) * 3
gens <- data.frame(x = c(2049.5, 2089.5, 2130), lab = c("Gen 1", "Gen 2", "Gen 3"))
pa <- ggplot(rate, aes(year, value, colour = label)) +
  geom_vline(xintercept = c(2069.5, 2109.5), linetype = "dashed", colour = "grey55", linewidth = 0.3) +
  # labels along the bottom: every curve stays above ~1.2 % in these spans, the top is crossed by HCC_2150
  geom_text(data = gens, aes(x, 0.25, label = lab), inherit.aes = FALSE, vjust = 0, size = 2.1,
            colour = "grey35", family = "Helvetica") +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(values = mck_scen, name = NULL) +
  scale_x_continuous(breaks = seq(2030, 2150, 10)) +
  scale_y_continuous(limits = c(0, ytop), breaks = seq(0, 12, 3),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Consumption loss rate (%)") + base +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")

# b: cumulative NPV of the loss rate at 5%, read at the generation boundaries of a and c
npv <- d %>% filter(variable == "Pol_Cos_Cns_Los_rat_NPV_5pc", year %in% c(2030, 2070, 2110, 2150)) %>%
  mutate(year = factor(year))
pb <- ggplot(npv, aes(year, value, colour = label, group = label)) +
  geom_line(linewidth = 0.3, alpha = 0.5) + geom_point(size = 1.4) +
  # one-point line draws nothing but gives BaU (shown in d-g only) a legend key
  geom_line(data = data.frame(year = factor("2030", levels = levels(npv$year)), value = npv$value[1],
                              label = factor("BaU", levels = ord_b)), linewidth = 0.3) +
  scale_colour_manual(values = pal_b, limits = ord_b, name = NULL) +
  scale_y_continuous(limits = c(0, ceiling(max(npv$value) / 2) * 2), expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Cumulative NPV\n(5% DCR, %)") + base + theme(legend.position = "none")

disc <- d %>% filter(grepl("_NPV_[135]pc$", variable), year == 2150, label != "BaU") %>%
  mutate(rate = as.integer(sub(".*_NPV_([135])pc$", "\\1", variable)))
pr <- ggplot(disc, aes(rate, value, colour = label, group = label)) +
  geom_line(linewidth = 0.3) + geom_point(size = 1.4) +
  geom_line(data = data.frame(rate = 1, value = disc$value[1], label = factor("BaU", levels = ord_b)), linewidth = 0.3) +
  scale_colour_manual(values = pal_b, limits = ord_b, name = NULL) +
  scale_x_continuous(breaks = c(1, 3, 5), labels = c("1%", "3%", "5%"), expand = expansion(mult = 0.08)) +
  scale_y_continuous(limits = c(0, ceiling(max(disc$value))), expand = expansion(mult = c(0, 0.02))) +
  labs(x = "Discount rate", y = "Cumulative NPV\nto 2150 (%)") + base +
  guides(colour = guide_legend(ncol = 1, override.aes = list(
    size = c(rep(1.6, 11), 0), shape = c(rep(16, 11), NA), linewidth = 0.6,
    linetype = c(rep("solid", 11), "22"), alpha = 1)))

# person (eta = 2, no pure time preference), applied to the ABSOLUTE consumption loss (loss rate x BaU consumption).
lw <- read.csv("data/ramsey_liu_weights.csv") %>% filter(arm == "tf") %>% select(year, mult)
gen <- rate %>% left_join(lw, by = "year") %>%
  mutate(w = value * mult, Generation = case_when(year <= 2069 ~ "Gen1", year <= 2109 ~ "Gen2", TRUE ~ "Gen3")) %>%
  group_by(label, Generation) %>% summarise(NPV = sum(w), .groups = "drop") %>%
  pivot_wider(names_from = Generation, values_from = NPV) %>%
  transmute(label,
            `Gen1 vs Gen2 (2030-2069 vs 2070-2109)` = (Gen1 - Gen2) / abs(Gen2) * 100,
            `Gen2 vs Gen3 (2070-2109 vs 2110-2150)` = (Gen2 - Gen3) / abs(Gen3) * 100) %>%
  pivot_longer(-label, names_to = "Comparison", values_to = "RelDiff")
# h (left of the bars): the discounted consumption loss that the bars sum by generation (US$ trillion, 2025 value)
wts <- rate %>% left_join(lw, by = "year") %>% mutate(w = value * mult / 1000)
pw <- ggplot(wts, aes(year, w, colour = label)) +
  geom_vline(xintercept = c(2069.5, 2109.5), linetype = "dashed", colour = "grey55", linewidth = 0.3) +
  geom_text(data = gens, aes(x, Inf, label = lab), inherit.aes = FALSE, vjust = 1.3, size = 2.1,
            colour = "grey35", family = "Helvetica") +
  geom_line(linewidth = 0.5) +
  scale_colour_manual(values = mck_scen, name = NULL) +
  scale_x_continuous(breaks = seq(2030, 2150, 20)) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.12))) +
  labs(x = NULL, y = "Discounted consumption loss\n(US$ trillion yr⁻¹)") + base +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "none")
cat("Ramsey relative difference (%), Fresh:\n"); print(as.data.frame(pivot_wider(gen, names_from = Comparison, values_from = RelDiff)), digits = 3)
pc <- ggplot(gen, aes(label, RelDiff, fill = Comparison)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.72) +
  geom_hline(yintercept = 0, colour = "black", linewidth = 0.3) +
  scale_fill_manual(values = c("#051C2C", "#00A9F4"), name = NULL) +
  scale_y_continuous(labels = function(x) paste0(round(x), "%"), expand = expansion(mult = c(0.05, 0.05))) +
  labs(x = NULL, y = "Relative difference (%)") + base +
  theme(axis.text.x = element_text(angle = 40, hjust = 1),
        legend.position = "top", legend.direction = "horizontal",  # above the panel: clear of the tallest bar
        legend.margin = margin(0, 0, -4, 0))

# F-gas infilling matched on 2015-2100): Burke et al. GDP change due to climate change (%, negative = loss, yearly);
# Takakura et al. economic impact (% of GDP, positive = loss, five-year steps)
imp_x <- scale_x_continuous(breaks = c(2050, 2100, 2150), limits = c(2015, 2150))  # four panels across: fewer ticks
bk <- read.csv("data/burke_world_11.csv") %>%
  mutate(label = factor(lab_b[scenario], levels = ord_b), value = impact * 100) %>% filter(year >= 2015)
tk <- read.csv("data/takakura_world_11.csv") %>%
  mutate(label = factor(lab_b[scenario], levels = ord_b), value = wld_total_pct) %>% filter(year >= 2015)
# d-g: the 11 scenarios plus BaU (grey, dashed), drawn first so the scenarios sit on top
ts_panel <- function(df, ylab, zero = FALSE) {
  ggplot(df, aes(year, value, colour = label, linetype = label == "BaU")) +
    (if (zero) geom_hline(yintercept = 0, colour = "black", linewidth = 0.3)) +
    geom_line(data = filter(df, label == "BaU"), linewidth = 0.5) +
    geom_line(data = filter(df, label != "BaU"), linewidth = 0.5) +
    scale_colour_manual(values = pal_b, name = NULL) +
    scale_linetype_manual(values = c(`FALSE` = "solid", `TRUE` = "22"), guide = "none") + imp_x +
    labs(x = NULL, y = ylab) + base + theme(legend.position = "none")
}
pd <- ts_panel(bk, "GDP change due to\nclimate change (%)", zero = TRUE)
pe <- ts_panel(tk, "Economic impact of climate\nchange (% of GDP)", zero = TRUE)
# population at risk of hunger (million) and the food price index (2020 = 1)
land <- read.csv("data/heatmap_iamc_11.csv") %>%
  mutate(label = factor(lab_b[scenario], levels = ord_b)) %>% filter(!is.na(label), year >= 2015)
pf <- ts_panel(filter(land, code == "Pop_Ris_of_Hun"), "Population at risk of\nhunger (million)")
pg <- ts_panel(filter(land, code == "Prc_Agr_Foo_Pro_Ind"), "Food price index\n(2020 = 1)")
cat("Burke 2150 (%):\n"); print(bk %>% filter(year == 2150) %>% select(label, value) %>% arrange(value), digits = 3)
cat("Takakura peak (% of GDP):\n"); print(tk %>% group_by(label) %>% summarise(peak = max(value), yr = year[which.max(value)]), digits = 3)

# row 1 freed on the right: its legend fits inside the page width instead of pushing rows c-e in; a narrower
fig <- free((pa | pb | pr) + plot_layout(widths = c(2.1, 1, 1)), side = "r") / (pd | pf | pg) /
  ((pw | pc) + plot_layout(widths = c(1, 1.7))) +
  plot_layout(heights = c(1, 0.85, 0.9)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 10, face = "bold", family = "Helvetica"))
ggsave("output/Figure_4.png", fig, width = 19.05 / 2.54, height = 13.76 / 2.54, dpi = 600,
       bg = "white", device = ragg::agg_png)
print(fig)
cat("Saved output/Figure_4.png\n")
