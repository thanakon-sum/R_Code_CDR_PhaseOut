# ============================================================================
# FIGURE 5 TRADE-OFFS ACROSS THE SCENARIOS THAT PHASE OUT CDR
# ============================================================================
# Each row is one indicator; colour ranks the scenarios from the least to the most challenge; (q) overall challenge
# score from the rows that differ by at least 10%. Rscript "Figure 5 ....R" si_long draws Fig. S19 (with No Phase-out).
# Data: data/sav_<scenario>.csv, data/fig4_welfare_annual.csv, data/ramsey_liu_weights.csv, data/heatmap_iamc_11.csv,
# data/burke_world_11.csv, data/gsat_p50.csv, data/si_iamc.csv (trimmed to the energy-crop land rows)
# Output: output/Figure_5.png
# Run: Rscript "Figure 5 Trade-offs across the scenarios that phase out CDR.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all four.
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
library(ggplot2); library(dplyr); library(tidyr); library(patchwork)
update_geom_defaults("text", list(family = "Helvetica"))
D <- "data"
lab <- c("TPCC_Ext" = "TPCC", "HCC_2110" = "HCC_2110", "HCC_2150" = "HCC_2150",
         "Med_2110" = "MCC_2110", "Med_2120" = "MCC_2120", "Med_2130" = "MCC_2130", "Med_2140" = "MCC_2140",
         "Med_2150" = "MCC_2150", "LCC_2110" = "LCC_2110", "LCC_2150" = "LCC_2150")
MODE <- commandArgs(TRUE)[1]; SI <- MODE %in% c("si", "si_long"); LONGROWS <- identical(MODE, "si_long")
if (SI) lab <- c("400C_2025CP" = "No Phase-out", lab)
OUT_PNG <- if (LONGROWS) "output/panels/si_fig5_with_nophaseout_longterm_fresh.png" else if (SI) "output/panels/si_fig5_with_nophaseout_fresh.png" else "output/Figure_5.png"
OUT_CSV <- if (LONGROWS) "output/fig5_with_nophaseout_longterm_si.csv" else if (SI) "output/fig5_with_nophaseout_si.csv" else "output/fig5_summary_heatmap_fresh.csv"
ord <- unname(lab)
short <- function(x) sub("_NoCC_No$", "", sub("^SSP2_", "", x))

# stranded
strd <- bind_rows(lapply(names(lab), function(s)
  read.csv(file.path(D, paste0("sav_", s, ".csv"))) %>% setNames(c("Y", "R", "A", "V")) %>%
    filter(A %in% c("DAC", "STO")) %>% group_by(Y) %>% summarise(V = sum(V), .groups = "drop") %>% mutate(s = s))) %>%
  group_by(s) %>% summarise(`Peak annual stranded value (B$/yr)` = max(V), `Cumulative stranded (B$)` = sum(V))

# welfare and inequity (annual)
w <- read.csv(file.path(D, "fig4_welfare_annual.csv")) %>% mutate(s = short(SCENARIO))
rate <- w %>% filter(variable == "Pol_Cos_Cns_Los_rat", year >= 2030, year <= 2150)
lw <- read.csv(file.path(D, "ramsey_liu_weights.csv")) %>% filter(arm == "tf") %>% select(year, mult)
ineq <- rate %>% left_join(lw, by = "year") %>%
  mutate(v = value * mult, G = case_when(year <= 2069 ~ "G1", year <= 2109 ~ "G2", TRUE ~ "G3")) %>%
  group_by(s, G) %>% summarise(v = sum(v), .groups = "drop") %>% pivot_wider(names_from = G, values_from = v) %>%
  transmute(s, `Gen 1 vs Gen 2 inequity (%)` = (G1 - G2) / abs(G2) * 100,
            `Gen 2 vs Gen 3 inequity (%)` = (G2 - G3) / abs(G3) * 100)
welf <- rate %>% group_by(s) %>% summarise(`Peak consumption loss rate (%)` = max(value)) %>%
  left_join(w %>% filter(variable == "Pol_Cos_Cns_Los_rat_NPV_5pc", year == 2150) %>%
              transmute(s, `Consumption loss NPV 5%, 2025-2150 (%)` = value), by = "s")

# energy, CCS, land (5-year IAMC); trap = trapezoid sum over 2025-2150 (same rule as Fig. 2c-d)
trap <- function(y, v) { o <- order(y); y <- y[o]; v <- v[o]; k <- y >= 2025 & y <= 2150 & !is.na(v)
  y <- y[k]; v <- v[k]; sum(diff(y) * (head(v, -1) + tail(v, -1)) / 2) }
iamc <- read.csv(file.path(D, "heatmap_iamc_11.csv")) %>% mutate(s = short(scenario)) %>%
  select(s, code, year, value) %>% pivot_wider(names_from = code, values_from = value)
ecl <- iamc %>% group_by(s) %>%
  summarise(`Peak DACCS electricity share (%)` = max(100 * Fin_Ene_Car_Man_Dir_Air_Cap_Ele / Sec_Ene_Ele, na.rm = TRUE),
            `Average DACCS electricity, 2025-2150 (EJ/yr)` = trap(year, Fin_Ene_Car_Man_Dir_Air_Cap_Ele) / 125,
            `Peak CCS sequestration (GtCO₂/yr)` = max(Car_Seq_CCS, na.rm = TRUE) / 1000,
            `Cumulative CCS sequestration, 2025-2150 (GtCO₂)` = trap(year, Car_Seq_CCS) / 1000,
            `Average population at risk of hunger, 2025-2150 (million)` = trap(year, Pop_Ris_of_Hun) / 125,
            `Peak food price index (2020 = 1)` = max(Prc_Agr_Foo_Pro_Ind[year >= 2020]) / Prc_Agr_Foo_Pro_Ind[year == 2020],
            `CCS sequestration in 2150 (GtCO₂/yr)` = Car_Seq_CCS[year == 2150] / 1000,
            `DACCS electricity in 2150 (EJ/yr)` = Fin_Ene_Car_Man_Dir_Air_Cap_Ele[year == 2150])
# so DACCS electricity in 2150 is 0.09 EJ/yr everywhere; the long-term commitment of No Phase-out is BECCS land (World, si_iamc.csv)
ecl <- ecl %>% left_join(read.csv(file.path(D, "si_iamc.csv")) %>% filter(region == "World", code == "Lan_Cov_Cro_Ene_Cro", year == 2150) %>%
  transmute(s = short(scenario), `Energy-crop land in 2150 (Mha)` = value), by = "s")

# economic impact
bk <- read.csv(file.path(D, "burke_world_11.csv")) %>% filter(year >= 2025, year <= 2150) %>% group_by(s = short(scenario)) %>%
  arrange(year, .by_group = TRUE) %>%
  # 2150, so its peak would be the 2150 value (the old row k, 5.6% spread, never scored); the yearly fall has a real peak (2083-2093).
  summarise(`Peak annual GDP change due to climate change, 2025-2150 (% points)` = min(100 * diff(impact)),
            `GDP change due to climate change, 2150 (%)` = 100 * impact[year == 2150])
  # 2025-2150 average, dropped: the Burke loss is already a running total, so averaging it counts early years again)

# temperature (median GSAT)
temp <- read.csv(file.path(D, "gsat_p50.csv"), check.names = FALSE) %>% mutate(s = short(Scenario)) %>%
  transmute(s, `Peak global mean surface air temperature, 2025-2150 (°C)` = do.call(pmax, across(as.character(2025:2150))),
            `Global mean surface air temperature, 2150 (°C)` = `2150`)

# categories top to bottom; each metric carries its number of decimals
rows <- list(
  "Warming"         = c("Peak global mean surface air temperature, 2025-2150 (°C)" = 2, "Global mean surface air temperature, 2150 (°C)" = 2),
  "CCS"             = c("Peak CCS sequestration (GtCO₂/yr)" = 2, "Cumulative CCS sequestration, 2025-2150 (GtCO₂)" = 0),
  "Energy"          = c("Peak DACCS electricity share (%)" = 1, "Average DACCS electricity, 2025-2150 (EJ/yr)" = 1),
  "Stranded\ncost"  = c("Peak annual stranded value (B$/yr)" = 1, "Cumulative stranded (B$)" = 0),
  "Welfare"         = c("Peak consumption loss rate (%)" = 2, "Consumption loss NPV 5%, 2025-2150 (%)" = 2),
  "Economy"         = c("Peak annual GDP change due to climate change, 2025-2150 (% points)" = 3, "GDP change due to climate change, 2150 (%)" = 1),
  "Land"            = c("Peak food price index (2020 = 1)" = 3, "Average population at risk of hunger, 2025-2150 (million)" = 1),
  "Inequity"        = c("Gen 1 vs Gen 2 inequity (%)" = 1, "Gen 2 vs Gen 3 inequity (%)" = 1))
if (LONGROWS) rows[["Long-term"]] <- c("CCS sequestration in 2150 (GtCO₂/yr)" = 2, "Energy-crop land in 2150 (Mha)" = 0)
dec <- unlist(unname(rows))
morder <- names(dec)
grp <- setNames(rep(names(rows), lengths(rows)), morder)
flip <- c("Peak annual GDP change due to climate change, 2025-2150 (% points)", "GDP change due to climate change, 2150 (%)")

full <- Reduce(function(a, b) full_join(a, b, by = "s"), list(temp, strd, welf, ineq, ecl, bk)) %>%
  filter(s %in% names(lab)) %>%
  pivot_longer(-s, names_to = "metric", values_to = "value") %>%
  filter(metric %in% morder) %>%  # the 2150 rows are only drawn in the si_long mode
  group_by(metric) %>%
  # inequity rows: only the size of the gap matters, not its sign (Liu et al. 2016, ref 37); cells still show the signed value
  mutate(x = ifelse(grepl("inequity", metric), abs(value), value),
         rel = (x - min(x)) / pmax(diff(range(x)), 1e-12),
         rel = ifelse(metric %in% flip, 1 - rel, rel)) %>% ungroup() %>% select(-x) %>%
  mutate(txt = sprintf(paste0("%.", dec[metric], "f"), value),
         grpf = factor(grp[metric], levels = names(rows)),
         metf = factor(metric, levels = rev(morder)),
         scenf = factor(lab[s], levels = ord))
stopifnot(nrow(full) == length(lab) * length(morder), !anyNA(full$value))
# inequity rows by absolute gap), as in Fig5_ValuePerspectives_2026Oct09/perspectives_numbers.py. The main mode writes the rows
lett <- setNames(letters[seq_along(morder)], morder)
spA <- full %>%
  mutate(o = ifelse(grepl("inequity", metric), abs(value), ifelse(metric %in% flip, -value, value))) %>%
  group_by(metric) %>% summarise(spread = (max(o) - min(o)) / abs(min(o)), .groups = "drop") %>%
  mutate(row = lett[metric], kept = spread >= 0.10) %>% arrange(row)
if (!SI || LONGROWS) write.csv(spA %>% transmute(row, metric, spread_pct = round(100 * spread, 2), kept),  # Fig. S19: own file (rows a-r)
  file.path("output", if (LONGROWS) "fig5_ruleA_rows_longterm.csv" else "fig5_ruleA_rows.csv"), row.names = FALSE)
keepA <- spA$metric[spA$kept]
cat("Rule A: rows left out of the score:", paste0(spA$row[!spA$kept], " (", round(100 * spA$spread[!spA$kept], 1), "%)", collapse = ", "), "\n")
write.csv(full %>% select(scen = scenf, metric, value), OUT_CSV, row.names = FALSE)

p <- ggplot(full, aes(scenf, metf, fill = rel)) +
  # rule A: rows not scored get a plain grey fill (no ranking colours) and dark numbers
  geom_tile(aes(fill = ifelse(metric %in% keepA, rel, NA)), colour = "white", linewidth = 0.5) +
  geom_text(aes(label = txt, colour = ifelse(!metric %in% keepA, "out", ifelse(rel > .58, "hi", "lo"))), size = 1.95, show.legend = FALSE) +
  facet_grid(grpf ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_gradientn(colours = c("#F3F6F9", "#A8E0D9", "#00A9F4", "#2251FF", "#051C2C"),
                       name = "Within each row", breaks = c(0, 1), labels = c("least challenge", "most challenge"), na.value = "#ECEFF2") +
  scale_colour_manual(values = c(hi = "white", lo = "#051C2C", out = "#3A3A3A")) +
  # row letters a-p, top to bottom, bold (ggtext markdown)
  scale_y_discrete(labels = function(x) ifelse(x %in% keepA, paste0(x, ", **", lett[x], "**"),
    paste0("<span style='color:#5A5A5A'>", x, ", **", lett[x], "**</span>"))) +
  labs(x = NULL, y = NULL) +
  theme_minimal(base_size = 7, base_family = "Helvetica") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 6.5, colour = "black"),
        axis.text.y = ggtext::element_markdown(size = 6.5, hjust = 1, colour = "grey15"),
        strip.placement = "outside",
        strip.text.y.left = element_text(angle = 90, face = "bold", size = 7, colour = "#051C2C", hjust = 0.5, lineheight = 0.9),
        panel.grid = element_blank(), panel.spacing.y = unit(3, "pt"),
        legend.position = "right", legend.key.height = unit(1.1, "cm"), legend.key.width = unit(0.3, "cm"),
        legend.title = element_text(size = 6.5), legend.text = element_text(size = 6),
        plot.margin = margin(4, 4, 4, 4))

# temperature, which differs between scenarios, so it joins the score; before, the 2100 temperature was left out: all within 0.04 C),
# every row weighted equally; warmer = more challenge
source("R/style.R")  # mck_scen
sc <- full %>% filter(metric %in% keepA) %>% group_by(scenf) %>% summarise(score = mean(rel)) %>% arrange(score) %>%
  mutate(nudge = ifelse(row_number() %% 2 == 1, 0.95, -0.95),  # labels alternate above and below the bar
         nudge = ifelse(LONGROWS & scenf == "No Phase-out", -0.95, nudge),
         g = " ")  # blank header strip: o gets the same columns as the heatmap, so the label sits beside a-n
cat("Overall challenge score (panel q):\n"); print(as.data.frame(sc[, 1:2]), digits = 3)
ramp_x <- seq(0.0025, 0.9975, by = 0.005)
po <- ggplot(sc, aes(score, 0)) +
  geom_tile(data = data.frame(x = ramp_x), aes(x, 0, fill = x), width = 0.005, height = 0.45, inherit.aes = FALSE) +
  scale_fill_gradientn(colours = c("#F3F6F9", "#A8E0D9", "#00A9F4", "#2251FF", "#051C2C"), guide = "none") +
  geom_point(size = 2.3, colour = "white") + geom_point(aes(colour = scenf), size = 1.7) +
  ggrepel::geom_text_repel(aes(label = scenf), nudge_y = sc$nudge, direction = "x", size = 2.1,
                           family = "Helvetica", colour = "grey15", segment.colour = "grey40",
                           segment.size = 0.2, min.segment.length = 0, box.padding = if (LONGROWS) 0.35 else 0.12, seed = 1,
                           xlim = if (LONGROWS) c(-0.02, 1.12) else c(NA, NA)) +  # Fig. S19: labels may use the empty space right of the bar
  (if (LONGROWS) coord_cartesian(clip = "off") else NULL) +
  scale_colour_manual(values = mck_scen, guide = "none") +
  scale_x_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.25), expand = expansion(add = 0.01),
                     labels = c("0\nleast challenge", "0.25", "0.50", "0.75", "1\nmost challenge")) +
  scale_y_continuous(limits = c(-1.5, 1.5), breaks = 0, labels = paste0("Overall challenge score, **", if (LONGROWS) "s" else "q", "**")) +
  facet_grid(g ~ ., switch = "y") +
  labs(x = paste0("Mean position within the ", length(keepA), " rows that differ by at least 10% across the ", if (LONGROWS) "11 scenarios" else "phase-out scenarios", ", each weighted equally"), y = NULL) +
  theme_minimal(base_size = 7, base_family = "Helvetica") +
  theme(axis.text.x = element_text(size = 6, colour = "black"), axis.title.x = element_text(size = 6.5),
        axis.text.y = ggtext::element_markdown(size = 6.5, hjust = 1, colour = "grey15"),
        strip.placement = "outside", strip.text.y.left = element_text(angle = 90, size = 7),
        panel.grid = element_blank(), plot.margin = margin(4, 4, 4, 4))

fig <- p / po + plot_layout(heights = c(14, 2.3))
ggsave(OUT_PNG, fig, width = 19.05 / 2.54, height = 16.6 / 2.54, dpi = 600,
       bg = "white", device = ragg::agg_png)
print(fig)
cat("Saved", OUT_PNG, "\n")
