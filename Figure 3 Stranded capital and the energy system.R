# ============================================================================
# FIGURE 3 STRANDED CAPITAL AND THE ENERGY SYSTEM
# ============================================================================
# (a) Annual stranded capital across all technologies, net of the baseline without climate policy. (b) Capacity
# utilisation by technology in HCC_2110. (c) Cumulative stranded capital before and after the phase-out. (d) Total
# electricity supply. (e) Electricity used by DACCS and its share. (f) Electricity in the year of the peak DACCS share.
# Panels come from R/fig3_stranding_cache.R, R/fig3abc_stranded_capital.R and R/fig3def_electricity.R.
# Data: data/sav_<scenario>.csv (stranded asset value by year, region and technology), data/copr_tpcc_dac.csv,
# data/copr_hcc2110.csv (capacity utilisation), data/iamc_fig.csv
# Output: output/Figure_3.png
# Run: Rscript "Figure 3 Stranded capital and the energy system.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all four.
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
source("R/fig3_stranding_cache.R")  # stranding caches for panels a-c (output/)
library(ggplot2); library(patchwork)
update_geom_defaults("text", list(family = "Helvetica"))
src <- function(f) { e <- new.env(); sys.source(file.path("R", f), envir = e); e }
e2 <- src("fig3abc_stranded_capital.R")
e3 <- src("fig3def_electricity.R")
source("R/style.R")

pal <- mck_scen; ord <- e2$scen_order
by_label <- setNames(mck_power[names(e2$tech_labels)], e2$tech_labels)  # panel b keys are display names
line_keys <- list(guides(colour = guide_legend(ncol = 1, override.aes = list(linewidth = 1))),
                  theme(legend.key.width = unit(0.5, "cm")))
q <- function(x) suppressMessages(x)  # silence "replacing the existing scale"

# a: one-point line draws nothing but gives No Phase-out (not plotted in a) a legend key for d-f
pa <- q(shrink(e2$p_a) + labs(title = NULL) +
  geom_line(data = data.frame(Y = 2025, V_B = 0, label = factor("No Phase-out", levels = ord)), linewidth = 0.5) +
  scale_color_manual(values = pal, limits = ord, breaks = ord, name = NULL)) + th + line_keys
pb <- q(shrink(e2$p_b) + labs(title = NULL) +
  scale_color_manual(values = by_label, breaks = e2$tech_labels[e2$tech_order], name = "Technology",
                     na.translate = FALSE)) + th + line_keys
pc <- q(shrink(e2$p_c) + labs(title = NULL) +
  scale_fill_manual(values = mck_power, labels = e2$tech_labels, breaks = e2$tech_order, name = NULL,
                    na.translate = FALSE)) +
  th + theme(legend.position = "none", strip.text = element_text(size = 7, face = "bold"))  # colours: legend of b
# segment labels at 5 pt need about 520 B$ of bar at this width; dark text on the light fills
seg <- pc$layers[[2]]
seg$data$seg_lbl[seg$data$V_B < 520] <- ""
seg$data$txt_col <- ifelse(as.character(seg$data$A_chr) %in% mck_light, "#051C2C", "white")
seg$aes_params$size <- 1.76           # 5 pt
# two label colours now: group by technology so the labels stack in the same order as the bars
seg$mapping <- aes(label = seg_lbl, colour = txt_col, group = A_chr)
pc$layers[[3]]$aes_params$size <- 1.76  # row totals, 5 pt
pc$layers[[3]]$aes_params$hjust <- -0.25  # clear of the last segment's label
# TPCC splits at its DACCS phase-out start (develop 2087, 3.3.3 2086; read from the cache) (2088 before the v3.2.0 rerun), not 2101: asterisk on its row, note in the empty lower right of the After facet
tpcc_start <- unique(read.csv("output/fig2_stranded_annual_bytech_fresh.csv") |> subset(label == "TPCC") |> _$phase_start)
stopifnot(length(tpcc_start) == 1, !is.na(tpcc_start))
after <- factor(levels(e2$plot_df$period)[1], levels = levels(e2$plot_df$period))
pc <- q(pc + scale_y_discrete(labels = function(x) ifelse(x == "TPCC", "TPCC*", x)) +
  geom_text(data = data.frame(period = after, x = 2600, y = 0.62,
                              lab = paste0("*Before and after are split at 2101 in all scenarios\nexcept TPCC, whose phase-out\nstarts in ", tpcc_start, ".")),
            aes(x, y, label = lab), inherit.aes = FALSE, hjust = 0, vjust = 0, size = 1.76, lineheight = 0.95,
            colour = "grey25", family = "Helvetica"))

# the electricity script uses a 15-pt base: bring its titles and line weights to the a-c scale
fix_ele <- theme(plot.title = element_blank(), axis.title.y = element_text(size = 7),
                 axis.title.y.right = element_text(size = 7),
                 line = element_line(linewidth = 10 / 22), rect = element_rect(linewidth = 10 / 22))
pd <- q(shrink(e3$p_a) + scale_color_manual(values = pal, breaks = ord, name = NULL)) + th + fix_ele
pe <- q(shrink(e3$p_b) + scale_color_manual(values = pal, breaks = ord, name = NULL)) + th + fix_ele
pf <- q(shrink(e3$p_c) + scale_fill_manual(values = c(other_ele = "#C9CED6", dac_ele = unname(mck_power["DACCS"])),
                                           labels = c(other_ele = "Other", dac_ele = "DACCS"), name = NULL)) + th + fix_ele +
  theme(legend.position = "top", legend.direction = "horizontal",
        legend.margin = margin(0, 0, -4, 0))  # above the panel, clear of the right axis

# free(side = "r"): row a-b's legends are not aligned with f's right axis (no empty column before b's legend);
# free(type = "label"): the y titles of a and d sit next to their own axis
fig <- free(free(pa, type = "label", side = "l") | pb, side = "r") / pc /
  (free(pd, type = "label", side = "l") | pe | pf) +
  plot_layout(heights = c(0.95, 1.55, 0.9)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 10, face = "bold", family = "Helvetica"),
        panel.grid = element_blank())  # no grid lines in any panel, as in Fig. 2

ggsave("output/Figure_3.png", fig, width = 19.05 / 2.54, height = 17.5 / 2.54, dpi = 600,
       bg = "white", device = ragg::agg_png)
print(fig)
cat("Saved output/Figure_3.png\n")
