# ============================================================================
# FIGURE 2 EMISSIONS, TEMPERATURE AND CDR DEPLOYMENT
# ============================================================================
# (a) Global net CO2 emissions (solid) and median global mean surface air temperature (dashed); the dashed
# horizontal line is the residual level of -2.10 GtCO2/yr. (b) Total CDR. (c) Cumulative CO2 stored in geological
# formations, 2025-2150, and (d) the part stored by DACCS. (e) CDR by technology in each scenario.
# Panels come from R/fig2a_emissions_temperature.R, R/fig2bcd_cdr_storage.R and R/fig2e_cdr_by_technology.R.
# Data: data/emis_co2.csv, data/gsat_p50.csv, data/ccs_iamc.csv, data/cdr_iamc.csv
# Output: output/Figure_2.png
# Run: Rscript "Figure 2 Emissions, temperature and CDR deployment.R" (or source() it, or open the .Rproj in RStudio); run_all.R draws all four.
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
library(ggplot2); library(patchwork)
update_geom_defaults("text", list(family = "Helvetica"))
src <- function(f) { e <- new.env(); sys.source(file.path("R", f), envir = e); e }
e1 <- src("fig2a_emissions_temperature.R")
e2 <- src("fig2e_cdr_by_technology.R")
e3 <- src("fig2bcd_cdr_storage.R")

source("R/style.R")  # shrink(), th, McKinsey palettes (shared with Fig. 3)

pal <- e3$scenario_colors; ord <- e3$scenario_order
# Default keeps the earlier colours.
MCK <- TRUE  # the published colours
tech_pal <- NULL
if (MCK) { pal <- mck_scen; tech_pal <- mck_cdr }
recol <- function(p) if (MCK) suppressMessages(p + scale_colour_manual(values = pal, breaks = ord, name = NULL)) else p
refill <- function(p) if (MCK) suppressMessages(p + scale_fill_manual(values = tech_pal, drop = FALSE, name = NULL)) else p
# scenario legend (and solid = emissions, dashed = temperature) sits to the right of a and serves every panel
pa <- shrink(e1$p) + scale_colour_manual(values = pal, breaks = ord, name = NULL) + th +
  guides(colour = guide_legend(ncol = 1, override.aes = list(linetype = "solid", linewidth = 0.8)),
         linetype = guide_legend(ncol = 1, override.aes = list(colour = "grey30", linewidth = 0.6))) +
  theme(legend.position = "right", legend.box = "vertical", legend.key.width = unit(0.6, "cm"),
        legend.margin = margin(0, 0, 0, 0), legend.box.spacing = unit(2, "pt"))

grid_b <- wrap_plots(c(lapply(e2$panels, function(q) refill(shrink(q))), list(guide_area())), ncol = 3) +
  plot_layout(guides = "collect") & th & theme(legend.text = element_text(size = 6.5)) &
  guides(fill = guide_legend(ncol = 2, byrow = TRUE))  # technology legend in two columns
# th is applied per panel (not page-wide) so e keeps its hidden y labels: d and e share one axis
pc <- recol(shrink(e3$p_line)) + th
pd <- recol(shrink(e3$p_box_c)) + th + guides(colour = guide_legend(ncol = 1, override.aes = list(size = 1.6)))
pe <- recol(shrink(e3$p_box_d)) + th + theme(axis.text.y = element_blank(), axis.ticks.y = element_blank(),
                                       axis.title.y = element_blank(), axis.line.y = element_blank())
row_cde <- (pc | pd | pe) + plot_layout(widths = c(3.2, 1, 1)) &
  theme(legend.position = "none")  # scenario legend is under a

# storage row directly under a (nearest the scenario legend), composition grid last; letters follow reading order
# free(side = "r"): a's legend column is not imposed on the rows below, so they keep the full width
fig <- free(pa, side = "r") / row_cde / wrap_elements(plot = grid_b) +  # plot=, not full=: leaves room for the grid's tag
  plot_layout(heights = c(1.35, 0.95, 2.15)) +
  plot_annotation(tag_levels = "a") &
  theme(plot.tag = element_text(size = 10, face = "bold", family = "Helvetica"))

set.seed(1)  # geom_jitter in d and e: same dot positions on every run
out <- if (MCK) "output/Figure_2.png" else "output/panels/fig2_overview_fresh.png"
ggsave(out, fig, width = 19.05 / 2.54, height = 20.4 / 2.54, dpi = 600,
       bg = "white", device = ragg::agg_png)
print(fig)
cat("Saved", out, "\n")
