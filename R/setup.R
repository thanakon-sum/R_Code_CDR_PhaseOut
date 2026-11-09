# Packages used by the figure scripts: installed from CRAN when missing. Output folders are created here.
pkgs <- c("ggplot2", "dplyr", "tidyr", "patchwork", "ragg", "scales", "tidyverse", "cowplot", "ggtext", "ggrepel")
miss <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(miss)) install.packages(miss, repos = "https://cloud.r-project.org")
dir.create("output/panels", recursive = TRUE, showWarnings = FALSE)
if (!interactive()) ragg::agg_png(tempfile(fileext = ".png"))  # print() in a script run: throwaway device, figures come from ggsave
