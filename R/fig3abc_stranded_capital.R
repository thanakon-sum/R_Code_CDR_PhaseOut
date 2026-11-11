# ============================================================
# Three-panel figure (v2 — all technologies in all panels):
# (a) Annual stranded capital — ALL technologies, net of BaU
# (b) COPR of ALL technologies in HCC_2110 (World avg, 2090 to 2130)
# (c) All-technology Before/After phase-out stacked bars
#     Johnson method, 5% DCR, net of BaU baseline
# One-time GDX->CSV cache: first run reads the cbnal0 GDX and writes the
# caches; every later run reads the CSVs and never touches GDX.
# ============================================================

library(tidyverse); library(cowplot)

base_dir <- NA  # GDX branch not used
SCALEF   <- 10
crf      <- function(r, n) r*(1+r)^n / ((1+r)^n - 1)

CACHE_ANNUAL <- "output/fig2_stranded_annual_bytech_fresh.csv"
CACHE_COPR   <- "output/fig2_copr_hcc_fresh.csv"
USE_CACHE    <- file.exists(CACHE_ANNUAL) && file.exists(CACHE_COPR)
if (!USE_CACHE) stop("Fresh caches missing: run R/fig3_stranding_cache.R first")

scenarios <- tribble(
  ~label,          ~file,                                        ~phase_start,
  "No Phase-out",  "global_17_SSP2_400C_2025CP_NoCC_No.gdx",    NA_real_,
  "TPCC",     "global_17_SSP2_TPCC_Ext_NoCC_No.gdx",       NA_real_,
  "HCC_2110",      "global_17_SSP2_HCC_2110_NoCC_No.gdx",       2101,
  "HCC_2150",      "global_17_SSP2_HCC_2150_NoCC_No.gdx",       2101,
  "MCC_2110",      "global_17_SSP2_Med_2110_NoCC_No.gdx",       2101,
  "MCC_2120",      "global_17_SSP2_Med_2120_NoCC_No.gdx",       2101,
  "MCC_2130",      "global_17_SSP2_Med_2130_NoCC_No.gdx",       2101,
  "MCC_2140",      "global_17_SSP2_Med_2140_NoCC_No.gdx",       2101,
  "MCC_2150",      "global_17_SSP2_Med_2150_NoCC_No.gdx",       2101,
  "LCC_2110",      "global_17_SSP2_LCC_2110_NoCC_No.gdx",       2101,
  "LCC_2150",      "global_17_SSP2_LCC_2150_NoCC_No.gdx",       2101
)
scen_order <- scenarios$label

pal_scen <- c(
  "No Phase-out"="#E31A1C", "TPCC"="#A65628",
  "HCC_2110"="#377EB8",     "HCC_2150"="#A6CEE3",
  "MCC_2110"="#FF7F00",     "MCC_2120"="#FDBF6F",
  "MCC_2130"="#B2DF8A",     "MCC_2140"="#33A02C",
  "MCC_2150"="#1F6B39",     "LCC_2110"="#6A3D9A",
  "LCC_2150"="#CAB2D6"
)

# Shared technology palette (consistent across all three panels)
tech_order  <- c("DACCS","E_BIN","E_GEO","E_WIN","E_WIF","E_SPV","E_BIO","E_NUC","E_HYD","E_GAS","E_OIL","E_COL")
tech_labels <- c(DACCS="DACCS", E_BIN="BECCS", E_GEO="Geothermal", E_WIN="Onshore wind", E_WIF="Offshore wind",
                 E_SPV="Solar", E_BIO="Bioenergy", E_NUC="Nuclear", E_HYD="Hydro",
                 E_GAS="Gas Power", E_OIL="Oil Power", E_COL="Coal Power")
tech_pal <- c(DACCS="#B2182B", E_BIN="#2166AC", E_GEO="#B5D87A", E_WIN="#A8DDD0", E_WIF="#4FA3B8",
              E_SPV="#FFE680", E_BIO="#D4AA82", E_NUC="#C9A0DC", E_HYD="#A8C8F0",
              E_GAS="#C8C8C8", E_OIL="#A0A0A0", E_COL="#686868")

# solar, scenario.gms:478, 0.10 otherwise), so the annuity converts from the model rate to 5% over the same lifetime
lft_map <- c(E_BIN=40, E_WIN=25, E_WIF=25, E_SPV=25, E_GAS=40, E_OIL=40,
             E_BIO=40, E_NUC=40, E_GEO=30, E_HYD=50, E_COL=40)
r_map   <- setNames(ifelse(names(lft_map) %in% c("E_WIN", "E_WIF", "E_SPV"), 0.05, 0.10), names(lft_map))
focus   <- names(lft_map)
AIV_DAC <- 793 * crf(0.05, 25)
CRF_STO <- crf(0.05, 25)

# COPR panel keys (needed by both cache-read and compute paths)
copr_all_techs <- c(focus, "DAC")
copr_display   <- c(setNames(tech_labels[focus], focus), DAC="DACCS")
copr_pal       <- c(tech_pal[focus], DAC=unname(tech_pal["DACCS"]))

detect_phaseout <- function(copr_df) {
  traj <- copr_df |>
    filter(as.character(A) == "DAC") |>
    mutate(Y=as.integer(as.character(Y)), v2=as.numeric(v)) |>
    filter(Y >= 2030) |>
    group_by(Y) |> summarise(m=mean(v2, na.rm=TRUE), .groups="drop") |> arrange(Y)
  if (nrow(traj) == 0) return(NA_real_)
  below <- traj$m < 0.85
  for (i in seq_along(below)) if (below[i] && all(below[i:length(below)])) return(traj$Y[i])
  NA_real_
}

# Loads one scenario's all-tech annual stranded (net of BaU). Uses the bau_* globals.
load_annual_all <- function(label, file, phase_start) {
  gdx <- file.path(base_dir, file)
  if (!file.exists(gdx)) { cat(sprintf("  MISSING: %s\n", file)); return(NULL) }
  cat(sprintf("  %s...\n", label))

  cap  <- rgdx.param(gdx, "QC_load");             names(cap)  <- c("Y","R","var","A","v")
  annu <- rgdx.param(gdx, "AnnuCostEneSup_load");  names(annu) <- c("Y","R","var","A","v")
  copr <- rgdx.param(gdx, "COPR_load");            names(copr) <- c("Y","R","A","v")
  qf   <- rgdx.param(gdx, "QF_load");              names(qf)   <- c("Y","R","var","A","v")

  if (label == "TPCC") {
    phase_start <- detect_phaseout(copr)
    cat(sprintf("    -> TPCC detected Yr* = %s\n",
                ifelse(is.na(phase_start),"NOT DETECTED",as.character(phase_start))))
  }
  if (is.na(phase_start)) phase_start <- 2101

  # Electricity techs
  d <- merge(cap[cap$var=="CAP", c("Y","R","A","v")],
             annu[annu$var=="CAP", c("Y","R","A","v")],
             by=c("Y","R","A"), suffixes=c("_cap","_ac")) |>
    merge(copr[, c("Y","R","A","v")], by=c("Y","R","A")) |>
    mutate(Y=as.integer(as.character(Y)), A_chr=as.character(A),
           CAP=as.numeric(v_cap), AC=as.numeric(v_ac), COPR=as.numeric(v)) |>
    filter(A_chr %in% focus, Y >= 2025, Y <= 2150)
  d$lft   <- lft_map[d$A_chr]
  d$scale <- crf(0.05, d$lft) / crf(r_map[d$A_chr], d$lft)
  elec <- d |>
    mutate(V_B = CAP * SCALEF * AC * pmax(0,1-COPR) * scale / 1e3) |>
    left_join(bau_elec_V, by=c("Y","R","A_chr")) |>
    mutate(V_bau=replace_na(V_bau,0), V_B=pmax(0, V_B-V_bau)) |>
    group_by(Y, A_chr) |> summarise(V_B=sum(V_B,na.rm=TRUE), .groups="drop")

  # DACCS (DAC + storage combined)
  copr_dac <- copr |> filter(as.character(A)=="DAC") |>
    mutate(Y=as.integer(as.character(Y)), COPR=as.numeric(v)) |> select(Y, R, COPR)
  dac <- qf |>
    filter(var=="CCAP", as.character(A)=="DAC") |>
    mutate(Y=as.integer(as.character(Y)), CCAP=as.numeric(v)) |>
    filter(Y >= 2025, Y <= 2150) |>
    left_join(copr_dac, by=c("Y","R")) |>
    mutate(COPR=replace_na(COPR,1), V_B=CCAP*SCALEF*AIV_DAC*pmax(0,1-COPR)/1e6) |>
    left_join(bau_dac_V, by=c("Y","R")) |>
    mutate(V_bau=replace_na(V_bau,0), V_B=pmax(0, V_B-V_bau))

  copr_sto <- copr |> filter(as.character(A)=="STO") |>
    mutate(Y=as.integer(as.character(Y)), COPR=as.numeric(v)) |> select(Y, R, COPR)
  sto <- qf |>
    filter(var=="CAP", as.character(A)=="STO") |>
    mutate(Y=as.integer(as.character(Y)), CAP_M=as.numeric(v)) |>
    filter(Y >= 2025, Y <= 2150) |>
    left_join(copr_sto, by=c("Y","R")) |>
    mutate(COPR=replace_na(COPR,1), V_B=CAP_M*CRF_STO*pmax(0,1-COPR)/1000) |>
    left_join(bau_sto_V, by=c("Y","R")) |>
    mutate(V_bau=replace_na(V_bau,0), V_B=pmax(0, V_B-V_bau))

  daccs <- bind_rows(dac, sto) |>
    group_by(Y) |> summarise(V_B=sum(V_B,na.rm=TRUE), .groups="drop") |>
    mutate(A_chr="DACCS")

  bind_rows(elec, daccs) |>
    mutate(label=label, phase_start=phase_start)
}

# ===================================================================
# Data: read cache if present, else compute from cbnal0 GDX and cache
# ===================================================================
if (USE_CACHE) {
  cat("Reading Fig 2 stranded cache (no GDX)...\n")
  all_annual     <- read.csv(CACHE_ANNUAL, stringsAsFactors = FALSE)
  copr_hcc_world <- read.csv(CACHE_COPR, stringsAsFactors = FALSE) |>
    mutate(tech = factor(copr_display[A_chr], levels = tech_labels[tech_order]))
} else {
  cat("Loading BaU baseline...\n")
  bau_gdx  <- file.path(base_dir, "global_17_SSP2_BaU_NoCC_No.gdx")
  bau_cap  <- rgdx.param(bau_gdx, "QC_load");             names(bau_cap)  <- c("Y","R","var","A","v")
  bau_annu <- rgdx.param(bau_gdx, "AnnuCostEneSup_load"); names(bau_annu) <- c("Y","R","var","A","v")
  bau_copr <- rgdx.param(bau_gdx, "COPR_load");           names(bau_copr) <- c("Y","R","A","v")
  bau_qf   <- rgdx.param(bau_gdx, "QF_load");             names(bau_qf)   <- c("Y","R","var","A","v")

  bau_d <- merge(bau_cap[bau_cap$var=="CAP", c("Y","R","A","v")],
                 bau_annu[bau_annu$var=="CAP", c("Y","R","A","v")],
                 by=c("Y","R","A"), suffixes=c("_cap","_ac")) |>
    merge(bau_copr[, c("Y","R","A","v")], by=c("Y","R","A")) |>
    mutate(Y=as.integer(as.character(Y)), A_chr=as.character(A),
           CAP=as.numeric(v_cap), AC=as.numeric(v_ac), COPR=as.numeric(v)) |>
    filter(A_chr %in% focus)
  bau_d$lft   <- lft_map[bau_d$A_chr]
  bau_d$scale <- crf(0.05, bau_d$lft) / crf(r_map[bau_d$A_chr], bau_d$lft)
  bau_elec_V  <- bau_d |>
    mutate(V_bau = CAP * SCALEF * AC * pmax(0,1-COPR) * scale / 1e3) |>
    select(Y, R, A_chr, V_bau)

  bau_copr_dac <- bau_copr |> filter(as.character(A)=="DAC") |>
    mutate(Y=as.integer(as.character(Y)), COPR=as.numeric(v)) |> select(Y, R, COPR)
  bau_dac_V <- bau_qf |>
    filter(var=="CCAP", as.character(A)=="DAC") |>
    mutate(Y=as.integer(as.character(Y)), CCAP=as.numeric(v)) |>
    left_join(bau_copr_dac, by=c("Y","R")) |>
    mutate(COPR=replace_na(COPR,1), V_bau=CCAP*SCALEF*AIV_DAC*pmax(0,1-COPR)/1e6) |>
    select(Y, R, V_bau)

  bau_copr_sto <- bau_copr |> filter(as.character(A)=="STO") |>
    mutate(Y=as.integer(as.character(Y)), COPR=as.numeric(v)) |> select(Y, R, COPR)
  bau_sto_V <- bau_qf |>
    filter(var=="CAP", as.character(A)=="STO") |>
    mutate(Y=as.integer(as.character(Y)), CAP_M=as.numeric(v)) |>
    left_join(bau_copr_sto, by=c("Y","R")) |>
    mutate(COPR=replace_na(COPR,1), V_bau=CAP_M*CRF_STO*pmax(0,1-COPR)/1000) |>
    select(Y, R, V_bau)

  cat("Loading all-tech annual data...\n")
  all_annual <- pmap_dfr(scenarios, load_annual_all)

  # COPR of ALL technologies in HCC_2110
  gdx_hcc  <- file.path(base_dir, "global_17_SSP2_HCC_2110_NoCC_No.gdx")
  copr_hcc <- rgdx.param(gdx_hcc, "COPR_load"); names(copr_hcc) <- c("Y","R","A","v")
  copr_hcc_world <- copr_hcc |>
    filter(as.character(A) %in% copr_all_techs) |>
    mutate(Y=as.integer(as.character(Y)), COPR=as.numeric(v), A_chr=as.character(A)) |>
    group_by(Y, A_chr) |>
    summarise(COPR=mean(COPR, na.rm=TRUE), .groups="drop") |>
    mutate(tech=copr_display[A_chr],
           tech=factor(tech, levels=tech_labels[tech_order]))

  write.csv(all_annual, CACHE_ANNUAL, row.names = FALSE)
  write.csv(copr_hcc_world[, c("Y","A_chr","COPR")], CACHE_COPR, row.names = FALSE)
  cat("Wrote caches:\n  ", CACHE_ANNUAL, "\n  ", CACHE_COPR, "\n")
}

# Panel (a) data: annual total by scenario
annual_total <- all_annual |>
  filter(label != "No Phase-out") |>
  group_by(label, Y) |>
  summarise(V_B=sum(V_B, na.rm=TRUE), .groups="drop") |>
  mutate(label=factor(label, levels=scen_order))

# Panel (c) data: Before/After aggregation from annual data
both_res <- all_annual |>
  mutate(period = ifelse(Y < phase_start, "Before phase-out (2025-2100)", "After phase-out (2100-2150)")) |>
  filter(Y <= 2150) |>
  group_by(label, A_chr, period) |>
  summarise(V_B=sum(V_B, na.rm=TRUE), .groups="drop")

# ===================================================================
# PLOTTING
# ===================================================================

# Panel (a): annual stranded, all tech total
p_a <- ggplot(annual_total, aes(x=Y, y=V_B, color=label)) +
  geom_line(linewidth=0.8) +
  scale_color_manual(values=pal_scen, breaks=scen_order[-1], name=NULL) +
  scale_x_continuous(breaks=seq(2025,2150,15)) +
  labs(title="(a) Annual stranded capital, all technologies, net of BaU (World)",
       x=NULL, y="B$ yr\u207b\u00b9") +
  theme_bw(base_size=10) +
  theme(plot.title=element_text(size=9, face="bold"),
        panel.grid.minor=element_blank(),
        legend.position="right",
        legend.key.size=unit(0.5,"lines"),
        legend.text=element_text(size=8))

# Panel (b): COPR all tech, HCC_2110 zoom 2090 to 2130
p_b <- ggplot(copr_hcc_world |> filter(Y >= 2090, Y <= 2130),
              aes(x=Y, y=COPR, color=tech)) +
  geom_line(linewidth=0.9) +
  geom_hline(yintercept=1, linetype="dashed", color="grey50", linewidth=0.4) +
  annotate("rect", xmin=2107, xmax=2115, ymin=-Inf, ymax=Inf,
           fill="steelblue", alpha=0.08) +
  scale_color_manual(values=setNames(copr_pal, copr_display[names(copr_pal)]),
                     breaks=tech_labels[tech_order], name="Technology",
                     na.translate=FALSE) +
  scale_x_continuous(breaks=seq(2090,2130,5)) +
  scale_y_continuous(limits=c(NA,1.002)) +
  labs(title="(b) COPR of all technologies in HCC_2110 (World avg, 2090 to 2130)",
       x=NULL, y="Capacity utilisation rate (COPR)") +
  theme_bw(base_size=10) +
  theme(plot.title=element_text(size=9, face="bold"),
        panel.grid.minor=element_blank(),
        legend.position="right",
        legend.key.size=unit(0.5,"lines"),
        legend.text=element_text(size=8))

# Panel (c): Before/After stacked bars
plot_df <- both_res |>
  mutate(label  = factor(label,  levels=rev(scen_order)),
         A_chr  = factor(A_chr,  levels=rev(tech_order)),
         period = factor(period, levels=c("After phase-out (2100-2150)","Before phase-out (2025-2100)")))

totals <- plot_df |>
  group_by(label, period) |>
  summarise(grand=sum(V_B, na.rm=TRUE), .groups="drop")

xmax <- (floor(max(totals$grand, na.rm=TRUE) * 1.30 / 1000) + 0.5) * 1000

# A segment is labelled only if the label fits inside it: about 1.2% of the axis per character plus a 1.5% margin
plot_df_lbl <- plot_df |>
  mutate(seg_lbl = ifelse(V_B >= 1000, paste0(round(V_B/1000,1)," T$"), paste0(round(V_B)," B$")),
         seg_lbl = ifelse(V_B > 200 & V_B > xmax * (0.012 * nchar(seg_lbl) + 0.015), seg_lbl, ""),
         txt_col = "white")

p_c <- ggplot(plot_df, aes(y=label, x=V_B, fill=A_chr)) +
  geom_col(width=0.72) +
  geom_text(data=plot_df_lbl,
            aes(label=seg_lbl, color=txt_col),
            position=position_stack(vjust=0.5),
            size=2.0, fontface="bold") +
  scale_color_identity(guide="none") +
  geom_text(data=totals,
            aes(y=label, x=grand, label=sprintf("%.0f B$", grand), fill=NULL),
            hjust=-0.05, size=2.8, color="grey25") +
  facet_wrap(~period, ncol=2) +
  scale_fill_manual(values=tech_pal, labels=tech_labels,
                    breaks=tech_order, name=NULL, na.translate=FALSE) +
  scale_x_continuous(limits=c(0, xmax), expand=expansion(mult=c(0,0)),
                     labels=scales::comma) +
  guides(fill=guide_legend(ncol=6, byrow=TRUE)) +
  labs(title="(c) All-technology stranded capital by phase-out timing (2025 to 2150, 5% DCR, net of BaU)",
       x="Stranded capital (B$)", y=NULL) +
  theme_bw(base_size=10) +
  theme(plot.title=element_text(size=9, face="bold"),
        legend.position="bottom",
        legend.key.size=unit(0.6,"lines"),
        legend.text=element_text(size=8),
        strip.text=element_text(size=10, face="bold"),
        panel.grid.minor=element_blank(),
        panel.grid.major.y=element_blank())

# Assemble: (a)+(b) top row, (c) full-width bottom
top_row <- plot_grid(p_a, p_b, ncol=2)
final   <- plot_grid(top_row, p_c, ncol=1, rel_heights=c(1, 1.8))

out <- "output/panels/johnson_alltech_v2_fresh.png"
ggsave(out, final, width=16, height=16, dpi=150)
cat("Saved:", out, "\n")
print(final)
cat("\n=== Panel (c) totals (B$, net of BaU) ===\n")
totals |>
  pivot_wider(names_from=period, values_from=grand) |>
  mutate(across(where(is.numeric), \(x) round(x))) |>
  print(n=20)
