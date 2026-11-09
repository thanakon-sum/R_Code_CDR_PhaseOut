# data/ (sav_<scenario>.csv = StrandedAssetValue(Y,R,A) from cbnal_annual, the
# model's own Johnson module 030_stranded_assets.gms, same formula and lifetimes as johnson_alltech_v2.R;
# copr_*.csv = COPR_load from the year files).
# Net of BaU exactly as before: max(0, scenario - BaU) per year, region and technology, then summed over
# regions; DACCS = DAC + STO, each netted separately.
library(dplyr); library(tidyr)
DIR <- "data"
scen <- c("No Phase-out" = "400C_2025CP", "TPCC" = "TPCC_Ext", "HCC_2110" = "HCC_2110",
          "HCC_2150" = "HCC_2150", "MCC_2110" = "Med_2110", "MCC_2120" = "Med_2120",
          "MCC_2130" = "Med_2130", "MCC_2140" = "Med_2140", "MCC_2150" = "Med_2150",
          "LCC_2110" = "LCC_2110", "LCC_2150" = "LCC_2150")
rd <- function(s) read.csv(file.path(DIR, paste0("sav_", s, ".csv")), stringsAsFactors = FALSE) |>
  setNames(c("Y", "R", "A", "V")) |> mutate(Y = as.integer(Y))
bau <- rd("BaU") |> rename(V_bau = V)

# DAC utilisation (World mean over regions), same phase-out rule as detect_phaseout() in the old script
tp <- read.csv(file.path(DIR, "copr_tpcc_dac.csv")) |> group_by(Y) |> summarise(m = mean(COPR), .groups = "drop") |>
  arrange(Y) |> mutate(below = m < 0.85)
# full use again by 2150, so "the last run of years below 85% that lasts to 2150" found no year; develop gives 2087 both ways)
last_full <- max(tp$Y[tp$Y <= 2100 & !tp$below])
tpcc_start <- min(tp$Y[tp$Y > last_full & tp$below])
stopifnot(is.finite(tpcc_start), tpcc_start <= 2100)
cat("TPCC phase-out start:", tpcc_start, "\n")

annual <- bind_rows(lapply(names(scen), function(lab) {
  rd(scen[[lab]]) |> filter(Y >= 2025, Y <= 2150) |>  # from policy start, so "before" = 2025 to the phase-out
    left_join(bau, by = c("Y", "R", "A")) |>
    mutate(V_B = pmax(0, V - coalesce(V_bau, 0)), A_chr = ifelse(A %in% c("DAC", "STO"), "DACCS", A)) |>
    group_by(Y, A_chr) |> summarise(V_B = sum(V_B), .groups = "drop") |>
    mutate(label = lab, phase_start = ifelse(lab == "TPCC", tpcc_start, 2101))
}))
write.csv(annual, "output/fig2_stranded_annual_bytech_fresh.csv", row.names = FALSE)

copr <- read.csv(file.path(DIR, "copr_hcc2110.csv"), stringsAsFactors = FALSE) |>
  filter(A %in% c("E_BIN", "E_WIN", "E_WIF", "E_SPV", "E_GAS", "E_OIL", "E_BIO", "E_NUC", "E_GEO", "E_HYD", "E_COL", "DAC")) |>
  group_by(Y, A_chr = A) |> summarise(COPR = mean(COPR), .groups = "drop")
write.csv(copr, "output/fig2_copr_hcc_fresh.csv", row.names = FALSE)

tot <- annual |> mutate(period = ifelse(Y < phase_start, "before", "after")) |>
  group_by(label, period) |> summarise(B = round(sum(V_B)), .groups = "drop") |> pivot_wider(names_from = period, values_from = B)
print(as.data.frame(tot))
