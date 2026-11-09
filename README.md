# R code for the main figures of "Should CDR Be Phased Out After 2100? Investigating the Long-term Sustainability of Carbon Dioxide Removal"

This folder draws Figs. 2 to 5 of the paper from the figure data in `data/`. Fig. 1 is a schematic drawing and has no code.

## How to run

1. Download and unzip this folder (any location; spaces in the path are fine).
2. Run the four figures in one of these ways:
   - Terminal: `Rscript run_all.R` from this folder, or `Rscript /path/to/CDRPhaseDown_figure_code/run_all.R` from anywhere.
   - RStudio: open `CDRPhaseDown_figure_code.Rproj`, then run `source("run_all.R")`.
   - One figure only: run its `Figure N ... .R` script in the same way.
3. The figures are written to `output/` (`Figure_2.png` to `Figure_5.png`); single panels are in `output/panels/`.

No path needs to be edited. Missing R packages are installed from CRAN on the first run. Each figure runs in its own R session.
`Rscript "Figure 5 Trade-offs across the scenarios that phase out CDR.R" si_long` draws Fig. S19 (Fig. 5 with No Phase-out and two long-term rows).

## Requirements

- R 4.3 or later.
- R packages: ggplot2, dplyr, tidyr, patchwork, ragg, scales, tidyverse, cowplot, ggtext, ggrepel.
- Tested on macOS with R 4.4.3, ggplot2 3.5.2, dplyr 1.1.4, tidyr 1.3.1, patchwork 1.3.0, ragg 1.4.0, scales 1.4.0, cowplot 1.1.3,
  ggtext 0.1.2 and ggrepel 0.9.6: the four figures are identical, pixel for pixel, to the published ones.
- The figures use the Helvetica font. Where it is not installed, a similar sans-serif font is used, so text positions may differ slightly.

## Scripts

| Script | Figure | Panels from |
|---|---|---|
| `Figure 2 Emissions, temperature and CDR deployment.R` | Fig. 2 | `R/fig2a_emissions_temperature.R`, `R/fig2bcd_cdr_storage.R`, `R/fig2e_cdr_by_technology.R` |
| `Figure 3 Stranded capital and the energy system.R` | Fig. 3 | `R/fig3_stranding_cache.R`, `R/fig3abc_stranded_capital.R`, `R/fig3def_electricity.R` |
| `Figure 4 Mitigation costs, climate and land impacts, and their distribution across generations.R` | Fig. 4 | (one script) |
| `Figure 5 Trade-offs across the scenarios that phase out CDR.R` | Fig. 5 | (one script) |

`R/style.R` holds the shared colours and text sizes; `R/setup.R` installs missing packages and creates `output/`.

## Data (`data/`)

All files are extracted from the AIM-Hub model results and the post-processing described in the Methods of the paper.

| File | Content |
|---|---|
| `emis_co2.csv` | Net CO₂ emissions (total, energy and industrial processes, AFOLU), World, 2005 to 2150, Mt CO₂/yr (IAMC format) |
| `gsat_p50.csv` | Median global mean surface air temperature above pre-industrial, 600-member probabilistic ensemble of MAGICC v7.5.3, K |
| `cdr_iamc.csv` | CDR by technology (IAMC variables `Carbon Removal|...`), World |
| `ccs_iamc.csv` | CO₂ stored in geological formations by capture source (IAMC variables `Carbon Capture|Geological Storage|...`) |
| `iamc_fig.csv` | Electricity supply and electricity used by DACCS (IAMC format) |
| `heatmap_iamc_11.csv` | CCS and DACCS storage, DACCS electricity, electricity supply, population at risk of hunger and food price index (long format) |
| `si_iamc.csv` | Land used for energy crops, World (long format; only the rows the scripts use) |
| `sav_<scenario>.csv` | Stranded asset value by year (`Y`), region (`R`) and technology (`A`), billion US$ (2005) per year, from the stranded-assets module of AIM-Hub; `DAC` + `STO` = DACCS, `E_*` = power plants |
| `copr_tpcc_dac.csv`, `copr_hcc2110.csv` | Capacity utilisation rate (0 to 1) by year, region and technology: DACCS in TPCC, all technologies in HCC_2110 |
| `fig4_welfare_annual.csv` | GDP and household consumption loss relative to the baseline without climate policy, annual and discounted at 1, 3 and 5% |
| `ramsey_liu_weights.csv` | Ramsey discount factors by year (elasticity of marginal utility 2, no pure time preference); `tf` = main runs |
| `burke_world_11.csv` | Change in world GDP due to climate change following Burke et al. (2015), % |
| `takakura_world_11.csv` | Change in world GDP due to climate change following the sectoral emulators of Takakura et al., % |

## Scenario names

| In the data | In the paper |
|---|---|
| `TPCC_Ext` (`TPCC_Flat` in the IAMC files) | TPCC |
| `HCC_2110`, `HCC_2150` | HCC_2110, HCC_2150 (high reliance on CDR) |
| `Med_2110` to `Med_2150` | MCC_2110 to MCC_2150 (medium reliance) |
| `LCC_2110`, `LCC_2150` | LCC_2110, LCC_2150 (low reliance) |
| `400C_2025CP` | No Phase-out |
| `BaU` | Baseline without climate policy |
| `1100C_2110` to `1100C_2150`, `1100C_2025CP` | MCC scenarios and No Phase-out with the 1,100 GtCO₂ budget (sensitivity set, IAMC files only) |

In the IAMC files every name carries the prefix `SSP2_` and the suffix `_NoCC_No`. The continued-trend and built-in-trends files
also hold the six runs of the 1,100 GtCO₂ budget set under those trends; the paper does not use them (Table S1).

## Full model results

The full model results in the IAMC template (all variables, 17 regions and the World, to 2150) for the main scenarios and the three
sensitivity sets (1,100 GtCO₂ budget, continued trends after 2100, built-in trends after 2100) are in the companion Zenodo dataset.

## License

CC BY 4.0
