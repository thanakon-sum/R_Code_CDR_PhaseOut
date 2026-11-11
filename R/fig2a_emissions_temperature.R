library(tidyverse)

sc_map <- c(
  "SSP2_400C_2025CP_NoCC_No"="No Phase-out","SSP2_TPCC_Ext_NoCC_No"="TPCC",
  "SSP2_Med_2110_NoCC_No"="MCC_2110","SSP2_Med_2120_NoCC_No"="MCC_2120","SSP2_Med_2130_NoCC_No"="MCC_2130",
  "SSP2_Med_2140_NoCC_No"="MCC_2140","SSP2_Med_2150_NoCC_No"="MCC_2150",
  "SSP2_HCC_2110_NoCC_No"="HCC_2110","SSP2_HCC_2150_NoCC_No"="HCC_2150",
  "SSP2_LCC_2110_NoCC_No"="LCC_2110","SSP2_LCC_2150_NoCC_No"="LCC_2150"
)

# Colour palette grouped by CDR type
pal <- c(
  "No Phase-out" = "#E41A1C",   # red, 400C baseline (extended CDR)
  "TPCC" = "#A65628",   # brown, TPCC (stop at 2100)
  "HCC_2110" = "#1F78B4",   # dark blue
  "HCC_2150" = "#A6CEE3",   # light blue
  "LCC_2110" = "#6A3D9A",   # dark purple
  "LCC_2150" = "#CAB2D6",   # light purple
  "MCC_2110" = "#FF7F00",   # dark orange
  "MCC_2120" = "#FDBF6F",   # light orange
  "MCC_2130" = "#33A02C",   # dark green
  "MCC_2140" = "#B2DF8A",   # light green
  "MCC_2150" = "#1A9850"    # medium green
)

# Read emissions (Mt CO2/yr -> Gt CO2/yr)
em <- read.csv("data/emis_co2.csv", check.names=FALSE, stringsAsFactors=FALSE)
em <- em[em$Variable=="Emissions|CO2" & !is.na(sc_map[em$Scenario]),]
yrs <- names(em)[grepl("^[0-9]{4}$", names(em))]
emi_df <- data.frame(Year=rep(as.integer(yrs), each=nrow(em)),
                     CO2_Gt=as.numeric(unlist(em[, yrs]))/1000,
                     scenario=rep(unname(sc_map[em$Scenario]), times=length(yrs)), stringsAsFactors=FALSE)
emi_df <- emi_df[emi_df$Year>=2005 & emi_df$Year<=2150,]

# Read temperature (climate-assessment 50th pct GSAT)
ca <- read.csv("data/gsat_p50.csv", stringsAsFactors=FALSE)
# Map scenario names
ca$display <- sc_map[ca$Scenario]
ca <- ca[!is.na(ca$display),]

# Get year columns (either Xnnnn or nnnn)
yr_all <- names(ca)[grepl("^X?[0-9]{4}$", names(ca))]
yr_use <- yr_all[as.integer(sub("^X","",yr_all)) >= 2005 & as.integer(sub("^X","",yr_all)) <= 2150]

temp_long <- reshape(ca[,c("display", yr_use)],
                     direction="long", varying=yr_use,
                     v.names="Temp", timevar="YrStr", times=yr_use)
temp_long$Year  <- as.integer(sub("^X","",temp_long$YrStr))
temp_long$scenario <- temp_long$display
temp_long$Temp  <- as.numeric(temp_long$Temp)
temp_long <- temp_long[temp_long$Year>=2005 & temp_long$Year<=2150,]

cat("Emissions rows:", nrow(emi_df), "\n")
cat("Temperature rows:", nrow(temp_long), "\n")

# Dual axis — temp floats in upper zone
emi_range  <- range(emi_df$CO2_Gt, na.rm=TRUE)
temp_range <- range(temp_long$Temp[temp_long$Year>=2025], na.rm=TRUE)
cat("Temp range:", round(temp_range,3), "\n")
temp_y_lo <- 8; temp_y_hi <- 42
to_emi  <- function(t) (t-temp_range[1])/(diff(temp_range))*(temp_y_hi-temp_y_lo)+temp_y_lo
to_temp <- function(e) (e-temp_y_lo)/(temp_y_hi-temp_y_lo)*diff(temp_range)+temp_range[1]

sc_order <- c("No Phase-out","TPCC","HCC_2110","HCC_2150",
              "MCC_2110","MCC_2120","MCC_2130","MCC_2140","MCC_2150",
              "LCC_2110","LCC_2150")
emi_df$scenario    <- factor(emi_df$scenario,    levels=sc_order)
temp_long$scenario <- factor(temp_long$scenario, levels=sc_order)
temp_long$Temp_scaled <- to_emi(temp_long$Temp)
emi_df$ltype    <- "Emissions (solid)"
temp_long$ltype <- "Temperature (dashed)"

#    TPCC is at -3.4 in 2100 and reaches the floor by 2110, so the floor is read at 2150; before: TPCC at 2100) ──
emi_tbl <- as_tibble(emi_df)
residual_level <- emi_tbl |>
  filter(scenario == "TPCC", Year == 2150) |>
  pull(CO2_Gt)
cat("Residual negative emission level (post-2100 floor, TPCC @ 2150):", round(residual_level, 2), "GtCO2/yr\n")

p <- ggplot() +
  geom_hline(yintercept=0, colour="grey70", linewidth=0.4) +
  geom_hline(yintercept=to_emi(1.5), linetype="dotted", colour="#E41A1C",
             linewidth=0.6, alpha=0.7) +
  geom_vline(xintercept=2025, linetype="dashed", colour="grey60", linewidth=0.5) +
  # Residual negative emissions reference line (2100_Ext level at 2100)
  geom_hline(yintercept=residual_level, linetype="dashed", colour="grey30",
             linewidth=0.6, alpha=0.7) +
  geom_line(data=emi_df,   aes(x=Year, y=CO2_Gt,       colour=scenario, linetype=ltype),
            linewidth=0.85, na.rm=TRUE) +
  geom_line(data=temp_long,aes(x=Year, y=Temp_scaled,  colour=scenario, linetype=ltype),
            linewidth=0.7,  na.rm=TRUE) +
  scale_colour_manual(values=pal, name=NULL,
    guide=guide_legend(nrow=3, byrow=TRUE,
                       override.aes=list(linetype="solid", linewidth=1.1))) +
  scale_linetype_manual(
    values=c("Emissions (solid)"="solid","Temperature (dashed)"="dashed"),
    name=NULL,
    guide=guide_legend(override.aes=list(colour="grey30",linewidth=1))) +
  scale_x_continuous(breaks=seq(2000,2150,25), limits=c(2005,2150)) +
  scale_y_continuous(
    name=expression("Annual CO"[2]*" emissions (GtCO"[2]*" yr"^{-1}*")"),
    limits=c(emi_range[1], max(emi_range[2], temp_y_hi + 0.5)),
    sec.axis=sec_axis(~to_temp(.),
      name="Global mean temperature above pre-industrial (°C)",
      breaks=seq(1.0,1.8,0.1))
  ) +
  annotate("text",x=2026,y=emi_range[1]*0.92,label="Policy start (2025)",
           hjust=0,size=2.8,colour="grey35") +
  annotate("text", x=2028, y=residual_level - 2.4,
           label=paste0("Residual emission\n(", sub("-", "\u2212", sprintf("%.2f", residual_level)), " GtCO\u2082 yr\u207b\u00b9)"), lineheight=0.95,
           hjust=0, vjust=1, size=2.8, colour="grey30", fontface="italic") +
  annotate("text",x=2140,y=to_emi(1.5)+0.9,vjust=0,label="1.5°C",  # bottom-anchored: clears the dotted line at any size
           hjust=0,size=2.8,colour="#E41A1C") +
  labs(x=NULL) +
  theme_classic(base_size=11) +
  theme(panel.border=element_rect(colour="black", fill=NA, linewidth=0.5)) +  # closed box: top line added
  theme(legend.position="bottom", legend.text=element_text(size=8.5),
        legend.key.width=unit(1.0,"cm"),
        legend.spacing.x=unit(0.3,"cm"),
        axis.title.y.right=element_text(colour="grey40"),
        axis.text.y.right=element_text(colour="grey40"),
        panel.grid.major=element_blank(),  # grid removed on request
        plot.background=element_rect(fill="white",colour=NA))

print(p)
ggsave("output/panels/fig1_emissions_temp_all_fresh.png", p, width=14, height=7, dpi=180, bg="white", device=ragg::agg_png)
cat("Saved.\n")

