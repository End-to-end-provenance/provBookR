# ==============================================================================
# Notebook 05: Forest Ecology & Ecosystem Monitoring
# Showcase: Longitudinal forest canopy dynamics, tree biomass, and diversity indices
# ==============================================================================

# 1. Simulate Forest Inventory & Plot Survey Data
set.seed(204)
years <- 2020:2026
plots <- paste0("PLOT-", sprintf("%02d", 1:8))

plot_survey <- expand.grid(year = years, plot_id = plots)
n_obs <- nrow(plot_survey)

plot_survey$dbh_cm <- round(runif(n_obs, min = 12.0, max = 65.0), 1)
plot_survey$canopy_pct <- round(pmin(98, pmax(35, rnorm(n_obs, mean = 75, sd = 12))), 1)
plot_survey$stem_count <- sample(45:180, n_obs, replace = TRUE)
plot_survey$species_richness <- sample(4:16, n_obs, replace = TRUE)

# 2. Ecological Allometric Biomass & Diversity Calculations
# Allometric equation for aboveground tree biomass (AGB in Mg/ha)
plot_survey$agb_biomass_mg_ha <- round(0.0673 * (plot_survey$dbh_cm ^ 2.47) * (plot_survey$stem_count / 1000), 2)

# Shannon Diversity Index approximation: H = log(richness) * (canopy_pct / 100)
plot_survey$shannon_index <- round(log(plot_survey$species_richness) * (plot_survey$canopy_pct / 100), 3)

# 3. Longitudinal Ecosystem Aggregation
annual_ecology_summary <- aggregate(
  cbind(dbh_cm, canopy_pct, agb_biomass_mg_ha, shannon_index) ~ year,
  data = plot_survey,
  FUN = function(x) round(mean(x), 2)
)

# 4. Generate Forest Biomass & Canopy Trend Diagnostics
png("forest_canopy_biomass_trends.png", width = 850, height = 550)
par(mfrow = c(1, 2), mar = c(4.5, 4.5, 3, 1))

# Panel A: Biomass Trajectory over Years
plot(
  annual_ecology_summary$year, annual_ecology_summary$agb_biomass_mg_ha,
  type = "b", col = "#1b5e20", lwd = 2.5, pch = 18, cex = 1.5,
  main = "Aboveground Biomass (Mg/ha)",
  xlab = "Survey Year", ylab = "Mean Biomass (Mg/ha)"
)
grid()

# Panel B: Canopy Cover vs Shannon Diversity Index
plot(
  plot_survey$canopy_pct, plot_survey$shannon_index,
  col = adjustcolor("#004d40", alpha.f = 0.6), pch = 16, cex = 1.2,
  main = "Canopy Cover vs Shannon Index",
  xlab = "Canopy Cover (%)", ylab = "Shannon Diversity (H')"
)
abline(lm(shannon_index ~ canopy_pct, data = plot_survey), col = "#d84315", lwd = 2, lty = 2)
grid()

dev.off()

# 5. Save Reproducible Forest Assessment Artifact
saveRDS(annual_ecology_summary, "forest_ecology_assessment.rds")

cat("Forest Ecology Analysis completed successfully! Monitored", 
    length(plots), "survey plots over", length(years), "years.\n")
