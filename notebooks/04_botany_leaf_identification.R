# ==============================================================================
# Notebook 04: Botany & Leaf Morphological Identification
# Showcase: Plant specimen morphometrics, feature engineering, and species classification
# ==============================================================================

# 1. Simulate Plant Specimen Measurement Collection
set.seed(108)
n_specimens <- 120
species_list <- c("Acer_rubrum", "Quercus_alba", "Ginkgo_biloba", "Rosa_carolina")

specimen_data <- data.frame(
  specimen_id = paste0("BOT-", 1000 + 1:n_specimens),
  species = factor(sample(species_list, n_specimens, replace = TRUE)),
  leaf_length_cm = round(runif(n_specimens, min = 4.5, max = 18.0), 2),
  leaf_width_cm = round(runif(n_specimens, min = 2.0, max = 14.0), 2),
  vein_density_per_cm2 = round(rnorm(n_specimens, mean = 24.5, sd = 4.2), 1),
  chlorophyll_index = round(runif(n_specimens, min = 32.0, max = 78.0), 1)
)

# 2. Morphological Feature Engineering & Trait Calculation
specimen_data$aspect_ratio <- round(specimen_data$leaf_length_cm / specimen_data$leaf_width_cm, 3)
specimen_data$leaf_area_approx_cm2 <- round(0.68 * specimen_data$leaf_length_cm * specimen_data$leaf_width_cm, 2)

# Filter valid botanical observations
valid_specimens <- specimen_data[specimen_data$aspect_ratio >= 0.8 & specimen_data$vein_density_per_cm2 > 10, ]

# 3. Botanical Species Morphometrics Aggregation
species_morphometrics <- aggregate(
  cbind(aspect_ratio, leaf_area_approx_cm2, vein_density_per_cm2, chlorophyll_index) ~ species,
  data = valid_specimens,
  FUN = function(x) round(mean(x), 2)
)

# 4. Diagnostic Morphological Scatter Plot Generation
png("botany_leaf_morphology.png", width = 800, height = 550)
palette(c("#2e7d32", "#1565c0", "#e65100", "#6a1b9a"))
plot(
  valid_specimens$leaf_area_approx_cm2, valid_specimens$aspect_ratio,
  col = valid_specimens$species, pch = 19, cex = 1.3,
  main = "Botany Specimen Morphometrics: Leaf Area vs Aspect Ratio",
  xlab = "Approximate Leaf Area (cm²)",
  ylab = "Aspect Ratio (Length / Width)"
)
legend(
  "topright", legend = levels(valid_specimens$species),
  col = 1:4, pch = 19, bty = "n", title = "Botanical Species"
)
grid()
dev.off()

# 5. Export Botanical Catalog Artifact
saveRDS(species_morphometrics, "botany_specimen_catalog.rds")

cat("Botany leaf identification analysis completed successfully! Cataloged", 
    nrow(valid_specimens), "specimens across", length(levels(valid_specimens$species)), "species.\n")
