# ==============================================================================
# Notebook 01: Everyday Data Science & Visualization Pipeline
# Showcase: Data ingestion, filtering, aggregation, and plot generation
# ==============================================================================

# 1. Simulate Raw Data Ingestion
set.seed(42)
sample_dates <- seq(as.Date("2026-01-01"), as.Date("2026-01-30"), by = "day")
raw_data <- data.frame(
  date = rep(sample_dates, each = 5),
  sensor_id = paste0("SN-", rep(1:5, times = length(sample_dates))),
  reading = round(runif(150, min = 15.0, max = 95.0), 2),
  status = sample(c("OK", "OK", "OK", "WARN", "ERR"), 150, replace = TRUE)
)

# 2. Data Cleaning & Filtering
clean_data <- raw_data[raw_data$status != "ERR", ]
filtered_data <- clean_data[clean_data$reading >= 20.0, ]

# 3. Statistical Summarization
daily_summary <- aggregate(reading ~ date, data = filtered_data, FUN = function(x) {
  c(mean = mean(x), sd = sd(x), min = min(x), max = max(x))
})
daily_summary <- data.frame(
  date = daily_summary$date,
  mean_reading = daily_summary$reading[, "mean"],
  sd_reading = daily_summary$reading[, "sd"]
)

# 4. Artifact Generation & Visualization
png("sensor_daily_trends.png", width = 800, height = 500)
plot(daily_summary$date, daily_summary$mean_reading, 
     type = "b", col = "#4a6fa5", pch = 19, lwd = 2,
     main = "Daily Sensor Reading Trends",
     xlab = "Date", ylab = "Mean Reading")
grid()
dev.off()

cat("Pipeline completed successfully! Processed", nrow(filtered_data), "valid readings.\n")
