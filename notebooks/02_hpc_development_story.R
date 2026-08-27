# ==============================================================================
# Notebook 02: HPC & Performance Benchmarking Story
# Showcase: Matrix factorization, temporal execution timing, and resource tracking
# ==============================================================================

# 1. Initialize High-Performance Parameters
matrix_size <- 400
n_iterations <- 5
execution_log <- vector("list", n_iterations)

# 2. Iterative Matrix Computations & Timing
for (i in 1:n_iterations) {
  start_time <- Sys.time()
  
  # Generate synthetic dense matrix and compute singular value decomposition
  A <- matrix(rnorm(matrix_size * matrix_size), nrow = matrix_size, ncol = matrix_size)
  svd_res <- svd(A)
  
  elapsed_ms <- as.numeric(difftime(Sys.time(), start_time, units = "secs")) * 1000
  
  execution_log[[i]] <- data.frame(
    iteration = i,
    matrix_dim = paste0(matrix_size, "x", matrix_size),
    singular_val_max = max(svd_res$d),
    elapsed_ms = round(elapsed_ms, 2)
  )
}

# 3. Consolidate Benchmark Results
benchmark_results <- do.call(rbind, execution_log)

# 4. Generate Performance Report Artifact
saveRDS(benchmark_results, "hpc_benchmark_metrics.rds")

cat("HPC Benchmark Story execution complete. Average iteration time:", 
    mean(benchmark_results$elapsed_ms), "ms\n")
