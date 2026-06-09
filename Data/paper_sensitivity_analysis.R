# =============================================================================
# sensitivity_analysis.R
#
# Two analyses:
#   1. Scenario 2 sensitivity sweep: robustness and rapidity as weight shifts
#      from non-failure event rate to completion rate
#   2. Scenario 4 Dirichlet sampling: mean and variance of resilience measures
#      over n random weight vectors drawn from Dirichlet(1,1,1,1)
# =============================================================================

library(data.table)
source("Code/resilience_curve.R")

# =============================================================================
# 1. Scenario 2: sensitivity sweep
# =============================================================================

w2 <- fread("Data/window_W2_availability_completion.csv")
dis_t_start_W2 <- 12
dis_t_end_W2   <- 38

df_W2 <- data.frame(
  non_failure_event_rate = w2$non_failure_event_rate,
  completion_rate        = w2$completion_rate
)

w_seq <- seq(0, 1, by = 0.05)
sens <- data.frame(
  w_availability = w_seq,
  robustness     = NA_real_,
  rapidity       = NA_real_
)

for (i in seq_along(w_seq)) {
  wa <- w_seq[i]
  wc <- 1 - wa
  
  res_i <- rc_composite(
    df      = df_W2,
    weights = c(wa, wc),
    t_start = dis_t_start_W2,
    t_end   = dis_t_end_W2,
    window  = 7,
    normalise = FALSE,
    recovery_threshold = 0.92,
    plot    = FALSE,
    baseline_value = wa * w2$non_failure_event_rate[dis_t_start_W2] +
      wc * w2$completion_rate[dis_t_start_W2]
  )
  
  sens$robustness[i] <- res_i$metrics$robustness
  sens$rapidity[i]   <- res_i$metrics$rapidity
}

# ---- Sensitivity plot --------------------------------------------------------

par(mfrow = c(1, 1), mar = c(5, 4, 1, 4) + 0.1)

plot(sens$w_availability, sens$robustness, type = "b", pch = 19,
     col = "steelblue", lwd = 2,
     ylim = range(sens$robustness, na.rm = TRUE),
     xlab = "Weight on non-failure event rate (1 - weight on completion rate)",
     ylab = "Robustness", main = "")
par(new = TRUE)
plot(sens$w_availability, sens$rapidity, type = "b", pch = 17,
     col = "darkred", lwd = 2, axes = FALSE, xlab = "", ylab = "",
     ylim = range(sens$rapidity, na.rm = TRUE))
axis(side = 4)
mtext("Recovery Slope", side = 4, line = 3)
legend("topleft",
       legend = c("Robustness", "Rapidity"),
       col = c("steelblue", "darkred"),
       pch = c(19, 17), lty = 1, lwd = 2, bty = "n")

cat("\n--- Scenario 2 sensitivity sweep ---\n")
print(sens)

# =============================================================================
# 2. Scenario 4: Dirichlet sampling
# =============================================================================

# ---- Load and prepare W4 data (same as all_4_metrics.R) ---------------------

d <- fread("Data/borg_signals_15min.csv")
d[, t_days := t_centre_sec / 86400]

w4 <- d[t_days >= 0.80 & t_days <= 1.30]
dis_rows <- which(w4$t_days >= 0.95 & w4$t_days <= 1.15)
dis_t_start_W4 <- min(dis_rows)
dis_t_end_W4   <- max(dis_rows)

# Pre-normalise CPU and active machines (same approach as all_4_metrics.R)
cpu_smooth  <- rc_smooth(w4$cpu_per_machine, window = 5)
cpu_norm    <- rc_normalise(cpu_smooth)
mach_smooth <- rc_smooth(w4$active_machines, window = 5)
mach_norm   <- rc_normalise(mach_smooth)

df_W4 <- data.frame(
  availability    = w4$non_failure_event_rate,
  completion_rate = w4$completion_rate,
  cpu_per_machine = cpu_norm,
  active_machines = mach_norm
)

# ---- Dirichlet sampler ------------------------------------------------------
# Dirichlet(alpha) can be sampled via normalised Gamma variates

rdirichlet <- function(n, alpha) {
  k <- length(alpha)
  draws <- matrix(rgamma(n * k, shape = rep(alpha, each = n)), nrow = n, ncol = k)
  draws / rowSums(draws)
}

# ---- Run n iterations -------------------------------------------------------

set.seed(42)
n_samples <- 10000
alpha     <- c(1, 1, 1, 1)   # uniform over the 4-simplex

cat(sprintf("\nDrawing %d weight vectors from Dirichlet(%s)...\n",
            n_samples, paste(alpha, collapse = ", ")))

weight_draws <- rdirichlet(n_samples, alpha)

dirichlet_results <- data.frame(
  robustness      = numeric(n_samples),
  rapidity        = numeric(n_samples),
  R               = numeric(n_samples)
)

# Progress counter
cat("Running composite resilience for each weight vector...\n")
for (i in seq_len(n_samples)) {
  w_i <- weight_draws[i, ]
  
  res_i <- rc_composite(
    df      = df_W4,
    weights = w_i,
    t_start = dis_t_start_W4,
    t_end   = dis_t_end_W4,
    window  = 5,
    normalise = FALSE,
    recovery_threshold = 0.95,
    plot    = FALSE
  )
  
  dirichlet_results$robustness[i] <- res_i$metrics$robustness
  dirichlet_results$rapidity[i]   <- res_i$metrics$rapidity
  dirichlet_results$R[i]          <- res_i$metrics$R
  
  if (i %% 2000 == 0) cat(sprintf("  %d / %d\n", i, n_samples))
}

# ---- Summary table -----------------------------------------------------------

n_rapidity_na <- sum(is.na(dirichlet_results$rapidity))

summary_table <- data.frame(
  Metric   = c("Robustness", "Recovery Slope", "R"),
  Mean     = c(mean(dirichlet_results$robustness, na.rm = TRUE),
               mean(dirichlet_results$rapidity, na.rm = TRUE),
               mean(dirichlet_results$R, na.rm = TRUE)),
  Variance = c(var(dirichlet_results$robustness, na.rm = TRUE),
               var(dirichlet_results$rapidity, na.rm = TRUE),
               var(dirichlet_results$R, na.rm = TRUE))
)

summary_table$Mean     <- round(summary_table$Mean, 4)
summary_table$Variance <- round(summary_table$Variance, 6)

cat("\n===================================================================\n")
cat("  Scenario 4: Dirichlet(1,1,1,1) weight sensitivity\n")
cat(sprintf("  n = %d samples\n", n_samples))
if (n_rapidity_na > 0) {
  cat(sprintf("  Note: rapidity was NA in %d / %d samples (%.1f%%)\n",
              n_rapidity_na, n_samples, 100 * n_rapidity_na / n_samples))
  cat("  (recovery not achieved within window for those weight combos)\n")
}
cat("===================================================================\n")
print(summary_table, row.names = FALSE)
cat("===================================================================\n")