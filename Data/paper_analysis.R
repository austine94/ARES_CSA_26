# =============================================================================
# paper_plots.R
#
# Produces publication-ready resilience curve plots and a summary table.
#
# Plots:
#   1. W1: Availability resilience curve
#   2. W2: Availability resilience curve
#   3. W2: Completion rate resilience curve
#   4. W2: Composite (0.5 avail + 0.5 completion)
#   5. W3: Composite (0.5 CPU + 0.5 machines)
#   6. W4: Composite (all 4 metrics)
#
# All plots use hours since trace start on the x-axis, no titles, and
# labelled y-axes.
#
# Table: robustness, rapidity, R for each plot.
# =============================================================================

library(data.table)
source("Code/resilience_curve.R")

# ---- Custom plot function for paper -----------------------------------------
# Replaces rc_plot with a cleaner version: no title, time in hours on x-axis,
# named y-axis.

paper_plot <- function(p, t_centre_sec, t_start, t_end, metrics,
                       ylab = expression(tilde(P)(t)),
                       ylim = NULL) {
  t_hours <- t_centre_sec / 3600
  if (is.null(ylim)) ylim <- range(c(p, 0, 1), na.rm = TRUE)
  
  plot(t_hours, p, type = "l", lwd = 2, col = "steelblue",
       xlab = "Hours since trace start", ylab = ylab,
       ylim = ylim, main = "")
  
  # Shade disruption window
  rect(t_hours[t_start], ylim[1], t_hours[t_end], ylim[2],
       col = rgb(1, 0, 0, 0.07), border = NA)
  abline(v = c(t_hours[t_start], t_hours[t_end]), col = "red", lty = 2)
  
  if (!is.null(metrics)) {
    abline(h = metrics$baseline, col = "darkgreen", lty = 3)
    points(t_hours[metrics$trough_index], p[metrics$trough_index],
           pch = 19, col = "red")
    if (!is.na(metrics$recovery_index)) {
      points(t_hours[metrics$recovery_index], p[metrics$recovery_index],
             pch = 19, col = "darkgreen")
      segments(t_hours[metrics$trough_index], p[metrics$trough_index],
               t_hours[metrics$recovery_index], p[metrics$recovery_index],
               col = "darkgreen", lwd = 2)
    }
    legend("bottomright",
           legend = c(expression(tilde(P)(t)), "baseline", "trough",
                      "recovery point", "disruption window"),
           col = c("steelblue", "darkgreen", "red", "darkgreen", "red"),
           lty = c(1, 3, NA, NA, 2),
           pch = c(NA, NA, 19, 19, NA),
           bty = "n", cex = 0.8)
  }
}

# ---- Results collector -------------------------------------------------------

results <- data.frame(
  Scenario    = character(),
  Robustness  = numeric(),
  Rapidity    = numeric(),
  R           = numeric(),
  stringsAsFactors = FALSE
)

add_result <- function(label, metrics) {
  results[nrow(results) + 1, ] <<- list(
    Scenario   = label,
    Robustness = round(metrics$robustness, 4),
    Rapidity   = ifelse(is.na(metrics$rapidity), NA, round(metrics$rapidity, 4)),
    R          = round(metrics$R, 4)
  )
}

# =============================================================================
# W1: Availability
# =============================================================================

w1 <- fread("Data/window_W1_availability.csv")
dis_t_start_W1 <- 15
dis_t_end_W1   <- 34

res_W1 <- rc_single(
  p         = w1$non_failure_event_rate,
  t_start   = dis_t_start_W1,
  t_end     = dis_t_end_W1,
  window    = 5,
  normalise = FALSE,
  recovery_threshold = 0.999,
  plot      = FALSE
)

paper_plot(res_W1$curve, w1$t_centre_sec,
           dis_t_start_W1, dis_t_end_W1, res_W1$metrics,
           ylab = "Non-failure event rate")
add_result("W1: Availability", res_W1$metrics)

# =============================================================================
# W2: Availability
# =============================================================================

w2 <- fread("Data/window_W2_availability_completion.csv")
dis_t_start_W2 <- 12
dis_t_end_W2   <- 38

res_W2_avail <- rc_single(
  p         = w2$non_failure_event_rate,
  t_start   = dis_t_start_W2,
  t_end     = dis_t_end_W2,
  window    = 9,
  normalise = FALSE,
  recovery_threshold = 0.9,
  plot      = FALSE,
  baseline_value = w2$non_failure_event_rate[dis_t_start_W2]
)

paper_plot(res_W2_avail$curve, w2$t_centre_sec,
           dis_t_start_W2, dis_t_end_W2, res_W2_avail$metrics,
           ylab = "Non-failure event rate")
add_result("W2: Availability", res_W2_avail$metrics)

# =============================================================================
# W2: Completion rate
# =============================================================================

res_W2_comp <- rc_single(
  p         = w2$completion_rate,
  t_start   = dis_t_start_W2,
  t_end     = dis_t_end_W2,
  window    = 7,
  normalise = FALSE,
  recovery_threshold = 0.92,
  plot      = FALSE,
  baseline_value = w2$completion_rate[dis_t_start_W2]
)

paper_plot(res_W2_comp$curve, w2$t_centre_sec,
           dis_t_start_W2, dis_t_end_W2, res_W2_comp$metrics,
           ylab = "Completion rate")
add_result("W2: Completion rate", res_W2_comp$metrics)

# =============================================================================
# W2: Composite (0.5 / 0.5)
# =============================================================================

df_comp_W2 <- data.frame(
  non_failure_event_rate = w2$non_failure_event_rate,
  completion_rate        = w2$completion_rate
)

res_W2_composite <- rc_composite(
  df      = df_comp_W2,
  weights = c(0.5, 0.5),
  t_start = dis_t_start_W2,
  t_end   = dis_t_end_W2,
  window  = 7,
  normalise = FALSE,
  recovery_threshold = 0.92,
  plot    = FALSE,
  baseline_value = 0.5 * w2$completion_rate[dis_t_start_W2] +
    0.5 * w2$non_failure_event_rate[dis_t_start_W2]
)

paper_plot(res_W2_composite$composite, w2$t_centre_sec,
           dis_t_start_W2, dis_t_end_W2, res_W2_composite$metrics,
           ylab = "Aggregation of Non-Failure + Completion")
add_result("W2: Aggregation", res_W2_composite$metrics)

# =============================================================================
# W3: Composite (0.5 CPU + 0.5 machines)
# =============================================================================

w3 <- fread("Data/window_W3_cpu_machines.csv")
dis_t_start_W3 <- 11
dis_t_end_W3   <- 42

df_comp_W3 <- data.frame(
  cpu_per_machine = w3$cpu_per_machine,
  active_machines = w3$active_machines
)

res_W3_composite <- rc_composite(
  df      = df_comp_W3,
  weights = c(0.5, 0.5),
  t_start = dis_t_start_W3,
  t_end   = dis_t_end_W3,
  window  = 9,
  normalise = TRUE,
  recovery_threshold = 0.999,
  plot    = FALSE
)

paper_plot(res_W3_composite$composite, w3$t_centre_sec,
           dis_t_start_W3, dis_t_end_W3, res_W3_composite$metrics,
           ylab = "Aggregation of CPU and Active Machines)")
add_result("W3: Aggregation", res_W3_composite$metrics)

# =============================================================================
# W4: All 4 metrics composite
# =============================================================================

d <- fread("Data/borg_signals_15min.csv")
d[, t_days := t_centre_sec / 86400]

w4 <- d[t_days >= 0.80 & t_days <= 1.30]

dis_rows <- which(w4$t_days >= 0.95 & w4$t_days <= 1.15)
dis_t_start_W4 <- min(dis_rows)
dis_t_end_W4   <- max(dis_rows)

cpu_smooth  <- rc_smooth(w4$cpu_per_machine, window = 5)
cpu_norm    <- rc_normalise(cpu_smooth)
mach_smooth <- rc_smooth(w4$active_machines, window = 5)
mach_norm   <- rc_normalise(mach_smooth)

df_comp_W4 <- data.frame(
  availability    = w4$non_failure_event_rate,
  completion_rate = w4$completion_rate,
  cpu_per_machine = cpu_norm,
  active_machines = mach_norm
)

res_W4_composite <- rc_composite(
  df      = df_comp_W4,
  weights = c(0.05, 0.40, 0.40, 0.15),
  t_start = dis_t_start_W4,
  t_end   = dis_t_end_W4,
  window  = 5,
  normalise = FALSE,
  recovery_threshold = 0.95,
  plot    = FALSE
)

paper_plot(res_W4_composite$composite, w4$t_centre_sec,
           dis_t_start_W4, dis_t_end_W4, res_W4_composite$metrics,
           ylab = "Aggregation of all 4 Metrics")
add_result("W4: Aggregation of all 4 metrics", res_W4_composite$metrics)

# =============================================================================
# Summary table
# =============================================================================

cat("\n")
cat("=================================================================\n")
cat("  Resilience metrics summary\n")
cat("=================================================================\n")
print(results, row.names = FALSE)
cat("=================================================================\n")
