# =============================================================================
# paper_illustrations.R
#
# Two illustrative diagrams for the paper:
#   1. An idealised resilience curve with raw noisy data + smoothed curve,
#      and robustness, rapidity, and resilience loss annotated
#   2. Failure rate vs success rate to illustrate metric direction
# =============================================================================

t <- seq(0, 100, by = 0.1)

# ---- Plot 1: Idealised resilience curve with annotations --------------------

# Clean underlying signal
dip_clean <- ifelse(t < 8, 1,
                    ifelse(t < 73, 0.65 + 0.35 * cos(pi * (t - 8) / 32),
                           1))

# Add noise to simulate raw data
set.seed(42)
dip_noisy <- dip_clean + rnorm(length(t), mean = 0, sd = 0.06)
dip_noisy <- pmin(pmax(dip_noisy, 0), 1.0)
# Smooth using local polynomial (LOESS)
lo <- loess(dip_noisy ~ t, span = 0.12)
dip_smooth <- predict(lo, t)
dip_smooth <- pmin(pmax(dip_smooth, 0), 1.05)

# Find key points on the smoothed curve
trough_val <- min(dip_smooth)
trough_t   <- t[which.min(dip_smooth)]

post_trough <- which(t > trough_t & dip_smooth >= 0.95)
recovery_t  <- t[post_trough[1]]

par(mar = c(4, 5, 1, 1))

# Raw noisy data in light grey
plot(t, dip_noisy, type = "l", lwd = 0.8, col = "grey75",
     xlab = "Time (hours)", ylab = "System Performance Metric",
     xlim = c(0, 75), ylim = c(0, 1.1), main = "")

# Smoothed curve on top
lines(t, dip_smooth, lwd = 3, col = "steelblue")

# Resilience loss: shade between baseline (1) and smoothed curve
shade_idx <- which(t >= 8 & t <= 73)
polygon(c(t[shade_idx], rev(t[shade_idx])),
        c(rep(1, length(shade_idx)), rev(dip_smooth[shade_idx])),
        col = rgb(0.6, 0.6, 0.9, 0.3), border = NA)
text(40, 0.85, "Resilience Loss", cex = 0.9, col = "slateblue4", font = 3)

# Baseline
abline(h = 1, col = "grey40", lty = 3, lwd = 1)

# Robustness
segments(0, trough_val, trough_t, trough_val, col = "firebrick", lty = 2, lwd = 1.5)
points(trough_t, trough_val, pch = 19, col = "firebrick", cex = 1.2)
text(trough_t - 12, trough_val + 0.04, "Robustness", cex = 0.9,
     col = "firebrick", font = 3)

# Recovery slope
points(recovery_t, dip_smooth[post_trough[1]], pch = 19, col = "darkgreen", cex = 1.2)
segments(trough_t, trough_val, recovery_t, dip_smooth[post_trough[1]],
         col = "darkgreen", lwd = 2, lty = 1)
mid_t <- (trough_t + recovery_t) / 2
mid_p <- (trough_val + dip_smooth[post_trough[1]]) / 2
text(mid_t + 12, mid_p - 0.04, "Recovery Slope", cex = 0.9, col = "darkgreen", font = 3)

# ---- Plot 2: Metric direction illustration ----------------------------------

dip2 <- ifelse(t < 24, 1,
               ifelse(t < 88, 0.65 + 0.35 * cos(pi * (t - 24) / 32),
                      1))

success <- dip2
failure <- 1 - dip2

par(mar = c(4, 5, 1, 1))
plot(t, failure, type = "l", lwd = 2.5, col = "firebrick",
     xlab = "Time (hours)", ylab = "System Performance Metric",
     ylim = c(0, 1.05), main = "")
lines(t, success, lwd = 2.5, col = "steelblue")

legend("right",
       legend = c("Failure Rate", "Success Rate"),
       col = c("firebrick", "steelblue"),
       lwd = 2.5, bty = "n", cex = 0.9)