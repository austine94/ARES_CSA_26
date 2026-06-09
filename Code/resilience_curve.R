# =============================================================================
# resilience_curve.R
#
# Prototype for using framework for constructing resilience curves from data centre
# performance metrics and deriving resilience measures.
#
# Crucial Note: all metrics are oriented so that 1 = healthy, 0 = failed.
# Time is assumed to be seq_along(P) and common across all metrics.
#
# Public functions:
#   rc_smooth()     - rolling-mean smoothing of raw system performance measures
#   rc_normalise()  - min/max or baseline normalisation onto [0, 1] - depends if user supplies baseline
#   rc_metrics()    - compute the four resilience measures (robustness, recovery, resilience index, normalised index)
#   rc_plot()       - plot a resilience curve with annotations for values
#   rc_single()     - Option 1: single-metric pipeline
#   rc_composite()  - Option 2: (weighted) multi-metric weighted pipeline
# =============================================================================


##### Smoothing #####



rc_smooth <- function(x, window = 5) {

  # Smooth data using a rolling mean
  # x numeric vector to be smoothed
  # window integer window size (>= 1). window = 1 returns x unchanged.
  # returns a numeric vector of same length as x; edges use partial windows
  
  if (!is.numeric(x)) stop("x must be numeric")
  if (window < 1) stop("window must be >= 1")
  if (window %% 2 == 0) stop("window must be odd so the rolling mean is symmetric around each point")
  
  n <- length(x)
  if (window == 1 || n == 0) return(x)
  
  out <- numeric(n)
  half <- (window - 1) %/% 2
  for (i in seq_len(n)) {
    lo <- max(1, i - half)
    hi <- min(n, i + half)
    out[i] <- mean(x[lo:hi], na.rm = TRUE)
  }
  out
}


###### Normalisation #######

rc_normalise <- function(x, p_ref = NULL, p_floor = NULL) {
  
  # Normalise a series onto [0, 1] with 1 = healthy
  # If baseline (p_ref) and floor (p_floor) are supplied, uses (x - p_floor) / (p_ref - p_floor).
  # Otherwise uses min/max of the observed series. Always clips to [0,1]
  #
  # x numeric vector
  # p_ref optional healthy reference value (mapped to 1)
  # p_floor optional failure floor value (mapped to 0)
  
  if (!is.null(p_ref) && !is.null(p_floor)) {
    if (p_ref <= p_floor) stop("p_ref must be greater than p_floor")
    out <- (x - p_floor) / (p_ref - p_floor)
  } else {
    rng <- range(x, na.rm = TRUE)
    if (diff(rng) == 0) {
      #constant series -> map to 1 (system performance is constant)
      return(rep(1, length(x)))
    }
    out <- (x - rng[1]) / (rng[2] - rng[1])
  }
  pmin(pmax(out, 0), 1)
}


###### Resilience metrics #####

# Compute the four resilience measures from a (smoothed, possibly normalised)
# performance curve P_tilde over a defined disruption window.
#
# p numeric vector of performance values (already smoothed/normalised)
# t_start integer index, start of disruption (inclusive)
# t_end   integer index, end of disruption (inclusive)
# recovery_threshold fraction of pre-disturbance baseline that counts as "recovered" (default 0.95)
# returns a list with:
#   robustness        - min P over [t_start, t_end]
#   rapidity          - recovery slope from trough to recovery point
#   resilience_loss   - integral of (1 - P) over [t_start, t_end]
#   R                 - 1 - resilience_loss / window_length
#   baseline          - mean P pre-disturbance (reference for rapidity)
#   trough_index      - index of the minimum within the window
#   recovery_index    - first index after trough where P >= threshold*baseline

rc_metrics <- function(p, t_start, t_end,
                       recovery_threshold = 0.95,
                       baseline_value = NULL) {
  n <- length(p)
  if (t_start < 1 || t_end > n || t_start >= t_end) {
    stop("Require 1 <= t_start < t_end <= length(p)")
  }
  
  #Pre-disturbance baseline (reference for rapidity recovery threshold)
  if (!is.null(baseline_value)) {
    baseline <- baseline_value
  } else {
    baseline <- p[1] #just keep it simple even though this is a bit flawed
  }
  
  window_idx <- t_start:t_end
  p_win <- p[window_idx]
  
  #add warning about integration if p(t) not normalised as gives weird outputs
  if (any(p < 0 | p > 1, na.rm = TRUE)) {
    warning("p is not fully in [0,1]; resilience_loss and R may not be meaningful")
  }
  
  
  #Robustness = min P(t) within disruption window
  robustness <- min(p_win, na.rm = TRUE)
  
  #Trough location (earliest occurrence if multiple)
  trough_local <- which.min(p_win)
  trough_index <- window_idx[trough_local]
  
  #Resilience loss = integral of (1 - P) over the window.
  #Computed using Trapezoidal rule with unit time steps.
  loss_vec <- 1 - p_win
  if (length(loss_vec) >= 2) {
    resilience_loss <- sum((loss_vec[-1] + loss_vec[-length(loss_vec)]) / 2)
  } else {
    resilience_loss <- loss_vec
  }
  
  #normalised resilience index
  window_length <- t_end - t_start  #number of unit intervals
  R <- 1 - resilience_loss / window_length
  
  #Rapidity = recovery slope from trough up to recovery point
  recovery_target <- recovery_threshold * baseline
  recovery_index <- NA_integer_
  rapidity <- NA_real_
  
  if (trough_index < t_end) {
    post_trough <- (trough_index + 1):t_end
    recovered <- which(p[post_trough] >= recovery_target)
    if (length(recovered) > 0) {
      recovery_index <- post_trough[recovered[1]]
      rapidity <- (p[recovery_index] - p[trough_index]) /
        (recovery_index - trough_index)
    } else {
      warning("System did not recover to ", recovery_threshold,
              " of baseline within the disruption window; rapidity = NA")
    }
  } else {
    warning("Trough is at end of window; rapidity = NA")
  }
  
  list(
    robustness = robustness,
    rapidity = rapidity,
    resilience_loss = resilience_loss,
    R = R,
    baseline = baseline,
    trough_index = trough_index,
    recovery_index = recovery_index
  )
}


##### Plotting #####

#Plot a resilience curve with disruption window and key points annotated.
rc_plot <- function(p, t_start, t_end, metrics = NULL,
                    main = "Resilience curve",
                    ylab = "P(t)", xlab = "t",
                    ylim = NULL) {
  t <- seq_along(p)
  if (is.null(ylim)) {
    ylim <- range(c(p, 0, 1), na.rm = TRUE)
  }
  
  plot(t, p, type = "l", lwd = 2, col = "steelblue",
       main = main, xlab = xlab, ylab = ylab, ylim = ylim)
  
  #shade disruption window
  rect(t_start, ylim[1], t_end, ylim[2],
       col = rgb(1, 0, 0, 0.07), border = NA)
  abline(v = c(t_start, t_end), col = "red", lty = 2)
  
  if (!is.null(metrics)) {
    abline(h = metrics$baseline, col = "darkgreen", lty = 3)
    points(metrics$trough_index, p[metrics$trough_index],
           pch = 19, col = "red")
    if (!is.na(metrics$recovery_index)) {
      points(metrics$recovery_index, p[metrics$recovery_index],
             pch = 19, col = "darkgreen")
      segments(metrics$trough_index, p[metrics$trough_index],
               metrics$recovery_index, p[metrics$recovery_index],
               col = "darkgreen", lwd = 2)
    }
    legend("bottomright",
           legend = c("P(t)", "baseline", "trough", "recovery point",
                      "disruption window"),
           col = c("steelblue", "darkgreen", "red", "darkgreen", "red"),
           lty = c(1, 3, NA, NA, 2),
           pch = c(NA, NA, 19, 19, NA),
           bty = "n", cex = 0.8)
  }
  invisible(NULL)
}

######
#Resilience Curve Functions
#####


# Option 1: single-metric pipeline

rc_single <- function(p,
                      t_start, t_end,
                      window = 5,
                      normalise = TRUE,
                      p_ref = NULL, p_floor = NULL,
                      recovery_threshold = 0.95,
                      baseline_value = NULL,
                      plot = TRUE,
                      main = "Resilience curve") {
  
  #This function takes a single metric and outputs resilience measures
  #The pipeline is:
  #Smooth -> (optionally) normalise -> compute metrics -> plot.
  #
  # p raw performance vector (1 = healthy convention; if not, supply appropriate p_ref / p_floor or pre-flip the series)
  # t_start,t_end disruption window indices
  # window smoothing window (default 5 points for rolling mean)
  # normalise TRUE/FALSE
  # p_ref,p_floor optional baseline / floor for normalisation
  # recovery_threshold fraction of baseline counted as recovered
  # baseline_value is the baseline used for the recovery threshold. Uses first point if NULL
  # plot whether to draw the curve
  # the main title if a plot is produced
  
  p_smooth <- rc_smooth(p, window = window)
  p_tilde  <- if (normalise) rc_normalise(p_smooth, p_ref, p_floor) else p_smooth
  
  m <- rc_metrics(p_tilde, t_start, t_end,
                  recovery_threshold = recovery_threshold,
                  baseline_value = baseline_value)
  
  if (plot) rc_plot(p_tilde, t_start, t_end, metrics = m, main = main)
  
  list(
    raw = p,
    smoothed = p_smooth,
    curve = p_tilde,
    metrics = m,
    settings = list(window = window, normalise = normalise,
                    p_ref = p_ref, p_floor = p_floor,
                    recovery_threshold = recovery_threshold,
                    t_start = t_start, t_end = t_end)
  )
}


###### Option 2: Multi-Metric Weighted pipeline

rc_composite <- function(df,
                         weights,
                         t_start, t_end,
                         window = 5,
                         normalise = TRUE,
                         p_ref = NULL, p_floor = NULL,
                         recovery_threshold = 0.95,
                         baseline_value = NULL,
                         plot = TRUE,
                         main = "Composite resilience curve") {
  
  #Composite resilience pipeline from a data frame of metrics
  #Each column is smoothed, optionally normalised, then combined as a
  #weighted sum into a single composite curve P_tilde, which is then
  #fed through rc_metrics() and rc_plot().
  # df data frame whose columns are performance metrics (1 = healthy)
  # weights numeric vector of weights, one per column of df.
  # Will be normalised to sum to 1; a warning is issued if they do not.
  # t_start,t_end disruption window
  # window smoothing window
  # normalise TRUE/FALSE applied per column
  # p_ref,p_floor optional vectors (length = ncol(df)) of per-column
  #        reference/floor values. Use NA in a slot to fall back to min/max
  #        for that column.
  # recovery_threshold fraction of baseline counted as recovered
  # baseline_value is the baseline used for the recovery threshold. Uses first point if NULL
  # plot whether to draw the composite curve
  # main plot title
  
  
  if (!is.data.frame(df)) stop("df must be a data frame")
  k <- ncol(df)
  if (length(weights) != k) {
    stop("length(weights) must equal ncol(df)")
  }
  if (any(weights < 0)) stop("weights must be non-negative")
  
  # Normalise weights to sum to 1; warn if they didn't already
  w_sum <- sum(weights)
  if (w_sum == 0) stop("weights sum to zero")
  if (!isTRUE(all.equal(w_sum, 1))) {
    warning("weights did not sum to 1 (sum = ", round(w_sum, 4),
            "); normalising internally")
    weights <- weights / w_sum
  }
  
  # Per-column smoothing + (optional) normalisation
  curves <- vector("list", k)
  for (j in seq_len(k)) {
    xj <- df[[j]]
    xj_s <- rc_smooth(xj, window = window)
    if (normalise) {
      pr <- if (!is.null(p_ref))   p_ref[j]   else NULL
      pf <- if (!is.null(p_floor)) p_floor[j] else NULL
      if (!is.null(pr) && is.na(pr)) pr <- NULL
      if (!is.null(pf) && is.na(pf)) pf <- NULL
      curves[[j]] <- rc_normalise(xj_s, p_ref = pr, p_floor = pf)
    } else {
      curves[[j]] <- xj_s
    }
  }
  
  # Weighted sum -> composite curve
  curve_mat <- do.call(cbind, curves)
  composite <- as.numeric(curve_mat %*% weights)
  
  m <- rc_metrics(composite, t_start, t_end,
                  recovery_threshold = recovery_threshold,
                  baseline_value = baseline_value)
  
  if (plot) rc_plot(composite, t_start, t_end, metrics = m, main = main)
  
  list(
    raw = df,
    component_curves = setNames(as.data.frame(curve_mat), names(df)),
    weights = weights,
    composite = composite,
    metrics = m,
    settings = list(window = window, normalise = normalise,
                    p_ref = p_ref, p_floor = p_floor,
                    recovery_threshold = recovery_threshold,
                    t_start = t_start, t_end = t_end)
  )
}