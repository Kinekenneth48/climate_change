model_gr <- function(cap, mse, intercept = as.numeric(NA), slope = as.numeric(NA),
                     lam, flat_line, metric_adjust = 1) {
  # ============================================================================
  # If no slope or intercept is specified, use Colorado model.
  # If only an intercept is specified, use the O'Rourke model.
  # If slope and intercept are specified, use the general model.
  if (is.na(slope)) {
    if (is.na(intercept)) {
      gr <- function(sL) {
        sL <- sL * metric_adjust
        gr_co(sL, cap)
      }
    } else {
      gr <- function(sL) {
        sL <- sL * metric_adjust
        gr_or(sL, intercept, mse, cap)
      }
    }
  } else {
    gr <- function(sL) {
      sL <- sL * metric_adjust
      gr_lam_log(sL, slope, intercept, mse, cap, lam, flat_line)
    }
  }

  return(gr)
}
