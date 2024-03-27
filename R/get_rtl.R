get_rtl <- function(tL, index, nDL, biasDL, covDL, biasR,
                    covR, roof_adjust, gl_safety, minLoad, phi, years = 1) {
  runs <- length(tL)

  if (max(tL) > 1e16) {
    return(-1)
  }

  sR <- stats::rnorm(runs, 0, 1)

  if (years == 1) {
    target_fails <- floor(-runs * log(pnorm(index)) / 50)
  } else if (years == 50) {
    target_fails <- floor(runs * (1 - pnorm(index)))
  } else {
    return(-1)
  }


  
  if (max(tL) < 1) {
    target_range <- c(-2, 2 * 1.6 * max(tL) + 1)
  } else {
    target_range <- c(median(tL), 2 * 1.6 * max(tL) + 1)
  }


  count_fails <- function(nPg, tL, tR, target, nDL, biasDL,
                          covDL, biasR, covR, roof_adjust, gl_safety, minLoad, phi) {
    # Nominal snow loads behave differently below 20 psf in ASCE 7.
    if (minLoad > 0) {
      nPf <- ifelse(nPg < 0.9576052, nPg, max(roof_adjust * nPg, 0.9576052))
    } else {
      nPf <- roof_adjust * nPg
    }

    nR <- (1.2 * nDL + gl_safety * nPf) / phi
    rd_mean <- biasR * nR - biasDL * nDL
    rd_sd <- sqrt(((biasR * covR * nR)^2) + ((biasDL * covDL * nDL)^2))

    fails <- sum(tL - (tR * rd_sd + rd_mean) > 0)
    return(fails - target)
  }


  target_load <- rep(as.numeric(NA), length(index))
  for (i in 1:length(index)) {
    target_load[i] <- pracma::fzero(count_fails,
      x = target_range,
      tL = tL,
      tR = sR,
      target = target_fails[i],
      nDL = nDL,
      biasDL = biasDL,
      covDL = covDL,
      biasR = biasR,
      covR = covR,
      roof_adjust = roof_adjust,
      gl_safety = gl_safety,
      minLoad = minLoad,
      phi = phi
    )$x
  }

  return(target_load)
}
