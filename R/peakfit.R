#' peakfit
#' peak fit from alsace package,
#' inspired by B. Ripley
#' @export
peakfit <- function (y, xloc) {
  peak.loc <- which(y > 0.5 * y[xloc])
  peak.loc.diff <- diff(peak.loc)
  boundaries <- c(0, which(diff(peak.loc) != 1), length(peak.loc) +
                    1)
  peaknrs <- rep(1:length(boundaries), c(boundaries[1],
                                         diff(c(boundaries))))
  peaknrs[boundaries[-1]] <- NA
  current.peak <- peaknrs[peak.loc == xloc]
  current.peak <- current.peak[!is.na(current.peak)]
  if (length(current.peak) == 0)
    return(rep(NA, 5))
  FWHM <- diff(range(peak.loc[peaknrs == current.peak],
                     na.rm = TRUE))
  pksd <- FWHM/(2 * sqrt(2 * log(2)))
  result <- c(xloc, pksd, FWHM, y[xloc], y[xloc]/dnorm(xloc, xloc, pksd))
  names(result) <- c("rt", "sd", "FWHM", "height", "area")
  return (result)
}
