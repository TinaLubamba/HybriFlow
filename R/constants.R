# Genotype risk classifications used throughout the package.
# (Kept as internal objects; use hpv_genotypes() to read them.)
high_risk <- c("16","18","26","31","33","35","39","45","51","52","53","56","58","59","66","68","73","82")
low_risk  <- c("6","11","40","42","43","44/55","54","61","62/81","67","69","70","71","72","81","84")

#' HPV genotype risk groups used by the package
#'
#' Returns the genotypes the package treats as high-risk and low-risk. These are
#' the targets on the HybriSpot kit and are used for the risk summaries, the
#' column colours in the exports, and the follow-up analysis.
#'
#' @return A named list with elements `high_risk` and `low_risk`
#'   (character vectors of genotype names).
#' @export
#' @examples
#' hpv_genotypes()$high_risk
hpv_genotypes <- function() {
  list(high_risk = high_risk, low_risk = low_risk)
}
