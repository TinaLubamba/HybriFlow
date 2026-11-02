#' Settings for the follow-up (longitudinal) analysis
#'
#' Defines the follow-up analysis. A baseline genotype is *persisted* if it is
#' detected at baseline and at every follow-up visit, and *cleared* if it is not
#' detected at one or more follow-up visits (even if it is detected again later);
#' these definitions are fixed and are not settings. Pass the result to the
#' `followup` argument of [run_hybriflow()] or to the `settings` argument of
#' [run_followup_analysis()].
#'
#' @param run `TRUE` to run the follow-up analysis, `FALSE` to build the sample
#'   database only.
#' @param visits_required Number of visits per participant in the study
#'   protocol (baseline plus follow-ups). Whole number, 2 or more.
#' @param clear_last_k Used only for the `ended_HPV_negative` column. `1` means
#'   a participant ended HPV-negative if negative at the last visit; `2` means
#'   negative at the last two visits, and so on.
#' @param qc_only `TRUE` uses only samples whose `Overall_QC_Status` is `PASS`.
#'
#' @return A validated list of settings.
#' @export
#' @examples
#' followup_settings()
#' followup_settings(visits_required = 4, clear_last_k = 2)
followup_settings <- function(run = TRUE,
                              visits_required = 3L,
                              clear_last_k = 1L,
                              qc_only = TRUE) {
  validate_followup_settings(list(
    run = isTRUE(run),
    visits_required = as.integer(visits_required),
    clear_last_k = as.integer(clear_last_k),
    qc_only = isTRUE(qc_only)
  ))
}

validate_followup_settings <- function(s) {
  if (is.na(s$visits_required) || s$visits_required < 2)
    stop("visits_required must be a whole number of 2 or more.")
  if (is.na(s$clear_last_k) || s$clear_last_k < 1 || s$clear_last_k > s$visits_required - 1)
    stop("clear_last_k must be between 1 and the number of follow-up visits.")
  s
}
