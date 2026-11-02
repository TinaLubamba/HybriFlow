# Column names used inside ggplot2 / dplyr expressions. Declaring them here tells
# R CMD check they are not missing variables (they are data columns).
utils::globalVariables(c(
  "LTH", "TH", "Value", "at_risk", "category", "ci_high", "ci_low", "event",
  "face", "fill", "genotype", "group", "group_lab", "id", "k", "lab", "label", "level", "line",
  "newly_detected", "pct", "pct_of_women", "process", "risk", "risk_lab",
  "women", "x", "xend", "y", "yend"
))
