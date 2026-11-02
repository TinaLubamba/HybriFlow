# Example: how to run the package after installing it
library(HybriFlow)
res <- run_hybriflow(
  data_folder   = "PATH/TO/YOUR/DATA",
  output_folder = "PATH/TO/OUTPUT",
  repeat_handling = "latest"
)
