library(testthat)
library(tensorflow)
print(reticulate::py_config())
test_check("tensorflow")
