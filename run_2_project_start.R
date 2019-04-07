source("src/main/R/imports.R")
source("src/subscript/R/project/setup_3_training_test_files.R")

# Parameters
attempts <- c(1:1)
output_prefix <- "FIE-243diff"
random_seed <- 0

# attempts <- c(1:1)
# output_prefix <- "FIE-243"
# random_seed <- 0

training_test_files(attempts, output_prefix, random_seed)