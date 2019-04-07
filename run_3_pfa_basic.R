source("src/main/R/imports.R")
source("src/subscript/R/pfa/pfa_1_initial_files.R")
source("src/subscript/R/pfa/pfa_2_coef_files.R")
source("src/subscript/R/pfa/pfa_3_prediction_files.R")
source("src/subscript/R/pfa/pfa_4_prediction_accuracy_files.R")


# Parameters
attempts <- c(1:1)
input_prefix <- "FIE-243diff"
output_prefix <- "FIE-243diff"
questions_location <- "data/questions/questions_details.243diff.csv"

initial_pfa_files(attempts, input_prefix, output_prefix, questions_location)
pfa_coef_files(attempts, input_prefix, output_prefix)
pfa_prediction_files(attempts, input_prefix, output_prefix)
pfa_prediction_accuracy_files(attempts, input_prefix, output_prefix)
