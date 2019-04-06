source("src/main/R/imports.R")
source("src/subscript/R/general/setup_1_question_links_file.R")
source("src/subscript/R/general/setup_2_answers_correct_files.R")

attempts <- c(1:1)

answers_location <- LOC$answers_location
question_links_location <- LOC$question_links_location

answers_correct_location <- LOC$answers_correct_location
answers_correct_minimal_location <- LOC$answers_correct_minimal_location
answers_correct_attempts_location <- LOC$answers_correct_attempts_location
answers_correct_wide_location <- LOC$answers_correct_wide_location

setup_question_links(answers_location, 
                     question_links_location)

setup_answers_correct_files(attempts,
                            answers_location,
                            answers_correct_location, 
                            answers_correct_minimal_location,
                            answers_correct_attempts_location,
                            answers_correct_wide_location) 