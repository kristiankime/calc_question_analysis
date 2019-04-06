# # ===========================
# # The "Inputs" to this file
# # AKA parameters / input file Locations / output file locations
# # ===========================
# 
# # --- parameters ---
# # attempts <- c(1:6)
# attempts <- c(1:1)
# 
# # --- input file Locations ---
# answers_location <- LOC$answers_location
# 
# # --- output file Locations ---
# answers_correct_location <- LOC$answers_correct_location
# answers_correct_minimal_location <- LOC$answers_correct_minimal_location
# answers_correct_attempts_location <- LOC$answers_correct_attempts_location
# answers_correct_wide_location <- LOC$answers_correct_wide_location

setup_answers_correct_files <- function(attempts,
                                        answers_location,
                                        answers_correct_location, 
                                        answers_correct_minimal_location,
                                        answers_correct_attempts_location,
                                        answers_correct_wide_location) {

  # Read in the answers file
  answers <- CALC$answer$initial_table(answers_location)
  
  # ===== Compute Correct Columns =======
  # Add various correct/incorrect information to the answers
  answers_correct <- CALC$answer$append_correct(answers)
  CALC$utility$write_csv_and_robj(answers_correct, answers_correct_location)
  
  # ===== Trim columns to create "Minimal" Answer File =====
  answers_correct_minimal <- answers_correct[order(anon_id, timestamp, question_id), list(anon_id, question_id, timestamp, all_correct)]
  CALC$utility$write_csv_and_robj(answers_correct_minimal, answers_correct_minimal_location)
  
  # ===== Compute "Correct Based on Number of attempts" version of the Answers data ====
  # ie we end up with one row per question not per attempt
  for(i in attempts) {
    answers_i <- CALC$answer$correct_based_on_n_answers(answers_correct_minimal, i)
    CALC$utility$write_csv_and_robj(answers_i, answers_correct_attempts_location(i))
  }
  
  # ===== Reshape "Correct Based on Number of attempts" to wide format ====
  # Now we have one row per student
  for(i in attempts) { 
    answers_i <- CALC$utility$read_robj_table(answers_correct_attempts_location(i))
    answers_wide_i <- CALC$answer$reshape_wide(answers_i)
    CALC$utility$write_csv_and_robj(answers_wide_i, answers_correct_wide_location(i))
  }
}