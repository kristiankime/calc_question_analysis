training_test_files <- function(attempts=c(1:1), output_prefix="FIE", random_seed=0){
  # ===========================
  # --- input file Locations ---
  answers_correct_minimal_location <- LOC$answers_correct_minimal_location
  answers_correct_wide_location <- LOC$answers_correct_wide_location
  answers_correct_attempts_location <- LOC$answers_correct_attempts_location
  
  # --- output file Locations ---
  anon_ids_location <- LOC$anon_ids_location
  anon_ids_test_location <- LOC$anon_ids_test_location(output_prefix)
  anon_ids_train_location <- LOC$anon_ids_train_location(output_prefix)
  
  answers_correct_test_wide_location <- LOC$answers_correct_test_wide_location(output_prefix)
  answers_correct_train_wide_location <- LOC$answers_correct_train_wide_location(output_prefix)
  
  answers_correct_test_long_location <- LOC$answers_correct_test_long_location(output_prefix)
  answers_correct_train_long_location <- LOC$answers_correct_train_long_location(output_prefix)
  # ===========================
  
  # ===========================
  # Processing Below here
  # ===========================
  # --- find all the ids ---
  answers_correct_minimal <- CALC$utility$read_robj_table(answers_correct_minimal_location)
  anon_ids <- unique(answers_correct_minimal$anon_id)
  CALC$utility$write_txt_and_robj(anon_ids, anon_ids_location)
  
  # ---- Separate into Training and Test ids ----
  anon_ids_length <- length(anon_ids)
  training_size <- floor(anon_ids_length * .8)
  test_size <- (anon_ids_length - training_size)
  
  # Compute train vs test and store them
  set.seed(random_seed) # Set set for reproducability
  training_anon_id <- sample(anon_ids, training_size)
  CALC$utility$write_txt_and_robj(training_anon_id, anon_ids_train_location)
  test_anon_id <- setdiff(anon_ids, training_anon_id)
  CALC$utility$write_txt_and_robj(test_anon_id, anon_ids_test_location)
  
  # Write out test and train for 1-6 attempts for the wide files
  for(i in attempts) {
    answers_correct_wide_i <- CALC$utility$read_robj_table(answers_correct_wide_location(i))
  
    answers_correct_wide_train_i <- answers_correct_wide_i[anon_id %in% training_anon_id, ]
    CALC$utility$write_csv_and_robj(answers_correct_wide_train_i, answers_correct_train_wide_location(i))
    
    answers_correct_wide_test_i <- answers_correct_wide_i[anon_id %in% test_anon_id, ]
    CALC$utility$write_csv_and_robj(answers_correct_wide_test_i, answers_correct_test_wide_location(i))
  }
  
  # Write out test and train for 1-6 attempts for the long files
  for(i in attempts) {
    answers_correct_attempt_i <- CALC$utility$read_robj(answers_correct_attempts_location(i))
    
    answers_correct_long_train_i <- answers_correct_attempt_i[anon_id %in% training_anon_id, ]
    CALC$utility$write_csv_and_robj(answers_correct_long_train_i, answers_correct_train_long_location(i))
    
    answers_correct_long_test_i <- answers_correct_attempt_i[anon_id %in% test_anon_id, ]
    CALC$utility$write_csv_and_robj(answers_correct_long_test_i, answers_correct_test_long_location(i))
  }
}