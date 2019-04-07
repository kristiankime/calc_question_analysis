setup_question_links <- function() {
  # ===========================
  # --- input file Locations ---
  answers_location <- LOC$answers_location

  # --- output file Locations ---
  question_links_location <- LOC$question_links_location # "outputs/questions/questions_links.csv"
  # ===========================
  
  # ===========================
  # Processing Below here
  # ===========================
  # Read in the answers file
  answers <- CALC$answer$initial_table(answers_location)
  
  # List of Questions and links to WebWork
  question_links <- CALC$question$links_table( unique(answers$problem_source) )
  
  CALC$utility$create_path(question_links_location)
  write.table(question_links, file=question_links_location, sep=",", row.names=FALSE)
}