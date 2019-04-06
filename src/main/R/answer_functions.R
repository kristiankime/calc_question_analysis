if(!exists("CALC")) {
  CALC <- list()
}

if(is.null(CALC[["answer"]])) {
  CALC$answer <- list()
}

# Read in the answers file with the right column types etc 
CALC$answer$initial_table <- function(file) {
  answers <- read.table(file,
                        header=FALSE, 
                        sep=",", 
                        stringsAsFactors=FALSE, 
                        quote = '"',
                        col.names =c("anon_id", "section",   "fy",      "problem_source", "correct_string", "timestamp", "answer_string"),
                        colClasses=c("integer", "character", "integer", "character",      "character",      "integer",   "character")
  )
  setDT(answers)
  return(answers)
}

# Add the columns that indicate if the answers got the whole questions right or which parts of questions various answers got right etc 
CALC$answer$append_correct <- function(answers) {
  answers$question_id = CALC$question$short_names(answers$problem_source)
  answers$num_parts <- nchar(answers$correct_string)
  answers$correct_count <- str_count(answers$correct_string, "1") 
  answers$all_correct <- answers$num_parts == answers$correct_count
  answers$percent_correct <- answers$correct_count / answers$num_parts
  return(answers)
}

CALC$answer$correct_based_on_n_answers <- function(answers_correct, n) {
  tmp <- answers_correct[,
                         list(correct=as.integer(any(head(all_correct,n))), timestamp=max(head(timestamp,n))), 
                         by=c("anon_id", "question_id")]
  return(tmp[order(anon_id, timestamp)])
}

CALC$answer$reshape_wide <- function(answers_cols) { # Note answers should come from "answer.correct_based_on_n_answers" function
  answers <- answers_cols[,list(anon_id, question_id, correct) ]
  student_answer.wide <- reshape(answers, idvar = "anon_id", timevar = "question_id", direction = "wide")
  names(student_answer.wide) <- gsub("correct.", "", names(student_answer.wide))
  return(student_answer.wide)
}

CALC$answer$irt_remove_id <- function(answers_wide) {
  # Remove the id column
  return(answers_wide[,-1])
}

CALC$answer$irt_question_cleanup <- function(answers_wide) {
  # Remove the id column
  student_answer <- answers_wide[,-1] 
  # Remove cols with all correct (aka 1) answers
  all_1_cols <- !apply(student_answer==1,2,all)
  student_answer.fixed_no1 <- student_answer[, all_1_cols, with = FALSE]
  # Remove cols with all incorrect (aka 0) answers
  all_0_cols <- !apply(student_answer.fixed_no1==1,2,all)
  student_answer.fixed_no0 <- student_answer.fixed_no1[, all_0_cols, with = FALSE]
}





CALC$answer$read_raw_data <- function(file) {
  return( read.table(file, 
                     header=FALSE, 
                     sep=",", 
                     quote = '"', 
                     stringsAsFactors=FALSE, 
                     col.names = c("anon_id",  "problem_source", "correct_string", "timestamp", "answer_string"),
                     colClasses = c("numeric", "character",      "character",      "numeric",   "character"),
                     strip.white=TRUE,
                     fill=FALSE)  )
}

CALC$answer$year_from_name <-function(name) {
  size <- nchar(name)
  substring(name, size-5, size-4)
}

CALC$answer$section_from_name <-function(name) {
  size <- nchar(name)
  substring(name, size-12, size-8)
}

# TODO why doesn't this import correctly?
# CALC$question$read_raw_data_tidy <- function(file) {
#   return( read_csv(file,
#                    col_names = c("anon_id",     "problem_source", "correct_string", "timestamp",   "answer_string"),
#                    col_types = c(col_integer(), col_character(),  col_character(),  col_integer(), col_character()),
#                    quote = '"',
#                    trim_ws=TRUE
#                    )  )
# }