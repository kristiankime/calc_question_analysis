if(!exists("CALC")) {
  CALC <- list()
}

if(is.null(CALC[["question"]])) {
  CALC$question <- list()
}

# Given a vector of the full question names return a vector of shortened names
CALC$question$short_names <- function(name) {
  return(
    sub("Library/Michigan", "LM", 
        sub("Library/LoyolaChicago/Precalc", "LL", 
            sub("Chap", "C", 
                sub("Sec", "S",  
                    sub(".pg", "", 
                        name))))))
}

# Given a vector of the short question names return a vector of long names
CALC$question$long_names <- function(name) {
  return(
    paste0(
    sub("LM", "Library/Michigan",  
        sub("LL", "Library/LoyolaChicago/Precalc",  
            sub("C", "Chap",  
                sub("S", "Sec",
                    name)))),
    ".pg"))
}

CALC$question$link <- function(names) {
  return( paste0("http://crescas.cs-i.brandeis.edu/webwork2/M10a_F14/Undefined_Set/1/?displayMode=MathJax&sourceFilePath=", names) )
}

# From a vector of unique question names build a table that has links to webwork
CALC$question$links_table <- function(unique_questions) {
  questions <- data.table(
    problem_source=unique_questions,
    question_id=CALC$question$short_names(unique_questions),
    link=paste("http://crescas.cs-i.brandeis.edu/webwork2/M10a_F14/Undefined_Set/1/?displayMode=MathJax&sourceFilePath=", unique_questions, sep="")
  )
  return(questions)
}

CALC$question$read_table <- function(file) {
  return( read.table(file, header=TRUE, sep=",", quote = '"', stringsAsFactors=FALSE)  )
}

CALC$question$question_has_feature <- function(questions_factors) {
  d <- as_tibble(questions_factors)
  
  function(question, factor) {
    if(factor == "None") {
      FALSE
    } else {
      qf <- d %>% filter(question_id==question)
      qf[factor][[1]]
    }
  }
}
