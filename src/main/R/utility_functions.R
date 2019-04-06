if(!exists("CALC")) {
  CALC <- list()
}

if(is.null(CALC[["utility"]])) {
  CALC$utility <- list()
}

CALC$utility$create_path <- function(path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
}

CALC$utility$read_csv <- function(file) {
  return( read.table(file, header=TRUE, sep=",", quote = '"', stringsAsFactors=FALSE)  )
}

CALC$utility$read_robj_table <- function(name) {
  return( readRDS(paste0(name, ".robj")) )
}

CALC$utility$read_robj <- function(name) {
  return( readRDS(paste0(name, ".robj")) )
}

CALC$utility$compare_matrix <- function(m1, m2) {
  nrow <- nrow(m1)
  ncol <- ncol(m1)
  return(
    foreach(r=1:nrow, c=1:ncol, .combine = c) %do% {
      v1 <- m1[[c]][[r]]
      v2 <- m2[[c]][[r]]
      eq <- identical(v1,v2)
      if(!eq) {
        return( list(list(r=r, c=c, v1=v1, v2=v2)) )
      } else {
        return( NULL )
      }
    }
  )
}

CALC$utility$check_correctness_monotonicity <- function(compare) {
  return(
    foreach( v = compare_matrix(answers.irt.first, answers.irt.any), .combine = c) %do% {
      if(v$v1 == 1 && v$v2 == 0) { return(v) }
      else { return(NULL) }
    }
  )
}

CALC$utility$write_csv <- function(data_table, location) {
  CALC$utility$create_path(location)
  write.table(data_table, file=paste0(location, ".csv"), sep=",", row.names=FALSE, na = "")
  return(location)
}

CALC$utility$write_csv_and_robj <- function(data_table, location) {
  CALC$utility$create_path(location)
  write.table(data_table, file=paste0(location, ".csv"), sep=",", row.names=FALSE)
  saveRDS(data_table, file=paste0(location, ".robj"))
  return(location)
}

CALC$utility$write_txt_and_robj <- function(vector, location) {
  CALC$utility$create_path(location)
  fileConn <- file(paste0(location, ".txt"))
  writeLines( sapply(vector, toString), fileConn)
  close(fileConn)
  saveRDS(vector, file=paste0(location, ".robj"))
}

CALC$utility$write_txt <- function(vector, location) {
  CALC$utility$create_path(location)
  fileConn <- file(paste0(location, ".txt"))
  writeLines( sapply(vector, toString), fileConn)
  close(fileConn)
}

CALC$utility$read_txt_as_vector <- function(location) {
  return(scan(paste0(location, ".txt"), character(), quote = ""))
}

# http://stackoverflow.com/questions/7235657/fastest-way-to-replace-nas-in-a-large-data-table
CALC$utility$na2False <- function(dt){
  dt[is.na(dt)] <- FALSE
  return(setDT(dt))
}

CALC$utility$rename_col <- function(mydf, oldCol, newCol) {
  names(mydf)[names(mydf) == oldCol] = newCol
  return(mydf)
}

CALC$utility$bino_conf_95_up <- function(ns, n) {
  nf <- n - ns
  inner <- CALC$utility$bino_conf_95_inner(ns, n)
  return( (ns + inner) / n )
}

CALC$utility$bino_conf_95_lo <- function(ns, n) {
  nf <- n - ns
  inner <- CALC$utility$bino_conf_95_inner(ns, n)
  return( (ns - inner) / n )
}

CALC$utility$bino_conf_95_inner <- function(ns, n) {
  z <- 1.96 # z for 95% conf int
  nf <- n - ns
  return( z * sqrt(  (ns * nf ) / n )  )
}

CALC$utility$remove_hashed <- function(str_vec) {
  str_vec[!grepl("^#", str_vec)]
}