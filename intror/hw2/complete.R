complete <- function(directory, id = 1:332) {
  if (missing(directory)) {
    stop("need directory!!")
  }
  if (missing(id)) {
    stop("need id!!")
  }
  kd<-do.call(rbind, lapply(id,function(fn) {
    filenam<-sprintf(fn, fmt="%03d.csv")
    filename<-paste(directory, filenam, collapse = NULL, sep="/")
    m<-read.csv(file=filename,header=T)
    c(fn, nrow(m[complete.cases(m),]))
  }))
  colnames(kd) <- c("id","nobs")
  data.frame(kd)
  
  
}

