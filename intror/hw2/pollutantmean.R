pollutantmean <- function(directory, pollutant, id = 1:332) {
  if (missing(directory)) {
    stop("need directory!!")
  }
  if (missing(pollutant) || !(pollutant %in% c("sulfate", "nitrate"))) {
    stop("need pollutant!!")
  }
  if (missing(id)) {
    id=1:332
  }
  filename<-sapply(id, sprintf, fmt="%03d.csv")
  filed<-paste(directory, filename, collapse = NULL, sep="/")
  ex = sapply(filed, file.exists)
  if (length(ex[0])>0) {
    stop("file or directory doesnt exists!?!")
  }
  df <- do.call(rbind,lapply(filed,function(fn) read.csv(file=fn,header=T)))
  dd<-sapply(df[pollutant], mean, na.rm = TRUE)
  dd<-unname(dd)
  dd<-round(dd, digit=3)
  dd
  
}

