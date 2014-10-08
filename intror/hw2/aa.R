add2 <- function(x, y) {
  x+y
}
above10 <- function(x) {
  use <- x > 10
  x[use]
}

above <- function(x, n=10) {
  use <- x > n
  x[use]
}

columnmean <- function(x, removeNA=T) {
  nc<-ncol(x)
  means<-numeric(nc)
  for (i in 1:nc) {
    means[i]<-mean(y[,i], na.rm=removeNA)
  }
  means
}

pollutantmean <- function(directory, pollutant, id = 1:332) {
  filenames <- list.files(directory, pattern="*.csv", full.names=TRUE)
  ldf <- lapply(filenames, read.csv)
}
