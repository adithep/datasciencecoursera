rankall <- function(outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## For each state, find the hospital of the given rank
  ## Return a data frame with the hospital names and the
  ## (abbreviated) state name
  c <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ndx <- with(c, order(State, Hospital.Name))
  cs <- c[ndx,]
  if (outcome=="heart attack") {
    v <- as.numeric(cs$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  } else if (outcome=="heart failure") {
    v <- as.numeric(cs$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  } else if (outcome=="pneumonia") {
    v <- as.numeric(cs$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  } else {
    stop("invalid outcome")
  }
  sasa <- unique(cs$State)
  m<-matrix(, nrow = 0, ncol = 2)
  for (dj in sasa) {
    t <- cs$State==dj
    eh<-cs$Hospital.Name[t]
    if (num=="best") {
      h <- which(v[t] == min(v[t], na.rm = T))
      ret<-c(eh[h][1], dj)
    } else if (num == "worst") {
      h <- which(v[t] == max(v[t], na.rm = T))
      ret<-c(eh[h][1], dj)
    } else if (is(num,"numeric")) {
      ra <- order(v[t], na.last = NA)
      rd<-eh[ra]
      ret <- c(rd[num], dj)
    } else {
      ret<-NA
    }
    m<-rbind(m, ret)
  }
  colnames(m) <- c("hospital","state")
  rownames(m) <- m[,2]
  data.frame(m)
}
