rankhospital <- function(state, outcome, num = "best") {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with the given rank
  ## 30-day death rate
  c <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
  ndx <- order(c$Hospital.Name)
  cs <- c[ndx,]
  if (length(cs$State[cs$State==state])<=0) {
    stop("invalid state")
  }
  if (outcome=="heart attack") {
    v <- as.numeric(cs$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
  } else if (outcome=="heart failure") {
    v <- as.numeric(cs$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
  } else if (outcome=="pneumonia") {
    v <- as.numeric(cs$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
  } else {
    stop("invalid outcome")
  }
  t <- cs$State==state
  eh<-cs$Hospital.Name[t]
  if (num=="best") {
    h <- which(v[t] == min(v[t], na.rm = T))
    ret<-eh[h][1]
  } else if (num == "worst") {
    h <- which(v[t] == max(v[t], na.rm = T))
    ret<-eh[h][1]
  } else if (is(num,"numeric")) {
    ra <- order(v[t], na.last = NA)
    rd<-eh[ra]
    ret <- rd[num]
  }
  ret
}