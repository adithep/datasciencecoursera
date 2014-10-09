best <- function(state, outcome) {
  ## Read outcome data
  ## Check that state and outcome are valid
  ## Return hospital name in that state with lowest 30-day death
  ## rate
  cs <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
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
  h <- which(v[t] == min(v[t], na.rm = T))
  eh<-cs$Hospital.Name[t]
  if (length(h)>1) {
    g<-sort(eh[h])
    ret<-g[1]
  } else {
    ret<-eh[h]
  }
  ret  

}
