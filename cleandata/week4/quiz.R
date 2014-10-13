url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2Fss06hid.csv"
f<-"ss06hid.csv"
download.file(url, f, "curl")
dt <- tbl_df(read.csv(f))
varNames <- names(dt)
varNamesSplit <- strsplit(varNames, "wgtp")
varNamesSplit[[123]]

###

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FGDP.csv"
f<-"FGDP.csv"
dtGDP <- tbl_df(read.csv(f, skip=4, nrows=215, stringsAsFactors=FALSE))
dtGDP <- dtGDP %>%
  filter(X != "") %>%
  select(X, X.1, X.3, X.4) %>%
  rename(CountryCode=X, rankingGDP=X.1, Long.Name=X.3, gdp=X.4)
dtGDP %>%
  mutate(isUnited=grepl("^United", Long.Name)) %>%
  summarise(b=sum(isUnited)) %>%
  print
dtGDP %>%
  mutate(ngdp=as.numeric(gsub(",", "", gdp))) %>%
  summarise(a=mean(ngdp, na.rm=TRUE)) %>%
  print

###
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fdata%2FEDSTATS_Country.csv"
f<-"FSTA.csv"
dtEd <- tbl_df(read.csv(f))
dt<-left_join(dtGDP, dtEd,by=c("CountryCode"))

dt%>%
  filter(grepl("fiscal year end", tolower(Special.Notes)), grepl("june", tolower(dt$Special.Notes)))%>%
  summarise(a=length(Special.Notes)) %>%
  print

###

library(lubridate)
library(quantmod)
amzn = getSymbols("AMZN",auto.assign=FALSE)
sampleTimes = index(amzn)
addmargins(table(year(sampleTimes), weekdays(sampleTimes)))