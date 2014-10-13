library(XML)
library(RCurl)
file_url<-"http://www.vasuexchange.com/"
lines<-readLines(file_url)
doc<-htmlTreeParse(lines, useInternal=T)
date<-xpathSApply(doc, "//td[@class='text_03']",xmlValue)
cost<-xpathSApply(doc, "//td[@class='text_022_16']",xmlValue)