initial<-read.table("table.txt", nrows=100)
classes=sapply(initial, class)
tabAll<-read.table("table.txt", colClasses=classes)