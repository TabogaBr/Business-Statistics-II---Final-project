#PART I: Descriptive Statistics
getwd()
data =read.table("student-mat.csv",sep=";",header=TRUE)
names(data)
head(data,10)
dim(data)

attach(data)
famsize
internet
freetime
absences
G3
exemple_sample = data[sample(nrow(data),5), c(5,8,9,13)]

#make our data in numeric :famsize
famsize[famsize=="LE3"] = 0 #3 or samller than 3
famsize[famsize=="GT3"] = 1 #bigger than 3 famsize
famsize=as.numeric(famsize) #make our data numeric
famsize
#make our data in numeric :internet
internet[internet=="no"] = 0 #non access to internet
internet[internet=="yes"] = 1 #access to internet
internet
internet=as.numeric(internet) #make our data numeric
internet
