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
Pstatus
G3
sample_columns = data[sample(nrow(data),), c(5,6,22,25,33)]

#make famsize numeric
famsize[famsize=="LE3"] = 0 # 3 or samller than 3
famsize[famsize=="GT3"] = 1 # bigger than 3 famsize
famsize=as.numeric(famsize) # make our data numeric
famsize

#make internet numeric
internet[internet=="no"] = 0 # non access to internet
internet[internet=="yes"] = 1 # access to internet
internet
internet=as.numeric(internet) # make our data numeric
internet

# make freetime binary and numeric
freetime_binary = ifelse(freetime>=1 & freetime<=3, 0, 1)
freetime_binary

# make parent's status numeric
Pstatus[Pstatus=="T"] = 1 # living apart
Pstatus[Pstatus=="A"] = 0 # living together
Pstatus=as.numeric(Pstatus)

# family size barplot
counts_fam = table(famsize)
percentages_fam = prop.table(counts_fam)
barplot(percentages_fam, col = c("skyblue", "salmon"), main = "Barplot of family size", xlab = "Family Size",
        ylab = "Density", names.arg = c("≤ 3", "> 3"), ylim = c(0, 1))
legend("topright", legend = c("Family Size ≤ 3", "Family Size > 3"), 
       fill = c("skyblue", "salmon"))

# internet access barplot
counts_internet = table(internet)
percentages_internet = prop.table(counts_internet)
barplot(percentages_internet, col = c("skyblue", "salmon"), main = "Barplot of internet access", xlab = "Internet access",
        ylab = "Density", names.arg = c("No", "Yes"), ylim = c(0, 1.2))
legend("topright", legend = c("No internet access", "Internet access"), 
       fill = c("skyblue", "salmon"))

# freetime after school barplot
counts_freetime = table(freetime_binary)
percentages_freetime = prop.table(counts_freetime)
barplot(percentages_freetime, col = c("skyblue", "salmon"), main = "Barplot of freetime after school", xlab = "Freetime after school",
        ylab = "Density", names.arg = c("≤ 3", "> 3"), ylim = c(0, 1))
legend("topright", legend = c("Freetime ≤ 3", "Freetime > 3"), 
       fill = c("skyblue", "salmon"))

# parent's cohabitation status barplot
counts_Pstatus = table(Pstatus)
percentages_Pstatus = prop.table(counts_Pstatus)
barplot(percentages_Pstatus, col = c("skyblue", "salmon"), main = "Barplot of parent's cohabitation status", xlab = "Parent's status",
        ylab = "Density", names.arg = c("Apart", "Together"), ylim = c(0, 1.2))
legend("topright", legend = c("Living apart", "Living together"), 
       fill = c("skyblue", "salmon"))

# Average grade histogram
hist(G3, probability = T)
abline(v = mean(G3), col = "red")

table(G3) # distribution of G3
prop.table(G3) # density of G3
