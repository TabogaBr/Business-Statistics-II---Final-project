# PART I: Descriptive Statistics
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

# make famsize numeric
famsize[famsize=="LE3"] = 0 # 3 or samller than 3
famsize[famsize=="GT3"] = 1 # bigger than 3 famsize
famsize=as.numeric(famsize) # make our data numeric
famsize

# make internet numeric
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

## Correlation boxplots ##
par(mfrow=c(1,2))
# Famsize and grades
boxplot(G3~famsize, data=data, col=c("skyblue", "salmon"), main = "Grades by family size",
        xlab = "Family size", ylab = "Grades")
# Internet and grades
boxplot(G3~internet, data=data, col=c("skyblue", "salmon"), main = "Grades by internet access",
        xlab = "Internet access", ylab = "Grades")
# Freetime and grades
boxplot(G3~freetime_binary, data=data, col=c("skyblue", "salmon"), main = "Grades by freetime",
        xlab = "Freetime after school", ylab = "Grades")
# Parent's status and grades
boxplot(G3~Pstatus, data=data, col=c("skyblue", "salmon"), main = "Grades by parent's cohabitation status",
        xlab = "Parent's cohabitation status", ylab = "Grades")

# PART II: Inferential Statistics

# qqnom function
my.qqnorm=function(x){
  scaled = scale(x, center = mean(x))
  sequence=seq(0.01,0.99,0.01)
  normal=qnorm(sequence, 0 , 1)
  reel=quantile(scaled,sequence)
  # this is how to keep the variable name "x" for the title
  variablename=deparse(substitute(x)) 
  Plot=plot(normal,reel, xlim = c(-3, 3), ylim = c(-3, 3), main=paste("Qqplot:", variablename))
  abline(0,1,col="blue", lty=3)
}
par(mfrow=c(2,2))
my.qqnorm(famsize)  # my.qqnorm of famsize
my.qqnorm(internet) # my.qqnorm of internet
my.qqnorm(freetime) # my.qqnorm of absences
my.qqnorm(Pstatus) # my.qqnorm of Note


############### HT 1 Famsize ###############

### Expectation H0 + H1 ###

### Estimator (paramètre d'intérêt) ###
smallfamily = G3[data$famsize=="LE3"]
smallfamily # final mark of the students which live in a small family (LE3)
bigfamily = G3[data$famsize=="GT3"]
bigfamily # final mark of the student which live in a big family (GT3)
mean(smallfamily) # 11
mean(bigfamily) # 10.17794
estimator_family = mean(smallfamily) - mean(bigfamily)
estimator_family # 0.8220641

### Variability (variance & CO) ###
var_mean_smallf=var(smallfamily)/length(smallfamily)
var_mean_smallf # 0.1566527
var_mean_bigf=var(bigfamily)/length(bigfamily)
var_mean_bigf # 0.07876339
var_famsize = var_mean_smallf + var_mean_bigf
var_famsize # 0.2354161
sd_estimator_famsize = sqrt(var_famsize)
sd_estimator_famsize # 0.4851969

### P-Value (with z-score calculation and pnorm) ###
zscore_famsize = ((estimator_family) - 0)/sd_estimator_famsize # calculation of the Z-score
zscore_famsize # 1.694289

# Calculation of p_value with (1 - pnorm(zscore))*2
pvalue_famsize = (1-pnorm(zscore_famsize))*2
pvalue_famsize # 0.0902103

# significance level of 0.05 sets a threshold beyond which results
# are considered statistically significant
# p-value > 0.05 (0.0902103 > 0.05) so in the case
# of our bilateral test we don't reject H0 in favor of H1

### Interval Confidence: confirm our hypothesis of the variable ###
# CI de 95%: the 95% deviation of the observations from the mean is smaller than 1.96 standard deviations (SD)
LCL_famsize=estimator_family - 1.96 * sqrt(var_famsize)
LCL_famsize # -0.128922
UCL_famsize=estimator_family + 1.96 * sqrt(var_famsize)
UCL_famsize # 1.77305
IC_famsize = c(LCL_famsize, UCL_famsize)  
IC_famsize # range of values
# estimator_family 0.8220641, so it's in the range


############### HT 2 Internet ###############

### Expectation H0 + H1 ###

### Estimator (paramètre d'intérêt) ###
grade_int = G3[data$internet == "yes"]
grade_int # grade_int = grades of the students who have internet access
grade_noint = G3[data$internet == "no"]
grade_noint # grade_noint = grades of the students who don't have internet access
mean(grade_int)
# average note of students who have internet access = 10.61702
mean(grade_noint)
# average note of students who don't have internet access = 9.409091
estimator_int= mean(grade_int) - mean(grade_noint)
estimator_int # 1.20793

### Variability (variance & CO) ###
var_int = var(grade_int)/length(grade_int)
# we calculate the variability if they have acces to the internet
var_int #0.06377182
var_noint = var(grade_noint)/length(grade_noint)
# we calculate the variavility if they don't have acces to the internet
var_noint # 0.3048845
var_estimator_int = var_int + var_noint
var_estimator_int # 0.3686563
sd_estimator_int = sqrt(var_estimator_int)
sd_estimator_int # 0.6071708

### P-Value (with z-score calculation and pnorm) ###
zscore_internet = ((estimator_int) - 0)/sd_estimator_int # calculation of the Z-score
zscore_internet # 1.989441
# Calculation of p_value with (1 - pnorm(zscore))*2
pvalue_internet = (1 - pnorm(zscore_internet))*2
pvalue_internet # 0.04665255

### Interval Confidence: confirm our hypothesis of the variable ###
LCL_int=estimator_int - 1.96 * sqrt(var_estimator_int)
LCL_int # 0.01787568
UCL_int=estimator_int + 1.96 * sqrt(var_estimator_int)
UCL_int # 2.397985
IC_int = c(LCL_int, UCL_int)  
IC_int
# estimator_int = 1.20793 so in IC

############### HT 3 Pstatus  ###############

### Expectation H0 + H1 ###

### Estimator (paramètre d'intérêt) ###
P_livingapart = G3[data$Pstatus=="T"]
P_livingapart # final mark of the student with parents living apart
P_livingtog = G3[data$Pstatus=="A"]
P_livingtog # final mark of the student with parents living together
mean(P_livingapart) # 10.32486
mean(P_livingtog) # 11.19512
estimator_pstatus = mean(P_livingtog) - mean(P_livingapart)
estimator_pstatus # 0.8702632

### Variability (variance & CO) ###
var_mean_apart=var(P_livingapart)/length(P_livingapart)
var_mean_apart # 0.06006339
var_mean_tog=var(P_livingtog)/length(P_livingtog)
var_mean_tog # 0.4490482
var_pstatus = var_mean_tog + var_mean_apart
var_pstatus # 0.5091116
sd_estimator_pstatus = sqrt(var_pstatus)
sd_estimator_pstatus # 0.7135205

### P-Value (with z-score calculation and pnorm) ###
zscore_pstatus = ((estimator_pstatus) - 0)/sd_estimator_pstatus # calculation of the Z-score
zscore_pstatus # 1.219675
# Calculation of p_value with (1 - pnorm(zscore))*2
pvalue_pstatus = (1 - pnorm(zscore_pstatus))*2
pvalue_pstatus # 0.2225881

### Interval Confidence: confirm our hypothesis of the variable ###
LCL_pstatus=estimator_pstatus - 1.96 * sqrt(var_pstatus)
LCL_pstatus # -0.5282371
UCL_pstatus=estimator_pstatus + 1.96 * sqrt(var_pstatus)
UCL_pstatus # 2.268763
IC_pstatus = c(LCL_pstatus, UCL_pstatus)  
IC_pstatus
# estimator_pstatus = 0.8702632 si in IC

############### HT 4 Freetime  ###############

### Estimator (paramètre d'intérêt) ###
S_lotofft = G3[data$freetime>3]
S_lotofft # final mark of the student with a lot of free time
S_notft = G3[freetime<=3]
S_notft # final mark of the student with not a lot of free time
mean(S_lotofft) # 10.65161
mean(S_notft) # 10.2625
estimator_freetime = mean(S_lotofft) - mean(S_notft)
estimator_freetime # 0.3891129

### Variability (variance & CO) ###
var_mean_lotofft =var(S_lotofft)/length(S_lotofft)
var_mean_lotofft # 0.1253954
var_mean_notft=var(S_notft)/length(S_notft)
var_mean_notft # 0.09174446
var_freetime = var_mean_lotofft + var_mean_notft
var_freetime # 0.2171398
sd_estimator_freetime = sqrt(var_freetime)
sd_estimator_freetime # 0.4659826

### P-Value (with z-score calculation and pnorm) ###
zscore_freetime = ((estimator_freetime) - 0)/sd_estimator_freetime #calculation of the Z-score
zscore_freetime # 0.8350373
# Calculation of p_value with (1 - pnorm(zscore))*2
pvalue_freetime = (1 - pnorm(zscore_freetime))*2
pvalue_freetime # 0.4036967

### Interval Confidence: confirm our hypothesis of the variable ###
LCL_freetime = estimator_freetime - 1.96 * sqrt(var_freetime)
LCL_freetime # -0.5242131
UCL_freetime = estimator_freetime + 1.96 * sqrt(var_freetime)
UCL_freetime # 1.302439
IC_freetime =c(LCL_int, UCL_int)  
IC_freetime
# estimator_freetime = 0.3891129 so in IC
