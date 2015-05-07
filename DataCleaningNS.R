library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(leaps) # best subset
library(glmnet) # lasso

##################################################
## INITIAL CLEANING OF GTD DATA ##
##################################################

setwd('/Users/nandinishah/Documents/Columbia/Sem2/ModelingSocialData-APMA/FinalProject')
#setwd('/Users/Gabi/dev/ModelingSocialData/MSDFinalProject')
rm(list=ls())
theme_set(theme_bw())


# read as csv: keep headers and include empty strings as NA
#data <- read.csv('2ns_gtd_06_to_13.csv',header=T, stringsAsFactor = F, na.strings ="")
datafull <- read.csv('gtd_06_to_13.csv', header=T, stringsAsFactor = F, na.strings ="")

# n <- nrow(datafull)
#data.cols <- names(datafull)

######### filter data for top 6 countries ###########
data.top6 <- filter(datafull, country_txt=="Iraq" | country_txt=="Pakistan" | country_txt=="Afghanistan" | 
                      country_txt=="India" | country_txt=="Philippines" | country_txt=="Thailand") #country_txt=="United States")
rm(datafull)
#@@@ United States versus Thailand

######### cleaning columns ###########

# loop through columns and add to cols.exclude any column with less than n/2 rows
# where n/2 is total number of rows in a complete column
# cols.exclude = vector for columns to remove - have less than 1/2 the total number of rows
reduce_cols <- function(dat){
  cols.exclude <- c()
  data.cols <- names(dat)
  n <- nrow(dat)
  for (i in 1:length(data.cols) ){
    empty.rows <- sum(is.na(dat[[data.cols[i]]]))
    if(n - empty.rows < n/2){
      cols.exclude <- rbind(cols.exclude, data.cols[i])
    }
  }
  return(cols.exclude)
}

# columns to be removed -  based on where there are less than n/2 rows
cols.remove = reduce_cols(data.top6)

# columns names in data
data.columns <- names(data.top6)

# reduce data based on whether there are more than n/2 rows in column
data.small <- data.top6[, !(data.columns %in% (cols.remove))]

# remove any columns with "_txt" - have numerical values already
txt.cols.remove <- names(select(.data = data.small, contains("_txt")))

# remove "_txt" columns
data.small <- data.small[, !(names(data.small) %in% txt.cols.remove)]

# columns with text or other info that may not be needed
other.cols <- c("eventid", "provstate", "city","latitude","longitude","specificity",
               "location","summary","targsubtype1","motive","weapdetail","propcomment","scite1","scite2",
               "dbsource", "target1", "corp1", "nkillter", "nkillus", "nwoundus","nwoundte")
# remove other columns with text or other information thats not needed
data.small <- data.small[, !(names(data.small) %in% other.cols)]
#@@@ What abt "nxxxus" cols, target1, extended etc? 
#@@@ Removed additionally - "target1", "corp1", "nkillter", "nkillus", "nwoundus","nwoundte"

# last 4 columns contain internation info - check that there is sufficient data filled
# -9 = unknown - check that there are at least n/2 knowns in these columns
internat.cols <- names(select(.data = data.small, contains("INT_")))
n.rows <- nrow(data.small)
int.exclude <- c()
for (i in 1:length(internat.cols) ){
   unknown.rows <- sum(data.small[,internat.cols[i]]==-9)
   if(n.rows - unknown.rows < n.rows/2){
     int.exclude <- rbind(int.exclude, internat.cols[i])
   }
}
#@@@ NS: Want to look at this in more detail.

# remove international columns with too many unknowns (-9) rows
data.small <- data.small[, !(names(data.small) %in% int.exclude)]

######### vectorizing text columns ###########

#@@@ There's 6k+ of these. Not sure if it makes sense to include. Too much variance, prolly not adding any value.
# # vectorize corp column from character
# corp <- data.small$corp1
# # unique names
# unique.corp <- unique(data.small$corp1)
# # match index 
# corp.index <- match(corp, unique.corp)
# # add to data.small
# data.small$corp.index <- corp.index


# vectorize gname column 
gname <- data.small$gname
# unique names
unique.gname <- unique(data.small$gname)
# match index 
gname.index <- match(gname, unique.gname)
# add to data.small
data.small$gname.index <- gname.index
# remove gname, target1 and corp columns (bc of characters) 
#data.small <- data.small[,!(names(data.small) %in% c("corp1","gname","target1"))]
data.small <- data.small[,!(names(data.small) %in% c("gname"))]
rm(gname.index)
rm(gname)
rm(unique.gname)
rm(unknown.rows)

#@@@ This might not be a good column to include. Redundant info. If we do use, should normalize it. Removed for now.
# # vectorize target1 column 
# target1 <- data.small$target1
# # unique names
# unique.target1 <- unique(data.small$target1)
# # match index 
# target1.index <- match(target1, unique.target1)
# # add to data.small
# data.small$target1.index <- target1.index

#@@@ Combining the response variables: nkill + nwound = nvictim
nvictim <- data.small$nkill + data.small$nwound
# removecols <- c()
# print("here4 first")
# column <- which(data.small%in%c("nvictim"))
# for (j in 1:length(dat[,i])){if (is.na(data.small[j,i]) || is.infinite) {removecols <- rbind(removecols, j) }}
#   dat <- dat[-c(removecols),]
#   print('nvictim Worked!')
# }
data.small$nvictim <- c(log2(nvictim))
# data.small$nkill <- NULL
# data.small$nwound <- NULL
rm(nvictim)

#@@@ Working with NAs
cleaningNAs <- function(dat, i){
  #"natlty" - assuming country of incidence
  if (names(dat)[i] == "natlty1"){
    #@@@ Has to be a better way to do this than below......
    print('here1')
    column <- which(names(dat)%in%c("country"))
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {dat[j,i] = dat[j, column]}}
    print('natlty1 Worked!')
  }
  if (names(dat)[i] == "guncertain1"){
    removecols <- c()
    print("here2")
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {removecols <- rbind(removecols, j) }}
    dat <- dat[-c(removecols),]
    print('guncertain1 Worked!')
  }
  if (names(dat)[i] == "ishostkid"){
    print("here3")
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {dat <- dat[-c(j),]} }
    print('ishostkid Worked!')
  }
  if (names(dat)[i] == "nvictim"){
    removecols <- c()
    print("here4")
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i]) || is.infinite(dat[j,i])) {removecols <- rbind(removecols, j) }}
    dat <- dat[-c(removecols),]
    print('nvictim Worked!')
  }
  
  #@@@ weapsubtype1
  if (names(dat)[i] == "weapsubtype1"){
    removecols <- c()
    print("here5")
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {removecols <- rbind(removecols, j)}}
    dat <- dat[-c(removecols),]
    print('weapsubtype1 Worked!')
    #fit <- lm(y ~ x1 + x2 + x3, data=mydata)
  }
  #@@@ nperps and nperpcap columns --- ignoring these cols for now. to figure LR.
  if (names(dat)[i] == "nperps"){
    removecols <- c()
    print("here7")
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {removecols <- rbind(removecols, j)}}
    dat <- dat[-c(removecols),]
    i = which(names(dat)%in%c("nperpcap"))
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {removecols <- rbind(removecols, j)}}
    dat <- dat[-c(removecols),]
    print('nperps Worked!')
  }
#   #@@@ nperps and nperpcap columns --- ignoring these cols for now. to figure LR.
#   if (names(dat)[i] == "nperps"){
#     removecols <- c()
#     print("here6")
#     column <- which(names(dat)%in%c("nperps"))
#     dat[,column] <- NULL
#     column <- which(names(dat)%in%c("nperpcap"))
#     dat[,column] <- NULL
#     print('nperps Worked!')
#   }
  return(dat)
}

########## Linear Regression for weapsubtype1 : WIP ################################################################## 
plot(lowess(x=data.small$weaptype1+data.small$property,y=data.small$weapsubtype1))

fit <- data.frame()
removecols <- c()
data.lm <- data.small
k = which(names(data.lm)%in%c("weapsubtype1"))
for (j in 1:length(data.lm$weapsubtype1)){if (is.na(data.lm[j,k])) { removecols <- rbind(removecols, j) } }
data.lm <- data.lm[-c(removecols),]
#data.lm.labels <- data.lm[,data.lm$weapsubtype1]
#data.lm$weapsubtype1 <- NULL

num.train <- floor(nrow(data.lm)*0.5)
train.ndx <- sample(1:nrow(data.lm), num.train, replace=F)
data.lm.train <- data.lm[train.ndx, ]
#data.lm.train.labels <- data.lm.labels[train.ndx, ]
data.lm.test <- data.lm[-train.ndx, ]
#data.lm.test.labels <- data.lm.labels[-train.ndx, ]

form <- as.formula(sprintf('weapsubtype1 ~ weaptype1 + nvictim + property + guncertain1'))
model <- lm(form, data=data.lm.train)
summary(model)
data.lm.train.labels <- predict(model, data.lm.train)
fit.train <- cor(predict(model, data.lm.train), data.lm.train$weapsubtype1)
fit.test <- cor(predict(model, data.lm.test), data.lm.test$weapsubtype1)

plot.data <- merge(model.adults, views.by.age.gender, by=c("age", "gender"))
ggplot(data=plot.data, aes(x=weaptype1, y=weapsubtype1)) +
  geom_line(aes()) +
  geom_point(aes(x=age, y=mean.daily.views, shape=gender)) +
  xlab('Age') + ylab('Daily pageviews') +
  theme(legend.title=element_blank(), legend.position=c(0.9,0.85))
ggsave(filename='./lectures/lecture_4/figures/mean_daily_pageviews_by_age_and_gender.pdf', width=8, height=4)


##################################################################################################################

NAVector <- c()
flag <- c()
for (i in 1:length(names(data.small)))
{
  ratiobefore = 0
  rationow = 0
  countbefore = 0
  countnow = 0
  if (anyNA(data.small[,i])) {
    countbefore = sum(is.na(data.small[,i]))
    ratiobefore = countbefore/length(data.small[,i])
    data.small <- cleaningNAs(data.small, i)
    countnow = sum(is.na(data.small[,i]))
    rationow = countnow/length(data.small[,i])
  }
  flag <- c(names(data.small)[i], anyNA(data.small[,i]), ratiobefore, countbefore, rationow, countnow) 
  NAVector <- rbind(NAVector,flag)
}


# # label - success
# label = data.frame(success = data.small$success, stringsAsFactors = F)

#@@@ splitting into train/test sets
set.seed(1010)
ndx <- sample(nrow(data.small), floor(nrow(data.small) * 0.9))
train <- data.small[ndx,]
test <- data.small[-ndx,]
trainX <- train[!names(test) %in% ("nvictim")]
trainY <- train[,as.numeric(names(test) %in% ("nvictim"))]
testX <- test[,!names(test) %in% ("nvictim")]
testY <- test[,names(test) %in% ("nvictim")]
rm(train)
rm(test)
rm(ndx)

#@@@ Lasso Regression on train
lambda = 0.01
#cvob1=glmnet(as.matrix(trainX),trainY, lambda=0.5) # NS: cv = cross-validation; default alpha=1 
cvob1=cv.glmnet(as.matrix(trainX),trainY) # NS: cv = cross-validation; default alpha=1 
plot(cvob1)
coef(cvob1)
cvob2=cv.glmnet(as.matrix(trainX),trainY,alpha=0)
plot(cvob2)
coef(cvob2)

tfit=glmnet(as.matrix(trainX),trainY,lower=-.7,upper=.5)
plot(tfit)

# subset selection
reg.model <- regsubsets(nvictim ~ ., data = data.small, nvmax = 20)
reg.summary <- summary(reg.model)

# point which maximizes adjusted rsquared, Cp and BIC - Best Subset Selection
max.adjR <- which.max(reg.summary$adjr2) 
min.cp <- which.min(reg.summary$cp) 
min.bic <- which.min(reg.summary$bic) 

## plot adjusted r2
plot(reg.summary$adjr2, xlab = "Size of Subset", ylab = "Adjusted RSq", type ="l", col="blue", main="Best subset selection")
# add point with max RSq
points(max.adjR, reg.summary$adjr2[max.adjR], col="red", cex=2, pch=20) 
grid()
# best model according to Adjusted R squared
print("Best subset based on Adjusted R^2")
coef(reg.model, max.adjR)


## plot Cp
plot(reg.summary$cp, xlab = "Size of Subset", ylab = "Cp", type ="l", col="blue", main="Best subset selection")
# add point with min Cp
points(max.adjR, reg.summary$cp[min.cp], col="red", cex=2, pch=20) 
grid()

# best model according to CP
print("Best subset based on Cp")
coef(reg.model, min.cp)


## plot BIC
plot(reg.summary$bic, xlab = "Size of Subset", ylab = "BIC", type ="l", col="blue", main="Best subset selection")
# add point with min BIC
points(min.bic, reg.summary$bic[min.bic], col="red", cex=2, pch=20) 
grid()

# best model according to BIC
print("Best subset based on BIC")
coef(reg.model, min.bic)


# plots - which subset to chose?
# based on adjusted R2
plot(reg.model, scale = "adjr2", main="Adjr2 - subsets",col="pink")
grid()
filename = "adjr2_subsets.png"
dev.copy(device = png, filename = filename) # save png
dev.off()

# based on CP
plot(reg.model, scale = "Cp", main ="Cp - subsets", col="blue")
grid()
filename = "cp_subsets.png"
dev.copy(device = png, filename = filename) # save png
dev.off()

# based on BIC 
plot(reg.model, scale = "bic", main="BIC - subsets", col="green")
grid()
filename = "bic_subsets.png"
dev.copy(device = png, filename = filename) # save png
dev.off()

