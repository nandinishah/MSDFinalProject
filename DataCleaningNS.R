library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(leaps) # best subset
library(glmnet) # lasso

##################################################
## INITIAL CLEANING OF GTD DATA ##
##################################################

#setwd('/Users/nandinishah/Documents/Columbia/Sem2/ModelingSocialData-APMA/FinalProject')
setwd('/Users/Gabi/dev/ModelingSocialData/MSDFinalProject')
rm(list=ls())
theme_set(theme_bw())


# read as csv: keep headers and include empty strings as NA
#data <- read.csv('2ns_gtd_06_to_13.csv',header=T, stringsAsFactor = F, na.strings ="")
datafull <- read.csv('gtd_06_to_13.csv', header=T, stringsAsFactor = F, na.strings ="")

# n <- nrow(datafull)
#data.cols <- names(datafull)

######### filter data for top 6 countries = 70% of full dataset (datafull) ###########
data.top6 <- filter(datafull, country_txt=="Iraq" | country_txt=="Pakistan" | country_txt=="Afghanistan" | 
                      country_txt=="India" | country_txt=="Philippines" | country_txt=="Thailand") 
rm(datafull)

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

# remove international columns with too many unknowns (-9) rows
data.small <- data.small[, !(names(data.small) %in% int.exclude)]

######### vectorizing text columns ###########

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

#@@@ Combining the response variables: nkill + nwound = ncasualty
ncasualty <- data.small$nkill + data.small$nwound
data.small$ncasualty <- c(log2(ncasualty))
data.small$nkill <- NULL
data.small$nwound <- NULL
rm(ncasualty)

#@@@ Working around NAs and Inf - since glmnet does not work with them
cleaningNAs <- function(dat, i){
  #"natlty" - assuming country of incidence based on source (GTD) documentation
  if (names(dat)[i] == "natlty1"){
    print('here1')
    column <- which(names(dat)%in%c("country"))
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {dat[j,i] = dat[j, column]}}
    print('natlty1 Worked!')
  }
  #"guncertain1" - deleting the rows since not a significant number
  if (names(dat)[i] == "guncertain1"){
    removecols <- c()
    print("here2")
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {removecols <- rbind(removecols, j) }}
    dat <- dat[-c(removecols),]
    print('guncertain1 Worked!')
  }
  #"ishostkid" - deleting the rows since not a significant number
  if (names(dat)[i] == "ishostkid"){
    print("here3")
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {dat <- dat[-c(j),]} }
    print('ishostkid Worked!')
  }
  #"ncasualty" - deleting the rows since this is the response variable
  if (names(dat)[i] == "ncasualty"){
    removecols <- c()
    print("here4")
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i]) || is.infinite(dat[j,i])) {removecols <- rbind(removecols, j) }}
    dat <- dat[-c(removecols),]
    print('ncasualty Worked!')
  }
  #"weapsubtype1" - deleting the rows since not a significant number
  #@@@ NS: implement LR if can
  if (names(dat)[i] == "weapsubtype1"){
    removecols <- c()
    print("here5")
    for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {removecols <- rbind(removecols, j)}}
    dat <- dat[-c(removecols),]
    print('weapsubtype1 Worked!')
  }
#   #"nperps" and "nperpcap" columns 
#   #@@@ NS: try to figure using LR.
#   if (names(dat)[i] == "nperps"){
#     removecols <- c()
#     print("here7")
#     for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {removecols <- rbind(removecols, j)}}
#     dat <- dat[-c(removecols),]
#     i = which(names(dat)%in%c("nperpcap"))
#     for (j in 1:length(dat[,i])){if (is.na(dat[j,i])) {removecols <- rbind(removecols, j)}}
#     dat <- dat[-c(removecols),]
#     print('nperps Worked!')
#   }
  #"nperps" and "nperpcap" columns --- ignoring these cols for now since metrics prove these are not significant
  if (names(dat)[i] == "nperps"){
    removecols <- c()
    print("here6")
    column <- which(names(dat)%in%c("nperps"))
    dat[,column] <- NULL
    column <- which(names(dat)%in%c("nperpcap"))
    dat[,column] <- NULL
    print('nperps Worked!')
  }
  return(dat)
}




########## WIP: Linear Regression for weapsubtype1 ################################################################## 
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

form <- as.formula(sprintf('weapsubtype1 ~ weaptype1 + ncasualty + property + guncertain1'))
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



#@@@ Removing the NAs by calling the function defined above for each column in the data set
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
    data.small <- cleaningNAs(data.small, i)  #calling the function
    countnow = sum(is.na(data.small[,i]))
    rationow = countnow/length(data.small[,i])
  }
  flag <- c(names(data.small)[i], anyNA(data.small[,i]), ratiobefore, countbefore, rationow, countnow) 
  NAVector <- rbind(NAVector,flag)
}
colnames(NAVector) <- c("feature","anyNA","ratiobefore","countbefore","rationow","countnow")
  

#@@@ splitting into train/test sets
set.seed(1010)
ndx <- sample(nrow(data.small), floor(nrow(data.small) * 0.9))
train <- data.small[ndx,]
test <- data.small[-ndx,]
trainX <- train[!names(test) %in% c("ncasualty")]
trainY <- train[names(test) %in% ("ncasualty")]
testX <- test[,!names(test) %in% c("ncasualty")]
testY <- test[,names(test) %in% ("ncasualty")]
rm(train)
rm(test)

#@@@ Lasso Regression on train set
cvob1=cv.glmnet(as.matrix(trainX),as.matrix(trainY),alpha=1,family="gaussian")
plot(cvob1)
grid()
filename = "lassoLogLambda.png"
dev.copy(device = png, filename = filename) # save png
dev.off()
coef(cvob1)
best_lambda <- cvob1$lambda.min

# Ridge regression
cvob2=cv.glmnet(as.matrix(trainX),as.matrix(trainY),alpha=0)
plot(cvob2)
coef(cvob2)

# glmnet without cv
cvob3=glmnet(as.matrix(trainX),as.matrix(trainY))
plot(cvob3,label=TRUE)
grid()
filename = "lassoL1Norm.png"
dev.copy(device = png, filename = filename) # save png
dev.off()
coef(cvob3)

#@@@ Lasso Regression on test set
#testPred <- predict(cvob1,as.matrix(testX))
#trainPred <- predict(cvob1,as.matrix(trainX))
RMSE.test <- sqrt(sum((testY-(predict(cvob1,as.matrix(testX))))^2)/length(testY))
RMSE.train <- sqrt(sum((trainY-(predict(cvob1,as.matrix(trainX))))^2)/nrow(trainY))
RMSE.test
RMSE.train

############ CI: Not possible because underlying distribution is not symmetric normal ###############

qplot(x=trainY$ncasualty, geom="histogram", binwidth=0.01) +
  geom_vline(xintercept=mean(trainY$ncasualty), linetype=2, color="red")
n=nrow(testX)
LCL <- testY - 1.96*sqrt(RMSE.test/n)
UCL <- testY + 1.96*sqrt(RMSE.test/n)
mean(mean(trainY$ncasualty) >= LCL & mean(trainY$ncasualty) <= UCL)




######################## Logistic regression on "success" column ##########################
#@@@ NS: To try and adjust weights to adjust for reporting bias
model <- glm(success ~ ., data=data.small[ndx, ], family="binomial") 
table(predict(model, data.small[-ndx, ]) > 0, data.small[-ndx, "success"])




######################## subset selection ##################################
reg.model <- regsubsets(ncasualty ~ ., data = data.small, nvmax = 26)
reg.summary <- summary(reg.model)

# point which maximizes adjusted rsquared, Cp and BIC - Best Subset Selection
max.adjR <- which.max(reg.summary$adjr2) 
min.cp <- which.min(reg.summary$cp) 
min.bic <- which.min(reg.summary$bic) 
min.rss <- which.min(reg.summary$rss)


# val.errors = rep(NA, 25)
# # put into modelmatrix format
# x.test <- model.matrix(testY~., data = testX)
# # loop through coefs and get predictions
# for (i in 1:25) {
#   coefi = coef(reg.model, id = i)
#   pred = x.test[, names(coefi)] %*% coefi
#   val.errors[i] = mean((testY - pred)^2)
# }



# selecting best model based on least rss
coef(reg.model, min.rss)
reg.model.coefs <- coef(reg.model, min.rss)
x.test <- model.matrix(testY~., data = testX)
reg.model.pred <- x.test[, names(coef(reg.model, min.rss))] %*% coef(reg.model, min.rss)
RMSE.test.SS <- sqrt(sum((testY-(reg.model.pred))^2)/length(testY))
RMSE.test <- sqrt(sum((testY-(predict(cvob1,as.matrix(testX))))^2)/length(testY))
RMSE.train <- sqrt(sum((trainY-(predict(cvob1,as.matrix(trainX))))^2)/nrow(trainY))
  
## plot adjusted r2
plot(reg.summary$adjr2, xlab = "Size of Subset", ylab = "Adjusted RSq", type ="l", col="blue", main="Best subset selection")
# add point with max RSq
points(max.adjR, reg.summary$adjr2[max.adjR], col="red", cex=2, pch=20) 
grid()

## plot Cp
plot(reg.summary$cp, xlab = "Size of Subset", ylab = "Cp", type ="l", col="blue", main="Best subset selection")
# add point with min Cp
points(max.adjR, reg.summary$cp[min.cp], col="red", cex=2, pch=20) 
grid()

## plot BIC
plot(reg.summary$bic, xlab = "Size of Subset", ylab = "BIC", type ="l", col="blue", main="Best subset selection")
# add point with min BIC
points(min.bic, reg.summary$bic[min.bic], col="red", cex=2, pch=20) 
grid()

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

#@@@ plot the RSS for different models 
## plot rss
plot(reg.summary$rss, xlab = "Size of Subset", ylab = "RSS", type ="l", col="blue", main="Best subset selection")
# add point with min RSS
points(min.rss, reg.summary$rss[min.rss], col="red", cex=2, pch=20) 
grid()
# best model according to RSS
print("Best subset based on RSS")
#coef(reg.model, min.rss)



#@@@ VISAL/GABY - Really tried doing this for sometime but it's just so much easier in excel :P If you can figure in R pls do.
#@@@ Comparing ranks of the coeffs of the two models
SScoef <- coef(reg.model, min.rss)
SScoef <- sort(SScoef,decreasing=TRUE)
Lascoef <- data.frame()
Lascoef <- rbind(names(data.small))
Lascoef <- rbind(Lascoef,temp[,1])
Lascoef <- sort(Lascoef[2,], decreasing=TRUE)
rank(SScoef)
rank(Lascoef[2,])


######################## Plotting ##################################

# look up countries, attacks
countries.index <- data.frame(country = data.top6$country_txt, country.id = data.top6$country, stringsAsFactors = F)
attacks.index <- data.frame(attack = data.top6$attacktype1_txt, attack.id = data.top6$attacktype1, stringsAsFactors = F)

countries.plot <- unique(countries.index[order(countries.index$country.id),])
attacks.plot <- unique(attacks.index[order(attacks.index$attack.id),])

#1) plot of casualties by country
plot.countries <- ggplot(data.small, aes(x=country, y = ncasualty)) +
  geom_point(aes(color = country, group = country), size = 2.2) +
  scale_x_continuous(breaks=c(8,86,95,140,160,200),
                   labels=c("Afghanistan", "India", "Iraq", "Pakistan", "Philippines","Thailand"),
                   name = "") +
  # change axis to show integers
  scale_y_continuous(name = "Casualties", labels = function (x) floor(x)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title.y = element_text(angle = 0)) +
  theme(panel.grid = element_line(size = 0.4, linetype = "dotted")) +
  scale_color_continuous(guide="none") +
  ggtitle('Number of Casualties by Country')
plot.countries
ggsave(filename = "casualties_countries.png", plot = plot.countries, path =".")
  

#2) plot of casualties by attacktype
plot.attacks <- ggplot(data.small, aes(x=attacktype1, y = ncasualty)) +  
  geom_point(aes(color="firebrick", group = attacktype1), size =2.2) +
  scale_x_discrete(breaks=c(1,2,3,4,5,6,7,8,9),
                   labels= c("Assassination","Armed\nAssault","Bombing/\nExpolsion","Hijacking",
                             "Hostage \ntaking","Kidnapping","Infra-\nstructure \nAttack",
                             "Unarmed \nAssault","Unknown"), name ="") +
  scale_y_continuous(name = "Casualties", labels = function (x) floor(x)) +
  scale_color_discrete(guide="none") +
  # hide tickmarks and rotate labels
  theme(axis.ticks = element_blank()) + 
  theme(axis.title.y = element_text(angle = 90)) +
  theme(axis.title.y = element_text(size = rel(1), angle = 0)) +
  theme(panel.grid = element_line(size = 0.4, linetype = "dotted")) +
  ggtitle('Number of Casualties by Attack Type') 
plot.attacks
ggsave(filename = "attacks_casualties.png", plot = plot.attacks, path =".")


#3) plot of casualties by multiple incidents
plot.multiple <- ggplot(data.small, aes(x=multiple, y = ncasualty)) +
  geom_point(aes(color = factor(multiple), group = multiple), size =2.3) +
  scale_x_continuous(name = "Multiple Incidents") +
  scale_y_continuous(name = "Casualties", labels = function (x) floor(x)) +
  theme(axis.ticks = element_blank()) +
  theme(axis.title.y = element_text(angle = 0)) +
  theme(panel.grid = element_line(size = 0.4, linetype = "dotted")) +
  scale_color_discrete(guide="none") +
  ggtitle('Number of Casualties by Multiple Incidents')
plot.multiple
ggsave(filename = "casualties_multiple.png", plot = plot.multiple, path =".")


#4) plot of casualties by day
plot.day <- ggplot(data.small, aes(x=iday, y = ncasualty)) +
  geom_point(aes(color = factor(iday), group = iday), size =2) +
  scale_x_discrete(name = "Day of Month") +
  scale_y_continuous(name = "Casualties", labels = function (x) floor(x) ) + 
  theme(axis.ticks = element_blank()) +
  theme(axis.title.y = element_text(angle = 0)) +
  theme(panel.grid = element_line(size = 0.4, linetype = "dotted")) +
  scale_color_discrete(guide="none") + coord_equal() +
  ggtitle('Number of Casualties by Day')
plot.day
ggsave(filename = "casualties_day.png", plot = plot.day, path =".")


