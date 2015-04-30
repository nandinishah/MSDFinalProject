library(ggplot2)
library(ggthemes)
library(scales)
library(dplyr)
library(leaps) # best subset


##################################################
## INITIAL CLEANING OF GTD DATA ##
##################################################

#setwd('/Users/nandinishah/Documents/Columbia/Sem2/ModelingSocialData-APMA/FinalProject')
setwd('/Users/Gabi/dev/ModelingSocialData/MSDFinalProject')
rm(list=ls())
theme_set(theme_bw())


# read as csv: keep headers and include empty strings as NA
data <- read.csv('2ns_gtd_06_to_13.csv',header=T, stringsAsFactor = F, na.strings ="")
# datafull <- read.csv('gtd_06_to_13.csv')

n <- nrow(data)
data.cols <- names(data)
# vector for columns to remove - have less than 1/2 the total number of rows
cols.exclude <- c()

# loop through columns and add to cols.exclude any column with less than n/2 rows
# where n/2 is total number of rows in a complete column
for (i in 1:length(data.cols) ){
  empty.rows <- sum(is.na(data[[data.cols[i]]]))
  if(n - empty.rows < n/2){
    cols.exclude <- rbind(cols.exclude, data.cols[i])
  }
}
# reduce data based on whether there are more than n/2 rows in column
data.small <- data[, !(data.cols %in% (cols.exclude))]

# columns with text or other info that may not be needed
text.cols <- c("eventid","location","summary", "motive","provstate","city",
               "latitude","longitude","specificity","target1","targsubtype1")
# remove additional columns w text - have numerical values
txt.cols.remove <- names(select(.data = data.small, contains("_txt")))

# remove text columns
data.small <- data.small[, !(names(data.small) %in% text.cols)]
data.small <- data.small[, !(names(data.small) %in% txt.cols.remove)]

# vectorize corp column from character
corp <- data.small$corp1
# unique names
unique.corp <- unique(data.small$corp1)
# match index 
corp.index <- match(corp, unique.corp)
# add to data.small
data.small$corp.index <- corp.index

# vectorize gname column 
gname <- data.small$gname
# unique names
unique.gname <- unique(data.small$gname)
# match index 
gname.index <- match(gname, unique.gname)
# add to data.small
data.small$gname.index <- gname.index

# remove gname and corp columns (bc of characters) 
data.small <- data.small[,!(names(data.small) %in% c("corp1","gname"))]

# rearrange columns so label is at far right
data.small <- data.small[,c(1:12,14:20,13)]

# label - success
label = data.frame(success = data.small$success, stringsAsFactors = F)

# subset selection
reg.model <- regsubsets(success ~ ., data = data.small, nvmax = 10)
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

