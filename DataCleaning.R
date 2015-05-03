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
#data <- read.csv('2ns_gtd_06_to_13.csv',header=T, stringsAsFactor = F, na.strings ="")
datafull <- read.csv('gtd_06_to_13.csv', header=T, stringsAsFactor = F, na.strings ="")

# n <- nrow(datafull)
#data.cols <- names(datafull)

######### filter data for top 6 countries ###########
data.top6 <- filter(datafull, country_txt=="Iraq" | country_txt=="Pakistan" | country_txt=="Afghanistan" | 
                      country_txt=="India" | country_txt=="Philippines" | country_txt=="United States")


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
               "dbsource")
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

# vectorize target1 column 
target1 <- data.small$target1
# unique names
unique.target1 <- unique(data.small$target1)
# match index 
target1.index <- match(target1, unique.target1)
# add to data.small
data.small$target1.index <- target1.index


# remove gname, target1 and corp columns (bc of characters) 
data.small <- data.small[,!(names(data.small) %in% c("corp1","gname","target1"))]

# rearrange columns so label is at far right
data.small <- data.small[,c(1:12,14:35,13)]

# label - success
label = data.frame(success = data.small$success, stringsAsFactors = F)

# subset selection
reg.model <- regsubsets(success ~ ., data = data.small, nvmax = 20)
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

