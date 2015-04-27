library(ggplot2)
library(ggthemes)
library(scales)


##################################################
## INITIAL CLEANING OF GTD DATA ##
##################################################

#setwd('/Users/nandinishah/Documents/Columbia/Sem2/ModelingSocialData-APMA/FinalProject')
setwd('/Users/Gabi/dev/ModelingSocialData/MSDFinalProject')
rm(list=ls())
theme_set(theme_bw())

library(dplyr)
library(ggplot2)

# read as csv: keep headers and include empty strings as NA
data <- read.csv('2ns_gtd_06_to_13.csv',header=T, stringsAsFactor = F, na.strings ="")
datafull <- read.csv('gtd_06_to_13.csv')

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
# columns with text
text.cols <- c("location","summary", "motive")
# reduce data based on whether there are more than n/2 rows in column
data.small <- data[, !(data.cols %in% (cols.exclude))]
# remove text columns
data.small <- data.small[, !(names(data.small) %in% text.cols)]


# label - success
label = data.frame(success = data.small$success, stringsAsFactors = F)

