##################################################
      ## INITIAL ANALYSIS ON GTD DATA ##
##################################################

setwd('/Users/nandinishah/Documents/Columbia/Sem2/ModelingSocialData-APMA/FinalProject')
rm(list=ls())
theme_set(theme_bw())

library(dplyr)
library(ggplot2)

data <- read.csv('2ns_gtd_06_to_13.csv',header=T)
datafull <- read.csv('gtd_06_to_13.csv')

########## Grouping data by region ##########
databyregion <- data %>% group_by(region_txt) %>% summarize(count=n()) %>% arrange(desc(count))
dim(databyregion)
head(databyregion)
#qplot(x=databyregion$region_txt, y=databyregion$count)

######### Grouping data by country ##########
databycountry <- data %>% 
  group_by(country_txt) %>% 
  summarize(count=n()) %>%
  arrange(desc(count))
head(databycountry)
sum(head(databycountry)$count)/sum(databycountry$count)*100
#qplot(x=databycountry$count, geom="histogram")

######### Data for top 6 countries ###########
dataforIraq <- filter(data, country_txt=="Iraq")
dataforPakistan <- filter(data, country_txt=="Pakistan")
dataforAfghanistan <- filter(data, country_txt=="Afghanistan")
dataforIndia <- filter(data, country_txt=="India")
dataforThailand <- filter(data, country_txt=="Thailand")
dataforPhilippines <- filter(data, country_txt=="Philippines")
dataforUS <- filter(data, country_txt=="United States")

######### Analysis by type of attack ###########
data %>% 
  group_by(attacktype1_txt) %>%       ### Bombing & explosions rule
  summarize(count=n()) %>%
  arrange(desc(count))

dataforIraq %>% 
  group_by(attacktype1_txt) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

dataforPakistan %>% 
  group_by(attacktype1_txt) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

dataforAfghanistan %>% 
  group_by(attacktype1_txt) %>%
  summarize(count=n()) %>%
  arrange(desc(count))


######### Analysis by target type ###########
dataforIraq %>% 
  group_by(targtype1_txt) %>%     ### Private citizens & property hottest target
  summarize(count=n()) %>%
  arrange(desc(count))

dataforPakistan %>% 
  group_by(targtype1_txt) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

dataforAfghanistan %>%            ### Police tops Private citizens & property!
  group_by(targtype1_txt) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

dataforIndia %>% 
  group_by(targtype1_txt) %>%
  summarize(count=n()) %>%
  arrange(desc(count))


########## Analysis by suicide attacks ###########
dataforIraq %>%                 ### Around 8%
  group_by(suicide) %>%     
  summarize(count=n()) %>%
  arrange(desc(count))

dataforPakistan %>%             ### Around 5%
  group_by(suicide) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

dataforAfghanistan %>%           ### Highest for Afghanistan ~ 12%
  group_by(suicide) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

dataforIndia %>%                ### Around 0.2%
  group_by(suicide) %>%
  summarize(count=n()) %>%
  arrange(desc(count))


############# Analysis by Group Name ##############
data %>%
  group_by(gname) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

data %>%
  filter(.,gname=="Communist Party of India - Maoist (CPI-Maoist)") %>%
  group_by(country_txt) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

data %>%
  filter(.,gname=="Taliban") %>%
  group_by(country_txt) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

data %>%
  filter(.,gname=="Tehrik-i-Taliban Pakistan (TTP)") %>%
  group_by(country_txt) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

data %>%
  filter(.,gname=="Al-Shabaab") %>%
  group_by(country_txt) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

data %>%
  filter(.,gname=="Boko Haram") %>%
  group_by(country_txt) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

############ INDIA ###############
dataforIndia %>%                    ### CPI rules
  group_by(gname) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

dataforIndia %>%                    ### CPI by geography
  filter(.,gname=="Communist Party of India - Maoist (CPI-Maoist)") %>%
  group_by(provstate) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

dataforIndia %>%                    ### CPI by geography
  filter(.,gname=="Communist Party of India - Maoist (CPI-Maoist)") %>%
  #group_by(provstate,attacktype1_txt) %>%
  group_by(provstate,targtype1_txt) %>%
  summarize(count=n()) %>%
  arrange(desc(count))
  
dataforIndia %>%                    ### LeT by geography
  filter(.,gname=="Lashkar-e-Taiba (LeT)") %>%
  group_by(provstate) %>%
  summarize(count=n()) %>%
  arrange(desc(count))


############ TALIBAN by geography ###############
dataforAfghanistan %>%                              ### Mainly police and military
  filter(.,gname=="Taliban") %>%
  group_by(provstate,targtype1_txt) %>%
  summarize(count=n()) %>%
  arrange(desc(count))

dataforPakistan %>%
  filter(.,gname=="Taliban") %>%
  group_by(provstate,targtype1_txt) %>%
  summarize(count=n()) %>%
  arrange(desc(count))


###########  MIDDLE-EAST CRISIS  ###########
dataforEgypt <- data %>%                                              ### Decent amount of data with political "motive"
  subset(.,region_txt=="Middle East & North Africa" & country_txt=="Egypt")
  
dataforTunisia <- data %>%                                            ### Not much data
  subset(.,region_txt=="Middle East & North Africa" & country_txt=="Tunisia")

dataforLibya <- data %>%                                            ### Decent amount of data
  subset(.,region_txt=="Middle East & North Africa" & country_txt=="Libya")

dataforLibya %>%
  group_by(gname) %>% #=="Gaddafi Loyalists")
  summarize(count=n()) %>%
  arrange(desc(count))
