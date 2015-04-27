library(ggplot2)
library(ggthemes)
library(scales)


##################################################
      ## INITIAL ANALYSIS ON GTD DATA ##
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


########## Grouping data by region ##########
databyregion <- data %>% group_by(region_txt) %>% summarize(count=n()) %>% arrange(desc(count))
dim(databyregion)
head(databyregion)
#qplot(x=databyregion$region_txt, y=databyregion$count)


# scatter plot of counts by region
plot.regions <- ggplot(databyregion, aes(x=count, y = region_txt, group = region_txt)) + 
  geom_point(aes(group = region_txt, color = region_txt), size=4.5) +
  scale_size_continuous(guide="none") +
  scale_color_discrete(guide="none") + scale_x_continuous(label=comma, name = "Count") +
  scale_y_discrete(name="") +
  ggtitle('Count by Region') 
plot.regions

# group by region and attack type
data.attack.region <- data %>% 
  group_by(region_txt, attacktype1_txt) %>%       ### Bombing & explosions rule
  summarize(count = n()) %>%
  arrange(desc(count))


# plot attack types by regions with facets
plot.attacks <- ggplot(data.attack.region, aes(x=count, y= region_txt)) +  
  geom_point(aes(color = attacktype1_txt, group = attacktype1_txt), size =2)  +
  facet_wrap(~attacktype1_txt,drop = T) + 
  scale_x_continuous(label=comma, name = "Count") +
  scale_color_discrete(guide="none") +
  scale_y_discrete(name="") + theme(axis.title.y = element_text(angle = 0)) +
  ggtitle('Attack Types by Region') 
plot.attacks
  

######### Grouping data by country ##########
databycountry <- data %>% 
  group_by(country_txt) %>% 
  summarize(count=n()) %>%
  arrange(desc(count))
head(databycountry)
sum(head(databycountry)$count)/sum(databycountry$count)*100
#qplot(x=databycountry$count, geom="histogram")

dataforIraq <- filter(data, country_txt=="Iraq")
dataforPakistan <- filter(data, country_txt=="Pakistan")
dataforAfghanistan <- filter(data, country_txt=="Afghanistan")
dataforIndia <- filter(data, country_txt=="India")
dataforThailand <- filter(data, country_txt=="Thailand")
dataforPhilippines <- filter(data, country_txt=="Philippines")
dataforUS <- filter(data, country_txt=="United States")

######### Data for top 6 countries ###########
data.top6 <- filter(data, country_txt=="Iraq" | country_txt=="Pakistan" | country_txt=="Afghanistan" | 
                      country_txt=="India" | country_txt=="Philippines" | country_txt=="United States")

# group top6 by attacktype
data.top6.byCountry <- data.top6 %>%
  group_by(country_txt, attacktype1_txt) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
data.top6.byCountry

# group by attacktype and target type
data.top6.byAttack <- data.top6 %>%
  group_by(country_txt, attacktype1_txt, targtype1_txt) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

# group by  nationality of target
data.top6.byNat <- data.top6 %>%
  group_by(country_txt, natlty1_txt) %>%
  summarize(count = n()) %>%
  arrange(desc(count))

######### PLOTS - TOP 6 ONLY ###########
# plot attack types (by country)
ggplot(data.top6.byCountry, aes(x=count, y = country_txt)) +  
  geom_point(aes(color = attacktype1_txt, group = attacktype1_txt), size =2)  +
  facet_wrap(~attacktype1_txt,drop = T) + 
  scale_x_continuous(label=comma, name = "Count") +
  scale_color_discrete(guide="none") +
  scale_y_discrete(name="") + theme(axis.title.y = element_text(angle = 0)) +
  ggtitle('Attack Types by Country - Top 6') 

# plot target type (by country)
ggplot(data.top6.byAttack, aes(x=count, y = country_txt)) +  
  geom_point(aes(color = targtype1_txt, group = targtype1_txt), size =2)  +
  facet_wrap(~targtype1_txt,drop = T) + 
  scale_x_continuous(label=comma, name = "Count") +
  scale_color_discrete(guide="none") +
  scale_y_discrete(name="") + theme(axis.title.y = element_text(angle = 0)) +
  ggtitle('Target Types by Country - Top 6') 
  
# plot nationality of target by country - top6
ggplot(data.top6.byNat, aes(x=count, y = country_txt)) +  
  geom_point(aes(color = natlty1_txt, group = natlty1_txt), size =2)  +
  facet_wrap(~ natlty1_txt, drop = T) + 
  scale_x_continuous(label=comma, name = "Count") +
  scale_color_discrete(guide="none") +
  scale_y_discrete(name="") + theme(axis.title.y = element_text(angle = 0)) +
  ggtitle('Nationality of Targets by Country - Top 6') 


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
