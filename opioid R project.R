medicaiddrugs=read.csv('omt-mdcd-r21-p11-v10-ytd19-geo-zip-1.csv')
opioidcounty=read.csv('~/Downloads/archive/Wide_Master.csv')
summary(medicaiddrugs)
library(tidyverse)
library(ggplot2)
install.packages(maps)
names(medicaiddrugs)

colnames(medicaiddrugs)[4] <- "State"

unique(medicaiddrugs$Geo_Lvl)
#clean up state data
statemedicaid=medicaiddrugs%>%filter(Geo_Lvl!='National' & Plan_Type=='All')
statemedicaid=select(statemedicaid,-c(Geo_Lvl,Plan_Type))
statemedicaid=select(statemedicaid, -c(starts_with('LA')))

nationalmedicaid=medicaiddrugs%>%filter(Geo_Lvl=='National'& Plan_Type=='All')  

#see trend in number of rx's nationally by year
ggplot(data=nationalmedicaid, aes(x=Year, y=Tot_Opioid_Clms))+geom_smooth()+
  labs(title='National Opioid Claims by Year', y='Total Opioid Claims')

#see trend in prescribing rate nationally by year
ggplot(data=nationalmedicaid, aes(x=Year, y=Opioid_Prscrbng_Rate))+
  geom_col(fill='blue')+
  labs(title='Yearly National Prescribing Rate', y='Opioid Prescribing Rate')

#see how states are aggregated for claims and if theres any outliers
ggplot(data=statemedicaid, aes(x=Year, y=Tot_Opioid_Clms))+geom_point()

ggplot(data=statemedicaid, aes(x=Year, y=Opioid_Prscrbng_Rate))+geom_point()


#see top prescribing states- Colorado on at the top several years
topstate=statemedicaid%>%filter(Year=='2014')%>%arrange(desc(Opioid_Prscrbng_Rate))


#clean up county data to find overdose deaths
opioidcounty=opioidcounty%>%drop_na(Other,GDP.Education..Health..Social.Assistance)
#get total opioid deaths per state per year 
opioidcounty$GDP.Education..Health..Social.Assistance = as.numeric(opioidcounty$GDP.Education..Health..Social.Assistance)
#opioiddeaths=opioidcounty%>%group_by(State,Year)%>%
 # mutate(total.opioid.deaths=sum(Other),totalGDPspending=sum(GDP.Education..Health..Social.Assistance))
opioiddeaths=aggregate(cbind(Other, GDP.Education..Health..Social.Assistance,Population) ~ State + Year, data = opioidcounty, sum, na.rm = TRUE)
sapply(opioidcounty, class)


#gonna attempt to join these dataframes. wish me luck
masterchart=left_join(statemedicaid,opioiddeaths, by=c('State','Year'))

#woooohooo it worked
colnames(masterchart)
#5yrchange is mostly NA so gonna get rid of it
masterchart=select(masterchart,-c(Opioid_Prscrbng_Rate_5Y_Chg))
#change name of other column
colnames(masterchart)[8]<-"Opioid/Narcotic Deaths"

#top 10 states each year for opioid rx rates
top2013=masterchart%>%filter(Year==2013)%>%
  arrange(desc(Opioid_Prscrbng_Rate))%>% slice(1:10)

top2014=masterchart%>%filter(Year==2014)%>%
  arrange(desc(Opioid_Prscrbng_Rate))%>% slice(1:10)

top2015=masterchart%>%filter(Year==2015)%>%
  arrange(desc(Opioid_Prscrbng_Rate))%>% slice(1:10)

top2016=masterchart%>%filter(Year==2016)%>%
  arrange(desc(Opioid_Prscrbng_Rate))%>% slice(1:10)

top2017=masterchart%>%filter(Year==2017)%>%
  arrange(desc(Opioid_Prscrbng_Rate))%>% slice(1:10)

top2018=masterchart%>%filter(Year==2018)%>%
  arrange(desc(Opioid_Prscrbng_Rate))%>% slice(1:10)

top2019=masterchart%>%filter(Year==2019)%>%
  arrange(desc(Opioid_Prscrbng_Rate))%>% slice(1:10)

#make a big dataframe of top ten states 
#from each year to see repeat offenders
top10allyears=rbind(top2013,top2014,top2015,top2016,top2016,top2018,top2019)

ggplot(data=top10allyears, aes(State,fill = State))+ 
geom_dotplot()
