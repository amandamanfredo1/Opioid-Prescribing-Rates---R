medicaiddrugs=read.csv('omt-mdcd-r21-p11-v10-ytd19-geo-zip-1.csv')
opioidcounty=read.csv('~/Downloads/archive/Wide_Master.csv')
cdcdeaths=read.csv('VSRR_Provisional_Drug_Overdose_Death_Counts.csv')
summary(medicaiddrugs)
library(tidyverse)
library(ggplot2)
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
opioiddeaths=aggregate(cbind(GDP.Education..Health..Social.Assistance,GDP.Total,Population, Less_Than_HS) ~ State + Year, data = opioidcounty, sum, na.rm = TRUE)
sapply(opioidcounty, class)

##cleaning up cdc data--------------------------------
cdcdeaths=cdcdeaths%>%filter(Indicator=='Number of Drug Overdose Deaths')
unique(cdcdeaths$State.Name)
colnames(cdcdeaths)[6]="Overdose.Deaths"
cdcdeaths=select(cdcdeaths, -c(Percent.Complete,Percent.Pending.Investigation,
                               Footnote, Footnote.Symbol, Predicted.Value, Period))
cdcdeaths=cdcdeaths%>%filter(Month=='December')


#ok this is the number of overdose deaths per year in each state. 
#can now get ride of month and state abbrev. still need to remove nyc
cdcdeaths=select(cdcdeaths, -c(State, Month, Indicator))
colnames(cdcdeaths)[3]='state'
`%notin%` <- Negate(`%in%`)
cdcdeaths=filter(cdcdeaths,state %notin% c('New York City'))
cdcdeaths=filter(cdcdeaths,state %notin% c('District of Columbia'))
cdcdeaths=filter(cdcdeaths,state %notin% c('United States'))
cdcdeaths$Overdose.Deaths=as.numeric(gsub(",", "", cdcdeaths$Overdose.Deaths))


##using another dataframe for state populations-------------------
statepop=read.table(file='state-population-estimations.tsv', sep='\t', header=TRUE)
statepop=filter(statepop,State %notin% c('United States', 'District of Columbia', 'Puerto Rico'))
statepop=select(statepop, -c(X2010, X2011, X2012, X2013))

pop2015=select(statepop, c("State", "X2015"))
pop2015$Year='2015'
colnames(pop2015) = c('State', 'Population', 'Year')

pop2016=select(statepop, c("State", "X2016"))
pop2016$Year='2016'
colnames(pop2016) = c('State', 'Population', 'Year')

pop2017=select(statepop, c("State", "X2017"))
pop2017$Year='2017'
colnames(pop2017) = c('State', 'Population', 'Year')

pop2018=select(statepop, c("State", "X2018"))
pop2018$Year='2018'
colnames(pop2018) = c('State', 'Population', 'Year')

pop2019=select(statepop, c("State", "X2019"))
pop2019$Year='2019'
colnames(pop2019) = c('State', 'Population', 'Year')

populations = rbind(pop2015, pop2016, pop2017, pop2018, pop2019)
populations$State=
  colnames(populations)[1]='state'


populations$Population=as.numeric(gsub(",", "", populations$Population))
populations$Year=as.numeric(populations$Year)

cdcdeaths=left_join(cdcdeaths,populations,  by=c('state','Year'))
cdcdeaths=cdcdeaths%>%select(-State)
cdcdeaths=cdcdeaths%>%mutate(Percent_deaths=(Overdose.Deaths/Population)*100)

#---------------------------------------------------

#gonna attempt to join medicaid and opioiddeaths dataframes. wish me luck
masterchart=left_join(statemedicaid,opioiddeaths, by=c('State','Year'))


#woooohooo it worked
colnames(masterchart)
#5yrchange is mostly NA so gonna get rid of it
masterchart=select(masterchart,-c(Opioid_Prscrbng_Rate_5Y_Chg,Other))


#create new columns gdp/gdptotal
masterchart=masterchart%>%mutate(Percent_GDP=(GDP.Education..Health..Social.Assistance/GDP.Total)*100)
masterchart$Percent_GDP=round(masterchart$Percent_GDP,3)
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


#rainbow chart- need to figure out y axis, and names at the bottom

ggplot(data=top10allyears, aes(State,fill = State))+ 
  geom_dotplot(binwidth = 1, method = "histodot")+ theme_classic()+
  theme(axis.text.x = element_blank(),axis.text.y=element_blank(),
        axis.ticks.y=element_blank(), axis.ticks = element_blank())+
  labs(title='Frequency in Top 10 Precribing Rates')


#colorado, nevada, wisconsin, washington state are on the top ten lists the most
top4states= masterchart%>%
  filter(State=='Colorado' | State=='Nevada' | State=='Wisconsin'|State=='Washington')

statesin10= masterchart%>%filter(State %in% top10allyears$State)

#line plot of prescribing rate over the years-all have downward trend-top 4 state
ggplot(data=top4states, aes(x=Year, y=Opioid_Prscrbng_Rate, color=State))+geom_line()


statesin10filt=statesin10%>%filter(Year<=2017)
statesin10$Year=as.character(statesin10$Year)


#me trying to do a map-----------------------------
colnames(masterchart)[3] <- "state"
library(usmap)
library(ggplot2)
master2013=masterchart%>%filter(Year==2013)
plot_usmap(data = master2013, values = "Opioid_Prscrbng_Rate", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Opioid Prescribing Rate", label = scales::comma, limits=c(1,12)
  ) + theme(legend.position = "right")+
  labs(title = "Opioid Prescribing Rate by State", subtitle = "Year: 2013")

##omg it workedddddddddddddd ahhhh

#year2015
master2015=masterchart%>%filter(Year==2015)
plot_usmap(data = master2015, values = "Opioid_Prscrbng_Rate", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Opioid Prescribing Rate", label = scales::comma,limits=c(1,12)
  ) + theme(legend.position = "right")+
  labs(title = "Opioid Prescribing Rate by State", subtitle = "Year: 2015")


#year2017
master2017=masterchart%>%filter(Year==2017)
plot_usmap(data = master2017, values = "Opioid_Prscrbng_Rate", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Opioid Prescribing Rate", label = scales::comma, limits=c(1,12)
  ) + theme(legend.position = "right")+
  labs(title = "Opioid Prescribing Rate by State", subtitle = "Year: 2017")

#year2019

master2019=masterchart%>%filter(Year==2019)
plot_usmap(data = master2019, values = "Opioid_Prscrbng_Rate", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Opioid Prescribing Rate", label = scales::comma, limits=c(1,12)
  ) + theme(legend.position = "right")+
  labs(title = "Opioid Prescribing Rate by State", subtitle = "Year: 2019")


#ok so thats good for now-----------------------------------
#lets see if i can compare to death rates from opioids


#year 2015
cdc2015=cdcdeaths%>%filter(Year==2015)
plot_usmap(data = cdc2015, values = "Percent_deaths", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "Percent Opioid Deaths", label = scales::comma, limits=c(0,.05)
  ) + theme(legend.position = "right")+
  labs(title = "Percent Opioid Deaths by State", subtitle = "Year: 2015")


#year2017
cdc2017=cdcdeaths%>%filter(Year==2017)
plot_usmap(data = cdc2017, values = "Percent_deaths", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "Percent Opioid Deaths", label = scales::comma, limits=c(0,.055)
  ) + theme(legend.position = "right")+
  labs(title = "Percent Opioid Deaths by State", subtitle = "Year: 2017")

#year2019
cdc2019=cdcdeaths%>%filter(Year==2019)
plot_usmap(data = cdc2019, values = "Percent_deaths", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "blue", name = "Percent Opioid Deaths", label = scales::comma, limits=c(0,.055)
  ) + theme(legend.position = "right")+
  labs(title = "Percent Opioid Deaths by State", subtitle = "Year: 2019")


#animating the maps animatingggggggggggggg-------------------------
#install.packages("gganimate")
#install.packages("gifski")
#install.packages("transformr")
install.packages("gapminder")
library(gapminder)
library(transformr)
library(gganimate)
masterchart
maprxrate=plot_usmap(data = masterchart, values = "Opioid_Prscrbng_Rate", color = "black") + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Opioid Prescribing Rate", label = scales::comma, limits=c(1,12)
  ) + theme(legend.position = "right")

map_with_animation <- maprxrate +
  transition_time(Year) +
  ggtitle('Year: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}')
num_years <- max(masterchart$Year) - min(masterchart$Year) + 1
animate(map_with_animation, nframes = num_years)

anim_save("maprxrate.gif")


#back to fancy line graphs---------------------------------
library(CGPfunctions)
#just shows gdp on social spending
statesin10$Year=as.character(statesin10$Year)
statesin10filt=statesin10%>%filter(Year<=2017)

#for all the states in top 10 list-------------------------------

newggslopegraph(statesin10filt, Year, GDP.Education..Health..Social.Assistance, State,
                Title = "GDP by State",
                SubTitle = "2013-2017",
                Caption = "GDP Spent on healthcare, education, and social assistance",
                DataLabelPadding = 0.2,
                DataLabelLineSize = 0.5,
                DataLabelFillColor = "lightblue",
                ThemeChoice = "gdocs")

newggslopegraph(statesin10filt, Year, Percent_GDP, State,
                Title = "Percent GDP by State",
                SubTitle = "2013-2017",
                Caption = "GDP Spent on healthcare, education, and social assistance",
                DataLabelPadding = 0.2,
                DataLabelLineSize = 0.5,
                DataLabelFillColor = "lightblue",
                ThemeChoice = "gdocs")

#states im focusing on--------------------------------------

favstate=masterchart%>%
  filter(state=='Colorado' | state=='New York' | state=='Nevada' | state=='Wisconsin'|
           state=='Virginia'|state=='Montana' | state=='West Virginia')
favstate$Year=as.character(favstate$Year)
favstate1=favstate%>%filter(Year<=2017)

newggslopegraph(favstate1, Year, Overdose.Deaths, state,
                Title = " Deaths by Opioids by State",
                SubTitle = "Year 2013-2017",
                Caption = "Percent of Total Deaths Attributed to Opioids or Narcotics",
                DataLabelPadding = 0.2,
                DataLabelLineSize = 0.5,
                DataLabelFillColor = "lightblue",
                ThemeChoice = "gdocs")

newggslopegraph(favstate1, Year, Percent_GDP, state,
                Title = "Percent GDP by State",
                SubTitle = "Year 2013-2017",
                Caption = "GDP Spent on healthcare, education, and social assistance",
                DataLabelPadding = 0.2,
                DataLabelLineSize = 0.5,
                DataTextSize = 2.5,
                DataLabelFillColor = "lightblue",
                ThemeChoice = "gdocs")

##trying to find the avg rx rate each year
avgrxrate=masterchart%>%group_by(Year)%>%summarise(Opioid_Prscrbng_Rate= mean(Opioid_Prscrbng_Rate))
avgrxrate$Opioid_Prscrbng_Rate=round(avgrxrate$Opioid_Prscrbng_Rate,2)
avgrxrate$State='National Avg'
favstate2=favstate[,c("Year", "state", "Opioid_Prscrbng_Rate")]


newrx=rbind(avgrxrate,favstate2)

newggslopegraph(newrx, Year, Opioid_Prscrbng_Rate, State,
                Title = "Opioid Prescribing Rate by State",
                SubTitle = "Year 2013-2019",
                Caption = "Percent of Medicaid Claims for Opioids",
                DataLabelPadding = 0.2,
                DataLabelLineSize = 0.5,
                DataTextSize = 1.5,
                DataLabelFillColor = "lightblue",
                ThemeChoice = "gdocs")
