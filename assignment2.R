library(ggplot2)
setwd('D:/Data/datasets')

list.files()  

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Question 1
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, 
#make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
PM25_summary<-tapply(NEI$Emissions,list(factor(NEI$year)),sum)

year<-as.integer(rownames(PM25_summary))
PM25<-as.integer(PM25_summary)
df<-data.frame(year,PM25)

#using base plot
plot(as.character(df$year),df$PM25,xlim=c(1999,2008),ylim=range(PM25),xaxt='n',pch="+")
axis(1, seq(1999,2008,by=3))
abline(lm(df$PM25 ~ df$year))
#it has decreased from 1999 to 2008.

#additional: using ggplot
ggplot(data=df,aes(factor(year),PM25,group=1))+geom_point()


#Question2
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.
NEI_baltimore<-NEI[NEI$fips=="24510",]
PM25_summary<-tapply(NEI_baltimore$Emissions,list(factor(NEI_baltimore$year)),sum)
year<-as.integer(rownames(PM25_summary))
PM25<-as.integer(PM25_summary)
df<-data.frame(year,PM25)

#using base plot
plot(as.character(df$year),df$PM25,xlim=c(1999,2008),ylim=range(PM25),xaxt='n',pch="+")
axis(1, seq(1999,2008,by=3))
abline(lm(df$PM25 ~ df$year))
#PM25 has decreased from 1999 to 2008 in baltimore city.

#additional: using ggplot
ggplot(data=df,aes(factor(year),PM25,group=1))+geom_point()

#Question3

#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable)
#which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City, 
#Which have seen increases in emissions from 1999-2008? 
#Use the ggplot2 plotting system to make a plot answer this question.

PM25_summary<-tapply(NEI_baltimore$Emissions,list(factor(NEI_baltimore$year),factor(NEI_baltimore$type)),sum)
library(reshape2)
df<-melt(PM25_summary)
names(df)<-c("year","type","PM25")
ggplot(data=df,aes(factor(year),PM25,group=1))+geom_point()+facet_grid(.~type)+geom_smooth(method="lm")

#Non-Road and Non-point types have seen sharp decrease in PM25 emission from 1999 to 2008 for baltimore city
#On-Road has seen a slower decrease in PM25 emission for baltimore
#Point type has seen an increase in PM25 emission for baltimore.

#Question4
#Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?

good<-grep(".*coal.*",SCC$EI.Sector,ignore.case=T)
SCCs_of_interest<-as.character(unique(SCC[good,]$SCC))

NEI_coal_emissions<-NEI[NEI$SCC %in% SCCs_of_interest,]

PM25_summary<-tapply(NEI_coal_emissions$Emissions,list(factor(NEI_coal_emissions$year)),sum)

year<-as.integer(rownames(PM25_summary))
PM25<-as.integer(PM25_summary)
df<-data.frame(year,PM25)

#using base plot
plot(as.character(df$year),df$PM25,xlim=c(1999,2008),ylim=range(PM25),xaxt='n',pch="+")
axis(1, seq(1999,2008,by=3))
abline(lm(df$PM25 ~ df$year))

#the emission of PM25 from coal source has decreased from 1999 to 2008

#additional: using ggplot
ggplot(data=df,aes(factor(year),PM25,group=1))+geom_point()+geom_smooth(method="lm")

#Question5
#How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
good<-grep(".*vehicle.*",SCC$EI.Sector,ignore.case=T)
SCCs_of_interest<-as.character(unique(SCC[good,]$SCC))

NEI_vehicle_emissions<-NEI[NEI$SCC %in% SCCs_of_interest,]

PM25_summary<-tapply(NEI_vehicle_emissions$Emissions,list(factor(NEI_vehicle_emissions$year)),sum)

year<-as.integer(rownames(PM25_summary))
PM25<-as.integer(PM25_summary)
df<-data.frame(year,PM25)

#using base plot
plot(as.character(df$year),df$PM25,xlim=c(1999,2008),ylim=range(PM25),xaxt='n',pch="+")
axis(1, seq(1999,2008,by=3))
abline(lm(df$PM25 ~ df$year))

#the emission of PM25 from vehicle sources has decreased from 1999 to 2008

#additional: using ggplot
ggplot(data=df,aes(factor(year),PM25,group=1))+geom_point()+geom_smooth(method="lm")

#Question 6
#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor vehicle sources in 
#Los Angeles County, California (fips == "06037").
#Which city has seen greater changes over time in motor vehicle emissions?

good<-grep(".*vehicle.*",SCC$EI.Sector,ignore.case=T)
SCCs_of_interest<-as.character(unique(SCC[good,]$SCC))

NEI_vehicle_emissions<-NEI[NEI$SCC %in% SCCs_of_interest,]
NEI_vehicle_emissions_citywise<-NEI_vehicle_emissions[NEI_vehicle_emissions$fips == "06037"|NEI_vehicle_emissions$fips =="24510",]
PM25_summary<-tapply(NEI_vehicle_emissions_citywise$Emissions,list(factor(NEI_vehicle_emissions_citywise$year),factor(NEI_vehicle_emissions_citywise$fips)),sum)

library(reshape2)
df<-melt(PM25_summary)
names(df)<-c("year","fips","PM25")
ggplot(data=df,aes(factor(year),PM25,group=1))+geom_point()+facet_grid(.~fips)+geom_smooth(method="lm")

fips_color<-rep(0,length(df$fips))
df$fips==6037

qplot(year,PM25,data=df,geom="density",color=fips)

lval<-df[df$fips==6037 & df$year==1999,"PM25"]
uval<-df[df$fips==6037 & df$year==2008,"PM25"]
perc_change6037<-(uval-lval)*100/lval 

lval<-df[df$fips==24510 & df$year==1999,"PM25"]
uval<-df[df$fips==24510 & df$year==2008,"PM25"]
perc_change24510<-(uval-lval)*100/lval 

#fips=24510 (california) has seen a greater change, -74% decrease in PM25, from 1999 to 2008
# compared to fips=6037 (los angeles), which has seen a slight increase of around 4% in PM25 from 1999 to 2008

#example on factors
data = c(1,2,2,3,1,2,3,3,1,2,3,3,1)
fdata = factor(data)
rdata = factor(data,labels=c("I","II","III"))
df2<-data.frame(data,1:length(data))
names(df2)<-c("group","values")
df2$group<-factor(df2$group,labels=c("I","II","III"))


