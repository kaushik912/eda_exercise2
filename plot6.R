library(ggplot2)
setwd('D:\\Data\\EDA lectures\\week2')

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

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
df$fips_2=factor(df$fips,labels=c("Los Angeles","Baltimore"))

png(file="plot6.png")
g<-ggplot(data=df,aes(factor(year),PM25,group=1))+geom_point()+facet_grid(.~fips_2)+geom_smooth(method="lm")
g<-g+labs(title="Emissions from vehicles split by City")+labs(x="year",y="PM 2.5 emission(in tons)")
print(g)
dev.off()

lval<-df[df$fips==6037 & df$year==1999,"PM25"]
uval<-df[df$fips==6037 & df$year==2008,"PM25"]
perc_change6037<-(uval-lval)*100/lval 

lval<-df[df$fips==24510 & df$year==1999,"PM25"]
uval<-df[df$fips==24510 & df$year==2008,"PM25"]
perc_change24510<-(uval-lval)*100/lval 

print(perc_change6037)
print(perc_change24510)

#Baltimore has seen a greater change, -74% decrease in PM25, from 1999 to 2008
# compared to ,California which has seen a slight increase of around 4% in PM25 from 1999 to 2008