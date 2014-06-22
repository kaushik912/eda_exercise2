library(ggplot2)
setwd('D:\\Data\\EDA lectures\\week2')

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

NEI_baltimore<-NEI[NEI$fips=="24510",]
#Question3

#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable)
#which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City, 
#Which have seen increases in emissions from 1999-2008? 
#Use the ggplot2 plotting system to make a plot answer this question.

PM25_summary<-tapply(NEI_baltimore$Emissions,list(factor(NEI_baltimore$year),factor(NEI_baltimore$type)),sum)
library(reshape2)
df<-melt(PM25_summary)
names(df)<-c("year","type","PM25")

png(filename="plot3.png",width=600)

g<-ggplot(data=df,aes(factor(year),PM25,group=1))+geom_point()+facet_grid(.~type)+geom_smooth(method="lm")
g<-g+labs(title="PM2.5 emissions by source type for baltimore city")+labs(x="year",y="PM2.5 emission(in tons)")
print(g)
dev.off()
#Non-Road and Non-point types have seen sharp decrease in PM25 emission from 1999 to 2008 for baltimore city
#On-Road has seen a slower decrease in PM25 emission for baltimore
#Point type has seen an increase in PM25 emission for baltimore.
