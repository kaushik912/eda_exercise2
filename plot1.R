library(ggplot2)
setwd('D:\\Data\\EDA lectures\\week2')

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Question 1
#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? Using the base plotting system, 
#make a plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.
PM25_summary<-tapply(NEI$Emissions,list(factor(NEI$year)),sum)

year<-as.integer(rownames(PM25_summary))
PM25<-as.integer(PM25_summary)
df<-data.frame(year,PM25)

png(filename="plot1.png")
#using base plot
plot(as.character(df$year),df$PM25,xlim=c(1999,2008),ylim=range(PM25),xaxt='n',pch="+",
     main="Total Emissions PM2.5 trending", xlab="Year",ylab="PM2.5 emitted(in tons)"
     )
axis(1, seq(1999,2008,by=3))
abline(lm(df$PM25 ~ df$year))
#it has decreased from 1999 to 2008.

dev.off()
#additional: using ggplot
ggplot(data=df,aes(factor(year),PM25,group=1))+geom_point()
