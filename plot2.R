library(ggplot2)
setwd('D:\\Data\\EDA lectures\\week2')

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Question2
#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510") from 1999 to 2008? 
#Use the base plotting system to make a plot answering this question.
NEI_baltimore<-NEI[NEI$fips=="24510",]
PM25_summary<-tapply(NEI_baltimore$Emissions,list(factor(NEI_baltimore$year)),sum)
year<-as.integer(rownames(PM25_summary))
PM25<-as.integer(PM25_summary)
df<-data.frame(year,PM25)

png(filename="plot2.png")
#using base plot
plot(as.character(df$year),df$PM25,xlim=c(1999,2008),ylim=range(PM25),xaxt='n',pch="+",
     xlab="year",ylab="PM2.5 emissions(in tons)",
     main="PM2.5 emissions for baltimore")
axis(1, seq(1999,2008,by=3))
abline(lm(df$PM25 ~ df$year))
#PM25 has decreased from 1999 to 2008 in baltimore city.
dev.off()

#additional: using ggplot
ggplot(data=df,aes(factor(year),PM25,group=1))+geom_point()
