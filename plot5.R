library(ggplot2)
setwd('D:\\Data\\EDA lectures\\week2')

NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

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
png(filename="plot5.png")
plot(as.character(df$year),df$PM25,xlim=c(1999,2008),ylim=range(PM25),xaxt='n',pch="+",
     main="emissions from vehicle sources",
     xlab="year",
     ylab="PM2.5 emissions(in tons)")
axis(1, seq(1999,2008,by=3))
abline(lm(df$PM25 ~ df$year))
dev.off()
#the emission of PM25 from vehicle sources has decreased from 1999 to 2008