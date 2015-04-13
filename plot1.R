require(dplyr)

NEI <- readRDS("summarySCC_PM25.rds")

emissions<-NEI %>% group_by(year) %>% summarise(sum(Emissions))
colnames(emissions)[2]<-"total_tons_PM2.5"

png(filename = "plot1.png", width=480, height=480)
plot(
  emissions$year,emissions$total_tons_PM2.5,
  ylab="Total Tons PM2.5",
  xlab="Year Data Collected",
  xaxt="n"
)
axis(1, at = seq(min(emissions$year), max(emissions$year), by = 1) )
abline(lm(emissions$total_tons_PM2.5 ~ emissions$year), col="red")
legend(
  "topright",
  legend ="Regression line, Total Tons PM2.5 / Year",
  col="red",
  lty=1,
  bty = "n"
)
title(
  main="Total emissions from PM2.5 have decreased in the United States from 1999 to 2008",
  cex.main = 0.8,
  font.main= 4,
  col.main= "blue",
  sub = "Plot showing the total PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.",
  cex.sub = 0.75,
  font.sub = 3,
  col.sub = "blue",
)

r_squared<-round(summary(lm(emissions$total_tons_PM2.5 ~ emissions$year))$r.squared, digits =4)
text(2004,6000000,paste("R-squared=", r_squared))

dev.off()
