##The first part is to get the data and unzip it

url <- "https://d396qusza40orc.cloudfront.net/exdata%2Fdata%2FNEI_data.zip"
download.file(url,destfile="./Data.zip",method="curl")
unzip(zipfile="./Data.zip")

#read in Data
NEI <- readRDS("summarySCC_PM25.rds")
SCC <- readRDS("Source_Classification_Code.rds")

#Question 1.  Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total 
#PM2.5 emission from all sources for each of the years 1999, 2002, 2005, and 2008.

library(lattice)
library(ggplot2)
library(dplyr)
NEIdf <- tbl_df(NEI)
#pipe data to filter in baltimore and determine emissions by type
NEIbalttype <- NEIdf %>% filter(fips == "24510") %>% select(Emissions, type, year) %>% 
        group_by(type, year) %>% summarise(EmissionsSum=sum(Emissions))

#plot data
ggplot(NEIbalttype, aes(factor(year),EmissionsSum, fill=type)) + 
        geom_point() +
        theme_bw() + guides(fill=FALSE) +
        facet_grid(.~type,scales = "free",space="free") + 
        labs(x="Year", y=expression("Total Emissions")) + 
        labs(title=expression("Total Emissions for Baltimore by Source Type"))

