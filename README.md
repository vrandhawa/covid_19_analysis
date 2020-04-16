# covid_19_analysis
#Analysis completed surrounding the 2019/2020 COVID-19 Pandemic
# By: Vijay Randhawa, MS

rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(reshape)


###Read in global data from CSSEGISandData
Corona_Cases.source_url<-"https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
Corona_Cases.fn<-paste0(basename(Corona_Cases.source_url))
download.file(Corona_Cases.source_url,destfile = Corona_Cases.fn)
Corona_Totals.raw<-read.csv(Corona_Cases.fn,header = T,stringsAsFactors = F)

###Read in global deaths from CSSEGISandData
Corona_Deaths.source_url<-"https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
Corona_Deaths.fn<-paste0(basename(Corona_Deaths.source_url))
download.file(Corona_Deaths.source_url,destfile = Corona_Deaths.fn)
Corona_Deaths.raw<-read.csv(Corona_Deaths.fn,header = T,stringsAsFactors = F)

###Convert data from wide format to long format
data_long_global_cases <- gather(Corona_Totals.raw, Date, Cases, starts_with("X") , factor_key=TRUE)
data_long_global_deaths <- gather(Corona_Deaths.raw, Date, Deaths, starts_with("X"), factor_key=TRUE)

###Merge deaths and cases data together 
merged_global_data <- merge(data_long_global_cases, data_long_global_deaths, by=c("Province.State", "Country.Region", "Lat", "Long", "Date"))

###Cleanup date format in merged dataset


###Subset data by India
india_data <- subset(merged_global_data, merged_global_data$Country.Region=="India")
india_data$Date <- gsub("X", "", india_data$Date)
india_data$Date <- str_sort(india_data$Date, numeric=TRUE)

###Melt cases and deaths together to plot
india_data_melt <- melt(data=india_data, id.vars=c("Province.State", "Country.Region", "Lat", "Long", "Date"), measure.vars=c("Cases", "Deaths"))

###Graph cases vs. deaths in India Original
ggplot(data=india_data_melt, aes(x=Date,y=value)) +
theme_classic() +
geom_point(aes(color=variable))


