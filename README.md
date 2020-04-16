# covid_19_analysis
#Analysis completed surrounding the 2019/2020 COVID-19 Pandemic
# By: Vijay Randhawa, MS

rm(list=ls())

library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
library(reshape)


### Read in global data from CSSEGISandData
Corona_Cases.source_url<-"https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
Corona_Cases.fn<-paste0(basename(Corona_Cases.source_url))
download.file(Corona_Cases.source_url,destfile = Corona_Cases.fn)
Corona_Totals.raw<-read.csv(Corona_Cases.fn,header = T,stringsAsFactors = F)

### Read in global deaths from CSSEGISandData
Corona_Deaths.source_url<-"https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
Corona_Deaths.fn<-paste0(basename(Corona_Deaths.source_url))
download.file(Corona_Deaths.source_url,destfile = Corona_Deaths.fn)
Corona_Deaths.raw<-read.csv(Corona_Deaths.fn,header = T,stringsAsFactors = F)

### Convert data from wide format to long format
data_long_global_cases <- gather(Corona_Totals.raw, Date, Cases, starts_with("X") , factor_key=TRUE)
data_long_global_deaths <- gather(Corona_Deaths.raw, Date, Deaths, starts_with("X"), factor_key=TRUE)

### Merge deaths and cases data together 
merged_global_data <- merge(data_long_global_cases, data_long_global_deaths, by=c("Province.State", "Country.Region", "Lat", "Long", "Date"))

### Cleanup date format in merged dataset


### Subset data by India, UK, US, and Quebec - Canada
tmp <- subset(merged_global_data, merged_global_data$Country.Region==c("India", "US"))
quebec <- subset(merged_global_data, merged_global_data$Country.Region=="Canada")
quebec <- subset(quebec, quebec$Province.State=="Quebec")
england <- subset(merged_global_data, merged_global_data$Country.Region=="United Kingdom")
england <- subset(england, england$Lat==55.3781)
family_data <- rbind(tmp, quebec, england)
family_data$Country.Region <- gsub("Canada", "Quebec, Canada", family_data$Country.Region)
family_data$Country.Region <- gsub("US", "United States", family_data$Country.Region)
family_data$Country.Region <- gsub("United Kingdom", "England", family_data$Country.Region)
family_data$Date <- gsub("X", "", family_data$Date)
family_data$Date <- gsub("\\.1\\.", ".01.", family_data$Date)
family_data$Date <- gsub("\\.2\\.", ".02." , family_data$Date)
family_data$Date <- gsub("\\.3\\.", ".03." , family_data$Date)
family_data$Date <- gsub("\\.4\\.", ".04." , family_data$Date)
family_data$Date <- gsub("\\.5\\.", ".05." , family_data$Date
family_data$Date <- gsub("\\.6\\.", ".06." , family_data$Date)
family_data$Date <- gsub("\\.7\\.", ".07." , family_data$Date)
family_data$Date <- gsub("\\.8\\.", ".08.", family_data$Date)
family_data$Date <- gsub("\\.9\\.", ".09.", family_data$Date)
family_data$Date <- as.Date(family_data$Date,format = "%m.%d.%y")


### Melt cases and deaths together to plot
family_data_melt <- melt(data=family_data, id.vars=c("Province.State", "Country.Region", "Lat", "Long", "Date"), measure.vars=c("Cases", "Deaths"))

### Generate Timestamp for plot title 
timestamp <- max(family_data_melt$Date)

### Graph cases vs. deaths in India Original

ggsave(filename = "SARS_CoV_2_Progression.png",
ggplot(data=family_data_melt, aes(x=Date,y=value, group=variable)) +
ggtitle(paste("SARS-CoV-2 Deaths/Cases Progression as of",timestamp)) +
theme_classic() +
geom_point(aes(color=variable), size =2) +
geom_line(aes(color=variable)) +
scale_color_manual(values = c("orangered3","springgreen4")) +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
scale_y_log10(labels=comma) +
xlab("") +
scale_x_date(labels = date_format("%m/%d"), breaks = date_breaks("7 days")) +
ylab("Number of People Impacted by SARS-CoV-2") +
theme(legend.title = element_blank()) +
theme(legend.text=element_text(size=12)) +
theme(text = element_text(size=13)) +
theme(strip.background =element_rect(fill="#0066FF")) +
theme(strip.text.x = element_text(size = 16, colour = "white")) +
facet_wrap(~Country.Region),
       width = 7, height = 7, dpi = 300, units = "in", device='png')



