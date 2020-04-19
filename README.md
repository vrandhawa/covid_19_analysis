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


### Read in Maryland and CC Cases
Corona_Cases_US.source_url<-"https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_US.csv"
Corona_Cases_US.fn<-paste0(basename(Corona_Cases_US.source_url))
download.file(Corona_Cases_US.source_url,destfile = Corona_Cases_US.fn)
Corona_Totals_US.raw<-read.csv(Corona_Cases_US.fn,header = T,stringsAsFactors = F)


### Read in Maryland and CC Deaths
Corona_Deaths_US.source_url<-"https://github.com/CSSEGISandData/COVID-19/raw/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_US.csv"
Corona_Deaths_US.fn<-paste0(basename(Corona_Deaths_US.source_url))
download.file(Corona_Deaths_US.source_url,destfile = Corona_Deaths_US.fn)
Corona_Deaths_US.raw<-read.csv(Corona_Deaths_US.fn,header = T,stringsAsFactors = F)


### Convert data from wide format to long format
data_long_global_cases <- gather(Corona_Totals.raw, Date, Cases, starts_with("X") , factor_key=TRUE)
data_long_global_deaths <- gather(Corona_Deaths.raw, Date, Deaths, starts_with("X"), factor_key=TRUE)
data_long_US_cases <- gather(Corona_Totals_US.raw, Date, Cases, starts_with("X") , factor_key=TRUE)
data_long_US_deaths <- gather(Corona_Deaths_US.raw, Date, Deaths, starts_with("X"), factor_key=TRUE)

### Merge deaths and cases data together 
merged_global_data <- merge(data_long_global_cases, data_long_global_deaths, by=c("Province.State", "Country.Region", "Lat", "Long", "Date"))


###Pull out necessary columnns from US deaths and cases dataframes
tmp_UScases <- select(data_long_US_cases, "Admin2", "Province_State", "Date", "Cases")
tmp_USdeaths <- select(data_long_US_deaths, "Admin2", "Province_State", "Date", "Deaths")
merged_US_data <- merge(tmp_UScases, tmp_USdeaths, by=c("Admin2", "Province_State", "Date"))

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
US_data_melt <- melt(data=merged_US_data, id.vars=c("Admin2", "Province_State", "Date"), measure.vars=c("Cases", "Deaths"))

US_data_melt$Date <- gsub("X", "", US_data_melt$Date)
US_data_melt$Date <- as.Date(US_data_melt$Date,format = "%m.%d.%y")

MD <- subset(US_data_melt, US_data_melt$Province_State=="Maryland")
MD$Location <- c("Maryland")
CC <- subset(MD, MD$Admin2=="Carroll")
CC$Location <- c("Carroll County")
combined_CC_MD <- rbind(CC,MD)

tmp <- combined_CC_MD %>%
  select(Location, everything())

rm(combined_CC_MD)
combined_CC_MD <- tmp
rm(tmp)



### Generate a diff from the day prior to track progression
dater <- family_data_melt %>%
    group_by(Country.Region, Lat, Long, variable) %>%
    arrange(Date) %>%
    mutate(diff = value - lag(value, default = first(value)))
    
md_dater <- combined_CC_MD %>%
    group_by(Location,Admin2, Province_State, variable) %>%
    arrange(Date) %>%
    mutate(diff = value - lag(value, default = first(value)))

### Generate Timestamp for plot title 
timestamp <- max(dater$Date)
named_file <- paste(timestamp,"_SARS_CoV_2_Progression.pdf", sep="")

timestamp_md <- max(md_dater$Date)
named_file_md <- paste(timestamp_md,"_Maryland_SARS_CoV_2_Progression.pdf", sep="")

### Graph cases vs. deaths in India, US, Quebec, and England

ggsave(path="world_results/",filename = named_file, 
ggplot(data=dater, aes(x=Date,y=diff, group=variable)) +
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
       width = 7, height = 7, dpi = 300, units = "in", device='pdf')
       


### Graph cases vs. deaths in Maryland and Carroll County, MD.       
ggsave(path="md_results/", filename = named_file_md, 
ggplot(data=md_dater, aes(x=Date,y=diff, group=variable)) +
ggtitle(paste("Maryland SARS-CoV-2 Deaths/Cases Progression as of",timestamp_md)) +
theme_classic() +
geom_point(aes(color=variable), size =2) +
geom_line(aes(color=variable)) +
scale_color_manual(values = c("orangered3","springgreen4")) +
theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
#scale_y_log10(labels=comma) +
xlab("") +
scale_x_date(labels = date_format("%m/%d"), breaks = date_breaks("7 days")) +
ylab("Number of People Impacted by SARS-CoV-2") +
theme(legend.title = element_blank()) +
theme(legend.text=element_text(size=12)) +
theme(text = element_text(size=11)) +
theme(strip.background =element_rect(fill="#0066FF")) +
theme(strip.text.x = element_text(size = 16, colour = "white")) +
facet_wrap(~Location),
       width = 7, height = 7, dpi = 300, units = "in", device='pdf')





