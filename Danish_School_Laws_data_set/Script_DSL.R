# Removing objects
rm(list=ls())

# Installing packages
install.packages("rvest")
install.packages("tidyverse")
install.packages("lubridate")
install.packages("tidytext")

# Loading packages from library
library(rvest)
library(tidyverse)
library(lubridate)
library(tidytext)

# Test of data-import and xpath

#Reading file no. 7
file_seven <- read_html("clean_data/0007.html") 

#Saving the xpath capturing the date
xp_dato ='//*[@id="c62764"]/div/div[2]/div[1]/div[2]'

# Extracting the date
file_seven %>% 
  html_nodes(xpath = xp_dato) %>% 
  html_text

#Saving the xpath capturing the title
xp_titel ='//*[@id="c62764"]/div/div[2]/div[3]/div[2]'

# Extracting the title
file_seven %>% 
  html_nodes(xpath = xp_titel) %>% 
  html_text

# Import of data
filenames <- list.files("./clean_data/", full.names = TRUE)

# Extracting the date and title from all html-files 
title <- c()
date <- c()
for(chr in filenames){
  print(paste(chr))
  chr %>%   
    read_html() %>% 
    html_nodes(xpath = '//*[@id="c62764"]/div/div[2]/div[3]/div[2]') %>%
    html_text() -> titel
  title <- append(title, titel)
  chr %>% 
    read_html() %>% 
    html_nodes(xpath = '//*[@id="c62764"]/div/div[2]/div[1]/div[2]') %>%
    html_text() -> dato
  date <- append(date, dato)
}

# Removing file no. 1690 (row 1611) as it is identical to file no. 1828
date1 = date[-1611]
title1 = title[-1611]

# Changing the date of file no. 0040 (row 31)
date1[31] = "29-03-1867"

# Changing the dates noted as year-month-date to a day-month-year format
date_dmy <- str_replace(date1, pattern = "^(\\d{4})-(\\d{2})-(\\d{2})","\\3-\\2-\\1" )

# Adding "01-01" to dates containg only the year of the law
date_dmy_alle <- str_replace(date_dmy, pattern = "^(\\d{4})", "01-01-\\1" )

# Changing the format of the dates from day-month-year to year-month-day
date_ymd <- str_replace(date_dmy_alle, pattern = "^(\\d{2})\\W(\\d{2})\\W(\\d{4})", "\\3-\\2-\\1" ) 

# Coercing the dates from characters into dates
date_ymd_dato <- ymd(date_ymd)

# Creating the data-set
danish_school_laws <- tibble("Dato (Date)" = date_ymd_dato, "Dokumenttitel (Title of the document)" = title1)
danish_school_laws_sorted <- danish_school_laws[do.call(order,danish_school_laws),]

# Checking to see if there are any missing values
anyNA(danish_school_laws_sorted)

# Printing the danish_school_laws_sorted data-frame to a csv-file
write.csv(danish_school_laws_sorted,"output_data/Danish_school_laws.csv",row.names = FALSE)

## Creating a plot view the number of documents per year 
# Deleting day and month from the dates of the documents
year <- str_replace(date_ymd_dato, pattern = "(\\d{4})\\W(\\d{2})\\W(\\d{2})", "\\1" )

# Creating a value consisting of the year-values as numeric
year_data = data.frame(as.numeric(year))

# For loop counting how many times each year appears in the data  
year_seq = seq(1500,2019, by = 1)
year_count = rep(NA,520)
for (i in year_seq){
  year_count[i-1499] = sum(year_data == i)
}

# Drawing the plot
plot(year_seq, year_count, type = "l", xlab = "Year", ylab = "Quantity",family = "serif")
abline(h=0, col="gray")

# Exporting the plot
jpeg(filename = "output_data/Visual_overview", width = 500, height = 432, quality = 75)
 plot(year_seq, year_count, type = "l", xlab = "Year", ylab = "Quantity",family = "serif")
 abline(h=0, col="gray")
 dev.off()
 