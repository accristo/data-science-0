#Import of Gold and Bitcoin historical data. Correlation Analysis
library(dplyr)
library(dslabs)
library(tidyverse)
library(htmlwidgets)
library(rvest)

url<-"https://goldprice.org/es"
dat_gold<-read_csv(url)
str(dat_gold)
dat_gold

url<-"https://www.indexmundi.com/commodities/?commodity=gold&months=120"
dat_gold<-read_html(url)
class(dat_gold)

tab_gold<-dat_gold %>% html_nodes("table")
tab_gold
head(tab_gold)
tab_gold<-tab_gold[[5]]
tab_gold<-tab_gold %>% html_table
tab_gold
class(tab_gold)

tab_gold<- tab_gold %>% setNames(c("Mont_Year", "Gold_Price_per_Ounce", "Change"))
tab_gold<-tab_gold %>%mutate_at(2,parse_number)
tab_gold

#Bitcoin historic price per month. Data scraping an wrangling

url_bitcoin <-"https://cointelegraph.com/bitcoin-price-index"
tab_bitcoin <- read_html(url_bitcoin) %>% html_nodes("table")
tab_bitcoin <- tab_bitcoin[[19]] %>% html_table(fill = TRUE)
head(tab_bitcoin)

installed.packages()

