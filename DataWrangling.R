# original wide data
library(tidyverse)
path<-system.file("extdata", package="dslabs")
filename<-file.path(path,"fertility-two-countries-example.csv")
wide_data<-read_csv(filename)
head(wide_data)
str(wide_data)

# tidy data from dslabs
library(dslabs)
data("gapminder")
tidy_data<-gapminder%>%filter(country %in%c("Germany", "South Korea"))%>%
  select(country,year,fertility)
head(tidy_data)

# gather wide data to make new tidy data
new_tidy_data<-wide_data %>% gather(year,fertility,`1960`:`2015`)
head(new_tidy_data)

# gather all columns except country
new_tidy_data<-wide_data%>%gather(year,fertility,-country)
head(new_tidy_data)

# gather treats column names as characters by default
class(tidy_data$year)
class(new_tidy_data$year)

# convert gathered column names to numeric
new_tidy_data<-wide_data%>%gather(year,fertility,-country,convert=TRUE)
head(new_tidy_data)

# ggplot works on new tidy data

new_tidy_data%>%
  ggplot(aes(year,fertility, color = country)) +
  geom_point()

# spread tidy data to generate wide data
new_wide_data<-new_tidy_data%>%spread(year,fertility)
head(new_wide_data)
select(new_wide_data,country,`1960`:`2015`)


# import data
path<-system.file("extdata", package="dslabs")
filename<-file.path(path,"life-expectancy-and-fertility-two-countries-example.csv")
library(dslabs)
library(tidyverse)
raw_dat<-read_csv(filename)
head(raw_dat)
select(raw_dat,1:5)

# gather all columns except country
dat<-raw_dat%>%gather(key,value,-country)
head(dat)
dat$key[1:5]

# separate on underscores
dat%>%separate(key,c("year","variable_name"),"_")
dat%>%separate(key,c("year","variable_name"))

# split on all underscores, pad empty cells with NA
dat%>%separate(key,c("year","first_variable_name", "second_variable_name", fill="right"))

# split on first underscore but keep life_expectancy merged
dat%>%separate(key,c("year", "variable_name"),sep="_",extra="merge")

# separate then spread
dat%>%separate(key,c("year","variable_name"),sep="_",extra="merge") %>%
  spread(variable_name,value)

# separate then unite
dat%>%separate(key,c("year","first_variable_name","second_variable_name"),fill="right")%>%
unite(variable_name,first_variable_name,second_variable_name,sep="_")

# full code for tidying data
dat%>%
  separate(key, c("year", "first_variable_name", "second_variable_name"), fill = "right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility = fertility_NA)



