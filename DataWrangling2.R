# see working directory
getwd()

# change your working directory
setwd()

# set path to the location for raw data files in the dslabs package and list files
system.file("extdata", package="dslabs")
path<-system.file("extdata", package="dslabs")
list.files(path)

# generate a full path to a file
filename <- "murders.csv"
fullpath <- file.path(path, filename)
fullpath

# copy file from dslabs package to your working directory
file.copy(fullpath, getwd())

# check if the file exists
file.exists(filename)


library(dslabs)
library(tidyverse)    # includes readr
library(readxl)

# inspect the first 3 lines
read_lines("murders.csv", n_max = 3)

# read file in CSV format
dat <- read_csv(filename)
dat

#read using full path
fullpath
dat <- read_csv(fullpath)
head(dat)

#Ex:
path <- system.file("extdata", package = "dslabs")
files <- list.files(path)
files

filename <- "murders.csv"
filename1 <- "life-expectancy-and-fertility-two-countries-example.csv"
filename2 <- "fertility-two-countries-example.csv"
dat=read.csv(file.path(path,filename))
dat1=read.csv(file.path(path,filename1))
dat2=read.csv(file.path(path,filename2))
head(dat)
str(dat)
head(dat1)
str(dat1)
head(dat2)
str(dat2)


# filename is defined in the previous video
# read.csv converts strings to factors
dat2<-read.csv(filename)
str(dat2)
class(dat2$abb)
class(dat2$state)

dat3<-read.csv(filename, stringsAsFactors=FALSE)
class(dat3$abb)
class(dat3$state)
class(dat3$region)

url<-"https://raw.githubusercontent.com/rafalab/dslabs/master/inst/extdata/murders.csv"
dat<-read_csv(url)
str(dat)

download.file(url,"murders.csv")
tempfile()
tempdir()
tmp_filename<-tempfile()
download.file(url,tmp_filename)
dat<-read_csv(tmp_filename)
file.remove(tmp_filename)

#Assesment

getwd()
filename <- "murders.csv"
path <- system.file("extdata", package = "dslabs")

file.copy(file.path(path, "murders.csv"), getwd())

setwd("/Users/Luis Santiago/Documents/data")
file.copy(file.path(path, filename), getwd())

file.copy(file.path(path, "murders.csv"), file.path(getwd(), "data"))

file.location <- file.path(system.file("extdata", package = "dslabs"), "murders.csv")
file.destination <- file.path(getwd(), "data")
file.copy(file.location, file.destination) 

#Assessment Part 2

library(tidyverse)
url<-"https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"
read_lines(url, n_max=3)
dat<-read_csv(url,col_names=FALSE)
head(dat)
str(dat)
download.file(url,"wdbc.data")
getwd()
setwd("C:/Users/Luis Santiago/Documents/Application Data")
nrow(dat)
str(dat)

#Reshaping Data
library(tidyverse)
library(dslabs)
data(gapminder)

#create and inspect a tidy dataframe
tidy_data<-gapminder%>%
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

#ploting tidy data is simple
tidy_data %>% ggplot(aes(year,fertility,color=country))+
  geom_point()

#Import and examine example of original Gapminder data in wide format
path<-system.file("extdata", package="dslabs")
path
list.files(path)
filename<-file.path(path,"fertility-two-countries-example.csv" )
filename
wide_data<-read_csv(filename)
head(wide_data)
select(wide_data,country,"1960":"1967")

# original wide data
library(tidyverse)
path<-system.file("extdata", package="dslabs")
path
list.files(path)
filename<-file.path(path,"fertility-two-countries-example.csv")
wide_data<-read_csv(filename)
head(wide_data)

# tidy data from dslabs
library(dslabs)
data(gapminder)
tidy_data<-gapminder %>%
  filter(country %in% c("South Korea", "Germany")) %>%
  select(country, year, fertility)
head(tidy_data)

#gather wide data to make tidy data
new_tidy_data <- wide_data %>%
  gather(year, fertility, "1960":"2015")
head(new_tidy_data)

#gather all columns except country
new_tidy_data <- wide_data %>%
  gather(year, fertility, -country)
new_tidy_data

#gather treats columns as characters by default
class(tidy_data$year)
class(new_tidy_data$year)

#convert gathered columns to numeric
new_tidy_data <-wide_data %>%
  gather(year, fertility, -country, convert=TRUE)
class(new_tidy_data$year)

#ggplot works on new tidy data
new_tidy_data %>%
  ggplot(aes(year, fertility, color=country)) +
  geom_point()

#spread tidy data to generate wide data

new_wide_data<- new_tidy_data %>%
  spread(year, fertility)
  select(new_wide_data,country, "1960":"2015")

#import data
path<-system.file("extdata", package="dslabs")
list.files(path)
filename<-file.path(path,"life-expectancy-and-fertility-two-countries-example.csv")
raw_dat<-read_csv(filename)
select(raw_dat,1:5)

#gather all columns except country
dat<-raw_dat %>%
  gather(key,value,-country)
dat
head(dat)
dat$key[1:5]

#separate on underscores
dat %>% separate(key, c("year","variable_name"),"_")

dat %>% separate(key, c("year","variable_name"))

#split on all underscores, pad empty cells with NA
dat %>% separate(key,c("year", "first_variable_name", "second_variable_name"),
                 fill="right")
#split on first underscore but keep life_expectancy merged
dat %>% separate(key,c("year", "variable_name"), sep="_", extra="merge")

#separate then spread
dat %>% separate(key, c("year", "variable_name"), sep="_", extra="merge") %>%
  spread(variable_name, value)

#separate then unite
dat %>% separate(key,c("year", "first_variable_name", "second_variable_name"),
                 sep="_", fill="right") %>%
  unite(variable_name, first_variable_name, second_variable_name,sep="_")

#full code for tidying data
dat %>% separate(key,c("year","first_variable_name","second_variable_name"),
                 sep="_", fill="right") %>%
  unite(variable_name, first_variable_name, second_variable_name, sep="_") %>%
  spread(variable_name, value) %>%
  rename(fertility=fertility_NA)


#Assesment 2
co2
co2_wide<- data.frame(matrix(co2, ncol=12, byrow=TRUE))%>%
  setNames(1:12) %>%
  mutate(year=as.character(1959:1997)) 
co2_wide
head(co2_wide)
class(co2_wide)
co2_tidy<-co2_wide %>%
  gather(key="month",value="co2", -year)
head(co2_tidy)

co2_tidy %>% ggplot(aes(as.numeric(month),co2,color=year))+
  geom_line()

library(dslabs)
data(admissions)
dat <- admissions %>% select(-applicants)
head(dat)
dat %>% spread(gender, admitted)

tmp<-gather(admissions,key,value,admitted:applicants)
head(tmp)

tmp2<-tmp %>%
  unite(col_name,key,gender,sep="_")
tmp2

tmp2 %>% spread(col_name,value)

# import US murders data
library(tidyverse)
library(ggrepel)
library(dslabs)
ds_theme_set()
data(murders)
head(murders)

#import us elections results data
data("polls_us_election_2016")
head(results_us_election_2016)
identical(results_us_election_2016$state,murders$state)

#join the murders table and the US election results table
tab<- left_join(murders,results_us_election_2016,by="state")
head(tab)
tab

#plot electoral votes vs population
tab %>% ggplot(aes(population/10^6,electoral_votes,label=abb)) +
  geom_point() +geom_text_repel() +
  scale_x_continuous(trans="log2")+
  scale_y_continuous(trans="log2")+
  geom_smooth(method="lm", se=FALSE)

#make two smaller tables to demonstrate joins
tab1 <- slice(murders,1:6) %>% select(state, population)
tab1
tab2 <- slice(results_us_election_2016,c(1:3,5,7:8)) %>% select(state, electoral_votes)
tab2

#experiment with different joins
left_join(tab1,tab2)
left_join(tab2,tab1)
right_join(tab1,tab2)
tab1 %>% left_join(tab2)
tab1 %>% right_join(tab2)
inner_join(tab1,tab2)
semi_join(tab1,tab2)
anti_join(tab1,tab2)
full_join(tab1,tab2)

#Binding
library(tidyverse)
library(dslabs)
library(ggrepel)
library(dplyr)
data(gapminder)
data("polls_us_election_2016")

bind_cols(a=1:3,b=4:6)
bind_cols(country=c("New Zealand", "USA", "Venezuela", "Colombia"),
          Variable=1:4)

zz<-cbind(a=1:3,b=4:6)
zz
class(zz)
zzz<-rbind(a=1:3,b=4:6)
zzz
class(zzz)

#join the murders table and the US election results table
tab<-left_join(murders, results_us_election_2016,by="state")
head(tab)

tab1<-tab[,1:3]
tab2<-tab[,4:6]
tab3<-tab[,7:9]
new_tab<-bind_cols(tab1,tab2,tab3)
head(new_tab)
class(new_tab)

tab1<-tab[1:2,]
tab2<-tab[3:4,]
other_new_tab<-bind_rows(tab1,tab2)
class(other_new_tab)


# intersect vectors or data frames
intersect(1:10, 6:15)
intersect(c("a","b","c"), c("b","c","d"))
tab1<-tab[1:5,]
tab2<-tab[3:7,]
intersect(tab1,tab2)

# perform a union of vectors or data frames
union(c(1:10),c(6:15))
union(c("a","b","c"),c("b","c","d"))
union(tab1,tab2)

# set difference of vectors or data frames
setdiff(1:10,6:15)
setdiff(6:15,1:10)
setdiff(tab1,tab2)

# setequal determines whether sets have the same elements, regardless of order
setequal(1:5,6:15)
setequal(1:5,5:1)
setequal(tab1,tab2)

#Assesment set operators
library(tidyverse)
library(dplyr)
tab11<-tab1%>%select(state,population)
tab11
tab22<-tab2%>%select(state,electoral_votes)
left_join(tab11,tab22,by="state")

library(Lahman)
top<-Batting %>%
  filter(yearID==2016) %>%
  arrange(desc(HR)) %>%
  slice(1:10)
top
top %>% as_tibble()

Master %>% as_tibble()

top_names <-left_join(top,Master,by="playerID") %>%
  select(playerID,nameFirst,nameLast,HR)
top_names

head(Salaries)
top_salaries <- Salaries %>% filter(yearID==2016) %>%
  right_join(top_names,by="playerID") %>% 
  select(nameFirst, nameLast,HR,salary)
top_salaries

head(AwardsPlayers)
AwardsPlayers %>% filter(yearID==2016) %>%
  inner_join(top_names,by="playerID")
awardsplayers2016<-AwardsPlayers %>% filter(yearID==2016)
top_names %>% inner_join(awardsplayers2016,by="playerID")
intersect(top_names$playerID,awardsplayers2016$playerID)
class(intersect(top_names$playerID,awardsplayers2016$playerID))
length(intersect(top_names$playerID,awardsplayers2016$playerID))

setdiff(awardsplayers2016$playerID,top_names$playerID)
length(setdiff(awardsplayers2016$playerID,top_names$playerID))

# import a webpage into R

library(rvest)
url <- "https://en.wikipedia.org/wiki/Murder_in_the_United_States_by_state"
h<- read_html(url)
class(h)
h

tab<-h %>% html_nodes("table")
tab<- tab[[2]]
tab

tab <- tab %>% html_table
class(tab)
tab

tab<- tab %>% setNames(c("state", "population", "total", "murders", 
                         "gun_murders", "gun_owner_ship", "total_rate", 
                         "murder_rate", "gun_murder_rate"))
head(tab)

#CCS
h <- read_html("http://www.foodnetwork.com/recipes/alton-brown/guacamole-recipe-1940609")
recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()

guacamole<-list(recipe,prep_time,ingredients)
guacamole

#create function to extract ingredients tables to all foods in this webpage
get_recipe <- function(url){
  h <- read_html(url)
  recipe <- h %>% html_node(".o-AssetTitle__a-HeadlineText") %>% html_text()
  prep_time <- h %>% html_node(".m-RecipeInfo__a-Description--Total") %>% html_text()
  ingredients <- h %>% html_nodes(".o-Ingredients__a-Ingredient") %>% html_text()
  return(list(recipe = recipe, prep_time = prep_time, ingredients = ingredients))
} 

get_recipe("http://www.foodnetwork.com/recipes/food-network-kitchen/pancakes-recipe-1913844")


#web scraping assesment
library(rvest)
url<-"https://web.archive.org/web/20181024132313/http://www.stevetheump.com/Payrolls.htm"
h<-read_html(url)

nodes<-html_nodes(h,"table")
class(nodes)
nodes
html_text(nodes[[8]])
html_text(nodes[[2]])
html_text(nodes[[1]])
html_table(nodes[[8]])

html_text(nodes[[1]])
html_table(nodes[[1]])

html_text(nodes[[2]])
html_table(nodes[[2]])
html_table(nodes[[3]])
html_table(nodes[[4]])

nodes
html_text(nodes[[21]])
html_table(nodes[[21]])
html_text(nodes[[20]])
html_table(nodes[[20]])
html_text(nodes[[19]])
html_table(nodes[[19]])

library(dplyr)
library(tidyverse)
library(dslabs)
library(ggrepel)

tab1<-html_table(nodes[[10]])
tab2<-html_table(nodes[[19]])
tab1
class(tab1)
tab2
class(tab2)
tab1<-html_table(nodes[[10]]) %>% select(-X1) %>% 
  setNames(c("Team","Payroll","Average")) %>%
  slice(-1)
tab1
tab2<-html_table(nodes[[19]]) %>%
  setNames(c("Team", "Payroll", "Average")) %>%
  slice(-1)
tab2
full_join(tab1,tab2,by="Team")


library(rvest)
library(tidyverse)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"

h<-read_html(url)
tab<-html_nodes(h,"table")
tab
?html_table
str(html_table(tab[[1]],fill=TRUE))
str(html_table(tab[[2]],fill=TRUE))
str(html_table(tab[[3]],fill=TRUE))
str(html_table(tab[[4]],fill=TRUE))
str(html_table(tab[[5]],fill=TRUE))


library(tidyverse)
library(dslabs)
library(ggrepel)
library(dplyr)
library(rvest)

#read in raw murders data from wikipedia
url<- "https://en.wikipedia.org/w/index.php?title=Gun_violence_in_the_United_States_by_state&direction=prev&oldid=810166167"
murders_raw<-read_html(url) %>%
  html_nodes("table") %>%
  html_table() %>%
  .[[1]] %>%
  setNames(c("state","population","total","murder_rate"))
murders_raw

#inspect data and column classes
head(murders_raw)
class(murders_raw$population)
class(murders_raw$total)


s<-"Hello"
s<-'Hello'
s<-'10"'
s<-"10""
s
s<-"5\'"
s <-'5\'10"'


# murders_raw defined in web scraping video
# direct conversion to numeric fails because of commas
murders_raw$population[1:3]
z<-as.numeric(murders_raw$population[1:3])
z

library(tidyverse)  #includes stringr

# murders_raw was defined in the web scraping section
# detect whether there are commas
commas<-function(x) any(str_detect(x,(",")))
murders_raw %>%summarize_all(funs(commas))

# replace commas with the empty string and convert to numeric
test_1<-str_replace_all(murders_raw$population,",","")
test_1<-as.numeric(test_1)
test_1

# parse_number also removes commas and converts to numeric
test_2<- parse_number(murders_raw$population)
test_2
identical(test_1,test_2)

murders_new<-murders_raw %>% mutate_at(2:3,parse_number)
murders_new %>% head


# load raw heights data and inspect
library(dslabs)
data(reported_heights)
class(reported_heights$height)

# convert to numeric, inspect, count NAs
x<-as.numeric(reported_heights$height)
head(x)
sum(is.na(x))
ind<-which(is.na(x))
ind

# keep only entries that result in NAs
reported_heights %>% mutate(new_height=as.numeric(height)) %>%
  filter(is.na(new_height)) %>%
  head(10)

# calculate cutoffs that cover 99.999% of human population
alpha<-1/10^6
qnorm(1-alpha/2,69.1,2.9)
qnorm(alpha/2,63.7,2.7)

# keep only entries that either result in NAs or are outside the plausible range of heights
library(tidyverse)
library(dplyr)
library(rvest)
library(ggrepel)
not_inches<-function(x,smallest=50,largest=84){
  inches<-suppressWarnings(as.numeric(x))
  ind<-is.na(inches) | inches<smallest |inches>largest
  ind
}

# number of problematic entries
problems<-reported_heights %>%
  filter(not_inches(height)) %>%
  .$height
problems
cat(problems)
length(problems)

# 10 examples of x'y or x'y" or x'y\"
pattern<- "^\\d\\s*'\\s*\\d{1,2}\\.*\\d*'*\"*$"
str_subset(problems,pattern) %>% head(n=10) %>% cat

# 10 examples of x.y or x,y
pattern <- "^[4-6]\\s*[\\.|,]\\s*([0-9]|10|11)$"
str_subset(problems,pattern) %>% head(n=10) %>% cat

# 10 examples of entries in cm rather than inches
ind<-which(between(suppressWarnings(as.numeric(problems))/2.54,54,81))
ind
ind<-ind[!is.na(ind)]
ind
problems[ind] %>% head(n=10) %>% cat


# load stringr through tidyverse
library(tidyverse)

# detect whether a comma is present
pattern<-","
str_detect(murders$total,pattern)
murders$total
str_detect(murders_raw$total,pattern)
str_subset(murders_raw$total,pattern)
ind<-which(str_detect(murders_raw$total,pattern))
ind

# show the subset of strings including "cm"
str_subset(reported_heights$height,"cm")

# use the "or" symbol inside a regex (|)
yes<-c("180 cm", "70 inches")
no<-c("180","70´´")
s<-c(yes,no)
str_detect(s,"cm")|str_detect(s,"inches")
str_detect(s,"cm|inches")

# highlight the first occurrence of a pattern
install.packages("htmlwidgets")
library(htmlwidgets)
str_view(s,pattern)

# highlight all instances of a pattern
str_view_all(s,pattern)
str_view_all(reported_heights$height,pattern)


library(tidyverse)
library(ggrepel)
library(rvest)
library(dplyr)
library(htmlwidgets)
library(dslabs)

# s was defined in the previous video
yes<-c("5","6","5'10","5 feet","4'11")
no<-c("",".","Five","six")
s<-c(yes,no)
pattern<-"\\d"

# [56] means 5 or 6
str_view(s,"[56]")

# [4-7] means 4, 5, 6 or 7
yes<-as.character(4:7)
no<-as.character(1:3)
s<-c(yes,no)
str_detect(s,"[4-7]")

# ^ means start of string, $ means end of string
pattern<- "^\\d$"
yes<-c("1","5","9")
no<-c("12","123"," 1","a4","b")
s<-c(yes,no)
str_view(s,pattern)

# curly braces define quantifiers: 1 or 2 digits
pattern<-"^\\d{1,2}$"
yes<-c("1","5","9","12")
no<-c("123","a4","b")
str_view(c(yes,no),pattern)

# combining character class, anchors and quantifier
pattern<- "^[4-7]'\\d{1,2}\"$"
yes<-c("5'7\"","6'2\"","5'12\"")
no<-c("6,2\"", "6.2\"","I am 5'11\"","3'2\"","64")
str_detect(yes,pattern)
str_detect(no,pattern)


# number of entries matching our desired pattern

not_inches<-function(x,smallest=50,largest=84){
  inches<-suppressWarnings(as.numeric(x))
  ind<-is.na(inches) | inches<smallest |inches>largest
  ind
}
# number of problematic entries
problems<-reported_heights %>%
  filter(not_inches(height)) %>%
  .$height
problems
pattern<- "^[4-7]'\\d{1,2}\"$"
sum(str_detect(problems,pattern))
str_view(problems,pattern)
cat(str_subset(problems,pattern))

# inspect examples of entries with problems
problems[c(2,10,11,12,15)] %>% str_view(pattern)
str_subset(problems,"inches")
str_subset(problems,"''")

# replace or remove feet/inches words before matching
pattern<-"^[4-7]'\\d{1,2}$"
problems %>% 
  str_replace("feet|ft|foot","'")  %>% # replace feet, ft, foot with ' 
  str_replace("inches|in|''|\"","") %>% # remove all inches symbols
  str_detect(pattern) %>%
  sum()

# R does not ignore whitespace
identical("hi", " hi")

# \\s represents whitespace
pattern_2<-"^[4-7]'\\s\\d{1,2}\"$"
str_subset(problems,pattern_2)

# * means 0 or more instances of a character
yes<-c("AB","A1B","A11B","A111B","A1111B")
no<-c("A2B","A21B")
str_detect(yes,"A1*B")
str_detect(no,"A1*B")

# test how *, ? and + differ
data.frame(string=c("AB","A1B","A11B","A111B","A1111B"),
           none_or_more=str_detect(yes,"A1*B"),
           none_or_once=str_detect(yes,"A1?B"),
           once_or_more=str_detect(yes,"A1+B"))

# update pattern by adding optional spaces before and after feet symbol
pattern<-"^[4-7]\\s*'\\s*\\d{1,2}$"
problems %>%
  str_replace("feet|ft|foot","'") %>% #replace feet, ft, foot with '
  str_replace("inches|in|''|\"","") %>% # remove all inches symbols
  str_detect(pattern) %>%
  sum()



# define regex with and without groups
pattern_without_groups<-"^[4-7],\\d*$"
pattern_with_groups<- "^([4-7]),(\\d*)$"

# create examples
yes<-c("5,9","5,11","6,", "6,1")
no<-c("5'9",",","2,8","6.1.1")
s<-c(yes,no)

# demonstrate the effect of groups
str_detect(s,pattern_without_groups)
str_detect(s,pattern_with_groups)

# demonstrate difference between str_match and str_extract
str_match(s,pattern_with_groups)
str_extract(s,pattern_with_groups)

# improve the pattern to recognize more events
pattern_with_groups<-"^([4-7]),(\\d*)$"
yes <- c("5,9", "5,11", "6,", "6,1")
no <- c("5'9", ",", "2,8", "6.1.1")
s<-c(yes,no)
str_replace(s,pattern_with_groups,"\\1'\\2")

# final pattern
pattern_with_groups<-"^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$"

# combine stringr commands with the pipe
str_subset(problems,pattern_with_groups) %>% head
str_subset(problems,pattern_with_groups) %>% 
  str_replace(pattern_with_groups,"\\1'\\2") %>% head


pattern<-"^{4-7]\\s*'\\s*\\d{1,2}$"
str_detect(pattern,reported_heights$height)
mean(str_detect(pattern,reported_heights$height))
sum(str_detect(pattern,reported_heights$height))
head(reported_heights$height)
reported_heights$height

# function to detect entries with problems
not_inches_or_cm<-function(x,smallest=50, tallest=84){
  inches<-suppressWarnings(as.numeric(x))
  ind<-!is.na(inches) & ((inches>=smallest & inches<=tallest) |
    (inches/2.54>=smallest & inches/2.54 <= tallest))
  !ind
}

# identify entries with problems
problems<- reported_heights %>% 
  filter(not_inches_or_cm(height)) %>%
  .$height
problems
length(problems)

converted<-problems %>%
  str_replace("feet|foot|ft","'") %>% #convert feet symbols to '
  str_replace("inches|in|''|\"","") %>% #remove inches symbols
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$","\\1'\\2") ##change format
cat(converted)

# find proportion of entries that fit the pattern after reformatting
pattern<-"^[4-7]\\s*'\\s*\\d{1,2}$"
index<-str_detect(converted,pattern)
mean(index)

converted[!index]


#Assesment
not_inches <- function(x, smallest = 50, tallest = 84) {
  inches <- suppressWarnings(as.numeric(x))
  ind <- is.na(inches) | inches < smallest | inches > tallest 
  ind
}
not_inches(x=70)
not_inches(x=85)

s<-c("70","5 ft","4'11","",".","Six feet")
str_view(s,"\\d\\d|ft")
str_view(s,"\\d*|ft")
str_view(s,"\d|ft")
str_view(s,"\\d|feet")
str_view(s,"(\\d*)|(ft)")

animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]"
str_detect(animals, pattern)

animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[A-Z]$"
str_detect(animals, pattern)

animals <- c("cat", "puppy", "Moose", "MONKEY")
pattern <- "[a-z]{4,5}"
str_detect(animals, pattern)

schools<-c("U. Kentucky","Univ New Hampshire","Univ. of Massachusetts",
             "University Georgia","U California","California State University")
schools %>% 
  str_replace("^Univ\\.?\\s|^U\\.?\\s", "University ") %>% 
  str_replace("^University of |^University ", "University of ")

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")

problems <- c("5.3", "5,5", "6 1", "5 .11", "5, 12")
pattern_with_groups <- "^([4-7])[,\\.\\s](\\d*)$"
str_replace(problems, pattern_with_groups, "\\1'\\2")


yes <- c("5 feet 7inches","5 7")
no <- c("5ft 9 inches", "5 ft 9 inches")
s <- c(yes, no)

converted <- s %>% 
  str_replace("feet|foot|ft", "'") %>% 
  str_replace("inches|in|''|\"", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")

pattern <- "^[4-7]\\s*'\\s*\\d{1,2}$"
str_detect(converted, pattern)

#Correccion
converted <- s %>% 
  str_replace("\\s*(feet|foot|ft)\\s*", "'") %>% 
  str_replace("\\s*(inches|in|''|\")\\s*", "") %>% 
  str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2")
str_detect(converted, pattern)



# first example - normally formatted heights
s <- c("5'10", "6'1")
tab<-data.frame(x=s)
tab

# the separate and extract functions behave similarly
tab%>%separate(x,c("feet","inches"),sep="'")
tab%>%extract(x,c("feet","inches"), regex="(\\d)'(\\d{1,2})")

# second example - some heights with unusual formats
s<-c("5'10","6'1\"","5'8inches")
tab<-data.frame(x=s)  
tab

# separate fails because it leaves in extra characters, but 
#extract keeps only the digits because #of regex groups
tab %>% separate(x,c("feet","inches"),sep="'",fill="right")
tab %>% extract(x,c("feet","inches"),regex="(\\d)'(\\d{1,2})")


# A deeper conversion
library(dplyr)
library(dslabs)
library(ggrepel)
library(htmlwidgets)
library(rvest)
library(tidyverse)

convert_format <- function(s){
    s %>%
    str_replace("feet|foot|ft", "'") %>% #convert feet symbols to '
    str_replace_all("inches|in|''|\"|cm|and", "") %>%  #remove inches and other symbols
    str_replace("^([4-7])\\s*[,\\.\\s+]\\s*(\\d*)$", "\\1'\\2") %>% #change x.y, x,y x y
    str_replace("^([56])'?$", "\\1'0") %>% #add 0 when to 5 or 6
    str_replace("^([12])\\s*,\\s*(\\d*)$", "\\1\\.\\2") %>% #change european decimal
    str_trim() #remove extra space
}
words_to_numbers <- function(s){
    str_to_lower(s) %>%  
    str_replace_all("zero", "0") %>%
    str_replace_all("one", "1") %>%
    str_replace_all("two", "2") %>%
    str_replace_all("three", "3") %>%
    str_replace_all("four", "4") %>%
    str_replace_all("five", "5") %>%
    str_replace_all("six", "6") %>%
    str_replace_all("seven", "7") %>%
    str_replace_all("eight", "8") %>%
    str_replace_all("nine", "9") %>%
    str_replace_all("ten", "10") %>%
    str_replace_all("eleven", "11")
}

# number of problematic entries
problems<-reported_heights %>%
  filter(not_inches(height)) %>%
  .$height
length(problems)

converted <- problems %>% words_to_numbers %>% convert_format
head(problems)
converted
remaining_problems<-converted[not_inches_or_cm(converted)]
length(remaining_problems)
pattern <- "^[4-7]\\s*'\\s*\\d+\\.?\\d*$"
index <- str_detect(remaining_problems, pattern)
remaining_problems[!index]


#Putting all together
pattern<-"^([4-7])\\s*'\\s*(\\d+\\.?\\d*)$"

smallest<-50
tallest<-84

new_heights <- reported_heights %>%
  mutate(original=height, height=words_to_numbers(height) %>% convert_format()) %>%
  extract(height,c("feet","inches"),regex=pattern,remove=FALSE) %>%
  mutate_at(c("height","feet","inches"), as.numeric) %>%
  mutate(guess=12*feet+inches) %>%
  mutate(height=case_when(
    !is.na(height) & between(height,smallest,tallest) ~ height, #inches 
    !is.na(height) & between(height/2.54,smalles,tallest) ~ height/2.54, #centimeters
    !is.na(height) & between(height*100/2.54,smallest,tallest) ~ height*100/2.54, #meters
    !is.na(guess) & inches<12 & between(guess, smallest,tallest) ~ guess, #feet'inches
    TRUE ~ as.numeric(NA))) %>%
  select(-guess)

#check all entries converted
new_heights %>% 
  filter(not_inches(original)) %>%
  select(original,height) %>%
  arrange(height) %>%
  view()

#shortest students
new_heights %>% arrange(height) %>% head(n=7)


#String Splitting
library(tidyverse)
library(dslabs)
library(rvest)
library(ggrepel)
library(dplyr)
library(htmlwidgets)

#read raw data murders line by line

filename<-system.file("extdata/murders.csv",package="dslabs")
lines<-readLines(filename)
lines %>% head()

#split at commas with string_split function, remove row column names
x<-str_split(lines,",")
x%>%head()
col_names1<-x[1]
col_names1
class(col_names1)
col_names<-x[[1]]
col_names
class(col_names)
identical(col_names,col_names1)
x<-x[-1]
head(x)

#extract the elements of each list entry
library(purrr)
map(x,function(y) y[1]) %>%head()

# extract columns 1-5 as characters, then convert to proper format - NOTE: DIFFERENT FROM VIDEO
dat<-data.frame(parse_guess(map_chr(x,1)),
                parse_guess(map_chr(x,2)),
                parse_guess(map_chr(x,3)),
                parse_guess(map_chr(x,4)),
                parse_guess(map_chr(x,5))) %>%
  setNames(col_names)
dat%>%head  

# more efficient code for the same thing
dat<-x%>%
  transpose() %>%
  map( ~ parse_guess(unlist(.))) %>%
  setNames(col_names) %>%
  as.data.frame()
dat

# the simplify argument makes str_split return a matrix instead of a list
x<-str_split(lines,",",simplify=TRUE)
x
col_names<-x[1,]
x<-x[-1,]
x%>%as_data_frame() %>%
  set_names(col_names) %>%
  mutate_all(parse_guess)
?parse_guess


#Case Study: Extracting a Table from a PDF
library(dslabs)
library(tidyverse)
library(dplyr)
library(ggrepel)
library(htmlwidgets)
library(rvest)

data("research_funding_rates")

#Downloading the data
library(pdftools)
temp_file<-tempfile()
?tempfile()
temp_file
url<-"http://www.pnas.org/content/suppl/2015/09/16/1510159112.DCSupplemental/pnas.201510159SI.pdf"
download.file(url,temp_file)
txt<-pdf_text(temp_file)
file.remove(temp_file)
txt

raw_data_research_funding_rates<-txt[2]
raw_data_research_funding_rates

data("raw_data_research_funding_rates")

#Looking at the download
raw_data_research_funding_rates %>% head
tab<-str_split(raw_data_research_funding_rates,"\n")
tab
tab<-tab[[1]]
tab %>% head
the_names_1<-tab[3]
the_names_2<-tab[4]

the_names_1
the_names_1<-the_names_1 %>% str_trim() %>%
  str_replace_all(",\\s.","") %>%
  str_split("\\s{2,}",simplify=TRUE)
the_names_1

the_names_2
the_names_2<-the_names_2 %>% str_trim() %>%
  str_split("\\s+",simplify=TRUE)
the_names_2

tmp_names<-str_c(rep(the_names_1,each=3),the_names_2[-1],sep="_")
tmp_names
the_names_2[-1]
the_names<-c(the_names_2[1],tmp_names)%>%
  str_to_lower() %>%
  str_replace_all("\\s","_")
the_names

new_research_funding_rates<-tab[6:14] %>%
  str_trim() %>%
  str_split("\\s{2,}",simplify=TRUE)%>%
  data.frame(stringsAsFactors=FALSE)%>%
  setNames(the_names) %>%
  mutate_at(-1,parse_number)

new_research_funding_rates %>% head()

identical(research_funding_rates,new_research_funding_rates)


#Recoding
# life expectancy time series for Caribbean countries
library(dslabs)
data("gapminder")
gapminder %>%
  filter(region=="Caribbean") %>%
  ggplot(aes(year,life_expectancy,color=country)) +
  geom_line()

# display long country names
gapminder %>%
  filter(region=="Caribbean") %>%
  filter(str_length(country)>=12) %>%
  distinct(country)
?distinct

# recode long country names and remake plot
library(dplyr)
library(tidyverse)
library(ggrepel)
library(rvest)
library(htmlwidgets)
library(dslabs)
data(gapminder)

gapminder %>%
  filter(region=="Caribbean") %>%
  mutate(country=recode(country,
                        'Antigua and Barbuda'="Barbuda",
                        'Dominican Republic'="DR",
                        'St. Vincent and the Grenadines'="St. Vincent",
                        'Trinidad and Tobado'="Trinidad")) %>%
  ggplot(aes(year,life_expectancy,color=country)) +
  geom_line()


#Assesment
?unnest()
table_schedule<-table()

library(rvest)
library(tidyverse)
library(stringr)
url <- "https://en.wikipedia.org/w/index.php?title=Opinion_polling_for_the_United_Kingdom_European_Union_membership_referendum&oldid=896735054"
tab <- read_html(url) %>% html_nodes("table")
polls <- tab[[5]] %>% html_table(fill = TRUE)
head(polls)
polls<- polls %>% setNames(c("dates", "remain", "leave", "undecided", "lead", 
                     "samplesize", "pollster", "poll_type", "notes"))
head(polls)
pattern <-"\\d*%$"
str_detect(polls$remain,pattern)
sum(str_detect(polls$remain,pattern))
ind<-which(str_detect(polls$remain,pattern))

polls1<-polls %>% filter(str_detect(remain,pattern))

parse_number(polls1$remain)/100
as.numeric(polls1$remain)/100
as.numeric(str_replace(polls1$remain, "%", ""))/100


temp <- str_extract_all(polls$dates, "\\d+\\s[a-zA-Z]+")
head(temp)
class(temp)
# take last element (handles polls that cross month boundaries)
end_date <- sapply(temp, function(x) x[length(x)])
head(end_date)
class(end_date)


# inspect the startdate column of 2016 polls data, a Date type
library(tidyverse)
library(dslabs)
data("polls_us_election_2016")
polls_us_election_2016$startdate %>% head
class(polls_us_election_2016$startdate)
as.numeric(polls_us_election_2016$startdate) %>% head

#ggplot is aware of dates
polls_us_election_2016 %>% filter(pollster=="Ipsos" & state=="U.S.") %>%
  ggplot(aes(startdate,rawpoll_trump)) +
  geom_line()

#lubridate: the didyverse date package
library(lubridate)

#select some random dates from polls
set.seed(2)
dates<- sample(polls_us_election_2016$startdate,10) %>% sort
dates

#extract month, day and year from date strings
data.frame(date=dates,
           month=month(dates),
           day=day(dates),
           year=year(dates))

#extract month labels
month(dates,label=TRUE)

#ymd work on diffeent date styles
x<-c(20090101,"2009-01-02","2009 01 03", "2009-1-4",
     "2009-1, 5", "Created on  2009 1 6", "200901 !!! 07")
ymd(x)

#different parses extract year, month and day in different orders
x<-"09/01/02"
ymd(x)
mdy(x)
ydm(x)
dmy(x)
dym(x)

#current time in your time zone
now()
#current time in GMT
now("GMT")
now() %>% hour()
now() %>% minute
now() %>% second()

#parse time
x<-c("12:34:56")
hms(x)

#parse datetime
x<-"Nov/2/2012 12:34:56"
mdy_hms(x)


#Text Mining
#Case Study: Trump tweets

library(tidyverse)
library(ggplot2)
library(lubridate)
library(tidyr)
library(scales)
set.seed(1)

url <- 'http://www.trumptwitterarchive.com/data/realdonaldtrump/%s.json'
trump_tweets<-map(2009:2017,~sprintf(url, .x)) %>%
  map_df(jsonlite::fromJSON, simplifyDataFrame=TRUE) %>%
  filter(!is_retweet & !str_detect(text,'^"')) %>%
  mutate(created_at = parse_date_time(created_at, orders = "a b! d! H!:M!:S! z!* Y!", tz="EST")) 

library(dslabs)
data("trump_tweets")

head(trump_tweets)
names(trump_tweets)
?trump_tweets

trump_tweets %>% select(text) %>% head
trump_tweets %>% count(source) %>% arrange(desc(n))
trump_tweets %>% extract(source,"source", "Twitter for (.*)") %>%
  count(source)

campaign_tweets <- trump_tweets %>%
  extract(source,"source", "Twitter for (.*)") %>%
  filter(source %in% c("Android","iPhone") &
  created_at >= ymd("2015-06-17") &
  created_at < ymd("2016-11-08")) %>%
  filter(!is_retweet) %>%
  arrange(created_at)

count(campaign_tweets)
head(trump_tweets)

#Use of data visualization to explore possiblity of two groups tweeting devices

ds_theme_set()
campaign_tweets %>% 
  mutate(hour=hour(with_tz(created_at,"EST"))) %>%
  count(source,hour) %>% 
  group_by(source) %>%
  mutate(percent=n/sum(n)) %>%
  ungroup() %>%
  ggplot(aes(hour,percent,color=source)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(labels=percent_format()) +
  labs(x= "Hour of day (EST)",
       y="% of Tweets",
       color="")

library(tidytext)
example <- data_frame(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example1<-tibble(line = c(1, 2, 3, 4),
                      text = c("Roses are red,", "Violets are blue,", "Sugar is sweet,", "And so are you."))
example1
example
example %>% unnest_tokens(word, text)

i <- 3008
campaign_tweets$text[i]
campaign_tweets[i,] %>% 
  unnest_tokens(word, text) %>%
  select(word)

pattern<-"([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
campaign_tweets[i,] %>%
  unnest_tokens(word,text,token="regex",pattern=pattern) %>%
  select(word)

campaign_tweets[i,] %>%
  mutate(text=str_replace_all(text,"https://t.co/[A-Za-z\\d]+|&amp;","")) %>%
  unnest_tokens(word,text,token="regex",pattern=pattern) %>%
  select(word)

tweet_words<-campaign_tweets %>%
  mutate(text=str_replace_all(text,"https://t.co/[A-Za-z\\d]+|&amp;","")) %>%
  unnest_tokens(word,text,token="regex",pattern=pattern)

tweet_words %>%
  count(word) %>%
  arrange(desc(n))

tweet_words<-campaign_tweets %>%
  mutate(text=str_replace_all(text,"https://t.co/[A-Za-z\\d]+|&amp;","")) %>%
  unnest_tokens(word,text,token="regex",pattern=pattern) %>%
  filter(!word %in% stop_words$word)

tweet_words %>% 
  count(word) %>%
  top_n(10, n) %>%
  mutate(word = reorder(word, n)) %>%
  arrange(desc(n))

tweet_words<-campaign_tweets %>%
  mutate(text=str_replace_all(text,"https://t.co/[A-Za-z\\d]+|&amp;","")) %>%
  unnest_tokens(word,text,token="regex",pattern=pattern) %>%
  filter(!word %in% stop_words$word & !str_detect(word,"^\\d+$")) %>%
  mutate(word=str_replace(word,"^'",""))

names(tweet_words)
android_iphone_or<-tweet_words %>% 
  count(word,source) %>%
  spread(source,n,fill=0) %>%
  mutate(or=(Android+0.5)/(sum(Android)-Android+0.5)/
           (iPhone+0.5)/(sum(iPhone)-iPhone+0.5))
android_iphone_or %>% arrange(desc(or))
android_iphone_or %>% arrange(or)  

android_iphone_or %>% filter(Android+iPhone>100) %>%
  arrange(desc(or))
android_iphone_or %>% filter(Android+iPhone>100) %>%
  arrange(or)

library(tidytext)
sentiments
get_sentiments("bing")
get_sentiments("afinn")

library(textdata)
get_sentiments("afinn")

get_sentiments("loughran")
get_sentiments("loughran") %>% count(sentiment)

get_sentiments("nrc")
get_sentiments("nrc") %>% count(sentiment)

?sentiments

nrc<-get_sentiments("nrc") %>% select(word, sentiment)
nrc

tweet_words %>% inner_join(nrc,by="word") %>%
  select(source, word, sentiment) %>% sample_n(10)

sentiment_counts<-tweet_words %>%
  left_join(nrc, by="word") %>%
  count(source, sentiment) %>%
  spread(source,n) %>%
  mutate(sentiment=replace_na(sentiment, replace="none"))
sentiment_counts                                

tweet_words %>% group_by(source)%>% summarize(n=n())

sentiment_counts %>%
  mutate(Android=Android/(sum(Android)-Android), 
         iPhone=iPhone/(sum(iPhone)-iPhone),
         or=Android/iPhone) %>%
  arrange(desc(or))

library(broom)
log_or<-sentiment_counts %>%
  mutate(log_or = log((Android/(sum(Android)-Android))/
           (iPhone/(sum(iPhone)-iPhone))),
            se = sqrt(1/Android + 1/(sum(Android)-Android)+
                        1/iPhone + 1/(sum(iPhone)-iPhone)),
            conf.low = log_or - qnorm(0.975)*se,
            conf.high = log_or + qnorm(0.975)*se) %>%
  arrange(desc(log_or))
log_or  

log_or %>%
  mutate(sentiment=reorder(sentiment, log_or),) %>%
  ggplot(aes(x=sentiment,ymin=conf.low,ymax=conf.high)) +
  geom_errorbar() +
  geom_point(aes(sentiment,log_or)) +
  ylab("Log adds ratio for association between Android and sentiment") +
  coord_flip()


android_iphone_or %>% inner_join(nrc) %>%
  filter(sentiment=="disgust" & Android + iPhone >10) %>% 
  arrange(desc(or))


android_iphone_or %>% inner_join(nrc, by = "word") %>%
  mutate(sentiment = factor(sentiment, levels = log_or$sentiment)) %>%
  mutate(log_or = log(or)) %>%
  filter(Android + iPhone > 10 & abs(log_or)>1) %>%
  mutate(word = reorder(word, log_or)) %>%
  ggplot(aes(word, log_or, fill = log_or < 0)) +
  facet_wrap(~sentiment, scales = "free_x", nrow = 2) + 
  geom_bar(stat="identity", show.legend = FALSE) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) 

#Assesment Part 1

library(dslabs)
library(lubridate)
options(digits=3)

data(brexit_polls)
head(brexit_polls)
zz<-brexit_polls$startdate
sum(month(zz)==4)
zz1<-brexit_polls %>% filter(month(startdate)==4) %>% .$startdate
str(zz1)
length(zz1)
sum(month(brexit_polls$startdate)==4)

zz2<-brexit_polls %>% 
  filter(round_date(enddate, unit="week")=="2016-06-12") %>%
  .$enddate
length(zz2)
sum(round_date(brexit_polls$enddate, unit="week")=="2016-06-12")

weekdays(brexit_polls$enddate)
brexit_polls %>% 
  mutate(weekday=weekdays(enddate)) %>%
  count(weekday) %>%
  arrange(desc(n))
table(weekdays(brexit_polls$enddate))

data(movielens)
head(movielens)
str(movielens)
movielens %>% filter(movieId==1029)

movielens %>%
  mutate(review_date = as_datetime(timestamp)) %>%
  count(movieId) %>%
  arrange(desc(n))
movielens %>% filter(movieId==356) %>%
  .$review_date

movielens1<-movielens %>%
  mutate(review_date = as_datetime(timestamp))
head(movielens1)
movielens1 %>%
  count(year(review_date)) %>%
  arrange(desc(n))
movielens1 %>%
  count(hour(review_date)) %>%
  arrange(desc(n))

dates <- as_datetime(movielens$timestamp)
reviews_by_year <- table(year(dates))    # count reviews by year
reviews_by_year
names(which.max(reviews_by_year))    # name of year with most reviews

reviews_by_hour <- table(hour(dates))    # count reviews by hour
names(which.max(reviews_by_hour))    # name of hour with most reviews


library(tidyverse)
library(gutenbergr)
library(tidytext)
options(digits=3)

gutenberg_metadata
str(gutenberg_metadata)

patternzz<-"Pride and Prejudice"
str_detect(gutenberg_metadata$title,patternzz)
indzz<-which(str_detect(gutenberg_metadata$title,patternzz))
indzz

gutenberg_metadata %>%
  filter(str_detect(title, "Pride and Prejudice"))
?gutenberg_works

gutenberg_works(title=="Pride and Prejudice")$gutenberg_id
gutenberg_works(title=="Pride and Prejudice")

str(gutenberg_metadata)
?gutenberg_download
Book_PandP<-gutenberg_download(1342)
words<-Book_PandP %>% unnest_tokens(word, text)
head(words)
str(words)
nrow(words)

library(tidytext)
words1 <- words %>% filter(!word %in% stop_words$word)
nrow(words1)
words2 <- words %>% anti_join(stop_words, by="word")
nrow(words2)

words3 <- words1 %>% filter(!str_detect(word, "\\d"))
nrow(words3)

words3 %>% count(word) %>% filter(n>100) %>% arrange(desc(n))
wordstable<-words3 %>% count(word) %>% filter(n>100)
nrow(wordstable)

words3 %>% count(word) %>% filter(n>100) %>% nrow()
words3 %>% count(word) %>% top_n(1,n) %>% pull(word)
words3 %>% count(word) %>% top_n(1,n) %>% pull(n)

afinn <- get_sentiments("afinn")
?get_sentiments

afinn_sentiments <- words3 %>% inner_join(afinn, by = "word")
nrow(afinn_sentiments)
head(afinn_sentiments)

positive_afinn <- afinn_sentiments %>% filter(value>0)
nrow(positive_afinn)/nrow(afinn_sentiments)
afinn_sentiments %>% filter(value==4) %>% nrow()

mean(afinn_sentiments$value)
sum(afinn_sentiments$value==4)
