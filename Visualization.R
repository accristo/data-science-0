#Data Visualisation
# load the dataset
library(dslabs)
data(heights)

# make a table of category proportions
prop.table(table(heights$sex))
hist(heights$height)
help(hist)
hist(heights$height)
histx<-seq(50,85,1)
hist(heights$height,histx)

help("sapply")

library(dslabs)
data(my_data)

a <- seq(min(my_data), max(my_data), length = 100)    # define range of values spanning the dataset
cdf_function <- function(x) {    # computes prob. for a single value
  mean(my_data <= x)
}
cdf_values <- sapply(a, cdf_function)
plot(a, cdf_values)



# define x as vector of male heights
library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
x
print(x)


# calculate the mean and standard deviation manually
average <- sum(x)/length(x)
SD <- sqrt(sum((x - average)^2)/length(x))
average
SD

# built-in mean and sd functions - note that the audio and printed values disagree
average <- mean(x)
SD <- sd(x)
c(average = average, SD = SD)

# calculate standard units
z <- scale(x)
z


# calculate proportion of values within 2 SD of mean
mean(abs(z) < 2)

library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)

# plot distribution of exact heights in data
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")

library(tidyverse)
library(dslabs)
data(heights)
summary(heights$height)

p <- seq(0.01, 0.99, 0.01)
quantile(heights$height, p)
percentiles <- quantile(heights$height, p)
percentiles[names(percentiles) == "25%"]
percentiles[names(percentiles) == "75%"]

p <- seq(0.01, 0.99, 0.01)
theoretical_quantiles <- qnorm(p, 69, 3)
theoretical_quantiles
qnorm(p)
pnorm(0.025)
pnorm(-1.96)

library(tidyverse)
library(dslabs)
data(heights)
index <- heights$sex=="Male"
x <- heights$height[index]
z <- scale(x)
index
x
z
mean(x <= 69.5)
p <- seq(0.05, 0.95, 0.05)
observed_quantiles <- quantile(x, p)
theoretical_quantiles <- qnorm(p, mean = mean(x), sd = sd(x))
observed_quantiles
theoretical_quantiles
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

observed_quantiles <- quantile(z, p)
theoretical_quantiles <- qnorm(p)
observed_quantiles
theoretical_quantiles
plot(theoretical_quantiles, observed_quantiles)
abline(0,1)

library(dslabs)
data(murders)
p<-seq(0.01,0.99,0.01)
p
str(murders)
x<-murders$total
x
data_quantile<-quantile(x,p)
data_quantile
theory_quantile<-qnorm(p,mean(x),sd(x))
theory_quantile
plot(data_quantile,theory_quantile)
abline(0,1)
z<-scale(x)
z
data_quantile(z,p)
data_quantile
theory_quantile<-qnorm(p)
theory_quantile
plot(theory_quantile,data_quantile)
abline(0,1)

library(dslabs)
data(murders)
library(tidyverse)

ggplot(data=murders)
murders %>% ggplot()
p<-ggplot(data=murders)
class(p)
p

library(tidyverse)
library(dslabs)
data(murders)

?geom_point

murders %>% ggplot() +
  geom_point(aes(x = population/10^6, y = total))

# add points layer to predefined ggplot object
p <- ggplot(data = murders)
p + geom_point(aes(population/10^6, total))

?geom_text
# add text layer to scatterplot
p + geom_point(aes(population/10^6, total)) +
  geom_text(aes(population/10^6, total, label = abb))
p+geom_point(aes(population/10^6,total),size=3)+geom_text(aes(population/10^6,total,label=abb))
p+geom_point(aes(population/10^6,total),size=3)+geom_text(aes(population/10^6,total,label=abb),nudge_x=1)

p<-murders%>%ggplot(aes(population/10^6,total,label=abb))
p+geom_point(size=3)+geom_text(nudge_x=1.5)
p+geom_point(size=3)+geom_text(x=10,y=800,label="xxxxx")


p<-murders%>%ggplot(aes(population/10^6,total,label=abb))
p+geom_point(size=3)+
  geom_text(nudge_x=0.05)+
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")
p+geom_point(size=3)+
  geom_text(nudge_x=0.05)+
  scale_x_log10()+
  scale_y_log10()
p+geom_point(size=3)+
  geom_text(nudge_x=0.05)+
  scale_x_log10()+scale_y_log10()+
  xlab("Population in millions (log scale)")+
  ylab("Total number of murders (log scale)")+
  ggtitle("US Gun Murders in 2010")

p<-murders%>%ggplot(aes(population/10^6,total,label=abb))+
  geom_text(nudge_x=0.05)+
  scale_x_log10()+scale_y_log10()+
  xlab("Population in millions (log scale)")+
  ylab("Total number of murders (log scale)")+
  ggtitle("US Gun Murders in 2010")
p+geom_point(size=3,col="blue")
p+geom_point(aes(col=region),size=3)

r<-murders%>%summarize(rate=sum(total)/sum(population)*10^6)%>%pull(rate)
r

p+geom_point(aes(col=region),size=3)+
  geom_abline(intercept=log10(r))
?geom_abline

p <-
  geom_abline(intercept = log10(r), lty = 2, color = "darkgrey") +
  geom_point(aes(col = region), size = 3)
p <- p + scale_color_discrete(name = "Region")    # capitalize legend title

library(dslabs)
ds_theme_set()

library(tidyverse)
library(ggthemes)
p+theme_economist()

library(tidyverse)
library(ggrepel)
library(ggthemes)
library(dslabs)
data(murders)
library(ggrepel)
r<-murders%>%summarize(rate=sum(total)/sum(population)*10^6)%>%.$rate
r
murders%>%ggplot(aes(population/10^6,total,label=abb))+
  geom_abline(intercept=log10(r),lty=2,color="darkgrey")+
  geom_point(aes(col=region),size=3)+
  geom_text_repel()+
  scale_x_log10()+
  scale_y_log10()+
  xlab("Population in millions (log scale)") +
  ylab("Total number of murders (log scale)")+
  ggtitle("US Gun Murders in 2010")+
  scale_color_discrete(name = "Region")+
  theme_economist()

library(tidyverse)
library(dslabs)
data(heights)

b<-heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))

b+geom_histogram()
b + geom_histogram(binwidth = 1)
b + geom_histogram(binwidth = 1, fill = "blue", col = "black") +
  xlab("Male heights in inches") +
  ggtitle("Histogram")
?geom_histogram

b + geom_density()
b + geom_density(fill = "blue")

b <- heights %>% filter(sex == "Male") %>%
  ggplot(aes(sample = height))
b + geom_qq()

params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
params
b + geom_qq(dparams = params) +
  geom_abline()

heights %>%
  ggplot(aes(sample = scale(height))) +
  geom_qq() +
  geom_abline()

p <- heights %>% filter(sex == "Male") %>% ggplot(aes(x = height))
p1 <- p + geom_histogram(binwidth = 1, fill = "blue", col = "black")
p2 <- p + geom_histogram(binwidth = 2, fill = "blue", col = "black")
p3 <- p + geom_histogram(binwidth = 3, fill = "blue", col = "black")

library(dplyr)
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 3)

p<-ggplot(murders)
p

str(murders)
str(heights)
?geom_density

library(tidyverse)
library(dslabs)
data(heights)
data(murders)

heights%>%group_by(sex)%>%summarize(average=mean(height),standard_deviation=sd(height))
p<-heights%>%group_by(sex)%>%summarize(average=mean(height),standard_deviation=sd(height))
str(p)

murders<-mutate(murders,rate=total/population*10^6)
murders
head(murders)
murders<-murders%>%mutate(rate=total/population*1000000)
head(murders)

murders%>%group_by(region)%>%summarize(average=mean(rate))
murders%>%group_by(region)%>%summarize(median_rate=median(rate))


library(tidyverse)
library(dslabs)
data(murders)

murders<-murders%>%mutate(murder_rate=total/population*1000000)

murders%>%arrange(population)%>%head()
murders%>%arrange(desc(murder_rate))%>%head()
murders %>% top_n(10, murder_rate)
murders %>% top_n(10)
murders%>%arrange(region,desc(murder_rate))%>%top_n(20)
murders%>%filter(region=="North Central")
murders%>%arrange(region,murder_rate)
murders%>%arrange(region,desc(murder_rate))

library(NHANES)
install.package("NHANES")
library(NHANES)
data(NHANES)
head(NHANES)
str(NHANES)

library(dslabs)
data(na_example)
head(na_example)
str(na_example)

mean(na_example)
mean(na_example,na.rm=TRUE)
sd(na_example,na.rm=TRUE)
ind<-is.na(na_example)
ind
sum(ind)
ind1<-which(is.na(na_example))
ind1

length(NHANES)
data(NHANES)
length(NHANES)


library(NHANES)
data(NHANES)
length(NHANES)

tab<-NHANES %>% filter(AgeDecade==" 20-29" & Gender=="female")
tab

library(dslabs)
library(dplyr)
library(tidyverse)
data(gapminder)
filter(gapminder,year==1962)%>%ggplot(aes(fertility,life_expectancy))+geom_point()
ds_theme_set()
filter(gapminder,year==1962)%>%ggplot(aes(fertility,life_expectancy))+geom_point()
filter(gapminder,year==1962)%>%ggplot(aes(fertility,life_expectancy,color=continent))+
       geom_point()
head(gapminder)

library(dslabs)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(ggthemes)
data(gapminder)
class(gapminder)
head(gapminder)

ds_theme_set()

filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility,life_expectancy,color=continent)) + geom_point() + 
  facet_grid(continent~year)

filter(gapminder, year %in% c(1962, 2012)) %>%
  ggplot(aes(fertility, life_expectancy, col = continent)) +
  geom_point() +
  facet_grid(.~ year)

years<-c(1962,1980,1990,2000,2012)
continents<-c("Europe","Asia")
filter(gapminder,year%in%years,continent%in%continents)%>%
  ggplot(aes(fertility,life_expectancy,color=continent))+
  geom_point()+
  facet_wrap(~year)

gapminder%>%filter(country=="United States")%>%
  ggplot(aes(year,fertility))+
  geom_line()

countries<-c("Germany", "South Korea")
gapminder%>%filter(country%in%countries)%>%
  ggplot(aes(year,fertility))+
  geom_line()
gapminder%>%filter(country%in%countries)%>%
  ggplot(aes(year,fertility,group=country))+
  geom_line()
gapminder%>%filter(country%in%countries)%>%
  ggplot(aes(year,fertility,color=country))+
  geom_line()

labels<-data.frame(country=countries,x=c(1975,1965),y=c(60,73))
labels

gapminder%>%filter(country%in%countries)%>%
  ggplot(aes(year,life_expectancy,color=country))+
  geom_line()+
  geom_text(data=labels,aes(x,y,label=country),size=5)+
  theme(legend.position="none")

ind<-is.na(gapminder$gdp)
ind
sum(ind)
ind1<-which(is.na(gapminder$gdp))
ind1

gapminder<-gapminder%>%mutate(dollars_per_day=gdp/population/365)
past_year<-1970
gapminder%>%filter(year==past_year & !is.na(gdp))%>%
  ggplot(aes(dollars_per_day))+
  geom_histogram()
gapminder%>%filter(year==past_year & !is.na(gdp))%>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth=1,color="black")

gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(log2(dollars_per_day))) +
  geom_histogram(binwidth = 1, color = "black")

gapminder %>%
  filter(year == past_year & !is.na(gdp)) %>%
  ggplot(aes(dollars_per_day)) +
  geom_histogram(binwidth = 1, color = "black") +
  scale_x_continuous(trans = "log2")



library(dslabs)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(ggthemes)
data(gapminder)

gapminder<-mutate(gapminder,dollars_per_day=gdp/population/365)
levels(gapminder$region)
length(levels(gapminder$region))

past_year<-1970
p<-gapminder%>%filter(year==past_year,!is.na(gdp))%>%
  ggplot(aes(region,dollars_per_day))
p+geom_boxplot()

p +geom_boxplot()+ theme(axis.text.x=element_text(angle=90,hjust=1))

head(gapminder)
p<-gapminder%>%filter(year==past_year,!is.na(gdp))%>%
  mutate(region=reorder(region,dollars_per_day,FUN=median))%>%
  ggplot(aes(region,dollars_per_day,fill=continent))+
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("")
p
p+scale_y_continuous(trans="log2")
p+scale_y_continuous(trans="log2") + geom_point(show.legend=FALSE)


library(dslabs)
library(dplyr)
library(tidyverse)
library(ggrepel)
library(ggthemes)
data(gapminder)

past_year<-1970
gapminder<-gapminder%>%mutate(dollars_per_day=gdp/population/365)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")
gapminder%>%filter(year==past_year,!is.na(gdp))%>%
  mutate(group=ifelse(region%in%west,"West","Developing"))%>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth=1,color="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(. ~ group)

present_year<-2010
gapminder%>%filter(year%in%c(past_year,present_year),!is.na(gdp))%>%
  mutate(group=ifelse(region%in%west,"West","Developing"))%>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth=1,color="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(. ~ group)

country_list_1<-gapminder%>%filter(year==past_year& !is.na(dollars_per_day))%>%
  .$country
country_list_1
country_list_2<-gapminder%>%filter(year==present_year&!is.na(dollars_per_day))%>%
  .$country
country_list_2
country_list<-intersect(country_list_1,country_list_2)

gapminder%>%filter(year%in%c(past_year,present_year),country%in%country_list)%>%
  mutate(group=ifelse(region%in%west,"West","Developing"))%>%
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth=1,color="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(. ~ group)

p<-gapminder%>%filter(year%in%c(past_year,present_year),country%in%country_list)%>%
  mutate(region=reorder(region,dollars_per_day,FUN=median))%>%
  mutate(group=ifelse(region%in%west,"West","Developing"))%>%
  ggplot()+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  xlab("") + scale_y_continuous(trans = "log2")

p+geom_boxplot(aes(region,dollars_per_day,fill=factor(year)))

gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>% group_by(group) %>%
  summarize(n = n()) %>% knitr::kable()

p <- gapminder %>%
  filter(year == past_year & country %in% country_list) %>%
  mutate(group = ifelse(region %in% west, "West", "Developing")) %>%
  ggplot(aes(dollars_per_day,y = ..count.., fill = group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.75) + facet_grid(year ~ .)

gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "West",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region %in% c("Caribbean", "Central America", "South America") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    TRUE ~ "Others"))
head(gapminder)
gapminder <- gapminder %>%
  mutate(group = factor(group, levels = c("Others", "Latin America", "East Asia", "Sub-Saharan Africa", "West")))
head(gapminder)
gapminder

p <- gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  ggplot(aes(dollars_per_day, fill = group)) +
  scale_x_continuous(trans = "log2")
p + geom_density(alpha = 0.2, bw = 0.75, position = "stack") +
  facet_grid(year ~ .)

gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2)) %>%
  ungroup() %>%
  ggplot(aes(dollars_per_day, fill = group, weight = weight)) +
  scale_x_continuous(trans = "log2") +
  geom_density(alpha = 0.2, bw = 0.75, position = "stack") + facet_grid(year ~ .)

gapminder %>%
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>%
  group_by(year) %>%
  mutate(weight = population/sum(population*2))

gapminder <- gapminder %>%
  mutate(group = case_when(
    .$region %in% west ~ "The West",
    .$region %in% "Northern Africa" ~ "Northern Africa",
    .$region %in% c("Eastern Asia", "South-Eastern Asia") ~ "East Asia",
    .$region == "Southern Asia" ~ "Southern Asia",
    .$region %in% c("Central America", "South America", "Caribbean") ~ "Latin America",
    .$continent == "Africa" & .$region != "Northern Africa" ~ "Sub-Saharan Africa",
    .$region %in% c("Melanesia", "Micronesia", "Polynesia") ~ "Pacific Islands"))
gapminder

surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income

surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  mutate(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income

surv_income <- gapminder %>%
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group)) %>%
  group_by(group) %>%
  summarize(income = sum(gdp)/sum(population)/365,
            infant_survival_rate = 1 - sum(infant_mortality/1000*population)/sum(population))
surv_income
surv_income %>% arrange(income)
surv_income %>% ggplot(aes(income, infant_survival_rate, label = group, color = group)) +
  scale_x_continuous(trans = "log2", limit = c(0.25, 150)) +
  scale_y_continuous(trans = "logit", limit = c(0.875, .9981),
                     breaks = c(.85, .90, .95, .99, .995, .998)) +
  geom_label(size = 3, show.legend = FALSE) 


library(dplyr)
library(ggplot2)
library(dslabs)
data(gapminder)
gapminder%>%mutate(dollars_per_day=gdp/population/365)%>%
  filter(year%in%c(1970,2010) & continent=="Africa" & !is.na(gdp))%>%
  ggplot(aes(dollars_per_day,infant_mortality,color=region,label=country))+
  scale_x_continuous(trans="log2")+
  geom_text()+
  facet_grid(year~.)

# dot plot showing the data
heights %>% ggplot(aes(sex, height)) + geom_point()
heights %>% ggplot(aes(sex, height)) + geom_boxplot()+
  geom_jitter(width = 0.2, alpha = 0.2)
  
library(tidyverse)
library(dslabs)
data(gapminder)

west <- c("Western Europe", "Northern Europe", "Southern Europe", "Northern America", "Australia and New Zealand")

dat <- gapminder %>%
  filter(year %in% c(2010, 2015) & region %in% west & !is.na(life_expectancy) & population > 10^7)

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0)) %>%
  mutate(year = as.factor(year)) %>%
  ggplot(aes(year, life_expectancy, group = country)) +
  geom_line(aes(color = country), show.legend = FALSE) +
  geom_text(aes(x = location, label = country, hjust = hjust), show.legend = FALSE) +
  xlab("") +
  ylab("Life Expectancy") 

dat %>%
  mutate(location = ifelse(year == 2010, 1, 2),
         location = ifelse(year == 2015 & country %in% c("United Kingdom", "Portugal"),
                           location + 0.22, location),
         hjust = ifelse(year == 2010, 1, 0))

library(ggrepel)
dat %>%
  mutate(year = paste0("life_expectancy_", year)) %>%
  select(country, year, life_expectancy) %>% spread(year, life_expectancy) %>%
  mutate(average = (life_expectancy_2015 + life_expectancy_2010)/2,
         difference = life_expectancy_2015 - life_expectancy_2010) %>%
  ggplot(aes(average, difference, label = country)) +
  geom_point() +
  geom_text_repel() +
  geom_abline(lty = 2) +
  xlab("Average of 2010 and 2015") +
  ylab("Difference between 2015 and 2010")

# import data and inspect
library(tidyverse)
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)

# assign dat to the per 10,000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reporting
the_disease <- "Measles"
dat <- us_contagious_diseases %>%
  filter(!state %in% c("Hawaii", "Alaska") & disease == the_disease) %>%
  mutate(rate = count / population * 10000 * 52/weeks_reporting) %>%
  mutate(state = reorder(state, rate))

# plot disease rates per year in California
dat %>% filter(state == "California" & !is.na(rate)) %>%
  ggplot(aes(year, rate)) +
  geom_line() +
  ylab("Cases per 10,000") +
  geom_vline(xintercept=1963, col = "blue")

dat %>% ggplot(aes(year, state, fill=rate)) +
  geom_tile(color = "grey50") +
  scale_x_continuous(expand = c(0,0)) +
  scale_fill_gradientn(colors = RColorBrewer::brewer.pal(9, "Reds"), trans = "sqrt") +
  geom_vline(xintercept = 1963, col = "blue") +
  theme_minimal() + theme(panel.grid = element_blank()) +
  ggtitle(the_disease) +
  ylab("") +
  xlab("")

# compute US average measles rate by year
avg <- us_contagious_diseases %>%
  filter(disease == the_disease) %>% group_by(year) %>%
  summarize(us_rate = sum(count, na.rm = TRUE)/sum(population, na.rm = TRUE)*10000)
dat %>%
  filter(!is.na(rate)) %>%
  ggplot() +
  geom_line(aes(year, rate, group = state), color = "grey50", 
            show.legend = FALSE, alpha = 0.2, size = 1) +
  geom_line(mapping = aes(year, us_rate), data = avg, size = 1, col = "black") +
  scale_y_continuous(trans = "sqrt", breaks = c(5, 25, 125, 300)) +
  ggtitle("Cases per 10,000 by state") +
  xlab("") +
  ylab("") +
  geom_text(data = data.frame(x = 1955, y = 50),
            mapping = aes(x, y, label = "US average"), color = "black") +
  geom_vline(xintercept = 1963, col = "blue")


options(digits = 3)    # report 3 significant digits
library(tidyverse)
library(titanic)
str(titanic_train)
titanic <- titanic_train %>%
  select(Survived, Pclass, Sex, Age, SibSp, Parch, Fare) %>%
  mutate(Survived = factor(Survived),
         Pclass = factor(Pclass),
         Sex = factor(Sex))

?titanic_train

titanic%>%filter(!is.na(Age) & !is.na(Sex))%>%ggplot(aes(Age,fill=Sex))+
  geom_density(alpha=0.2)+geom_vline(xintercept = 35)
titanic%>%filter(Age==40)%>%ggplot(aes(Sex))+geom_bar()
titanic%>%filter(!is.na(Age) & !is.na(Sex))%>%ggplot(aes(Age))+
  geom_histogram(binwidth=1)+facet_grid(.~Sex)
titanic%>%filter(!is.na(Age) & !is.na(Sex))%>%ggplot(aes(Sex,Age))+
  geom_boxplot()+geom_jitter(width = 0.2, alpha = 0.2)
titanic%>%filter(!is.na(Age) & !is.na(Sex) & Age>=18 & Age<=35) %>%
  ggplot(aes(Age))+geom_histogram(binwidth=1)+facet_grid(.~Sex)

titanic_male<-titanic$Sex=="male"
sum(titanic_male)
titanic_female<-titanic$Sex=="female"
sum(titanic_female)
titanic%>%count(Sex)

params <- titanic %>%
  filter(!is.na(Age)) %>%
  summarize(mean = mean(Age), sd = sd(Age))
params
titanic%>%filter(!is.na(Age))%>%ggplot(aes(sample=Age))+geom_qq(dparams=params)+
  geom_abline()

titanic%>%filter(!is.na(Survived) & !is.na(Sex))%>%ggplot(aes(Sex, fill=Survived))+
  geom_bar()
titanic%>%filter(!is.na(Age) & !is.na(Survived))%>%ggplot(aes(Age,fill=Survived))+
  geom_density(alpha=0.2)
?geom_density()
titanic%>%filter(!Fare==0)%>%ggplot(aes(Survived, Fare))+geom_boxplot()+
  geom_jitter(width=0.2, alpha=0.2)+scale_y_continuous(trans="log2")
titanic%>%ggplot(aes(Pclass,fill=Survived))+geom_bar()                  
 ?geom_bar()
titanic%>%ggplot(aes(Pclass,fill=Survived))+geom_bar(position=position_fill())
titanic%>%ggplot(aes(Survived,fill=Pclass))+geom_bar(position=position_fill())

titanic%>%filter(!is.na(Age))%>%ggplot(aes(Age,y=..count..,fill=Survived))+
  geom_density(alpha=0.3)+facet_grid(Sex~Pclass)
titanic%>%filter(!is.na(Age))%>%ggplot(aes(Age,y=..count..,fill=Survived))+
  geom_density(alpha=0.3)+facet_grid(.~Pclass)
titanic%>%filter(!is.na(Age))%>%ggplot(aes(Age,y=..count..))+
  geom_density(alpha=0.3)+facet_grid(.~Pclass)
