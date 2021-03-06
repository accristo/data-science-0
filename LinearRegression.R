install.packages("Lahman")
library(Lahman)
library(tidyverse)
ds_theme_set()

#Code: Scatterplot of the relationship between HRs and wins
str(Teams)
Teams%>%filter(yearID %in% 1961:2001) %>%
  mutate(HR_per_game = HR/G, R_per_game = R/G) %>%
  ggplot(aes(HR_per_game,R_per_game)) + 
  geom_point(alpha=0.5)

#Code: Scatterplot of the relationship between stolen bases and wins
Teams%>%filter(yearID %in% 1961:2001) %>%
  mutate(SB_per_game = SB/G,R_per_game = R/G) %>%
  ggplot(aes(SB_per_game,R_per_game)) + 
  geom_point(alpha=0.5)



#Code: Scatterplot of the relationship between bases on balls and runs
Teams %>%filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G,R_per_game = R/G) %>%
  ggplot(aes(BB_per_game,R_per_game)) +
  geom_point(alpha=0.5)

?Teams
Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(AB_per_game=AB/G,R_per_game=R/G) %>%
  ggplot(aes(AB_per_game,R_per_game)) +
  geom_point(alpha=0.5)

Teams%>%filter(yearID %in% 1961:2001) %>%
  mutate(E_per_game=E/G,W_per_game=W/G) %>%
  ggplot(aes(E_per_game,W_per_game)) +
  geom_point(alpha=0.5)

Teams%>%filter(yearID %in% 1961:2001) %>%
  mutate(X2B_per_game=X2B/G,X3B_per_game=X3B/G) %>%
  ggplot(aes(X2B_per_game,X3B_per_game)) +
  geom_point(alpha=0.5)

# create the dataset
library(tidyverse)
install.packages(HistData)
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights <- GaltonFamilies %>%
  filter(gender=="male")%>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father,childHeight) %>%
  rename(son=childHeight)
galton_heights

# means and standard deviations
galton_heights %>% 
  summarize(mean(father), mean(son), sd(father), sd(son))

# scatterplot of father and son heights
galton_heights %>%
  ggplot(aes(father,son)) +
  geom_point(alpha=0.5)

library(tidyverse)
library(Lahman)
library(HistData)
rho<-mean(scale(x)*scale(y))
galton_heights <- GaltonFamilies %>%
  filter(gender=="male")%>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father,childHeight) %>%
  rename(son=childHeight)
galton_heights%>%summarize(r=cor(father,son))%>%pull(r)

# compute sample correlation
R<-sample_n(galton_heights,25,repalece=TRUE) %>% 
  summarize(r=cor(father,son))
R

# Monte Carlo simulation to show distribution of sample correlation
B<-1000
N<-25
R<-replicate(B,{
  sample_n(galton_heights,N,replace=TRUE) %>%
    summarize(r=cor(father,son)) %>% pull(r)
})
R
qplot(R,geom="histogram",binwidth=0.05,color=I("black"))

# expected value and standard error
mean(R)
sd(R)

# QQ-plot to evaluate whether N is large enough
data.frame(R) %>% 
  ggplot(aes(sample=R)) + 
  stat_qq()+
  geom_abline(intercept=mean(R),slope = sqrt((1-mean(R)^2)/(N-2)))

Teams%>%filter(yearID %in% 1961:2001) %>%
  mutate(R_per_game=R/G,AB_per_game=AB/G) %>%
  summarize(r=cor(R_per_game,AB_per_game)) %>%
  pull(r)

Teams%>%filter(yearID %in% 1961:2001) %>%
  mutate(W_per_game=W/G,E_per_game=E/G) %>%
  summarize(r=cor(W_per_game,E_per_game)) %>%
  pull(r)

Teams%>%filter(yearID %in% 1961:2001) %>%
  mutate(X2B_per_game=X2B/G,X3B_per_game=X3B/G) %>%
  summarize(r=cor(X2B_per_game,X3B_per_game)) %>%
  pull(r)


library(tidyverse)
library(HistData)
library(Lahman)
galton_heights <- GaltonFamilies %>%
  filter(gender=="male")%>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father,childHeight) %>%
  rename(son=childHeight)
galton_heights

# number of fathers with height 72 or 72.5 inches
sum(galton_heights$father==72)
sum(galton_heights$father==72.5)

# predicted height of a son with a 72 inch tall father
conditional_avg <- galton_heights %>%
  filter(round(father)==72) %>%
  summarize(avg=mean(son)) %>%
  pull(avg)
conditional_avg

# stratify fathers' heights to make a boxplot of son heights
galton_heights %>% mutate(father_strata=factor(round(father))) %>%
  ggplot(aes(father_strata,son)) +
  geom_boxplot() +
  geom_point()

# center of each boxplot
galton_heights %>% 
  mutate(father=round(father)) %>%
  group_by(father) %>%
  summarize(son_conditional_avg=mean(son)) %>%
  ggplot(aes(father,son_conditional_avg)) +
  geom_point()

# calculate values to plot regression line on original data
mu_x<-mean(galton_heights$father)
mu_y<-mean(galton_heights$son)
s_x<-sd(galton_heights$father)
s_y<-sd(galton_heights$son)
r<-cor(galton_heights$father,galton_heights$son)
m<-r*s_y/s_x
b<-mu_y-m*mu_x

# add regression line to plot
galton_heights %>%
  ggplot(aes(father,son)) +
  geom_point(alpha=0.5) +
  geom_abline(intercept = b, slope=m)


library(Lahman)
library(tidyverse)
library(HistData)
library(dslabs)
ds_theme_set()

galton_heights <- GaltonFamilies %>%
  filter(gender=="male")%>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father,childHeight) %>%
  rename(son=childHeight)
galton_heights


galton_heights %>%
  mutate(z_father=round((father-mean(father))/sd(father))) %>%
  filter(z_father %in% -2:2) %>%
  ggplot() +
  stat_qq(aes(sample=son)) +
  facet_wrap(~z_father)

# compute a regression line to predict the son's height from the father's height

mu_x<-mean(galton_heights$father)
mu_y<-mean(galton_heights$son)
s_x<-sd(galton_heights$father)
s_y<-sd(galton_heights$son)
r<-cor(galton_heights$father,galton_heights$son)
m_1<-r*s_y/s_x
b_1<-mu_y-m_1*mu_x

# compute a regression line to predict the father's height from the son's height
m_2<-r*s_x/s_y
b_2<-mu_x-m_2*mu_y

galton_heights%>%
  ggplot(aes(father,son))+
  geom_point()+
  geom_abline(intercept=b_1,slope=m_1,col="blue") +
  geom_abline(intercept=-b_2/m_2,slope=1/m_2,col="red")

#test
str(GaltonFamilies)

set.seed(1989)
female_heights<-GaltonFamilies%>%
  filter(gender=="female") %>%
  group_by(family)%>%
  sample_n(1)%>%
  ungroup()%>%
  select(mother,childHeight)%>%
  rename(daughter=childHeight)
  
m_mother <- mean(female_heights$mother)
m_daughter <- mean(female_heights$daughter)
s_mother <-sd(female_heights$mother)
s_daughter <-sd(female_heights$daughter)
r <- cor(female_heights$mother,female_heights$daughter)

m_mother
m_daughter
s_mother
s_daughter
r

m_d<-r*s_daughter/s_mother
m_d
b_d<-m_daughter-m_d*m_mother
b_d
r^2

d_m60<-b_d+m_d*60
d_m60

# find regression line for predicting runs from BBs
bb_slope<-Teams%>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB_per_game = BB/G,R_per_game=R/G) %>%
  lm(R_per_game ~ BB_per_game,data=.) %>%
  .$coef %>%
  .[2]
bb_slope

# compute regression line for predicting runs from singles
singles_slope <-Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(Singles_per_game = (H-HR-X2B-X3B)/G,R_per_game=R/G) %>%
  lm(R_per_game ~ Singles_per_game,data=.) %>%
  .$coef %>%
  .[2]
singles_slope

# calculate correlation between HR, BB and singles
Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(Singles=(H-HR-X2B-X3B)/G,BB=BB/G,HR=HR/G) %>%
  summarize(cor(BB,HR),cor(Singles,HR),cor(BB,Singles))

library(dslabs)
library(Lahman)
library(tidyverse)
library(HistData)


# stratify HR per game to nearest 10, filter out strata with few points
dat<-Teams%>%filter(yearID %in% 1961:2001) %>%
  mutate(HR_strata=round(HR/G,1),BB_per_game=BB/G,R_per_game=R/G)%>%
  filter(HR_strata >=0.4 & HR_strata <= 1.2)

head(dat)
        
# scatterplot for each HR stratum
dat%>%
  ggplot(aes(BB_per_game,R_per_game))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm")+
  facet_wrap(~ HR_strata)

# calculate slope of regression line after stratifying by HR
dat%>%
  group_by(HR_strata)%>%
  summarize(Slope=cor(BB_per_game,R_per_game)*sd(R_per_game)/sd(BB_per_game))

# stratify by BB
dat<-Teams%>%filter(yearID %in% 1961:2001)%>%
  mutate(BB_strata=round(BB/G,1),HR_per_game=HR/G,R_per_game=R/G)%>%
  filter(BB_strata >=2.8 & BB_strata<=3.9)
head(dat)

#scatter plot for each BB stratum
dat%>%ggplot(aes(HR_per_game,R_per_game))+
  geom_point(alpha=0.5)+
  geom_smooth(method="lm")+
  facet_wrap(~ BB_strata)

# slope of regression line after stratifying by BB
dat%>%
  group_by(BB_strata) %>%
  summarize(slope=cor(HR_per_game,R_per_game)*sd(R_per_game)/sd(HR_per_game))

help(return)

# compute RSS for any pair of beta0 and beta1 in Galton's data
library(HistData)
data("GaltonFamilies")
set.seed(1983)
galton_heights<-GaltonFamilies%>%
  filter(gender=="male") %>%
  group_by(family) %>%
  sample_n(1) %>%
  ungroup() %>%
  select(father,childHeight) %>%
  rename(son=childHeight)
head(galton_heights)

rss<-function(beta0,beta1,data){
  resid<-galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

# plot RSS as a function of beta1 when beta0=25
beta1=seq(0,1,len=nrow(galton_heights))
beta1
results<-data.frame(beta1=beta1,rss=sapply(beta1,rss,beta0=25))
results
results%>%ggplot(aes(beta1,rss)) + geom_line()+geom_line(aes(beta1,rss))



# fit regression line to predict son's height from father's height
fit<-lm(son~father,data=galton_heights)
fit

# summary statistics
summary(fit)


# Monte Carlo simulation
B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>% 
    lm(son ~ father, data = .) %>% 
    .$coef 
})
lse <- data.frame(beta_0 = lse[1,], beta_1 = lse[2,]) 

# Plot the distribution of beta_0 and beta_1
library(gridExtra)
p1 <- lse %>% ggplot(aes(beta_0)) + geom_histogram(binwidth = 5, color = "black") 
p2 <- lse %>% ggplot(aes(beta_1)) + geom_histogram(binwidth = 0.1, color = "black") 
grid.arrange(p1, p2, ncol = 2)

# summary statistics
sample_n(galton_heights,N,replace=TRUE)%>%
  lm(son~father,data=.)%>%
  summary%>%
  .$coef
lse%>%summarize(se_0=sd(beta_0),se_1=sd(beta_1)) 


lse%>%summarize(cor(beta_0,beta_1))

B <- 1000
N <- 50
lse <- replicate(B, {
  sample_n(galton_heights, N, replace = TRUE) %>%
    mutate(father = father - mean(father)) %>%
    lm(son ~ father, data = .) %>% .$coef 
})
cor(lse[1,], lse[2,]) 


# plot predictions and confidence intervals
galton_heights%>%ggplot(aes(son,father))+
  geom_point()+
  geom_smooth(method="lm")

# predict Y directly
fit<-galton_heights%>%lm(son~father,data=.)
Y_hat<-predict(fit,se.fit=TRUE)
Y_hat
names(Y_hat)

# plot best fit line
galton_heights%>%
  mutate(Y_hat=predict(lm(son~father,data=.)))%>%
  ggplot(aes(father,Y_hat))+
  geom_line()

#Assesment

rss<-function(beta0,beta1,data){
  resid<-galton_heights$son - (beta0+beta1*galton_heights$father)
  return(sum(resid^2))
}

beta1 = seq(0, 1, len=nrow(galton_heights))
results <- data.frame(beta1 = beta1,
                      rss = sapply(beta1, rss, beta0 = 36))
results %>% ggplot(aes(beta1, rss)) + geom_line() + 
  geom_line(aes(beta1, rss), col=2)


library(Lahman)
Teams%>%filter(yearID %in% 1961:2001) %>%
  mutate(R_per_game=R/G,BB_per_game=BB/G,HR_per_game=HR/G) %>%
  lm(R_per_game~BB_per_game+HR_per_game,data=.)


galton_heights %>% ggplot(aes(father, son)) +
  geom_point() +
  geom_smooth(method = "lm")  


model <- lm(son ~ father, data = galton_heights)
predictions <- predict(model, interval = c("confidence"), level = 0.95)
summary(predictions)
data <- as_tibble(predictions) %>% bind_cols(father = galton_heights$father)

ggplot(data, aes(x = father, y = fit)) +
  geom_line(color = "blue", size = 1) + 
  geom_ribbon(aes(ymin=lwr, ymax=upr), alpha=0.2) + 
  geom_point(data = galton_heights, aes(x = father, y = son))


set.seed(1989) #if you are using R 3.5 or earlier
library(HistData)
data("GaltonFamilies")
options(digits = 3)    # report 3 significant digits

female_heights <- GaltonFamilies %>%     
  filter(gender == "female") %>%     
  group_by(family) %>%     
  sample_n(1) %>%     
  ungroup() %>%     
  select(mother, childHeight) %>%     
  rename(daughter = childHeight)
head(female_heights)

fit_daughter<-lm(mother~daughter,data=female_heights)
summary(fit_daughter)

fit<-female_heights%>%lm(mother~daughter,data=.)


Y_hat<-predict(fit,se.fit=TRUE)
Y_hat
summary(Y_hat)

# fit regression line to predict mother�s height from daughter�s height
fit_mother<-lm(mother~daughter,female_heights)
fit_mother
# summary statistics
summary(fit_mother)

first_mother<-female_heights$mother[1]
first_mother

mu_x<-mean(female_heights$daughter)
mu_y<-mean(female_heights$mother)
s_x<-sd(female_heights$daughter)
s_y<-sd(female_heights$mother)
r<-cor(female_heights$daughter,female_heights$mother)
m_1<-r*s_y/s_x
b_1<-mu_y-m_1*mu_x

library(Lahman)
bat_02 <- Batting %>% filter(yearID == 2002) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
stats_02<-bat_02%>%
  group_by(playerID)%>%
  summarize(mean_singles=mean(singles),mean_bb=mean(bb))
nrow(stats_02%>%filter(mean_singles>0.2))
nrow(stats_02%>%filter(mean_bb>0.2))

bat_99_01 <- Batting %>% filter(yearID %in% 1999:2001) %>%
  mutate(pa = AB + BB, singles = (H - X2B - X3B - HR)/pa, bb = BB/pa) %>%
  filter(pa >= 100) %>%
  select(playerID, singles, bb)
bat_99_01
stats_99_01<-bat_99_01%>%
  group_by(playerID)%>%
  summarize(mean_singles=mean(singles),mean_bb=mean(bb))
stats_99_01
nrow(stats_99_01%>%filter(mean_singles>0.2))
nrow(stats_99_01%>%filter(mean_bb>0.2))

stats_99_02<-inner_join(bat_02,stats_99_01)
stats_99_02%>%
  summarize(singles_cor=cor(singles,mean_singles),bb_cor=cor(bb,mean_bb))

stats_99_02%>%
  ggplot(aes(mean_singles,singles))+
  geom_point()
stats_99_02%>%
  ggplot(aes(mean_bb,bb))+
  geom_point()

# fit regression line to predict singles from mean_singles 
fit_singles<-lm(singles~mean_singles,data=stats_99_02)
fit_singles
# summary statistics
summary(fit_singles)

# fit regression line to predict bb from mean_bb 
fit_bb<-lm(bb~mean_bb,data=stats_99_02)
fit_bb
# summary statistics
summary(fit_bb)

# stratify by HR
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)
head(dat)
class(dat)
str(dat)

# calculate slope of regression lines to predict runs by BB in different HR strata
dat %>%  
  group_by(HR) %>%
  summarize(slope = cor(BB,R)*sd(R)/sd(BB))

# use lm to get estimated slopes - lm does not work with grouped tibbles
dat %>%  
  group_by(HR) %>%
  lm(R ~ BB, data = .)%>%
  .$coef

# inspect a grouped tibble
dat %>% group_by(HR) %>% head()
dat %>% group_by(HR) %>% class()

# inspect data frame and tibble
Teams
as_tibble(Teams)

# subsetting a data frame sometimes generates vectors
class(Teams[,20])

# subsetting a tibble always generates tibbles
class(as_tibble(Teams[,20]))

# pulling a vector out of a tibble
class(as_tibble(Teams)$HR)

# access a non-existing column in a data frame or a tibble
Teams$hr
as_tibble(Teams)$HR
as_tibble(Teams)$hr

# create a tibble with complex objects
tibble(id = c(1, 2, 3), func = c(mean, median, sd))


library(dslabs)
library(Lahman)
library(tidyverse)
library(HistData)

# use do to fit a regression line to each HR stratum
dat <- Teams %>% filter(yearID %in% 1961:2001) %>%
  mutate(HR = round(HR/G, 1), 
         BB = BB/G,
         R = R/G) %>%
  select(HR, BB, R) %>%
  filter(HR >= 0.4 & HR<=1.2)
head(dat)
class(dat)
str(dat)

dat %>%
  group_by(HR) %>%
  do(fit=lm(R~ BB,dat=.))

# using do without a column name gives an error
dat %>%
  group_by(HR) %>%
  do(lm(R~BB,dat=.))

# define a function to extract slope from lm
get_slope<-function(data){
  fit<-lm(R~BB,data=data)
  data.frame(slope=fit$coefficients[2],
  se=summary(fit)$coefficient[2,2])
}
get_slope

# return the desired data frame
dat %>%
  group_by(HR) %>%
  do(get_slope(.))

# not the desired output: a column containing data frames
dat %>%
  group_by(HR) %>%
  do(slope=get_slope(.))

# data frames with multiple rows will be concatenated appropriately
get_lse <- function(data){
  fit<-lm(R~BB,data=data)
  data.frame(term=names(fit$coefficients),
             estimate=fit$coefficients,
             se=summary(fit)$coefficient[,2])
}

dat %>%
  group_by(HR) %>%
  do(get_lse(.))


# use tidy to return lm estimates and related information as a data frame
library(broom)
fit<-lm(R~BB,data=dat)
tidy(fit)

# add confidence intervals with tidy
tidy(fit, conf.int=TRUE)

# pipeline with lm, do, tidy
dat%>%
  group_by(HR) %>%
  do(tidy(lm(R~BB,dat=.),conf.int=TRUE)) %>%
  filter(term=="BB") %>%
  select(HR,estimate,conf.low,conf.high)

# make ggplots
dat%>%
  group_by(HR) %>%
  do(tidy(lm(R~BB,dat=.),conf.int=TRUE)) %>%
  filter(term=="BB") %>%
  select(HR,estimate,conf.low,conf.high) %>%
  ggplot(aes(HR,y=estimate,ymin=conf.low,ymax=conf.high)) +
  geom_errorbar() +
  geom_point()

# inspect with glance
glance(fit)

#Assesment
library(tidyverse)
library(HistData)
data("GaltonFamilies")
set.seed(1) # if you are using R 3.5 or earlier
galton <- GaltonFamilies %>%
  group_by(family, gender) %>%
  sample_n(1) %>%
  ungroup() %>% 
  gather(parent, parentHeight, father:mother) %>%
  mutate(child = ifelse(gender == "female", "daughter", "son")) %>%
  unite(pair, c("parent", "child"))

galton

galton %>%
  group_by(pair) %>%
  filter(pair=="father_daughter")

galton %>%
  group_by(pair) %>%
  filter(pair=="mother_son")

galton %>%
  group_by(pair) %>%
  summarize(rho=cor(midparentHeight,childHeight))

galton %>%
  filter(pair=="father_daughter") %>%
  summarize(rho=cor(midparentHeight,childHeight))
galton %>%
  filter(pair=="father_son") %>%
  summarize(rho=cor(midparentHeight,childHeight))
galton %>%
  filter(pair=="mother_daughter") %>%
  summarize(rho=cor(midparentHeight,childHeight))
galton %>%
  filter(pair=="mother_son") %>%
  summarize(rho=cor(midparentHeight,childHeight))

galton %>%
  group_by(pair) %>%
  summarize(rho=cor(parentHeight,childHeight))


library(broom)
# pipeline with lm, do, tidy
galton%>%
  group_by(pair) %>%
  do(tidy(lm(childHeight~parentHeight,dat=.),conf.int=TRUE))%>%
  filter(term=="parentHeight")

galton%>%
  group_by(pair) %>%
  do(tidy(lm(childHeight~parentHeight,dat=.),conf.int=TRUE))%>%
  filter(term=="parentHeight") %>%
  ggplot(aes(pair,y=estimate,ymin=conf.low,ymax=conf.high)) +
  geom_errorbar()+geom_point()
  
  dat%>%
  group_by(HR) %>%
  do(tidy(lm(R~BB,dat=.),conf.int=TRUE)) %>%
  filter(term=="BB") %>%
  select(HR,estimate,conf.low,conf.high) %>%
  ggplot(aes(HR,y=estimate,ymin=conf.low,ymax=conf.high)) +
  geom_errorbar() +
  geom_point()

?year
  
  library(dslabs)
  library(Lahman)
  library(tidyverse)
  library(HistData)
  library(broom)
  data(Teams)
  # linear regression with two variables
  fit<-Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(BB = BB/G, HR = HR/G,  R = R/G) %>%  
    lm(R ~ BB + HR, data = .)
  tidy(fit, conf.int = TRUE)

  fit<-Teams %>% 
    filter(yearID %in% 1961:2001) %>% 
    mutate(BB = BB/G, HR = HR/G,  R = R/G, 
           singles=(H-X2B-X3B-HR)/G,
           doubles=X2B/G,
           triples=X3B/G) %>%  
    lm(R ~ BB + HR + singles+doubles + triples, data = .)
  tidy(fit, conf.int = TRUE)
  
  # regression with BB, singles, doubles, triples, HR
  fit<-Teams %>%
    filter(yearID %in% 1961:2001) %>%
    mutate(BB=BB/G, HR=HR/G, R=R/G,
           singles=(H-X2B-X3B-HR)/G,
           doubles=X2B/G,
           triples=X3B/G) %>%
    lm(R~BB+HR+singles+doubles+triples,data=.)
  tidy(fit,conf.int=TRUE)
  coefs<-tidy(fit,conf.int=TRUE)
coefs    


# predict number of runs for each team in 2002 and plot
Teams %>%
  filter(yearID %in% 2002) %>%
  mutate(BB=BB/G,HR=HR/G,R=R/G,
         singles = (H-X2B-X3B-HR)/G,
         doubles=X2B/G,
         triples=X3B/G) %>%
  mutate(R_hat=predict(fit,newdata=.)) %>%
  ggplot(aes(R_hat,R,label=teamID)) +
  geom_point()+
  geom_text(nudge_x=0.1,nudge_y=-0.05,cex=2)+
  geom_abline()

pa_per_game<-Batting %>% filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game=sum(AB+BB)/max(G)) %>%
  mutate(team= teamID, pa_avg=mean(pa_per_game))
pa_per_game
Batting
class(Batting)
class(Teams)
str(Batting)
str(pa_per_game)
str(pa_per_game)
head(pa_per_game)

# average number of team plate appearances per game
pa_per_game<-Batting %>% filter(yearID == 2002) %>%
  group_by(teamID) %>%
  summarize(pa_per_game=sum(AB+BB)/max(G)) %>%
  pull(pa_per_game) %>%
  mean

# compute per-plate-appearance rates for players available in 2002 using previous data
players<- Batting %>% filter(yearID %in% 1999:2001) %>%
  group_by(playerID) %>%
  mutate(PA=BB+AB) %>%
  summarize(G=sum(PA)/pa_per_game,
            BB=sum(BB)/G, 
            HR=sum(HR)/G,
            singles=sum(H-X2B-X3B-HR)/G,
            doubles=sum(X2B)/G,
            triples=sum(X3B)/G,
            AVG=sum(H)/sum(AB),
            PA=sum(PA)) %>%
  filter(PA>=300) %>%
  select(-G) %>%
  mutate(R_hat=predict(fit, newdata=.))
head(players)  

# plot player-specific predicted runs
qplot(R_hat,data=players,geom="histogram",binwidth=0.5,color=I("black"))

# add 2002 salary of each player
players <- Salaries %>% 
  filter(yearID == 2002) %>%
  select(playerID, salary) %>%
  right_join(players, by="playerID")

# add defensive position
position_names <- c("G_p","G_c","G_1b","G_2b","G_3b","G_ss","G_lf","G_cf","G_rf")
tmp_tab <- Appearances %>% 
  filter(yearID == 2002) %>% 
  group_by(playerID) %>%
  summarize_at(position_names, sum) %>%
  ungroup()  
pos <- tmp_tab %>%
  select(position_names) %>%
  apply(., 1, which.max) 
players <- data_frame(playerID = tmp_tab$playerID, POS = position_names[pos]) %>%
  mutate(POS = str_to_upper(str_remove(POS, "G_"))) %>%
  filter(POS != "P") %>%
  right_join(players, by="playerID") %>%
  filter(!is.na(POS)  & !is.na(salary))
players

Master
str(Master)
players<-Master %>%
  select(playerID, nameFirst, nameLast, debut) %>%
  mutate(debut=as.Date(debut)) %>%
  right_join(players,by="playerID")
str(players)  

# top 10 players
players %>% select(nameFirst, nameLast, POS, salary, R_hat) %>%
  arrange(desc(R_hat)) %>%
  top_n(10)

# players with a higher metric have higher salaries
players %>% ggplot(aes(salary, R_hat, color=POS)) +
  geom_point() + 
  scale_x_log10()

library(tidyverse)
library(dslabs)

# remake plot without players that debuted after 1998
players %>% 
  filter(year(debut) < 1998) %>%
  ggplot(aes(salary, R_hat, color=POS)) +
  geom_point() + 
  scale_x_log10()

#Proposing and Solving the lp
library(reshape2)
library(lpSolve)

players <- players %>% filter(debut <= "1997-01-01" & debut > "1988-01-01")
constraint_matrix <- acast(players, POS ~ playerID, fun.aggregate = length)
npos <- nrow(constraint_matrix)
constraint_matrix <- rbind(constraint_matrix, salary = players$salary)
constraint_dir <- c(rep("==", npos), "<=")
constraint_limit <- c(rep(1, npos), 50*10^6)
lp_solution <- lp("max", players$R_hat,
                  constraint_matrix, constraint_dir, constraint_limit,
                  all.int = TRUE) 

#This algorithm chooses these 9 players:
our_team <- players %>%
  filter(lp_solution$solution == 1) %>%
  arrange(desc(R_hat))
our_team %>% select(nameFirst, nameLast, POS, salary, R_hat)

#We note that these players all have above average BB and HR rates while the same is not true for singles
?mad()
my_scale<-function(x) (x-median(x))/mad(x)
players %>% mutate(BB=my_scale(BB),
                   singles=my_scale(singles),
                   doubles=my_scale(doubles),
                   triples=my_scale(triples),
                   HR=my_scale(HR),
                   R_hat=my_scale(R_hat)) %>%
  filter(playerID %in% our_team$playerID) %>%
  select(nameFirst,nameLast, BB, singles, doubles, triples, HR, AVG, R_hat) %>%
  arrange(desc(R_hat))

str(Fielding)

#The code to create a table with player ID, their names, and their most played position:
library(Lahman)
playerInfo<- Fielding %>%
  group_by(playerID) %>%
  arrange(desc(G)) %>%
  slice(1) %>%
  ungroup() %>%
  left_join(Master, by="playerID") %>%
  select(playerID, nameFirst, nameLast, POS)
head(playerInfo)

#The code to create a table with only the ROY award winners and add their batting statistics:
ROY <- AwardsPlayers %>%
  filter(awardID == "Rookie of the Year")%>%
  left_join(playerInfo, by="playerID") %>%
  rename(rookie_year = yearID) %>%
  right_join(Batting, by="playerID") %>%
  mutate(AVG=H/AB) %>%
  filter(!POS=="P")
head(ROY)

#The code to keep only the rookie and sophomore seasons and remove players who did not play sophomore seasons:
ROY <- ROY %>%
  filter(yearID==rookie_year | yearID==rookie_year+1) %>%
  group_by(playerID) %>%
  mutate(rookie=ifelse(yearID==min(yearID),"rookie","sophomore")) %>%
  filter(n()==2)%>%
  ungroup %>%
  select(playerID, rookie_year, rookie, nameFirst, nameLast, AVG)

#The code to use the spread function to have one column for the rookie and sophomore 
#years batting averages:
  
ROY <- ROY %>% spread(rookie,AVG) %>% arrange(desc(rookie))
ROY

#The code to calculate the proportion of players who have a lower batting average their sophomore year:
mean(ROY$sophomore-ROY$rookie <=0)

#The code to do the similar analysis on all players that played the 2013 and 2014 seasons 
#and batted morethan 130 times (minimum to win Rookie of the Year):
two_years <- Batting %>%
  filter(yearID %in% 2013:2014) %>%
  group_by(playerID,yearID) %>%
  filter(sum(AB)>=130) %>%
  summarize(AVG=sum(H)/sum(AB)) %>%
  ungroup() %>%
  spread(yearID,AVG) %>%
  filter(!is.na(`2013`) & !is.na(`2014`)) %>%
  left_join(playerInfo, by="playerID") %>%
  filter(POS!="P") %>%
  select(-POS) %>%
  arrange(desc(`2013`)) %>%
  select(nameFirst, nameLast, `2013`,`2014`)
two_years

#The code to see what happens to the worst performers of 2013:
arrange(two_years,`2013`)

#The code to see  the correlation for performance in two separate years:
qplot(`2013`, `2014`, data=two_years)
summarize(two_years,cor(`2013`,`2014`))


#The code to use dslabs function rfalling_object to generate simulations 
#of dropping balls:
library(dslabs)
falling_object<-rfalling_object
head(falling_object)
str(falling_object)
#The code to draw the trajectory of the ball:
falling_object %>% 
  ggplot(aes(time,observed_distance)) +
  geom_point() +
  ylab("Distance in meters") +
  ylab("Time in seconds")

#The code to use the lm() function to estimate the coefficients:
fit <- falling_object %>%
  mutate(time_sq = time^2) %>%
  lm(observed_distance~time+time_sq, data=.)
tidy(fit)

#The code to check if the estimated parabola fits the data:
augment(fit) %>%
  ggplot() +
  geom_point(aes(time, observed_distance)) +
  geom_line(aes(time, .fitted), col = "blue")

#The code to see the summary statistic of the regression:
tidy(fit,conf.int=TRUE)


#Assesment
# regression with BB, singles, doubles, triples, HR
fit<-Teams %>%
  filter(yearID %in% 1961:2001) %>%
  mutate(BB=BB/G, HR=HR/G, R=R/G,
         singles=(H-X2B-X3B-HR)/G,
         doubles=X2B/G,
         triples=X3B/G) %>%
  lm(R~BB+HR+singles+doubles+triples,data=.)
tidy(fit,conf.int=TRUE)
coefs<-tidy(fit,conf.int=TRUE)
coefs    
fit$coefficients

TeamAB<-data.frame(Teamx=c("teamA","teamB"),BB=c(2,1),singles=c(4,6),doubles=c(1,2),
                   triples=c(0,1), HR=c(1,0))
TeamAB

TeamAB %>%
  mutate(R_hat=predict(fit,newdata=.))


library(dslabs)
library(Lahman)
library(tidyverse)
library(HistData)
library(broom)
data(Teams)

fit<-Teams%>%filter(yearID=="1971")%>%
  mutate(BB=BB/G,HR=HR/G,R=R/G)%>%
  lm(R~HR+BB,data=.)
coef<-tidy(fit,conf.int=TRUE)
coef



# define a function to extract slope from lm
get_slope<-function(data){
  fit<-lm(R~BB+HR,data=data)
  data.frame(slopeBB=fit$coefficients[2],slopeHR=fit$coefficients[3],
             se=summary(fit)$coefficient[2,2])
}
get_slope

# return the desired data frame
datos<-Teams%>%filter(yearID %in% c(1961:2018)) %>%
  mutate(BB=BB/G,HR=HR/G,R=R/G)

# return the desired data frame
datos %>%
  group_by(yearID) %>%
  do(get_slope(.))%>%
  ggplot(aes(yearID,slopeBB))+
  geom_point()+
  geom_smooth(method="lm")
  
res <- Teams %>%
  filter(yearID %in% 1961:2018) %>%
  group_by(yearID) %>%
  do(tidy(lm(R ~ BB + HR, data = .))) %>%
  ungroup() 
res
res %>%
  filter(term == "BB") %>%
  ggplot(aes(yearID, estimate)) +
  geom_point() +
  geom_smooth(method = "lm")

res1<- res %>%
  filter(term == "BB") %>% 
  select(yearID,estimate) %>%
  do(tidy(lm(estimate~yearID,data=.))) %>%
  ungroup()
res1
 

#General Assesment 

library(tidyverse)
library(broom)
library(Lahman)

Teams_small <- Teams %>% 
  filter(yearID %in% 1961:2001) %>% 
  mutate(avg_attendance = attendance/G) %>%
  mutate(win_strata=round(W/10))%>%
  group_by(win_strata)%>%
  filter(win_strata>=5 & n()>=20)%>%
  ungroup()

nrow(Teams_small)


library(tidyverse)
library(broom)
library(Lahman)
library(dslabs)

# generate the Monte Carlo simulation
N<-25
g<-1000000
sim_data<-tibble(group=rep(1:g,each=N),x=rnorm(N*g),y=rnorm(N*g))
head(sim_data)
sim_data1<-data.frame(group=rep(1:g,each=N),x=rnorm(N*g),y=rnorm(N*g))
head(sim_data1)

res<-sim_data %>%
  group_by(group) %>%
  summarize(r=cor(x,y)) %>%
  arrange(desc(r))
res
res %>% top_n(10)

# plot points from the group with maximum correlation
sim_data %>% filter(group==res$group[which.max(res$r)]) %>%
  ggplot(aes(x,y))+
  geom_point() +
  geom_smooth(method="lm")

# histogram of correlation in Monte Carlo simulations
res %>% ggplot(aes(x=r)) + geom_histogram(binwidth=0.1, color="black")

# linear regression on group with maximum correlation
sim_data %>% 
  filter(group==res$group[which.max(res$r)]) %>%
  do(tidy(lm(y ~ x, data=.)))


# simulate independent X, Y and standardize all except entry 23
set.seed(1985)
x<-rnorm(100,100,1)
y<-rnorm(100,84,1)
x[-23]<-scale(x[-23])
y[-23]<-scale(y[-23])

# plot shows the outlier
qplot(x,y,alpha=0.5)

# outlier makes it appear there is correlation
cor(x,y)
cor(x[-23],y[-23])

# use rank instead
qplot(rank(x), rank(y))
cor(rank(x), rank(y))

# Spearman correlation with cor function
cor(x,y,method="spearman")


# cause and effect reversal using son heights to predict father heights
library(HistData)
data("GaltonFamilies")
GaltonFamilies %>% 
  filter(children==1 & gender=="male") %>%
  select(father,childHeight) %>%
  rename(son=childHeight) %>%
  do(tidy(lm(father~son,data=.)))
  

library(dslabs)
library(tidyverse)
library(broom)
data(admissions)  
admissions

# percent men and women accepted
admissions %>%
  group_by(gender) %>%
  summarize(percentage=round(sum(admitted*applicants)/sum(applicants),1))

# test whether gender and admission are independent
admissions %>%
  group_by(gender) %>%
  summarize(total_admitted=round(sum(admitted/100*applicants)),
            not_admitted=sum(applicants)-sum(total_admitted)) %>%
  select(-gender) %>%
  do(tidy(chisq.test(.)))

# percent admissions by major
admissions%>%
  select(major,gender,admitted) %>%
  spread(gender,admitted) %>%
  mutate(women_minus_men=women-men)

library(dslabs)
library(tidyverse)
library(broom)
data(admissions)  
admissions
# plot total percent admitted to major versus percent women applicants
admissions %>%
  group_by(major) %>%
  summarize(major_selectivity=sum(admitted*applicants)/sum(applicants),
  percent_women_applicants=sum(applicants*(gender=="women"))/
    sum(applicants)*100) %>%
  ggplot(aes(major_selectivity,percent_women_applicants, label=major))+
  geom_text()

# plot number of applicants admitted and not
admissions %>%
  mutate(yes=round(admitted/100*applicants), no=applicants-yes) %>%
  select(-applicants,-admitted) %>%
  gather(admission, number_of_students, -c("major","gender")) %>%
  ggplot(aes(gender,number_of_students,fill=admission)) +
  geom_bar(stat="identity", position="stack")+
  facet_wrap(.~ major)

admissions %>%
  mutate(percent_admitted=admitted*applicants/sum(applicants)) %>%
  ggplot(aes(gender,y=percent_admitted, fill = major)) +
  geom_bar(stat="identity", position="stack")

# condition on major and then look at differences
admissions %>% ggplot(aes(major,admitted,col=gender,size=applicants)) +
  geom_point()

# average difference by major
admissions %>%
  group_by(gender) %>% summarize(average=mean(admitted))
