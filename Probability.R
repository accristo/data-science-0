library(gtools)
number<-"Three"
suit<-"Hearts"
paste(number,suit)

letters[1:5]
as.character(1:5)
paste(letters[1:5],as.character(1:5))

expand.grid(pants=c("blue", "black"), shirts=c("white", "gray", "plaid"))

suits <- c("Diamonds", "Clubs", "Hearts", "Spades")
numbers <- c("Ace", "Deuce", "Three", "Four", "Five", "Six", "Seven", 
             "Eight", "Nine", "Ten", "Jack", "Queen", "King")
deck<-expand.grid(suit=suits,number=numbers)
deck
deck<-paste(deck$number,deck$suit)
deck


# probability of drawing a king
kings<-paste("King",suits)
kings
deck%in%kings
mean(deck%in%kings)


permutations(5,2)      
all_phone_numbers <- permutations(10, 7, v = 0:9)
all_phone_numbers
n <- nrow(all_phone_numbers)
n
index <- sample(n, 5)
index
all_phone_numbers[index,]

permutations(3,2)
combinations(3,2)

hands <- permutations(52,2, v = deck)
hands
first_card <- hands[,1]
first_card
second_card <- hands[,2]
second_card
sum(first_card%in%kings)
sum(first_card%in%kings&second_card%in%kings)/sum(first_card%in%kings)
aces <- paste("Ace", suits)
aces
facecard <- c("King", "Queen", "Jack", "Ten")
facecard <- expand.grid(number = facecard, suit = suits)
facecard
facecard <- paste(facecard$number, facecard$suit)
facecard
hands <- combinations(52, 2, v=deck) # all possible hands
hands
mean(hands[,1] %in% aces & hands[,2] %in% facecard)
mean((hands[,1] %in% aces & hands[,2] %in% facecard)|
       (hands[,2] %in% aces & hands[,1] %in% facecard))

hand <- sample(deck, 2)
hand
B <- 10000
results <- replicate(B, {
  hand <- sample(deck, 2)
  (hand[1] %in% aces & hand[2] %in% facecard) | 
    (hand[2] %in% aces & hand[1] %in% facecard)
})
mean(results)

library(gtools)

n<-50
bdays<-sample(1:365,n,replace=TRUE)
bdays
duplicated(bdays)
any(duplicated(bdays))

B<-10000
results<-replicate(B,{
  bdays<-sample(1:365,n,replace=TRUE)
  any(duplicated(bdays))
})
mean(results)
results

compute_prob<-function(n,B=10000){
  same_day<-replicate(B,{
    bdays<-sample(1:365,n,replace=TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
compute_prob
n<-seq(1,60)

x<-1:10
sqrt(x)
y<-1:10
x*y
x<-1:10
sapply(x,sqrt)
prob<-sapply(n,compute_prob)
prob
plot(n,prob)

exact_prob<-function(n){
  prob_unique<-seq(365,365-n+1)/365
  1-prod(prob_unique)
}
exact_prob
eprob<-sapply(n,exact_prob)
eprob
plot(n,prob)
lines(n,eprob,col="red")

B <- 10^seq(1, 5, len = 100)    # defines vector of many B values
B
compute_prob <- function(B, n = 22){    # function to run Monte Carlo simulation with each B
  same_day <- replicate(B, {
    bdays <- sample(1:365, n, replace = TRUE)
    any(duplicated(bdays))
  })
  mean(same_day)
}
prob <- sapply(B, compute_prob) 
prob 
plot(log10(B), prob, type = "l") 

?sample
zzz<-c("lose","lose","lose","lose")
duplicated(zzz)
any(duplicated(zzz))
any(zzz %in% "win")

doors<-as.character(1:3)
doors
prize<-sample(c("car", "goat", "goat"))
prize
prize_door<-doors[prize=="car"]
prize_door
prize
my_pick<-sample(doors,1)
my_pick
!doors
show<-sample(doors[!doors%in%c(prize_door, my_pick)],1)
show
show<-doors[!doors%in%c(prize_door, my_pick)]
show


B<-10000
stick<-replicate(B, {
  doors<-as.character(1:3)
  prize<-sample(c("car", "goat", "goat"))
  prize_door<-doors[prize=="car"]
  my_pick<-sample(doors,1)
  show<-sample(doors[!doors%in%c(prize_door, my_pick)],1)
  stick<-my_pick
  stick==prize_door
  })
stick
mean(stick)

!doors%in%c(prize_door, my_pick)

B<-10000
switch<-replicate(B, {
  doors<-as.character(1:3)
  prize<-sample(c("car", "goat", "goat"))
  prize_door<-doors[prize=="car"]
  my_pick<-sample(doors,1)
  show<-sample(doors[!doors%in%c(prize_door, my_pick)],1)
  switch<-doors[!doors%in%c(my_pick,show)]
  switch==prize_door
})
mean(switch)
?list
?rep
beads <- rep( c("red", "blue"), times = c(2,3))
beads
?exand.grid
?expand.grid
result<-sample(c(0,1),6,replace=TRUE)
result
sum(result)

p <- seq(0.5, 0.95, 0.025)
p

N<-seq(1,25,2)
N

library(gtools)
library(tidyverse)

combinations(8,3)
permutations(8,3)
corredores<-1:8
podio<-rep(corredores,c(1,2,3))
podio<-c(1,2,3)
list(podio)
expand.grid(list(corredores),list(podio))
corredores
list(corredores)

permutations(3,3)
3/8
(1/8)*(1/7)*(1/6)

jamaica_podio<-permutations(3,3)%in%permutations(8,3)
jamaica_podio
sum(jamaica_podio)

carrera<-sample(8,3)
carrera
sum(carrera)

B=10000
podio<-replicate(B, {
  carrera<-sample(8,3)
  sum(carrera)
})
podio
filter(podio=6)

podiodf<-data.frame(corrida=1:10000,result=podio)
podio
podio_jamaica<-filter(podiodf,result==6)
podio_jamaica
length(podio_jamaica)
levels(podio_jamaica)
178/10000
podio_jamaica_ind<-podio$result


nrow(podio_jamaica)

set.seed(1)
B=10000
runners<-c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", 
               "Netherlands", "France", "South Africa")
podium<-replicate(B, {
  podium1<-sample(runners,3)
  all(podium1 %in% c("Jamaica", "Jamaica", "Jamaica"))
})
sum(podium)/B

combinations(6,1)
combinations(6,2)
combinations(2,1)
length(combinations(6,1))
length(combinations(6,2))
nrow(combinations(6,2))
nrow(combinations(6,1))*nrow(combinations(6,2))*nrow(combinations(2,1))
nrow(combinations(6,1))*nrow(combinations(6,2))*nrow(combinations(3,1))
nrow(combinations(6,1))*nrow(combinations(6,3))*nrow(combinations(3,1))

meal_combinations<-function(n_entrees){
  nrow(combinations(n_entrees,1))*nrow(combinations(6,2))*nrow(combinations(3,1))
}
n_entrees<-1:12
sapply(n_entrees,meal_combinations)


meal_combinations1<-function(n_sides){
  nrow(combinations(n_sides,1))*nrow(combinations(6,3))*nrow(combinations(3,1))
}
n_sides<-2:12
sapply(n_sides,meal_combinations1)


library(tidyverse)
head(esoph)
length(esoph)
data(esoph)
levels(esoph)
sum(esoph$ncontrols)
str(esoph)
esoph$ncontrols
esoph$ncases
sum(esoph$ncases)
all_cases<-sum(esoph$ncases)
all_controls<-sum(esoph$ncontrols)
esoph$alcgp
prob_hac_ncases<-esoph%>%filter(alcgp=="120+")%>%.$ncases
sum(prob_hac_ncases)
prob_hac_ncontrols<-esoph%>%filter(alcgp=="120+")%>%.$ncontrols
sum(prob_hac_ncontrols)
sum(prob_hac_ncases)/sum(prob_hac_ncontrols)


library(tidyverse)
esoph
esoph%>%filter(alcgp)
esoph%>%filter(alcgp=="120+")%>%.$ncontrols
esoph%>%filter(alcgp=="120+")%>%.$ncases
sum(esoph%>%filter(alcgp=="120+")%>%.$ncases)
sum(esoph%>%filter(alcgp=="120+")%>%.$ncontrols)
mean(esoph%>%filter(alcgp=="120+")%>%.$ncases)
mean(esoph%>%filter(alcgp=="120+")%>%.$ncontrols)
mean(esoph%>%filter(alcgp=="120+")%>%.$ncases)/mean(esoph%>%
                                                      filter(alcgp=="120+")%>%.$ncontrols)
str(esoph)
esoph%>%.$ncases
esoph%>%.$ncontrols
sum(esoph%>%.$ncases)
sum(esoph%>%filter(alcgp=="120+")%>%.$ncases)
sum(esoph%>%filter(alcgp=="120+")%>%.$ncases)/sum(esoph%>%.$ncases)

sum(esoph%>%.$ncases)
sum(esoph%>%filter(alcgp=="120+")%>%.$ncases)/(sum(esoph%>%filter(alcgp=="120+")%>%.$ncases)+
                                                 sum(esoph%>%filter(alcgp=="120+")%>%.$ncontrols))
sum(esoph%>%filter(alcgp=="0-39g/day")%>%.$ncases)/(sum(esoph%>%filter(alcgp=="0-39g/day")%>%.$ncases)+
                                                 sum(esoph%>%filter(alcgp=="0-39g/day")%>%.$ncontrols))
esoph%>%filter(alcgp=="0-39g/day")%>%.$ncases
sum(esoph%>%filter(tobgp %in% c("10-19", "20-29", "30+"))%>%.$ncases)/
  sum((esoph%>%.$ncases))
sum(esoph%>%filter(tobgp %in% c("10-19", "20-29", "30+"))%>%.$ncontrol)/
  sum((esoph%>%.$ncontrol))
sum(esoph%>%filter(alcgp == c("120+"))%>%.$ncases)/
  sum((esoph%>%.$ncases))
sum(esoph%>%filter(tobgp == c("30+"))%>%.$ncases)/
  sum((esoph%>%.$ncases))
sum(esoph%>%filter(alcgp == c("120+"))%>%.$ncases)/
  sum((esoph%>%.$ncases))*sum(esoph%>%filter(tobgp == c("30+"))%>%.$ncases)/
  sum((esoph%>%.$ncases))
sum(esoph%>%filter(tobgp == "30+" & alcgp=="120+")%>%.$ncases)/
  sum((esoph%>%.$ncases))
(sum(esoph%>%filter(tobgp == c("30+"))%>%.$ncases)+sum(esoph%>%filter(alcgp == "120+")%>%.$ncases))/
  sum((esoph%>%.$ncases))
  
sum(esoph%>%filter(tobgp == c("30+"))%>%.$ncases)
sum(esoph%>%filter(alcgp == "120+")%>%.$ncases)
sum(esoph%>%.$ncases)
sum(esoph%>%filter(alcgp == c("120+"))%>%.$ncontrols)/
  sum((esoph%>%.$ncontrols))
sum(esoph%>%filter(tobgp == "30+")%>%.$ncontrols)/
  sum((esoph%>%.$ncontrols))
a<-sum(esoph%>%filter(alcgp == "120+")%>%.$ncases)/
  (sum(esoph%>%filter(alcgp == "120+")%>%.$ncases)+
  sum(esoph%>%filter(alcgp == "120+")%>%.$ncontrols))
b<-sum(esoph%>%filter(alcgp == "120+")%>%.$ncontrols)/
  (sum(esoph%>%filter(alcgp == "120+")%>%.$ncases)+
     sum(esoph%>%filter(alcgp == "120+")%>%.$ncontrols))
a/b

sum(esoph%>%filter(tobgp == "30+" & alcgp=="120+")%>%.$ncontrols)/
  sum((esoph%>%.$ncontrols))

sum(esoph%>%filter(tobgp == "30+")%>%.$ncontrols)
sum(esoph%>%filter(alcgp=="120+")%>%.$ncontrols)
sum(esoph%>%.$ncontrols)
0.068+0.084-0.013
0.33/0.139
0.225/0.068
sum(esoph%>%filter(alcgp == "120+")%>%.$ncases)/(sum(esoph%>%.$ncases)+sum(esoph%>%.$ncontrols))
sum(esoph%>%filter(alcgp == "120+")%>%.$ncontrols)/(sum(esoph%>%.$ncases)+sum(esoph%>%.$ncontrols))
sum(esoph%>%filter(alcgp == "120+")%>%.$ncases)
sum(esoph%>%.$ncases)


library(tidyverse)
sum(esoph%>%filter(alcgp == "120+")%>%.$ncases)/sum(esoph%>%.$ncases)
sum(esoph%>%filter(alcgp == "120+")%>%.$ncontrols)/sum(esoph%>%.$ncontrols)
(sum(esoph%>%filter(alcgp == "120+")%>%.$ncases)/sum(esoph%>%.$ncases))/
  (sum(esoph%>%filter(alcgp == "120+")%>%.$ncontrols)/sum(esoph%>%.$ncontrols))


library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
x
F <- function(a) mean(x <= a)
1 - F(70)    # probability of male taller than 70 inches
y<-50:80
y
z<-sapply(y,F)
plot(y,z)


library(tidyverse)
library(dslabs)
data(heights)
x <- heights %>% filter(sex=="Male") %>% pull(height)
1 - pnorm(70.5, mean(x), sd(x))
table(x)
plot(prop.table(table(x)), xlab = "a = Height in inches", ylab = "Pr(x = a)")
# probabilities in actual data over length 1 ranges containing an integer
mean(x <= 68.5) - mean(x <= 67.5)
mean(x <= 69.5) - mean(x <= 68.5)
mean(x <= 70.5) - mean(x <= 69.5)
pnorm(68.5, mean(x), sd(x)) - pnorm(67.5, mean(x), sd(x))
pnorm(69.5, mean(x), sd(x)) - pnorm(68.5, mean(x), sd(x))
pnorm(70.5, mean(x), sd(x)) - pnorm(69.5, mean(x), sd(x))
mean(x <= 70.9) - mean(x <= 70.1)
pnorm(70.9, mean(x), sd(x)) - pnorm(70.1, mean(x), sd(x))


x<-seq(-4,4,length=100)
data.frame(x,f=dnorm(x))
data.frame(x,f=dnorm(x))%>%ggplot(aes(x,f))+geom_line()

data(heights)
x<-heights%>%filter(sex=="Male")%>%pull(height)
n<-length(x)
avg<-mean(x)
s<-sd(x)
simulated_heights=rnorm(n,avg,s)
data.frame(simulated_heights=simulated_heights)%>%ggplot(aes(simulated_heights))+
  geom_histogram(line="Black",binwidth = 2)

B<-10000
tallest<-replicate(B, {
  simulated_data<-rnorm(800,avg,s)
  max(simulated_data)
})
mean(tallest>=7*12)

x<-seq(-4,4,length.out=100)
x
x<-seq(-4,4,length=100)
x
data.frame(x,f=dnorm(x))%>%ggplot(aes(x,f))+geom_line()
?qnorm
qnorm(0.99)

set.seed(16)
act_scores<-rnorm(10000,20.9,5.7)
avg_act_scores<-mean(act_scores)
sd_act_scores<-sd(act_scores)
sd(act_scores)
str(act_scores)
act_sc<-data.frame(act_scores)
head(act_sc)
perfect_score<-(act_sc>=36)
sum(perfect_score)
1-pnorm(30,avg_act_scores,sd_act_scores)
pnorm(10,avg_act_scores,sd_act_scores)

x<-1:36
f_x<-dnorm(x,20.9,5.7)
plot(x,f_x)

z_scores<-scale(act_scores)
avg_z_scores<-mean(z_scores)
sd_z_scores<-sd(z_scores)
1-pnorm(2,avg_z_scores,sd_z_scores)
z_scores<-rnorm(10000,0,1)
avg_z_scores<-mean(z_scores)
sd_z_scores<-sd(z_scores)
1-pnorm(2,avg_z_scores,sd_z_scores)
qnorm(avg_act_scores+2*sd_act_scores,avg_act_scores,sd_act_scores)
qnorm(0.975,avg_act_scores,sd_act_scores)
avg_act_scores+2*sd_act_scores

prob_act_scores<-function(x) {
  pnorm(x,avg_act_scores,sd_act_scores)
}
x<-1:36
prob_act_scores(x)
qnorm(0.95,20.9,5.7)

p<-seq(0.01,0.99,0.01)
quantile(act_scores,p)
sample_quantiles<-quantile(act_scores,p)

theoretical_quantiles<-qnorm(p,20.9,5.7)
plot(theoretical_quantiles,sample_quantiles)


color<-rep(c("Black","Red","Green"), c(18,18,2))
color
n<-1000
X <- sample(ifelse(color == "Red", -1, 1), n, replace = TRUE)
X

# sampling model 2: define urn inside sample function by noting probabilities
x<-sample(c(-1,1),n,replace=TRUE,prob=c(9/19,10/19))
x[1:10]
S<-sum(x)
S

n<-1000
B<-10000
S<-replicate(B, {
  X<-sample(c(-1,1),n,replace=TRUE,prob=c(9/19,10/19))
  sum(X)
})
mean(S<0)
S
library(tidyverse)
s<-seq(min(S),max(S),length=100)
normal_density<-data.frame(s=s,f=dnorm(s,mean(S),sd(S)))
normal_density <- data.frame(s = s, f = dnorm(s, mean(S), sd(S))) # generate normal density for S
data.frame(S=S)%>%
  ggplot(aes(S,..density..))+
  geom_histogram(color="black",binwidth=10)+
  ylab("Probability")+
  geom_line(data=normal_density,mapping=aes(s,f),color="blue")
normal_density

d<-c(1,-0.25)*c(0.2,0.8)
d
sqrt(44)*abs(-0.25-1)*sqrt(0.2*0.8)    
se1<-sqrt(44)*abs(-0.25-1)*sqrt(0.2*0.8)
1-pnorm(8,mean=0,sd=se1)

set.seed(1)
B<-10000
n<-44
eight_grade_students<-replicate(B, {
  sum(sample(c(1,-0.25),n,replace=TRUE,prob=c(0.2,0.8)))
})
mean(eight_grade_students>8)

e<-c(1,0)*c(0.25,0.75)
sum(e)
sum(e)*44



p<-seq(0.25,0.95,0.05)
grade_p_students<-function(p){
  B<-10000
  n<-44
  result<-replicate(B, {
    sum(sample(c(1,0),n,replace=TRUE,prob=c(p,1-p)))
  })
  mean(result>35)
}
xxx<-sapply(p,grade_p_students)
data.frame(p=p,grade_35=xxx)


sum(c(6,-1)*c(5/38,1-5/38))
abs(-1-6)*sqrt(5/38*(1-5/38))

n<-500
roulette<-sample(c(6,-1),n,replace=TRUE,prob=c(5/38,1-5/38))
roulette
mean(roulette)

avg_expected_roulette<-6*5/38-1*(1-5/38)
avg_expected_roulette
se_avg_expected_roulette<-(1/sqrt(n))*abs(-1-6)*sqrt(5/38*(1-5/38))
sum_expected_roulette<-n*avg_expected_roulette
sum_expected_roulette
se_sum_expected_roulette<-sqrt(n)*abs(-1-6)*sqrt(5/38*(1-5/38))
se_sum_expected_roulette
pnorm(0,avg_expected_roulette,se_avg_expected_roulette)

n<-1000
#loss per forclosure
loss_per_forclosure<-200000
p<-0.02
defaults<-sample(c(1,0),n,replace=TRUE,prob=c(p,1-p))
sum(defaults*loss_per_forclosure)

#monte carlo simulation
B<-10000
losses<-replicate(B, {
  defaults<-sample(c(1,0),n,replace=TRUE,prob=c(p,1-p))
  sum(defaults*loss_per_forclosure)
})
mean(losses)

#plotting expected losses
library(tidyverse)
library(dslabs)
library(gtools)
data.frame(losses_in_millions=losses/10^6)%>%ggplot(aes(losses_in_millions))+
  geom_histogram(binwidth=0.6,col="black")

#explected value and standard error of the sum of 1000 loans
n*(p*loss_per_forclosure+(1-p)*0) #expected value
sqrt(n)*abs(-0-loss_per_forclosure)*sqrt(p*(1-p)) #standard error

#calculating interest rate with expected value 0
x<-loss_per_forclosure*p/(1-p)
x
#interest rate
x/180000

#Code: Calculating interest rate for 1% probability of losing money
l <- loss_per_forclosure
z <- qnorm(0.01)
l
z
x <- -l*( n*p - z*sqrt(n*p*(1-p)))/ ( n*(1-p) + z*sqrt(n*p*(1-p)))
x
x/180000    # interest rate
loss_per_forclosure*p + x*(1-p)    # expected value of the profit per loan
n*(loss_per_forclosure*p + x*(1-p)) # expected value of the profit over n loans

#Code: Monte Carlo simulation for 1% probability of losing money
B <- 100000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_forclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)    # expected value of the profit over n loans
mean(profit<0)    # probability of losing money


#Code: Expected value with higher default rate and interest rate
p<-0.04
loss_per_foreclosure<--200000
r<-0.05
x<-r*180000
loss_per_forclosure*p+x*(1-p)

#Code: Calculating number of loans for desired probability of losing money
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-l)^2*p*(1-p))/(l*p + x*(1-p))^2)
n    # number of loans required
n*(loss_per_foreclosure*p + x * (1-p))    # expected profit over n loans

#Code: Monte Carlo simulation with known default probability
B <- 10000
p <- 0.04
x <- 0.05 * 180000
profit <- replicate(B, {
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-p, p), replace = TRUE) 
  sum(draws)
})
mean(profit)


#Code: Monte Carlo simulation with unknown default probability
p <- 0.04
x <- 0.05*180000
profit <- replicate(B, {
  new_p <- 0.04 + sample(seq(-0.01, 0.01, length = 100), 1)
  draws <- sample( c(x, loss_per_foreclosure), n, 
                   prob=c(1-new_p, new_p), replace = TRUE)
  sum(draws)
})
mean(profit)    # expected profit
mean(profit < 0)    # probability of losing money
mean(profit < -10000000)    # probability of losing over $10 million

data.frame(profit_in_millions=profit/10^6)%>%ggplot(aes(profit_in_millions))+
  geom_histogram(binwidth=5,col="black")

data.frame(losses_in_millions=losses/10^6)%>%ggplot(aes(losses_in_millions))+
  geom_histogram(binwidth=0.6,col="black")

X_hat<-0.48
se<-sqrt(X_hat*(1-X_hat)/25)
pnorm(0.01/se)-pnorm(-0.01/se)
