library(dslabs)
installed.packages()
library(tidyverse)

a<-1
b<-1
c<--1
ls()
  
(-b+sqrt(b^2-4*a*c))/2*a
(-b-sqrt(b^2-4*a*c))/2*a

ls()
ls

help(!)

libraries(dslabs)
library(dslabs)
data(murders)
class(murders)
str(murders)
murders$population
head(murders)
names(murders)
pop<-murders$population
length(pop)
class(pop)
class(murders$state)
z<-3==2
class(z)
z
class(murders$region)
levels(murders$region)

str(murders)

a<-2
b<--1
c<--4
x<-(-b+sqrt(b^2-4*a*c))/2*a
y<-(-b-sqrt(b^2-4*a*c))/2*a

print(x)
print(y)

help(log)
log4(1024)
log(1024,4)

(1+sqrt((-1)^2-4*2*(-4)))/(2*2)
(1-sqrt((-1)^2-4*2*(-4)))/(2*2)

library(dslabs)
data(movielens)
str(movielens)
class(movielens)
class(title)
class(movielens$title)
class(movielens$genre)
levels(movielens$genres)
nlevels(movielens$genres)


codes<-c(380,124,318)
codes
country<-c("italy","canada","egypt")
country
codes<-c(italy=380,canada=124,egypt=318)
codes

codes<-c(380,124,318)
country<-c("italy","canada","egypt")
names(codes)<-country
codes
codes[2]
codes[1:2]
codes[c(1,3)]

library(dslabs)
data(murders)
sort(murders$total)

x <- c(31, 4, 15, 92, 65)
x
sort(x)    # puts elements in order

index <- order(x)    # returns index that will put x in order
index
x[index]    # rearranging by this index puts elements in order
order(x)

murders$state[1:10]
murders$abb[1:10]
index<-order(murders$total)
murders$abb[index]
max(murders$total)
i_max<-which.max(murders$total)
i_max
murders$state[i_max]

rank(murders$total)

x <- c(31, 4, 15, 92, 31)
x
rank(x)    # returns ranks (smallest to largest)

x <- c(31, 4, 15, 92, 65)
x
rank(x) 

help("!")

library(dslabs)
data(murders)
murders$state[which.max(murders$population)]

murder_rate<-murders$total/murders$population*100000
murder_rate
murders$state[order(murder_rate,decreasing=TRUE)]

x <- c(2, 43, 27, 96, 18)
x
y<-sort(x)
print(y)

helP(!)

library(dplyr)
library(dslabs)
data(murders)

avg<-function(x){
  s<-sum(x)
  n<-length(x)
}
x<-c(3,6,8,10,4,2,16)
avg(x)
# a simple scatterplot of total murders versus population
x <- murders$population /10^6
y <- murders$total
plot(x, y)
# a histogram of murder rates
install.packages("dplyr")
library(dplyr)
library(dslabs)
data(murders)
murders<-mutate(murders,rate=total/population*100000)
hist(murders$rate)

library(dslabs)
data(heights)
options(digits = 3)
class(heights)

str(heights)
mean(heights$height)
ind<-heights$height>mean(heights$height)
ind
sum(ind)


library(dslabs)
data(heights)
ind1<-heights$height>mean(heights$height)
ind1
ind2<-heights$sex=="Female"
ind2
ind3<-ind1&ind2
ind3
sum(ind3)

mean(ind2)

ind4<-which.min(heights$height)
ind4
heights$height[ind4]
heights$sex[ind4]

ind5<-match(min(heights$height),heights$height)
ind5
heights$sex[ind5]
max(heights$height)
min(heights$height)
x<-min(heights$height):max(heights$height)
x
x<-seq(min(heights$height),max(heights$height))
x
y<-match(heights$height,x)
y
z<-x%in%heights$height
z
w<-which(x%in%heights$height)
heights$height[w]
x1<-match(x,heights$hegiht)
x1

m<-heights$height%in%x
m
sum(!m)

sum((!x%in%heights$height))

heights<-mutate(heights,ht_cm=height*2.54)
library(dplyr)
heights<-mutate(heights,ht_cm=height*2.54)
heights
mean(heights$ht_cm)
heights2<-filter(heights,sex=="Female")
heights2
class(heights2)
length(heights2)

str(heights2)
mean(heights2$ht_cm)

library(dslabs)
data(olive)
head(olive)
str(olive)

plot(olive$palmitic,olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(palmitic~region,data=olive)

avg<-function(x){
  s<-sum(x)
  n<-length(x)
  s/n
  }

x<-c(8,23,11,26,78,99)
avg(x)
mean(x)
identical(avg(x),mean(x))

compute_s_n<-function(n){
  x<-1:n
  sum(x)
}

for(i in 1:5){
  print(i)
}

m<-25
s_n<-vector(length=m)
for(n in 1:m){
  s_n[n]<-compute_s_n(n)
}

n<-1:m
plot(n,s_n)

head(data.frame(s_n=s_n,formula=n*(n+1)/2))
head(data.frame(n=n,s_n=s_n,formula=n*(n+1)/2))

plot(n,s_n)
lines(n,n*(n+1)/2)

library(dplyr)
library(dslabs)
data(murders)

sss<-nchar(murders$state)
sss
head(sss)
help(vector)


