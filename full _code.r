dat<-read.csv("femaleMiceWeights.csv")



### possiamo scaricare i dati in questo modo da github ### ----

library(downloader) 

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleMiceWeights.csv"
filename <- "femaleMiceWeights.csv" 
download(url, destfile=filename)


dat
colnames(dat)


dat[12,2]


weight<-dat$Bodyweight
weight[11]
#or
dat$Bodyweight[11]

length(dat$Bodyweight)
hfdiet<-dat[dat$Diet== "hf",]
hfdiet
mean(hfdiet$Bodyweight)
#or if you check manually the dataframe

mean(weight [13:24])


?sample()
?set.seed()

set.seed(1)
try<-dat$Bodyweight [13:24]
try
sample(try,size = 1)
#or
set.seed(1)
i <- sample(13:24, 1)
dat$Bodyweight[i]




#################################################
install.packages("dplyr")
library(dplyr)

dat<-read.csv("femaleMiceWeights.csv")
View(dat)

controls<-filter (dat, Diet=="chow")
View(controls)
#for view one part of dataset
select(controls, Bodyweight)
#create a numeric vector
#we just gave a name from previous function
controls<-select(controls, Bodyweight)
unlist(controls) # for semplify the creation of a vector

#now we introduce the pipe function, it's like to say "then", you can link more functions
#it's just an example,doesn't matter if is not correct, it's just to understand the pipe function
controls<-filter(dat,Diet=="chow") %>%
  select(Bodyweight)%>%
  unlist
View(controls)




#############################################################

install.packages("downloader")
library(downloader)
url="https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/msleep_ggplot2.csv"
filename <- basename(url)
download(url,filename)


msleep<-read.csv("msleep_ggplot2.csv")
class(msleep)
primates<-filter(msleep, order=="Primates")
View(primates)
nrow(primates)
class(primates)


sleep_prim<-filter(msleep, order=="Primates")%>%
  select(sleep_total)

  
sleep_prim1<-filter(msleep, order=="Primates")%>%
  select(sleep_total)%>%
  unlist()

View(sleep_prim1)
mean(sleep_prim1)  
  

sleep_prim2<-filter(msleep, order=="Primates")%>%
  summarise(sleep_total)%>%
  unlist()

View(sleep_prim2)
mean(sleep_prim2)

########################################
#EDA EXPLORATIVE DATA ANALYSIS ----



install.packages("UsingR")
library(UsingR)
x=father.son$fheight

#we can select only 20 random samples
#,1 is numbers of decimal after ","

round(sample(x,20),1)

#now we can create an Histogram
hist(x)

#Given the above histogram, how many people are between the ages of 35 and 45?
sum(age>=35 & age<45)


#we can specify the bins and add better labels
#floor works in this way= 2.4=2
#ceiling=2.4=3

bins <- seq(floor(min(x)),ceiling(max(x)),0.5)
hist(x,breaks=bins,xlab="Height",main="Adult men heights")
#or
hist(x,breaks=seq(floor(min(x)),ceiling(max(x))),main="",xlab="Height")


#Empirical cumulative density function (CDF)----

myCDF <- ecdf(x)
##We will evaluate the function at these values:
xs<-seq(floor(min(x)),ceiling(max(x)),0.1) 
### and then plot them:
plot(xs,myCDF(xs),type="l",xlab="x=Height",ylab="F(x)")
dev.off()


#Normal approximation and QQplot----

popsd <- function(x) sqrt(mean((x-mean(x))^2)) #normal distribution
popsd(x)
mu<- mean(x)
mean(x>70)
#0.2059369= 20%


#The pnorm() is a built-in R function that returns the value of the cumulative density function (cdf) 
#of the normal distribution given a certain random variable x, and a population mean Î¼, and the population standard deviation Ï.

#prnorm(x) give us the area under the curve that have values from inf to x

1-pnorm(70,mean(x),sd(x))

#0.1997182= circa 20% this is a prediction
#a good approximation (near mean(x>70))


#pnorm() have one more argument called lower.tail, if we use lower.tail=FALSE
#pnorm() give us the values from x to inf


#we can continue this idea and we can try with
mean(x<70)
pnorm(70,mean(x),sd(x))

#another good approximation, we can try with other numbers but
#if we want to do this sistematically we can use a plot called QQ-plot


#The following R code generates the quantiles for a standard Normal distribution from 0.01 to 0.99 
#by increments of 0.01:
perc_seq <- seq(0.01,0.99,0.01)
quant_s <- quantile(x,ps) #we can see the values of our example

#And then we can compute the same percentiles
#for the normal distribution (popsd(x)).
normalquant_s <- qnorm(perc_seq,mean(x),popsd(x))

#we can do a plot now
plot(normalquant_s,quant_s,xlab="Normal percentiles",ylab="Height percentiles")
abline(0,1) ##identity line


#There's a function that make this much easier, which is the qqnorm function.
#That's specific to the normal distribution.
qqnorm(x)
qqline(x) 

###########################################
#excercise

load("skew.Rdata")
dim(dat) #Retrieve or set the dimension of an object


#Using QQ-plots, compare the distribution of each column of the matrix 
#to a normal. That is, use qqnorm() on each column. To accomplish this quickly,
#ywe can use the following line of code to set up a grid for 3x3=9 plots.
#(mfrow means we want a multifigure grid filled in row-by-row.Another choice is mfcol.)

par(mfrow = c(3,3))


#now we can use a for loop

for (i in 1:9) {
  y<-dat[,i]
  qqnorm(y,xlab="quantiles", main=paste0("Col No=",i))
  qqline(y)
}
#qqnorm(dat[,9]) single code 

#plotting multiple histograms
par(mfrow = c(3,3))
#Then you can use a for loop, to loop through the columns, and display one qqnorm() plot at a time. 
for (i in 1:9) {
  x <- dat[,i]
  hist(x,xlab="X",main=paste0("Col.No=",i))
}

#hist(dat[,9]) this is the single code

#BOXPLOT----
#Data is not always normally distributed
library(UsingR)

hist(exec.pay)
#we can see that this is not a normal distribution and is we can try to use qqnorm:
qqnorm(exec.pay)
qqline(exec.pay)

#it Isn't worth to use, but we can use a BOXPLOT
boxplot(exec.pay,ylab="10,000s of dollars",ylim=c(0,400))

#the plot explain us 3 quantile :25, 50% and 75 percentile
#the lines out of the box represent the range of the box 
#the values out of the box are considered like anomal values

#################################
#EXERCISE

View(InsectSprays)
head(InsectSprays)

ins<-InsectSprays
plot(ins)
#for a data frame
#using split:
#boxplot(split(values, factor))

#using a formula:
#boxplot(values ~ factor)


boxplot(split(ins$count,ins$spray))
boxplot(ins$count~ins$spray)
par(mfcol=c(2,2))
dev.off()

load(nym.2002)
class(nym.2002)
install.packages("refalib")
library(dplyr)
library(refalib)
data("nym.2002", package = "UsingR")

View(nym.2002)
nrow(nym.2002)#for see the number of row in a dataframe

mtime<-filter(nym.2002, gender=="Male")
View(mtime)
ftime<-filter(nym.2002, gender=="Female")
View(ftime)

par(mfrow=c(1,3))

boxplot(mtime$time,ftime$time, ylab="time", xlab="male/female",ylim=c(150,600))
hist(mtime$time,ylim = c(0,300), xlim = c(range(nym.2002$time)))
hist(ftime$time,ylim = c(0,300), xlim = c(range(nym.2002$time)))
dev.off()


mean(males$time)
mean(female$time)

nrow(males)
nrow(female)




####Excercise 3
install.packages("dslabs")
library(dslabs)
library(dplyr)
data("heights")
h<-heights$height

hist(h)
boxplot(h)





















































































library(dplyr)
dat<- read.csv("femaleMiceWeights.csv")

control<- filter(dat, Diet=="chow") %>%
  select(Bodyweight)%>%
  unlist

treatment <-filter(dat, Diet=="hf") %>%
  select(Bodyweight)%>%
  unlist

mean(control)
mean(treatment)


#GUARDARE http://genomicsclass.github.io/book/pages/random_variables.html

library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- "femaleControlsPopulation.csv"
if (!file.exists(filename)) download(url,destfile=filename)
population <- read.csv(filename)
population <- unlist(population) # turn it into a numeric

control<-sample(population,12)
mean(control)
treatment<-sample(population,12)
mean(treatment)
obsdiff<-mean(treatment)-mean(control)
print(obsdiff)


n <- 10000
null <- vector("numeric",n)
for (i in 1:n) {
  control <- sample(population,12)
  treatment <- sample(population,12)
  null[i] <- mean(treatment) - mean(control)
}

#The values in null form what we call the null distribution

mean(null>=obsdiff)




###############exercise

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )
print(x)


#Make sure that you are using the correct random number generator (RNG) settings by calling the following command:
RNGkind("Mersenne-Twister", "Inversion", "Rejection")

#Random Variables Exercises #1
#1/1 point (graded)
#What is the average of these weights?
avertot<-mean(x)

#Random Variables Exercises #2
#1 point possible (graded)
#Set the seed to 1:

set.seed(1) #generator of random numbers (it print the same random numbers, it's very good for test)
            #is you don't use set.seed and try t run 10 times the same code, it will give you different numbers


#Take a random sample of size 5. What is the absolute value (use abs()) of the difference between the average 
#of the sample and the average of all the values?

randsiz<-sample(x,5)
avesamp<-mean(randsiz)
print(avesamp)

absval<-abs(avesamp-avertot)
print(absval)


#Random Variables Exercises #3
#1 point possible (graded)
#After setting the seed at 5, set.seed(5), take a random sample of size 5. 
#What is the absolute value of the difference between the average of the sample and the average of all the values?

set.seed(5)
randsiz1<-sample(x,5)
avesamp1<-mean(randsiz1)
print(avesamp1)
absval1<-abs(avesamp1-avertot)
print(absval1)


#with the NULL HYPOTHESIS we are being skeptics, and we we are speculating that the diffent diets don't are important for the different weights
#let's try with some examples

library(dplyr)
dat<- read.csv("femaleMiceWeights.csv")
control<- filter(dat, Diet=="chow") %>%
  select(Bodyweight)%>%
  unlist

treatment <-filter(dat, Diet=="hf") %>%
  select(Bodyweight)%>%
  unlist

mean(control)
mean(treatment)

obs<- mean(treatment)- mean(control)
print(obs)

#now we compare with controls

population <-read.csv("femaleControlsPopulation.csv")
population<- unlist(population)


control<- sample(population,12)
treatment<-sample(population,12)
mean(treatment)-mean(control)

n<-10000
nulls<-vector("numeric",n)
for (i in 1:n){
  control<- sample(population,12)
  treatment<-sample(population,12)
  nulls[i]<- mean(treatment)-mean(control)
}
 
hist(nulls) 
max(nulls)

sum(nulls>obs)/n
#or we can use "mean". it's easier
mean(nulls>obs)
#Now, we can also compute how often it is bigger in absolute value.
mean(abs(nulls)>obs)

#The p-value is the answer to the question, what
#is the probability that an outcome from the null distribution
#is bigger than what we observed when the null hypothesis is true.




##################
#EXERCISE

#Null Distributions Exercises #1
#1/1 point (graded)
#Set the seed at 1, then using a for-loop take a random sample of 5 mice 1,000 times. Save these averages.

#What proportion of these 1,000 averages are more than 1 gram away from the average of x ?

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

set.seed(1)

n<-1000
nulls1<-vector("numeric",n)
for (i in 1:n){
  samp<-sample(x,5)
  nulls1[i]<-mean(samp)
}

hist(nulls1)
ris<- mean(abs(nulls1 - mean(x))>1)
ris


#Null Distributions Exercises #2
#1 point possible (graded)
#We are now going to increase the number of times we redo the sample from 1,000 to 10,000. Set the seed at 1, then using a for-loop take a random sample of 5 mice 10,000 times. Save these averages.
#What proportion of these 10,000 averages are more than 1 gram away from the average of x ?


library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

set.seed(1)

n<-10000
nulls1<-vector("numeric",n)
for (i in 1:n){
  samp<-sample(x,5)
  nulls1[i]<-mean(samp)
}

hist(nulls1)
ris<- mean(abs(nulls1 - mean(x))>1)
ris

###DISTRIBUTION----

#In the video we just watched, Rafa looked at distributions of heights, and asked what was the probability of someone being shorter than a given height. 
#In this assessment, we are going to ask the same question, but instead of people and heights, we are going to look at whole countries and the average life expectancy in those countries.
#We will use the data set called "Gapminder" which is available as an R-package on Github. This data set contains the life expectancy, GDP per capita, and population by country, every five years, from 1952 to 2007.
#It is an excerpt of a larger and more comprehensive set of data available on Gapminder.org External link, and the R package of this dataset was created by the statistics professor Jennifer Bryan External link.

install.packages("gapminder")
library(gapminder)
data(gapminder)
head(gapminder)

dat1952<-gapminder[gapminder$year==1952, ]
x<-dat1952$lifeExp

hist(x)

#In statistics, the empirical cumulative distribution function (or empirical cdf or empirical distribution function) is the function F(a) for any a, which tells you the proportion of the values which are less than or equal to a.
#We can compute F in two ways: the simplest way is to type mean(x <= a). This calculates the number of values in x which are less than or equal to a, divided by the total number of values in x, in other words the proportion of values less than or equal to a.
#The second way, which is a bit more complex for beginners, is to use the ecdf() function. This is a bit complicated because this is a function that doesn't return a value, but a function.

mean(x<=40)
plot(ecdf(x))


prop = function() {
  mean(x <= q)
}
qs = seq(from=min(x), to=max(x), length=20)
props = sapply(qs, prop)
plot(qs, props)
props = sapply(qs, function(q) mean(x <= q))

















#######################################
#EXERCISE

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/femaleControlsPopulation.csv"
filename <- basename(url)
download(url, destfile=filename)
x <- unlist( read.csv(filename) )

#Here x represents the weights for the entire population.

#Using the same process as before (in Null Distribution Exercises), set the seed at 1, then using a for-loop take a random sample of 5 mice 1,000 times. Save these averages.
#After that, set the seed at 1, then using a for-loop take a random sample of 50 mice 1,000 times. Save these averages:

# make averages5
set.seed(1)
n <- 1000
averages5 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,5)
  averages5[i] <- mean(X)
}

# make averages50
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}

par(mfrow=c(1,2))
hist(averages5)
hist(averages50)


#Normal Distribution Exercises #2
#1/1 point (graded)
#For the last set of averages, the ones obtained from a sample size of 50, what proportion are between 23 and 25?

mean( averages50 < 25 & averages50 > 23)


#Normal Distribution Exercises #3
#1 point possible (graded)
#Note that you can use the function pnorm() to find the proportion of observations below a cutoff x given a normal distribution
#with mean mu and standard deviation sigma with pnorm(x, mu, sigma) or pnorm( (x-mu)/sigma ).

#What is the proportion of observations between 23 and 25 in a normal distribution with average 23.9 and standard deviation 0.43?
#  Hint: Use pnorm() twice.
pnorm( (25-23.9) / 0.43)  - pnorm( (23-23.9) / 0.43) 
#the formula is X-mean/SD 


###########
#EXERCISE

#For these exercises, we will be using the following dataset:
  
library(downloader) 
library(dplyr)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- read.csv(filename)

#remove the lines that contain missing values
dat<- na.omit(dat)

#Population, Samples, and Estimates Exercises #1
#1/1 point (graded)
#Use dplyr to create a vector x with the body weight of all males on the control (chow) diet.
#What is this population's average?


x<-filter(dat,Diet=="chow")
x<-filter(x,Sex=="M")
mean(x$Bodyweight)
#But easier to do this
x <- filter(dat, Sex=="M" & Diet=="chow") %>% 
  select(Bodyweight) %>% 
  unlist

mean(x)

sd(x) #deviazione standard


#Population, Samples, and Estimates Exercises #3
#1 point possible (graded)
#Set the seed at 1. Take a random sample of size 25 from x.
#What is the sample average?

set.seed(1)
X<-sample(x,25)
mean(X)

#Population, Samples, and Estimates Exercises #4
#1 point possible (graded)
#Use dplyr to create a vector y with the body weight of all males on the high fat hf) diet.
#What is this population's average?

y<-filter(dat, Sex=="M" & Diet=="hf")%>% 
  select(Bodyweight)%>%
  unlist
mean(y)
sd(y)


#Population, Samples, and Estimates Exercises #6
#1 point possible (graded)
#Set the seed at 1. Take a random sample  of size 25 from y.
#What is the sample average?

set.seed(1)
Y<-sample(y,25)
mean(Ransam1)



#What is the difference in absolute value?
absdiff<-abs(mean(y)-mean(x))
absdiff1<-abs(mean(Y)-mean(X))
abs(absdiff- absdiff1)
#or, easier
abs( ( mean(y) - mean(x) ) - ( mean(Y) - mean(X) ) )

#Population, Samples, and Estimates Exercises #8
#1 point possible (graded)
#Repeat the above for females, this time setting the seed to 2.
#What is the difference in absolute value between  and ?
#Make sure to set the seed to 2 before each sample() call. This function should be called twice.

x <- filter(dat, Sex=="F" & Diet=="chow") %>% 
  select(Bodyweight) %>% 
  unlist
set.seed(2)
X <- sample(x,25)
y <- filter(dat, Sex=="F" & Diet=="hf") %>% 
  select(Bodyweight) %>% 
  unlist
set.seed(2)
Y <- sample(y,25)

absdiff<-abs(mean(y)-mean(x))
absdiff1<-abs(mean(Y)-mean(X))
absdiff-absdiff1
abs( ( mean(y) - mean(x) ) - ( mean(Y) - mean(X) ) )   

#CTL central limit theorem ----
#exercise

library(downloader) 
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/mice_pheno.csv"
filename <- basename(url)
download(url, destfile=filename)
dat <- na.omit( read.csv(filename) )

#Central Limit Theorem Exercises #1
#1 point possible (graded)
#If a list of numbers has a distribution that is well approximated by the normal distribution, 
#what proportion of these numbers are within one standard deviation away from the list's average?
#Use the pnorm() function. You can look up more information with ?pnorm.

pnorm(1)-pnorm(-1)


#Central Limit Theorem Exercises #2
#1 point possible (graded)
#What proportion of these numbers are within two standard deviations away from the list's average?

pnorm(2)-pnorm(-2)



#Central Limit Theorem Exercises #3
#1 point possible (graded)
#What proportion of these numbers are within three standard deviations away from the list's average?

pnorm(3)-pnorm(-3)


#Central Limit Theorem Exercises #4
#1 point possible (graded)
#Define y to be the weights of males on the control diet.
#What proportion of the mice are within one standard deviation away from the average weight?

y <- filter(dat, Sex=="M" & Diet=="chow") %>% 
  select(Bodyweight) %>%
  unlist
z <- ( y - mean(y) ) / sd(y)
mean( abs(z) <=1 )

qqnorm(z)
abline(0,1)

#We will now take a sample of size 25 from the population of males on the chow diet. The average of this sample is our random variable.
#We will use the replicate() function to observe 10,000 realizations of this random variable. Set the seed at 1, then generate these 10,000 averages.
#Make a histogram and qq-plot of these 10,000 numbers against the normal distribution.
#We can see that, as predicted by the CLT, the distribution of the random variable is very well approximated by the normal distribution.

y <- filter(dat, Sex=="M" & Diet=="chow") %>% 
  select(Bodyweight) %>% 
  unlist
set.seed(1)
avgs <- replicate(10000, mean( sample(y, 25)))
par(mfrow=c(1,2))
hist(avgs)
qqnorm(avgs)
qqline(avgs)

#What is the average of the distribution of the sample average?
#what is the standard deviation?
mean(avgs)

sd(avgs)



#T-Test----
library(dplyr)

setwd("c:/Harvard/project1")
dat<-read.csv("femaleMiceWeights.csv")

control<- filter(dat,Diet=="chow")%>%
  select(Bodyweight)%>%
  unlist

treatment<- filter(dat, Diet=="hf")%>%
  select(Bodyweight)%>% unlist

ttest<- t.test(treatment, control)
ttest

#Welch Two Sample t-test

#data:  treatment and control
#t = 2.0552, df = 20.236, p-value = 0.053
#alternative hypothesis: true difference in means is not equal to 0
#95 percent confidence interval:
#  -0.04296563  6.08463229
#sample estimates:
#  mean of x mean of y 
#26.83417  23.81333 

qqnorm(control)
qqline(control)

qqnorm(treatment)
qqline(treatment)


#EXERCISE 2 WEEKS----

#1 point possible (graded)
#The CLT is a result from probability theory. Much of probability theory was originally 
#inspired by gambling. This theory is still used in practice by casinos. For example, 
#they can estimate how many people need to play slots for there to be a 99.9999% probability 
#of earning enough money to cover expenses. Let's try a simple example related to gambling.
#Suppose we are interested in the proportion of times we see a 6 when rolling n=100 die. 
#This is a random variable which we can simulate with x=sample(1:6, n, replace=TRUE)
#and the proportion we are interested in can be expressed as an average: mean(x==6).
#Because the die rolls are independent, the CLT applies.
#We want to roll n dice 10,000 times and keep these proportions. 
#This random variable (proportion of 6s) has mean p=1/6 and variance p*(1-p)/n.
#So according to CLT z = (mean(x==6) - p) / sqrt(p*(1-p)/n) should be normal with mean 0 and SD 1. 
#Set the seed to 1, then use replicate to perform the simulation, and report what proportion of times z was
#larger than 2 in absolute value (CLT says it should be about 0.05).



set.seed(1)
n <- 100
sides <- 6
p <- 1/sides
zs <- replicate(10000,{
  x <- sample(1:sides,n,replace=TRUE)
  (mean(x==6) - p) / sqrt(p*(1-p)/n)
}) 
qqnorm(zs)
abline(0,1)#confirm it's well approximated with normal distribution
mean(abs(zs) > 2)

#0.0431

RNGkind("Mersenne-Twister", "Inversion", "Rejection")


#Set the seed at 1, then using a for-loop take a random sample of 5 mice 1,000 times. 
#Save these averages. What percent of these 1,000 averages are more than 1 ounce away from the average of x ?
set.seed(1)
n <- 1000
averages50 <- vector("numeric",n)
for(i in 1:n){
  X <- sample(x,50)
  averages50[i] <- mean(X)
}
hist(averages50) ##take a look
mean( abs( averages50 - mean(x) ) > 1)

#WEEK 3----
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)


bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke)

n<-25
set.seed(1)
dat.ns<-sample(bwt.nonsmoke,n)
dat.s<-sample(bwt.smoke,n)
tval<-t.test(dat.ns,dat.s)$statistic
tval

#pvalue
pval <- 1-(pnorm(abs(tval))-pnorm(-abs(tval))) 
pval


#WEEK 3 ----
set.seed(1)

setwd("C:/Harvard/project1/")
chowPopulation<-read.csv("femaleControlsPopulation.csv")
chowPopulation<-unlist(chowPopulation)

mu_chow<- mean(chowPopulation)
print(chowPopulation)
mean(chowPopulation)


N<-30


chow<- sample(chowPopulation,N) #campione di 30
print(mean(chow))

se.clt<- sd(chow)/sqrt(N) #teorema del limite centrale 

print(se.clt)

(mean(chow)- mean(chowPopulation)) / se.clt 

pnorm(2)- pnorm(-2) #le code finali della distribuzione normale


Q<- qnorm(1-0.05/2) 

interval<- c(mean(chow)-Q*se.clt, mean(chow)+Q*se.clt)
interval

interval[1] <- mu_chow & interval [2] > mu_chow #?????


library(rafalib)

B<- 250
mypar()

plot(mean(chowPopulation)+c(-7,7), c(1,1), type="n", 
     xlab="weight", ylab="interval", ylim=c(1,B))

abline(v=mean(chowPopulation)) #abbiamo inserito la linea centrale nel plot corrispondente alla linea di media

for (i in 1:B) {
  chow<-sample(chowPopulation,N)
  se.clt<-sd(chow)/sqrt (N)
  interval <-c(mean(chow)-Q*se.clt, mean(chow)+Q*se.clt)
  covered<- mean(chowPopulation)<=interval[2]
  color<- ifelse(covered,1,2)
  lines(interval, c(i,i),col=color)
} #questo è un ciclo for per creare un plot con linee dove ci viene mostrato come i valori fuori dalla media siano distribuiti, 
#possiamo vedere di un colore diverso le linee fuori dalla nostra media
 

#q<-qt(1-0.05/2, df=4)
#n<- 5

#for (i in 1:B) {
#  chow<-sample(chowPopulation,n)
#  se.clt<-sd(chow)/sqrt (n)
#  interval <-c(mean(chow)-q*se.clt, mean(chow)+q*se.clt)
#  covered<- mean(chowPopulation)<=interval[2]
#  color<- ifelse(covered,1,2)
#  lines(interval, c(i,i),col=color)
#}


#Esercizi ----
library(dplyr)
library(downloader)
url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/babies.txt"
filename <- basename(url)
download(url, destfile=filename)
babies <- read.table("babies.txt", header=TRUE)


bwt.nonsmoke <- filter(babies, smoke==0) %>% select(bwt) %>% unlist 
bwt.smoke <- filter(babies, smoke==1) %>% select(bwt) %>% unlist

library(rafalib)
mean(bwt.nonsmoke)-mean(bwt.smoke)
popsd(bwt.nonsmoke)
popsd(bwt.smoke) #deviazione standard della popolazione


#Es1
set.seed(1)
N<-25

dat.ns<- sample(bwt.nonsmoke,N)
dat.s<-sample(bwt.smoke,N)

tval <- t.test(dat.s, dat.ns)$statistic
tval
2 * pnorm( - abs( tval ))

(CLTQuantity <- qnorm(1-0.01/2))

(tQuantity <- qt(1- 0.01/2, df=(2*N-2)))


#Set the seed at 1, then use the replicate function to repeat the code used in the exercise above 10,000 times. What proportion of the time do we reject at the 0.05 level?

N=5
set.seed(1)
rejects <- replicate(10000,{
  dat.ns <- sample(bwt.nonsmoke , N)
  dat.s <- sample(bwt.smoke , N)
  t.test(dat.s, dat.ns)$p.value < 0.05
})
mean(rejects)


#WEEK 4----

library(rafalib)
data(father.son,package="UsingR") ##available from CRAN
x <- father.son$fheight

#for normal distribution
ps <- ( seq(0,99) + 0.5 )/100 
qs <- quantile(x, ps)
normalqs <- qnorm(ps, mean(x), popsd(x))
plot(normalqs,qs,xlab="Normal percentiles",ylab="Height percentiles")
abline(0,1) ##identity line

#Note how close these values are. Also, note that we can see these
#qq-plots with less code (this plot has more points than the one we
#constructed manually, and so tail-behavior can be seen more clearly).

#ATTENTION
#r qqplot_example2, fig.cap="Second example of qqplot. Here we use the function qqnorm which computes the theoretical normal quantiles automatically."}
qqnorm(x)
qqline(x) 

dfs <- c(3,6,12,30)
mypar(2,2)
for(df in dfs){
  x <- rt(1000,df)
  qqnorm(x,xlab="t quantiles",main=paste0("d.f=",df),ylim=c(-6,6))
  qqline(x)
}


#Boxplots ----

#Data is not always normally distributed. Income is a widely cited
#example. In these cases, the average and standard deviation are not
#necessarily informative since one can't infer the distribution from
#just these two numbers. The properties described above are specific to
#the normal. For example, the normal distribution does not seem to be a
#good approximation for the direct compensation for 199 United States
#CEOs in the year 2000.


data(exec.pay,package="UsingR")
mypar(1,2)
hist(exec.pay) 
qqnorm(exec.pay)
qqline(exec.pay)

boxplot(exec.pay, ylab="10,000s of dollars", ylim=c(0,400))

#BELLO QUESTO ESEMPIO----
dev.off()
data(father.son,package="UsingR")
x=father.son$fheight
y=father.son$sheight
plot(x,y, xlab="Father's height in inches", 
     ylab="Son's height in inches", 
     main=paste("correlation =",signif(cor(x,y),2)))
#STRATIFICAZIONE
  groups <- split(y,round(x)) 
boxplot(groups)
print(mean(y[ round(x) == 72]))


#ESERCIZI----
#Use dplyr to create two new data frames: males and females, with the data for each gender. 
#For males, what is the Pearson correlation between age and time to finish?

library(dplyr)
data(nym.2002, package="UsingR")
class(data)
as.data.frame(nym.2002)
males<- nym.2002 [nym.2002$gender =="Male",]
females<- nym.2002 [nym.2002$gender =="Female",]


x=males$age
y=males$time
plot(x,y,xlab="Age",ylab="time",main=paste("correlation =",signif(cor(x,y),2)))

x1=females$age
y1=females$time
plot(x1,y1,xlab="Age",ylab="time",main=paste("correlation =",signif(cor(x1,y1),2)))


nym.2002$age_grcut <- cut(nym.2002$age, 
                      breaks = c(20, 25, 30, 35, 40, 50, 60, 70, 81), 
                      labels = c("20-25", "25-30", "30-35", "35-40", "40-50", "50-60", "60-70", "70-81"), right = FALSE)

boxplot(time~age_grcut, nym.2002)



#COURSE 2 LINEAR MODELS

library(UsingR)
data("father.son",package="UsingR")

mean(father.son$sheight)

#Using the father.son dataset described above, we want to know the expected height of sons 
#if we condition on the father being 71 inches. Create a list of son heights for sons that 
#have fathers with heights of 71 inches (round to the nearest inch).

#What is the mean of the son heights for fathers that have a height of 71 inches 
#(don't round off your answer)?

mean(father.son$sheight[round(father.son$fheight)==71])
#70.54082

#WEEK 1----

#In R we have vectors and matrices. You can create your own vectors with the function c()

vector <-c(1,2,3,4)

#Vectors are also the output of many functions such as:

rnorm(10)
#You can turn vectors into matrices using functions such as rbind(), cbind() or matrix().

#WE CAN CREATE A MATRIX LIKE

X<-matrix(1:1000,100,10)
X

#Using the function cbind, create a 10 x 5 matrix with first column x=1:10. Then columns 2x, 3x, 4x and 5x in columns 2 through 5.

x<- 1:10
x

m<- cbind(x, 2*x,3*x,4*x,5*x)
m


