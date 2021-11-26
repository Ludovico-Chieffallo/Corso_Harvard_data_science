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


