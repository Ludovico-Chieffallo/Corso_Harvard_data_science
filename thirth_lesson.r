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
