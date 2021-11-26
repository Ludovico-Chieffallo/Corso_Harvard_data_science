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







#sapply() on a custom function
#Suppose we want to plot the proportions of countries with life expectancy q for a range of different years. R has a built in function for this, plot(ecdf(x)), but suppose we didn't know this. The function is quite easy to build, by turning the code from question 1.1 into a custom function, and then using sapply(). Our custom function will take an input variable q, and return the proportion of countries in x less than or equal to q. The curly brackets, { and }, allow us to write an R function which spans multiple lines:

prop = function(q) {
  mean(x <= q)
}
#Try this out for a value of q: prop(40)

#ow let's build a range of qs that we can apply the function to:

qs = seq(from=min(x), to=max(x), length=20)
#Print qs to the R console to see what the seq() function gave us. Now we can use sapply() to apply the prop function to each element of qs:

props = sapply(qs, prop)
#Take a look at props, either by printing to the console, or by plotting it over qs:

plot(qs, props)
#Note that we could also have written this in one line, by defining the prop function inside of sapply() but without naming it:

props = sapply(qs, function(q) mean(x <= q))
#This last style is called using an "inline" function or an "anonymous" function. Let's compare our homemade plot with the pre-built one in R:

plot(ecdf(x))

               prop = function() {
  mean(x <= q)
}
qs = seq(from=min(x), to=max(x), length=20)
props = sapply(qs, prop)
plot(qs, props)
props = sapply(qs, function(q) mean(x <= q))
