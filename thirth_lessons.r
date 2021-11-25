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
control1<-sample(population,12)
mean(control1)
diff<-mean(control1)-mean(control)
print(diff)


n <- 10000
null <- vector("numeric",n)
for (i in 1:n) {
  control <- sample(population,12)
  control1 <- sample(population,12)
  null[i] <- mean(control1) - mean(control)
}

#The values in null form what we call the null distribution

mean(null>=diff)




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

set.seed(1) #generator of random numbers (it print the same random number, it's very good for test)

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

