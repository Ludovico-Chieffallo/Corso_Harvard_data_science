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
#of the normal distribution given a certain random variable x, and a population mean μ, and the population standard deviation σ.

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
