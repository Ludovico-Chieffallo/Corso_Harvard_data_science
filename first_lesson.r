#Exercise #1
#1/1 point (graded)
#Create a numeric vector containing the numbers 2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23.

#What is the average of these numbers?

num<-c(2.23, 3.45, 1.87, 2.11, 7.33, 18.34, 19.23)
mean(num)


#Exercise #3
#1/1 point (graded)
#Use a for loop to determine the value of ∑25i=1 i^2

sum <- 0
for(i in 1:25){
   sum<-sum + i^2
sum}


#Exercise #4
#1/1 point (graded)
#The cars dataset is available in base R. You can type cars to see it. Use the class() function to determine what type of object is cars.

class(cars)

#Exercise #5
#1/1 point (graded)
#How many rows does the cars object have?

50

#Exercise #6
#1/1 point (graded)
#What is the name of the second column of cars?

dist


#Exercise #7
#1/1 point (graded)
#The simplest way to extract the columns of a matrix or data.frame is using [. For example you can access the second column with cars[,2].

#What is the average distance traveled in this dataset?


media<-mean(cars[,2])


#Exercise #8
#1/1 point (graded)
#Familiarize yourself with the which() function. Which row of cars has a a distance of 85?

which(cars$dist == 85)
