library(tidyverse)
installed.packages(c("caret","nnet"))
library(caret)
library(nnet)
####use data from the princeton website to predict use of contraceptive based on age
#and education
##Rodríguez, G. (2007). Lecture Notes on Generalized Linear Models. URL: http://data.princeton.edu/wws509/notes/
con.dat<-read.table("https://data.princeton.edu/wws509/datasets/brazil.raw")
head(con.dat)
####
con.data <- data.frame(matrix(c(
  +    1,   3,  61, 232,
  +    2,  80, 137, 400,
  +    3,216, 131, 301,
  +    4,268,  76, 203,
  +    5,197,  50, 188,
  +    6,150,  24, 164,
  +    7, 91,  10, 183), 7, 4, byrow=TRUE))
head(con.data)
names(con.data) <- c("age","ster","other","none")
con.data$education<-as.factor(con.data$education)
###age sequence
ages <- paste(seq(15,45,5),seq(19,49,5),sep="-")
con.data$ageg<-ages[con.data$age]
###convert nominal into dummy variables
#colnames(con.dat)<- c("agegroup","contraceptivetype","frequency")

######## putting no method as our base category against which
##results will be compared
con.data$Y<-as.matrix(con.data[,c("none","ster","other")])
#con.data$Y<-as.numeric(con.data$Y)
#Y<- cbind(con.data[,c("none","ster","other")])
###

msat<- nnet::multinom(con.data$Y ~ con.data$ageg, data= con.data)
msat
summary(msat)
#####square of age
con.data$age.s <- seq(17.5,47.5,5)[con.data$age] #mean of the age range 
#to square it

con.data$agesq <- con.data$age.s ^2
mlq <- multinom(Y ~ age.s + agesq, data=con.data)
summary(mlq)# this is all in log range
##transforming and finding odds ratio
B<- coef(mlq)
-0.5 * B[,"age.s"]/B[,"agesq"] ### The log-odds of using sterilization rather than no method increase rapidly with age to reach a maximum at 36.5. 
#The log-odds of using a method other than sterilization rather than no method increase slightly to reach a maximum at age 28.5 and then decline.
##this was derived by setting derivative =0
##compare the mode fit using null model, to see if variable is of any use
null<-(multinom(Y~1,data=con.data))
dev.diff<- deviance(null)-deviance(mlq)
dev.diff
summary(null)
summary(mlq)
###test for goodness of fit of the model
pchisq((deviance(null)-deviance(mlq)),4,lower.tail = FALSE) ## Reject null hyp, quadratic model is better
##lets do GOF with the saturated model, df=4
pchisq((deviance(mlq)-deviance(msat)),8, lower.tail = F) ##reject null hyp that model is as good as saturated model ,df=8

######lets plot is
obs <- log(con.data$Y[,2:3]/con.data$Y[,1])
age <- seq(15,50,0.5)
fit <- cbind(1,age,age^2) %*% t(coef(mlq))
method <- c("ster","other")
color=c("red","blue")
plot(con.data$age, obs[,1], type="n", 
          xlab="age", ylab="log-odds", xlim=c(15,55))
for (j in 1:2) {
    points(con.data$age.s, obs[,j], pch=19, col=color[j])
     lines(age, fit[,j], col=color[j])
    text(52, obs[7,j], label=method[j], col=color[j])
   }
###other than age class 17.5 model fits the data

