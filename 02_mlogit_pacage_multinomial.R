library(mlogit)

heat<-Heating
head(heat)
H <- mlogit.data(heat, shape="wide", choice="depvar", varying=c(3:12))
##converts the data into longform
m<- mlogit(depvar~ic+oc,data=H)
m.2<- mlogit(depvar~1,data=H)
summary(m.2)
apply(fitted(m.2, outcome=FALSE), 2, mean)
summary(m)
###questions to ask
#Do the estimated coefficients have the expected signs?
## yea they are negative as expected as the cost goes high, choice goes low
##are the coeffecients statistically? 
#yea z value is high aadifferent than 0 and p value is <0.05
####are the predicted values = observed alternative frequency
#observed: Frequencies of alternatives:
#ec       er       gc       gr       hp 
#0.071111 0.093333 0.636667 0.143333 0.055556 
#predicted
apply(fitted(m, outcome=FALSE), 2, mean)
#they are very close, if we run this model ithout intercept as 
#below they are quiet different
# ec         er         gc         gr         hp 
#0.07111111 0.09333333 0.63666667 0.14333333 0.05555556 
m.no<- mlogit(depvar~ic+oc|0,data=H) #no intercept model
summary(m.no)
apply(fitted(m.no, outcome=FALSE), 2, mean)
