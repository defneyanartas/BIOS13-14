rm(list=ls())

#read the data 

blossoms = read.csv("blossoms.csv")                                              #reading csv to an object
head(blossoms)                                                                   #checking the first rows to understand the structure

#clean the data points that lack a measurement for GA or GAD

blossoms<-blossoms[!is.na(blossoms$GAD),]                                        #this indexes to all the columns that rows don't have Na for GAD and overwrites the data object 
blossoms<-blossoms[!is.na(blossoms$GA),]                                         #this indexes to all the columns that rows don't have Na for GA and overwrites the data object 

#explore the data 

summary(blossoms)
sd(blossoms$GAD)
sd(blossoms$GA)
se_GAD=mean(blossoms$GAD)/sqrt(201)
se_GA=mean(blossoms$GA)/sqrt(201)
library(plyr)
library(knitr)
popstats = ddply(blossoms, .(pop), summarize,                                    #making a statistics summary table
                 GADm=mean(GAD,na.rm=T),
                 GADsd=sd(GAD,na.rm=T),
                 GAm=mean(GA,na.rm=T),
                 GAsd=sd(GA,na.rm=T))
popstats[,-1] = round(popstats[,-1], 2)                                          #rounding the results to be better readable
kable(popstats)
popstats<-data.frame(popstats,"N"=c(30,34,25,14,16,22,25,19,16))                 #updating with se and number of observations
popstats<-data.frame(popstats,"GADse"= popstats$GADm/sqrt(popstats$N))
popstats<-data.frame(popstats,"GAse"= popstats$GAm/sqrt(popstats$N))


                                   
aggregate(blossoms, by=list(blossoms$pop),FUN=length)                            #tells us the number of measurements(observations) per population
cor(blossoms[,c(4,11)],use="pairwise")                                           #indexing to columns these certain because they are continuous values that we are interested in. 
pairs(blossoms[,c(4,11)],panel="panel.smooth")                                   #these two functions give a list of correlations coefficients and scatter plots between variables. 
plot(blossoms$GAD,blossoms$GA, xlab="Gland-anther Distance(mm)", ylab="Gland Area(mm^2)")                                                   #this is the scatter plot of the variables we are interested in.



#fit data to a linear model and perform ANCOVA

y<-blossoms$GA                                                                   #response variable assigned to y
x<-blossoms$GAD                                                                  #continuous predictor assigned to x
z<-blossoms$pop                                                                  #categorical predictor assigned to z
 
m = lm(y~x*z)                                                                    #fitting the model that takes the interaction into account.
plot(m)                                                                          #model diagnostics to see if the assumptions apply (homogeneity of sd etc.)
summary(m)                                                                       #summary lets us see the coefficients of the model and lets us interpret the slopes of the regression lines for each categorical variable
coefficients(m)
anova(m)                                                                         #we can also see the results of the analysis of variance
library(ggplot2)
ggplot(blossoms, aes(x = x, y = y, colour = z)) +                                #we use ggplot to make a scatter plot between continous predictor and response and the categories are colored accroding to their groups.
  geom_point(size = 1) +                                                                                   
  geom_smooth(method="lm", fill=NA)+
  labs(x="Gland-anther distance(mm)",y="Gland-area(mm^2)",colour="Population")+  #we are drawing the regression lines according to the model here.
  theme_bw()                                                                     #theme is chosen

