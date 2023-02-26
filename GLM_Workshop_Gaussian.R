###################################################
############ GLM Workshop 2023 ####################
####### Bede Ffinian Rowe Davies ##################
###################################################
############# Gaussian GLMs #######################
###################################################


## From the introduction we created some fake/simulated data where we know there are differences 

# we will try analysing this data first, then we will download some real-life data to also analyse.
# the below code will recreate the df 

library(tidyverse)

Year <- seq(from=1950,to=2023,by=1)

Treatment <- c("Control","Treatment 1","Treatment 2")

Rep<- seq(from=1,to=10,by=1)

df1<-expand.grid(Year=Year,Treatment=Treatment,Rep=Rep)


Response<-rnorm(n=nrow(df),mean = 15,sd=8)

df1$Response<-Response

# this time we will replace the response column rather than making a new one

df<-df1 %>% 
  mutate(Response=case_when(Treatment=="Control"~Response*((Year-1949)/30)-10,
                                    Treatment=="Treatment 1"~Response*((Year-1949)/60)+10,
                                    Treatment=="Treatment 2"~Response*((Year-1949)/500)-4))

# Modelling wise we may have multiple different expectations from this data, are all treatments changing with time? in the same way?
# The most complex model would be the interaction of Treatment and Year

lm1<- lm(Response~Treatment*Year,df)

# We can now check the residuals show no patterns

plot(lm1)

lm2<- lm(Response~Treatment+Year,df)

# We can now check the residuals show no patterns

plot(lm2)

lm3<- lm(Response~Year,df)

# We can now check the residuals show no patterns

plot(lm3)

lm4<- lm(Response~Treatment,df)

# We can now check the residuals show no patterns

plot(lm4)

lm5<- lm(Response~1,df)

# We can now check the residuals show no patterns

plot(lm5)





