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


Response<-rnorm(n=nrow(df1),mean = 15,sd=8)

df1$Response<-Response

# this time we will replace the response column rather than making a new one

df<-df1 %>% 
  mutate(Response=case_when(Treatment=="Control"~Response*((Year-1949)/30)-10,
                                    Treatment=="Treatment 1"~Response*((Year-1949)/60)+10,
                                    Treatment=="Treatment 2"~Response*((Year-1949)/500)-4))

# Modelling wise we may have multiple different expectations from this data, are all treatments changing with time? in the same way?
# The most complex model would be the interaction of Treatment and Year
# Which would assume that the effect of Year is different depending on the Treatment level 
# (decrease over time in one treatment, increase over time in another)


glm1.1<-glm(Response~Treatment*Year,df, family=gaussian)

plot(glm1.1)

# this is the same as using lm() and not specifying the family, as it is less code we will do this for gaussian models

lm1.1<- lm(Response~Treatment*Year,df)

# We can now check the residuals show no patterns

plot(lm1.1)

# This is annoying as we have to press enter in the console to see all the plots
# We will install some packages from the easystats ecosystem of packages for this

install.packages("performance")
library(performance)

check_model(lm1.1)

# That's better, we now have some interesting plots but our model doesnt look very good (Probably as we have simulated the data itself)

# also we can look at the effect of Year and Treatment where there is no interaction
# This equates to their being an overall effect of Year and then an effect of Treatment

lm1.2<- lm(Response~Treatment+Year,df)

check_model(lm1.2)

lm1.3<- lm(Response~Year,df)

check_model(lm1.3)

lm1.4<- lm(Response~Treatment,df)

check_model(lm1.4)

lm1.5<- lm(Response~1,df)

check_model(lm1.5)

## None of these model assessments are any good but we shall try the same method with some real world data and then make some decisions
# packages often have their own example datasets within them, or a package can be used to store just data without functions etc.


install.packages("palmerpenguins")

library(palmerpenguins)
data(penguins)
# this becomes a 'promise' of a data set, we have to do something with it to get it properly, lets take a look inside

glimpse(penguins)

# This gives us two datasets in our global environment


# As we are going to make simple models here we will clean the data to make our lives easier, this will mean removing NA values
# Removing NA's would normally be something I would not recommend, we (or someone) has worked extremely hard to create this data
# So we shouldn't ever remove data, but NA replacement is complicated and not the subject for now

# Using summary() we can see which columns have NAs and which don't.

summary(penguins)

# Through going into the object and ordering one of the columns we find that the NA's (which order to last always) are the same rows
# So we only have to remove two rows to remove the NAs in the biometrics columns, there are more NA's in the sex column
# if we want to remove NA's there are many ways, to be selective we can filter our dataset 

# Here we will make use of ! this means the opposite of the clause (not this)
# We also use %in% which is used to tell filter there are more than one element
# or NAs that we want to get rid of as NA is not classed like normal data

penguins_someNAs<-penguins %>% 
  filter(!body_mass_g%in%NA)

# If we now look at the number of rows of the datasets we can see only two rows were removed. (not all the NAs)

nrow(penguins)
nrow(penguins_someNAs)

summary(penguins_someNAs)

# Still 9 NAs in sex

penguins_noNAs<-penguins_someNAs %>% 
  filter(!sex%in%NA)

summary(penguins_noNAs)

# All sorted

# So now we will try prove the obvious
# Does the flipper length of penguins change between species and between sexes
# Whether we use an interaction or not depends on if our scientific thought 
# believes the relationship of Species to flipper length is different between sexes (sexual dimorphism may not be consistent across species)


lm2.1<-lm(flipper_length_mm~species*sex,data=penguins_noNAs)

check_model(lm2.1)

# As we only have factors in our model we don't see a 'cloud' of points, but the line still is flat and horizontal so this is good
# As the diagnostics are good we can look at the results

summary(lm2.1)

# Okay there are a lot of numbers here but what does it actually mean
# first lets plot the raw data, boxplots are probably the best for categorical factors
# We can re-use some of our code from the intro for appearance and colours

ggplot(penguins_noNAs)+
  geom_boxplot(aes(x=sex,y=flipper_length_mm,fill=species))+
  scale_fill_manual(values=c("darkcyan","darkorange","grey30"))+
  labs(x="Sex",y="Response Variable (Flipper Length (mm))")+
  theme_classic()

# Now we can also see what the model believes about our data 
# This should be similar to our raw data but not identical
# to do this we make simulated raw data with this same predictor variables in
# we then use the model to predict the response variable based on those predictor variables

# Therefore, we make a data set with sex and species 
# the model then predicts the average Flipper length in mm based on those species and sexes.


NewData<-expand_grid(sex=c("male","female"),
                     species=c("Adelie","Chinstrap","Gentoo"))

NewData$response<-predict(lm2.1,NewData)


ggplot(NewData)+
  geom_point(aes(x=sex,y=response,fill=species))+
  scale_fill_manual(values=c("darkcyan","darkorange","grey30"))+
  labs(x="Sex",y="Response Variable (Flipper Length (mm))")+
  theme_classic()










