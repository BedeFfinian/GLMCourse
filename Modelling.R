########### Modelling in R

  
#  Packages often have their own example datasets within them, or a package can be used to store just data without functions etc.

#  We will look at the palmer penguins dataset

# Real World Data


#install.packages("palmerpenguins")

library(palmerpenguins)
data(penguins)


#  This becomes a 'promise' of a data set, we have to do something with it to get it properly, lets take a look inside



library(dplyr)
glimpse(penguins)



#  This gives us two datasets in our global environment
#  
#  Lets plot some of the data to see how they are related before we use more formal analyses




plot(penguins)



#  The base plot() function will create a grid that is each column of the data set is plotted against all the others, this is fine for continuous data (Such as bill_depth_mm)
#  
#  but it isn't great for the categorical data (what does sex= 1 mean??)
#  
#  lets use ggplot2 again to make some nicer plots
#  
#  ggplot starts with an empty ggplot() call that creates a blank sheet and we then layer on our information using the + symbol


library(ggplot2)

ggplot(data=penguins)+
  geom_point(aes(x=bill_length_mm,y=body_mass_g))



#  This creates a simple scatter plot (note the error about missing values, these are NA values and we will deal with this later)
#  
#  We can maybe try colour our points based on their sex or species or island



ggplot(data=penguins)+
  geom_point(aes(x=bill_length_mm,y=body_mass_g, colour=sex))




#  Some of these points do seem to be in clusters, maybe there are differences between species



ggplot(data=penguins)+
  geom_point(aes(x=bill_length_mm,y=body_mass_g, colour=sex, shape=species))



#  this is a bit clearer, but maybe we want to show each species as a separate panel (or sometimes called facet)



ggplot(data=penguins)+
  geom_point(aes(x=bill_length_mm,y=body_mass_g, colour=sex, shape=species))+
  facet_wrap(~species)



#  We can even split further into a grid of species and islands



ggplot(data=penguins)+
  geom_point(aes(x=bill_length_mm,y=body_mass_g, colour=sex, shape=species))+
  facet_grid(island~species)




#  lets look at some of the other data in the dataset



ggplot(data=penguins)+
  geom_point(aes(x=species,y=flipper_length_mm, colour=sex))



#  seems like a scatter plot isnt the best type of plot when one of our axes is categorical, 
#  
#  lets try a boxplot



ggplot(data=penguins)+
  geom_boxplot(aes(x=species,y=flipper_length_mm, colour=sex))



#  We can see from this plot that there are clear differences by species and by sex of flipper length,
#  
#  lets model that difference and see if the difference is significant,
#  
#  As we are going to make simple models here we will clean the data to make our lives easier, this will mean removing NA values.
#  Removing NA's would normally be something I would not recommend, we (or someone) has worked extremely hard to create this data, 
#  so we shouldn't ever remove data, but NA replacement is complicated and not the subject for now.
#  
#  Using summary() we can see which columns have NAs and which don't.
#  

summary(penguins)


#  Through going into the object and ordering one of the columns we find that the NA's (which order to last always) are the same rows
#  So we only have to remove two rows to remove the NAs in the biometrics columns, there are more NA's in the sex column
#  if we want to remove NA's there are many ways, to be selective we can filter our dataset 
#  
#  To subset data we can use the filter() function from the dplyr package, we create a logic clause that then filters the dataset by that clause/statement,
#  
#  for example if we want to select all rows of the data set where the data is from a female penguin we can do this by



female_penguins<- penguins %>% 
  filter(sex=="female")


female_penguins



#  or we might want all the penguins above 5 kg



chonky_penguins<- penguins %>% 
  filter(body_mass_g>= 5000)


chonky_penguins




#  there are a range of symbols we can use such as more than >, less than <, more than or equal to >=, less than or equal to <=, is equal to == 
#  
#  we can even use multiple clauses or statements in one call to filter, 
#  
#  so if we want all the "chonky" female penguins 



chonky_female_penguins<- penguins %>% 
  filter(body_mass_g>= 5000 & sex=="female")


chonky_female_penguins




#  sometimes we might want to filter with multiple answers of a categorical variable, 
#  
#  for example if we wanted all penguins from Biscoe and Torgersen island
#  
#  to do this we can make a vector of the names we want, then filter by that vector 




Islands_we_Want<-c("Biscoe","Torgersen")

Biscoe_Torgersen_penguins<- penguins %>% 
  filter(island%in%Islands_we_Want)

Biscoe_Torgersen_penguins




#  Here we will make use of ! this means the opposite of the clause (not this)
#  
#  We also use %in% which is used to tell filter there are more than one element
#  
#  or NAs that we want to get rid of as NA is not classed like normal data
#  

penguins_someNAs<-penguins %>% 
  filter(!body_mass_g%in%NA)


#  If we now look at the number of rows of the datasets we can see only two rows were removed. (not all the NAs)


nrow(penguins)
nrow(penguins_someNAs)

summary(penguins_someNAs)


#  Still 9 NAs in sex


penguins_noNAs<-penguins_someNAs %>% 
  filter(!sex%in%NA)

summary(penguins_noNAs)


#  All sorted

################### Modelling Categorical Factors

#  So now we will try prove the obvious
#  Does the flipper length of penguins change between species and between sexes
#  Whether we use an interaction or not depends on if our scientific thought 
#  believes the relationship of Species to flipper length is different between sexes (sexual dimorphism may not be consistent across species)






lm2.1<-lm(flipper_length_mm~species*sex,data=penguins_noNAs)



#  We could apply a linear model to almost all data but often it will meet our assumptions
#  
#  to assess these assumptions we can look at the residual distance between the model line and the points
#  
#  these are called the residuals of the model
#  
#  if we run this code we can see a line through points (raw data in grey) and the distances (residuals in red) from the line (model in blue). 



n=50
a=2
b=2
xGaus=seq(1,100,length.out=n)
yGaus=a+b*xGaus
o=10
uGaus=rnorm(n,mean = yGaus,sd=o)

data.frame(y=yGaus,u=uGaus,x=xGaus) %>% 
ggplot()+
  geom_line(aes(x=x,y=y),linewidth=2,colour="darkcyan",alpha=0.7)+
  geom_segment(aes(x=x,xend=x,y=y,yend=u),colour="red")+
  geom_point(aes(x=x,y=u),size=2,colour="grey50")

  


#  We can now check visually the residuals from our model show no patterns


plot(lm2.1)


#  This is annoying as we have to press enter in the console to see all the plots
#  
#  We will install some packages from the easystats ecosystem of packages for this. There are other packages we could also use for this (including one of my own) but performance is good





#install.packages("performance")
library(performance)

check_model(lm2.1)





#  As we only have factors in our model we don't see a 'cloud' of points, but the line still is flat and horizontal so this is good
#  
#  As the diagnostics are good we can look at the results


summary(lm2.1)


#  Okay there are a lot of numbers here but what does it actually mean
#  
#  first lets plot the raw data, boxplots are probably the best for categorical factors
#  
#  We can re-use some of our code from the intro for appearance and colours





ggplot(penguins_noNAs)+
  geom_boxplot(aes(x=species,y=flipper_length_mm,fill=sex))+
  scale_fill_manual(values=c("darkcyan","darkorange"))+
  labs(x="Sex",y="Response Variable (Flipper Length (mm))")+
  theme_classic()


#  Now we can also see what the model believes about our data 
#  
#  This should be similar to our raw data but not identical
#  
#  to do this we make simulated raw data with this same predictor variables in
#  
#  we then use the model to predict the response variable based on those predictor variables
#  
#  Therefore, we make a data set with just sex and species (be careful of spelling and capitalisation, R wants it identical)
#  
#  the model then predicts the average Flipper length in mm based on those species and sexes. 
#  
#  We can also tell the Predict function to predict error (Standard Error here)






NewData<-expand.grid(sex=c("male","female"),
                     species=c("Adelie","Chinstrap","Gentoo"))

Pred<-predict(lm2.1,NewData,se.fit=TRUE)

NewData$response<-Pred$fit

NewData$se.fit<-Pred$se.fit


ggplot(NewData)+
  geom_point(aes(x=species,y=response,colour=sex),
             position=position_dodge(0.8))+
  geom_errorbar(aes(x=species,ymax=response+se.fit,
                    ymin=response-se.fit,colour=sex),
                width=0.1,
                position=position_dodge(0.8))+
  scale_colour_manual(values=c("darkcyan","darkorange"))+
  labs(x="Sex",y="Response Variable (Flipper Length (mm))")+
  theme_classic()



############### Modelling Continuous Variables

#  Okay that is what we do with linear models of categorical factors
#  
#  But what if we want to see the relationship between flipper_length_mm and bill_length_mm
#  
#  But we know there are species differences and sexual differences in flipper length
#  
#  As Males always tend to be larger lets just assess species differences in their flipper to bill relationship
#  
#  
#  Lets plot the raw data first 






ggplot(penguins_noNAs)+
  geom_point(aes(x=bill_length_mm,y=flipper_length_mm,colour=species))+
  scale_colour_manual(values=c("darkcyan","darkorange","grey30"))+
  labs(x="Bill Length (mm)",y="Flipper Length (mm)")+
  theme_classic()


#  We can see from the raw data that we will expect to find some strong linear relationships





lm3.1<-lm(flipper_length_mm~species*bill_length_mm,data=penguins_noNAs)

check_model(lm3.1)

summary(lm3.1)


#  As we hypothesised before modelling that there would be different bill to flipper relationships between species
#  The interaction model follows our scientific assumptions
#  Therefore, it would be incorrect to use lower complexity models (without the interactoin for example)
#  
#  To predict again we want to create lines for each species
#  To do this we want to create fake bill length data over the same range for each species
#  
#  Here we will use the seq() function that creates a sequence of values from your first number to your last number 
#  And you can chose the length of the vector it creates or the distance between each individual value



NewData_<-expand.grid(bill_length_mm=seq(from=min(penguins_noNAs$bill_length_mm),
                                         to=max(penguins_noNAs$bill_length_mm),
                                         length.out=1000),
                      species=c("Adelie","Chinstrap","Gentoo"))


#  As the different species won't be across all of these bill length ranges 
#  we should also remove values outside of each species range


Gentoo_Range<-penguins_noNAs %>% 
  filter(species=="Gentoo") %>% 
  summarise(min=min(bill_length_mm),
            max=max(bill_length_mm))

Adelie_Range<-penguins_noNAs %>% 
  filter(species=="Adelie") %>% 
  summarise(min=min(bill_length_mm),
            max=max(bill_length_mm))

Chinstrap_Range<-penguins_noNAs %>% 
  filter(species=="Chinstrap") %>% 
  summarise(min=min(bill_length_mm),
            max=max(bill_length_mm))


NewData_2<-NewData_ %>% 
  mutate(Range=case_when(species=="Gentoo" &
                              bill_length_mm>=Gentoo_Range$min &
                              bill_length_mm<=Gentoo_Range$max~"Good",
                         species=="Adelie" &
                           bill_length_mm>=Adelie_Range$min &
                           bill_length_mm<=Adelie_Range$max~"Good",
                         species=="Chinstrap" &
                           bill_length_mm>=Chinstrap_Range$min &
                           bill_length_mm<=Chinstrap_Range$max~"Good"
  )) %>% 
  filter(!Range%in%NA) %>% 
  select(-Range)


#  As we didnt create a case when for outside of the good range then they will be NAs and we can filter them out
#  Then we remove the created Range column


Pred_2<-predict(lm3.1,NewData_2,se.fit=TRUE)

NewData_2$response<-Pred_2$fit

NewData_2$se.fit<-Pred_2$se.fit


#  So now we have many data points that can be used to draw the linear model outputs





ggplot()+
  geom_ribbon(data=NewData_2,mapping=aes(x=bill_length_mm,ymax=response+se.fit,
                                                 ymin=response-se.fit,fill=species),
              alpha=0.4)+
  geom_line(data=NewData_2,mapping=aes(x=bill_length_mm,y=response,colour=species),
             alpha=0.4)+
  scale_color_manual(values=c("darkcyan","darkorange","grey30"))+
  scale_fill_manual(values=c("darkcyan","darkorange","grey30"))+
  labs(x="Bill Length (mm)",y="Response Variable (Flipper Length (mm))")+
  theme_classic()


#  This looks good but lets maybe add the raw data values onto the same figure as the model outputs





ggplot()+
  geom_point(data=penguins_noNAs,mapping = aes(x=bill_length_mm,
                                               y=flipper_length_mm,
                                               colour=species),
             alpha=0.4,size=0.8)+
  geom_ribbon(data=NewData_2,mapping=aes(x=bill_length_mm,ymax=response+se.fit,
                                         ymin=response-se.fit,fill=species),
              alpha=0.4)+
  geom_line(data=NewData_2,mapping=aes(x=bill_length_mm,y=response,colour=species),
            alpha=0.4)+
  scale_color_manual(values=c("darkcyan","darkorange","grey30"))+
  scale_fill_manual(values=c("darkcyan","darkorange","grey30"))+
  labs(x="Bill Length (mm)",y="Response Variable (Flipper Length (mm))")+
  theme_classic()







