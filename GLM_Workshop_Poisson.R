###################################################
############ GLM Workshop 2023 ####################
####### Bede Ffinian Rowe Davies ##################
###################################################
############## Poisson GLMs #######################
###################################################


library(tidyverse)
library(performance)

# Okay now we know the general method for applying models
# The method for applying a poisson model follows the same methods
# and uses the same diagnostic methods

# We will download the data set warpbreaks

data("warpbreaks")

# This data has the count of the number of breaks across types of wool and tension
# We will look at the how wool type and tension effect the number of breaks 
# Again first we will summarise the data and also visualise it, often the best way to find issues/mistakes

summary(warpbreaks)

# This tells us there are two types and three tensions

ggplot()+
  geom_boxplot(warpbreaks,mapping=aes(x=tension,y=breaks,colour=wool))+
  scale_colour_manual(values=c("darkcyan","darkorange"))+
  labs(x="Tensions",y="Wool Breaks")+
  theme_classic()

# This seems to show different breaks across wools and tensions

# Thus we want to analyse with interaction term 

glm1.1<-glm(breaks~tension*wool,warpbreaks,family=poisson)

check_model(glm1.1)


summary(glm1.1)

# We can see some significant differences
# lets plot them, again fake data then predict


NewData<-expand_grid(wool=c("A","B"),
                     tension=c("L","M","H"))

Pred<-predict(glm1.1,NewData,se.fit=TRUE,"response")

NewData$response<-Pred$fit

NewData$se.fit<-Pred$se.fit

  ggplot(NewData)+
  geom_point(aes(x=tension,y=response,colour=wool),
             position=position_dodge(0.8))+
  geom_errorbar(aes(x=tension,ymax=response+se.fit,
                    ymin=response-se.fit,colour=wool),
                width=0.1,
                position=position_dodge(0.8))+
  scale_colour_manual(values=c("darkcyan","darkorange"))+
  labs(x="Tension",y="Response Variable (Number of Breaks)")+
  theme_classic()

# We have tension which is an ordered factor but r orders things alphabetically 
# so we can tell r the order is L M H

NewData %>% 
  mutate(tension=factor(tension,levels=c("L","M","H"))) %>% 
ggplot()+
  geom_point(aes(x=tension,y=response,colour=wool),
             position=position_dodge(0.8))+
  geom_errorbar(aes(x=tension,ymax=response+se.fit,
                    ymin=response-se.fit,colour=wool),
                width=0.1,
                position=position_dodge(0.8))+
  scale_colour_manual(values=c("darkcyan","darkorange"))+
  labs(x="Tension",y="Response Variable (Number of Breaks)")+
  theme_classic()



# We will now look at another df that is built-in to r

data(Seatbelts)

summary(Seatbelts)

# it is a 'time-series' type of dataframe so we convert it

install.packages("timetk")
library(timetk)

seatbelts_df<-tk_tbl(Seatbelts) %>% 
  rownames_to_column("MonthsN")

# This gives us the number of traffic deaths over time in UK
# we will see if the number of drivers as well as the occurence of seatbelt laws
# effects the number of drivers


glm2.1<-glm(DriversKilled~drivers*law,data=seatbelts_df,family=poisson)

check_model(glm2.1)

summary(glm2.1)

NewData_Seatbelts<-data.frame(law=seatbelts_df$law,
                               drivers=seatbelts_df$drivers)


Pred_Seat<-predict(glm2.1,NewData_Seatbelts,se.fit=TRUE,"response")

NewData_Seatbelts$response<-Pred_Seat$fit

NewData_Seatbelts$se.fit<-Pred_Seat$se.fit

NewData_Seatbelts %>% 
  mutate(law=as.factor(law)) %>% 
ggplot()+
  geom_ribbon(aes(x=drivers,ymax=response+se.fit,
                  ymin=response-se.fit,fill=law),
              alpha=0.6)+
  geom_line(aes(x=drivers,y=response,
                colour=law))+
  scale_colour_manual(values=c("darkcyan","darkorange"))+
  scale_fill_manual(values=c("darkcyan","darkorange"))+
  labs(x="Number of Drivers",y="Response Variable (Number of Driver Deaths)")+
  theme_classic()


