###################################################
############ GLM Workshop 2023 ####################
####### Bede Ffinian Rowe Davies ##################
###################################################
########## Introduction to R ######################
###################################################

# R is a open source statistical programming language. It can used through many Graphic User Interfaces (GUI) my preferences is 
# to use RStudio but VSCode is good and you can also code in base R. T
# This workshop will rely on using code written in RStudio and locations of things (Script, Console, Environment, Plots) with be RStudio specific but the code could be run in any GUI.

# Scripts are saved code that you are editing (What I am writing in currently), you then execute (run) the code in the console (Normally below the script window)
# You can execute code one line by having your cursor on that line in the script or select many lines then click the run buttom or cmd+enter (mac) or ctrl+enter (pc)
# Everything to the right of a hastag '#' is not executed, therefore we can use this to make comments or writing in scripts

# R code can be used to do simple calculations with values or even create lists, vectors, values, dataframes and more complex objects in the "global environment". (normally top right)

6*6

# We can use either <- or = to assign a value, list or dataframe into an object

a<- 17

# We use c() to concatenate elements together, which means make them into a vector

b<- c(1,5,5,3,7)

# We can then perform different functions between objects

a*b

# We can even save the results to a new object

c<-a*b

# R relies upon packages, groups of specific functions, which can be installed from the internet and then loaded into a script. 
# Base R, a package always already installed and loaded with R, is very powerful and useful but less user friendly for some tasks.
# From Base R we can use the install.packages() function to install a package from online.
install.packages("dplyr") # you only have to do this when you first want the package or want to updated it.
# once a package is installed we have to tell R that we want to use functions from this package so we load it
library(dplyr) # this needs to be run every new R session when this package is used.

# We can now run functions from the dplyr library, specifically dplyr is a library, which is part of a group of packages called the tidyverse
# We will use this group of packages for reading data into R (readr), 

















