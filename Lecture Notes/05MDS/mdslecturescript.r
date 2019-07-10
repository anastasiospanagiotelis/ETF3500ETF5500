rm(list=ls())
library(magrittr)
library(dplyr)
library(ggplot2)
#Compute distances
load('Beer.RData')
filter(Beer,rating=='Fair')%>% #Only fair beers
  select_if(is.numeric)%>% #Only metric data
  scale%>% #Standardise
  dist->delta #Distance

#Attach Beer Names to distance object
filter(Beer,rating=='Fair')%>% #Only fair beers
  use_series(beer)%>% #Get beer names
  abbreviate(6)-> #Abbreviate
  attributes(delta)$Labels #Assign to d

#Classical MDS
mdsout<-cmdscale(delta)

#Get output from mds in a dataframe and plot with ggplot.
mdsout%>%
  as_data_frame(rownames='BeerName')%>%
  ggplot(aes(x=V1,y=V2,label=BeerName))+geom_text()


mdsout<-cmdscale(delta,eig=TRUE)
