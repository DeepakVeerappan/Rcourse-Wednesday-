##Day1 step by step learning 

###loading libraries 
library(dplyr)
library(ggplot2)
library(gridExtra)
library(tidyverse)
library(colorspace)

##data import
compensation <-read.csv("Data/compensation.csv")
compensation

##clear R brain
rm(list=ls())

## dimension shows # of rows and columns 
dim(compensation)

## variable names/column header
names(compensation)

## lists the first few on the top of the dataframe
head(compensation)

##lists the first few on the bottom of the dataframe
tail(compensation)

## summary structure of the dataset
str(compensation)

## Dplyr alternative and handy functions compared to R Base

tbl_df(compensation)

glimpse(compensation)

## Select function is for columns 
select(compensation, Root)

select(compensation, Root, Fruit)

select(compensation, -Root)

select(compensation, -Fruit)

## Slice function is for rows

slice(compensation,2)
slice(compensation,2:5)

## "c" means combine 
slice(compensation,c(2,5))

## Modified datasets

comp2<-select(compensation,-Root)

## function to get 8th row
slice(compensation,8)

select(compensation,Fruit)

## 8th row of fruits column
slice(comp2,8)

slice(compensation,Fruit)

comp3<-select(compensation,Fruit)

slice(comp3,8)

slice(compensation,8)

select(slice(compensation,8),Fruit)

## Piping method 
compensation %>%  
select(Fruit) %>%
  slice(8)

### filter logical operation and subsetting 
filter(compensation,Fruit>80)
## piping method

compensation %>%  
filter(Fruit>80)%>% 
select(Root) 
  
## creating new variables
mutate(compensation,
       RFratio=Root/Fruit)

## rename variables 
comp3<-rename(compensation,
       Frt = Fruit)
Frt

str(compensation)

## rearrange data 
arrange (compensation,-Root)
arrange (comp3,-Frt)

## summarising data in R
summarise(compensation, mean(Fruit))
summarise(compensation, sd(Fruit))
summarise(compensation, length(Root))
summarise(compensation, sum(Root))
##standard deviation√var/length 

##mean & SD summarised
summarise(compensation, meanFruit=mean(Fruit),
          sdFruit=sd(Fruit))

##summarise by groups
summarise(group_by(compensation, Grazing),
   meanFruit=mean(Fruit),
          sdFruit=sd(Fruit))

##summarise by piping
compensation %>%  
  group_by(Grazing) %>%  
summarise(
  meanFruit =mean(Fruit),
  sdFruit=sd(Fruit))

## summarise by groups ratio

## make a new variable and then get the summary of this 
mut<-mutate(compensation,
       RFratio=Root/Fruit)

## or include it in the summary function
summarise(group_by(compensation, Grazing),
          meanFruit = mean(Fruit),
          sdFruit = sd(Fruit),
          RFratio = mean(Root/Fruit))


##summarise this by piping
compensation %>%  
  group_by(Grazing) %>%  
  summarise(
    meanFruit =mean(Fruit),
    sdFruit=sd(Fruit), RFratio=mean(Root/Fruit))

## exercise 
summarise(group_by(compensation, Grazing),
          medianratio = median(Root/Fruit),
          varratio = var(Root/Fruit),
          samplessizeratio = length(Root/Fruit))

summarise(group_by(compensation, Grazing),
          medianratio = median(Root/Fruit),
          varratio = var(Root/Fruit),
          samplessizeratio = n())

MRP<-summarise(group_by(compensation, Grazing),
               medianratio = median(Root/Fruit),
               varratio = var(Root/Fruit),
               samplessizeratio = n())

##summarise this by piping
compensation %>%  
  group_by(Grazing) %>%  
  summarise(
    medianratio = median(Root/Fruit),
    varratio = var(Root/Fruit),
    samplessizeratio = length(Root/Fruit))

MRP<-compensation %>%  
  group_by(Grazing) %>%  
  summarise(
    medianratio = median(Root/Fruit),
    varratio = var(Root/Fruit),
    samplessizeratio = length(Root/Fruit))
MRP

## GGPLOT2
# three main skeleton of GGPLOT dataframe, aesthetic components(aes), 
# geometric objects(geom_point()) is the first layer to add to the plot
# inherits things from the top layer to the bottom layer

ggplot(compensation, aes(x=Root, y=Fruit))+
  geom_point()

##color the points
ggplot(compensation, aes(x=Root, y=Fruit))+
  geom_point(col="turquoise")
colors()

##size of the points default is 1
ggplot(compensation, aes(x=Root, y=Fruit))+
  geom_point(col="turquoise", size=4)+
  ylab("Fruit(pecs)")+
  xlab("Root(mm)")

##editing the x and y lab
ggplot(compensation, aes(x=Root, y=Fruit))+
  geom_point(col="turquoise", size=4)+
  ylab("Fruit(pecs)")+
  xlab("Root(mm)")

##color by catagories
ggplot(compensation, aes(x=Root, y=Fruit, col=Grazing))+
  geom_point(size=4)+
  ylab("Fruit(pecs)")+
  xlab("Root(mm)")
  
##color of choice for catagories
ggplot(compensation, aes(x=Root, y=Fruit, col=Grazing))+
  geom_point(size=4)+
  ylab("Fruit(pecs)")+
  xlab("Root(mm)")+
  scale_color_manual(values=c("green","blue"))
  
##themes
ggplot(compensation, aes(x=Root, y=Fruit, col=Grazing))+
  geom_point(size=4)+
  ylab("Fruit(pecs)")+
  xlab("Root(mm)")+
  scale_color_manual(values=c("green","blue"))+
  theme_bw()
  
##adding a line
ggplot(compensation, aes(x=Root, y=Fruit, col=Grazing))+
  geom_point(size=4)+
  ylab("Fruit(pecs)")+
  xlab("Root(mm)")+
  scale_color_manual(values=c("green","blue"))+
  theme_bw()+
  geom_smooth(method=lm)

##adding a line
ggplot(compensation, aes(x=Root, y=Fruit, col=Grazing))+
  geom_point(size=4)+
  ylab("Fruit(pecs)")+
  xlab("Root(mm)")+
  scale_color_manual(values=c("green","blue"))+
  theme_bw()+
  xlim(c(0,10)+
  geom_smooth(method=lm)

  ggplot(compensation, aes(x=Root, y=Fruit, col=Grazing))+
    geom_point(size=4)+
    ylab("Fruit(pecs)")+
    xlab("Root(mm)")+
    scale_color_manual(values=c("green","blue"))+
    scale_shape_manual(values=c(3,4))+
    theme_bw()+
    geom_smooth(method=lm)  
  
  ###saving a graph
  ggsave("plot1.pdf")
  
  ###exporting data
  write.csv("compensation.csv", new.compensation,
            col.names = TRUE, row.names = FALSE,
            quote = FALSE)
  
  scale_x_continuous(values=c(1,10))+
    
    
 ##histogram
    ggplot(compensation, aes(x=Fruit)) +
    geom_histogram(bins=20, fill="red")
    
##Way facet wrap adjacent
  ggplot(compensation, aes(x=Fruit)) +
    geom_histogram(bins=20)+
    facet_wrap(~Grazing)
  
##Way facet wrap stack above   
  ggplot(compensation, aes(x=Fruit)) +
    geom_histogram(bins=20)+
    facet_wrap(~Grazing,ncol=1)
  
###Box plots
  ggplot(compensation, aes(x=Grazing,y=Fruit,fill=Grazing)) +
    geom_boxplot()+
    geom_point()+
    scale_fill_manual(values=c("green","blue"))
  
  ggplot(compensation, aes(x=Grazing,y=Fruit,fill=Grazing)) +
    geom_boxplot()+
    geom_jitter()+
    scale_fill_manual(values=c("green","blue"))
  
  ggplot(compensation, aes(x=Grazing,y=Fruit,fill=Grazing)) +
    geom_boxplot()+
    geom_jitter(alpha=0.8)+
    scale_fill_manual(values=c("green","blue"))
  
  ggplot(compensation, aes(x=Grazing,y=Fruit,fill=Grazing)) +
    geom_boxplot()+
    geom_polygon()
  
  