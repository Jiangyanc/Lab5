library(tidyverse)
library(ggplot2)
library(readr)
# Import dataset
lab2 <- read_csv("d:/R projects/lab2.csv")
coverage <- read_csv("d:/R projects/coverage.csv", skip = 2,n_max = 52)
expenditures <- read_csv("d:/R projects/expenditures.csv",skip = 2,n_max = 52)

# Create a plot for activity2
time <- c("base","first","second")
category <- c("sport","qol","pain")
lab<-gather(lab2,"time-category","score",2:10)
lab<-separate(lab,col = "time-category",into = c("time","category"),sep = "_")
lab<-lab[order(lab$patient_id),]
lab %>%
  group_by(time,category) %>%
  select(time,category,score) %>%
  summarise(smean=mean(score),ssd=sd(score)) %>%
  ggplot(aes(x=time,y=smean,group=category,color=category))+
    geom_point(position=position_dodge(0.1))+
    geom_line(position=position_dodge(0.1))+
    geom_errorbar(aes(ymin=smean-ssd,ymax=smean+ssd,width=0.1),position=position_dodge(0.1))+
    labs(x="Time of report",y="Sample mean and standard deviation")

# Assignment of lab2
coverage1 <- gather(coverage,"year__type","spending",2:29)
coverage1 <- separate(coverage1,col="year__type",into = c("year","type"),sep = "__")
coverage1 <- coverage1[order(coverage1$Location),]
expenditures1 <- gather(expenditures,"year__type","spending",2:25)
expenditures1 <- separate(expenditures1,col = "year__type",into = c("year","type"),sep = "__")
expenditures1 <- expenditures1[order(expenditures1$Location),]
CovExp <- merge(coverage1,expenditures1,by=c("Location","year","type","spending"),all=TRUE)
CovExp<-filter(CovExp,year>=2013,year<=2016)