###Bhattacharya reference interval study

##Padmanaban

##28th March 2023

## Set wd
setwd("C:/Users/drpad/OneDrive - Christian Medical College/Desktop/Reference interval analysis/Manuscript")
###Homocysteine

###import

ua <- read.csv("Data/uric_acid.csv")
ua$AGE <- as.numeric(ua$AGE)
ua_men <- ua[ua$GENDER=="M",]

ua_adult_men <- ua_men[ua_men$AGE>=19 & ua_men$AGE<60,]
nrow(ua_adult_men)

ua_adult_men$RESULT <- as.numeric(ua_adult_men$RESULT)

library(ggplot2)

##Histogram
ggplot(ua_adult_men, aes(x=RESULT)) +
  geom_histogram(binwidth=0.5,color="black", fill="white")+
  xlim(0,20)+ylab("Count")+xlab("Serum uric acid (mg/dL)")+
  labs(title="Histogram of serum uric acid results for adult men")

ggsave("UricAcid_adult_men.png",height = 9, width=16, units="cm",dpi=300)

uricm60<-as.data.frame(table(cut(ua_adult_men$RESULT,breaks = seq(0,25,by=0.5))))

uricm60$logy<-log(uricm60$Freq)


difflogy<- as.data.frame(diff(uricm60$logy))

dydx<- difflogy$`diff(uricm60$logy)`/0.5
x<-as.data.frame(seq(0.25,24.25,by=0.5))

Final<- cbind(dydx,x)



ggplot(Final, aes(x=`seq(0.25, 24.25, by = 0.5)`, y=dydx))+
  geom_point()+ylab("d(log(frequency))/dx")+xlab("Serum uric acid (mg/dL)")+
  labs(title="Bhattacharya plot of serum uric acid results for adult men")+
  xlim(0,20)

ggsave("UricAcid_adult_men_bhattacharya.png",height = 9, width=16, units="cm",dpi=500)

###import

ua <- read.csv("Data/uric_acid.csv")
ua$AGE <- as.numeric(ua$AGE)
ua_women <- ua[ua$GENDER=="F",]

ua_adult_women <- ua_women[ua_women$AGE>=19 & ua_women$AGE<60,]
nrow(ua_adult_women)

ua_adult_women$RESULT <- as.numeric(ua_adult_women$RESULT)

library(ggplot2)

##Histogram
ggplot(ua_adult_women, aes(x=RESULT)) +
  geom_histogram(binwidth=0.5,color="black", fill="white")+
  xlim(0,20)+ylab("Count")+xlab("Serum uric acid (mg/dL)")+
  labs(title="Histogram of serum uric acid results for adult women")

ggsave("UricAcid_adult_women.png",height = 9, width=16, units="cm",dpi=300)

uricf60<-as.data.frame(table(cut(ua_adult_women$RESULT,breaks = seq(0,25,by=0.5))))

uricf60$logy<-log(uricf60$Freq)

difflogy<- as.data.frame(diff(uricf60$logy))

dydx<- difflogy$`diff(uricf60$logy)`/0.5
x<-as.data.frame(seq(0.25,24.25,by=0.5))

Final<- cbind(dydx,x)

ggplot(Final, aes(x=`seq(0.25, 24.25, by = 0.5)`, y=dydx))+
  geom_point()+ylab("d(log(frequency))/dx")+xlab("Serum uric acid (mg/dL)")+
  labs(title="Bhattacharya plot of serum uric acid results for adult women")+
  xlim(0,20)

ggsave("UricAcid_adult_women_bhattacharya.png",height = 9, width=16, units="cm",dpi=500)














