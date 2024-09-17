###Bhattacharya reference interval study

##Padmanaban

##28th March 2023

## Set wd
setwd("C:/Users/drpad/OneDrive - Christian Medical College/Desktop/Reference interval analysis/Manuscript")
###Homocysteine

###import

ho <- read.csv("Data/homocysteine.csv")
ho$AGE <- as.numeric(ho$AGE)
ho_adult <- ho[ho$AGE>18,]
nrow(ho_adult)
ho_adult$RESULT <- as.numeric(ho_adult$RESULT)

library(ggplot2)

##Histogram
ggplot(ho_adult, aes(x=RESULT*1.35189941868325)) + 
  geom_histogram(binwidth=1,color="black", fill="white")+
  xlim(0,64.1)+ylab("Count")+xlab("Plasma homocysteine (micro g/dL)")+
  labs(title="Histogram of plasma homocysteine results for adults")

ggsave("Homocystine_adult.png",height = 9, width=16, units="cm",dpi=300)


##calculating y axis frequency

y_axis<-as.data.frame(table(cut(ho_adult$RESULT, breaks=seq(0,65,by=1))))

#taking log y

y_axis$log_y<-log(y_axis$Freq)


# taking d(log y)

diff_log_y<-as.data.frame(diff(y_axis$log_y))


# d(log y)/dx

dy_dx<-as.data.frame(diff_log_y/1)


#setting the x axis

x_value<- as.data.frame(seq(0.5,63.5,by=1))


#linear plot

final_plot<-cbind(x_value, dy_dx)

ggplot(final_plot, aes(x=`seq(0.5, 63.5, by = 1)`*1.35189941868325, y=`diff(y_axis$log_y)`))+
  geom_point()+ylab("d(log(frequency))/dx")+xlab("Plasma homocysteine (micro g/dL)")+
  labs(title="Bhattacharya plot of plasma homocysteine results for adults")+
  xlim(0,40)

ggsave("Homocysteine_adult_bhattacharya.png",height = 9, width=16, units="cm",dpi=500)






