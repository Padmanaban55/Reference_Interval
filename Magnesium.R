###Bhattacharya reference interval study

##Padmanaban

##28th March 2023

## Set wd
setwd("C:/Users/drpad/OneDrive - Christian Medical College/Desktop/Reference interval analysis/Manuscript")
###Magnesium

###import

mg <- read.csv("Data/magnesium.csv")

mg_adult <- mg[mg$AGE>18,]
nrow(mg_adult)
mg_adult$RESULT <- as.numeric(mg_adult$RESULT)

library(ggplot2)

##Histogram
ggplot(mg_adult, aes(x=RESULT)) + 
  geom_histogram(binwidth=0.1,color="black", fill="white")+
  xlim(0,5)+ylab("Count")+xlab("Serum magnesium (mg/dL)")+
  labs(title="Histogram of serum magnesium results for adults")

ggsave("Magnesium_adult.png",height = 9, width=16, units="cm",dpi=300)


##calculating y axis frequency

y_axis<-as.data.frame(table(cut(mg_adult$RESULT, breaks=seq(0,6,by=0.25))))

#taking log y

y_axis$log_y<-log(y_axis$Freq)


# taking d(log y)

diff_log_y<-as.data.frame(diff(y_axis$log_y))


# d(log y)/dx

dy_dx<-as.data.frame(diff_log_y/0.25)


#setting the x axis

x_value<- as.data.frame(seq(0.125,5.75,by=0.25))


#linear plot

final_plot<-cbind(x_value, dy_dx)

ggplot(final_plot, aes(x=`seq(0.125, 5.75, by = 0.25)`,y=`diff(y_axis$log_y)`))+
  geom_point()+ylab("d(log(frequency))/dx")+xlab("Serum magnesium (mg/dL)")+
  labs(title="Bhattacharya plot of serum magnesium results for adults")

ggsave("Magnesium_adult_bhattacharya.png",height = 9, width=16, units="cm",dpi=500)





table(mg_adult$GENDER)
