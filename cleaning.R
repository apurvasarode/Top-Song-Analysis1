#Top Songs Analysis

#importing dataset top10s and copying it to test data
data = read.csv('C:\\Users\\Apurva Sarode\\Desktop\\Spotify_mva.csv')
View(data)

#Data Cleaning

#Adding column Rank which will denote rank of a song based on it popularity. 
# popularity from 90 - 100 is Rank 10 and so on
for(x in 1:length(data$pop)){
  if(data[x,15] <= 100 && data[x,15] >= 80){
    data[x,16] = 5
    }else if(data[x,15] < 80 && data[x,15] >= 60){
      data[x,16] = 4
      }else if(data[x,15] < 60 && data[x,15] >= 40){
        data[x,16] = 3
        }else if(data[x,15] < 40 && data[x,15] >= 20){
          data[x,16] = 2
          }else if(data[x,15] < 20 && data[x,15] >= 0){
            data[x,16] = 1
          }
}
data$pop <- NULL
dim(data)

#removing values with 0 BPM and duration as 0 seconds
data_clean <- data[-c(433),]
names(data_clean)[15]<- "rating" 

View(data_clean)

#EDA
#checking the ranges for all columns
dim(data_clean)

library(plyr)
library(ggplot2)

#Finding top genre for 3 years
year1 = data_clean[data_clean$year == 2010,]
gen1 = count(year1$top.genre)
barplot(gen1$freq, names.arg = gen1$x,main = 'Top Genres for 2010',xlab = 'Genre',ylab = 'No. of songs')

year2 = data_clean[data_clean$year == 2015,]
gen2 = count(year2$top.genre)
barplot(gen2$freq, names.arg = gen2$x,main = 'Top Genres for 2015',xlab = 'Genre',ylab = 'No. of songs')

year3 = data_clean[data_clean$year == 2019,]
gen3 = count(year3$top.genre)
barplot(gen3$freq, names.arg = gen3$x,main = 'Top Genres for 2019',xlab = 'Genre',ylab = 'No. of songs')


#Histogram view of audio properties
hist(data_clean$bpm, breaks=12,col="blue",xlab="BPM") 
hist(data_clean$nrgy, breaks=12,col="blue",xlab="Energy")
hist(data_clean$dnce, breaks=12,col="blue",xlab="Danceability")
hist(data_clean$live, breaks=12,col="blue",xlab="Live")
hist(data_clean$val, breaks=12,col="blue",xlab="valence")
hist(data_clean$acous, breaks=12,col="blue",xlab="acousticity")
hist(data_clean$spch, breaks=12,col="blue",xlab="Speechibility")

#Line chart for popularity and Duration
ggplot(data_clean) +geom_line(aes(x = dur, y = rating, color = "blue"))

#tests

# T-Test on dataset columns Duration and rating
t.test(data_clean$dur,data_clean$rating, var.equal = TRUE, paired=FALSE)

#Comparing relation between two top genre from 2010 to 2019.
star5 = data_clean[which(data_clean$rating==5),]

with(star5,t.test(dnce[top.genre=="dance pop"],dnce[top.genre=="pop"],var.equal=TRUE))
with(star5,t.test(nrgy[top.genre=="dance pop"],nrgy[top.genre=="pop"],var.equal=TRUE))
with(star5,t.test(bpm[top.genre=="dance pop"],bpm[top.genre=="pop"],var.equal=TRUE))
with(star5,t.test(val[top.genre=="dance pop"],val[top.genre=="pop"],var.equal=TRUE))
