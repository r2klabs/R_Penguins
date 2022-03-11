library(tidyverse)
library(lubridate)

#read in data set 
df<-read.csv("penguins_size.csv", sep=",")

head(df)
tail(df)

#subsetting data  (4,5,6,7,8,9,0)
df[c('species', 'flipper_length_mm')]
df[0:2,3:5]

#subseting into new data frames
FEMALE<-df[which (df$sex=='FEMALE'),]
MALE<-df[which (df$sex=='MALE'),]

nrow(df)

summary(df)

mean(df$body_mass_g, na.rm=TRUE)

#BAD BAD CODE
#colMeans(df, na.rm=TRUE)

colMeans(df[sapply(df, is.numeric)],na.rm=TRUE)
sd(df$body_mass_g, na.rm=TRUE)
var(df$body_mass_g, na.rm=TRUE)

tail(df)
sample_n(df, size=10)

df %>% group_by(sex) %>%
  count()

df2<-read.csv("penguins_lter.csv", sep=",")

colnames(df2)
df2<-df2[-c(2,4,6,7,17)] # Drop columns

column_names<-c('studyName', 'species','island','clutchCompletion', 'date_egg', 'bill_length_mm',
                'bill_depth_mm','flipper_length_mm','body_mass_g','sex','delta_15','delta_13')
column_names
colnames(df2)<-column_names #rename columns
colnames(df2)

df2$species
library(data.table)
df3<-copy(df2)

df2$species

#Split the column
df2<-df2 %>% 
  separate(species,c("species"))
colnames(df2)

#Check Data Types
str(df2)

#Convert date
library(lubridate)
#df2$date_egg<-as.POSIXct(df2$date_egg, format='%m/%d/%y')
df2$date_egg<-mdy(df2$date_egg)
df2$study_day<-df2$date_egg-min(df2$date_egg)

df2$study_day

#Get rid of incomplete cases
df2<-df2[complete.cases(df2), ]

df2 %>% group_by(sex) %>%
  count()

df2<-subset(df2, sex!=".")
df2<-subset(df2, sex!="")

write.csv(df2, "processed_penguins.csv", row.names=F)
df3<-read.csv("processed_penguins.csv")
head(df3)


#Plotting with R
ggplot(df2, aes(bill_length_mm, flipper_length_mm, color=sex)) +
  geom_point() + labs(x="Bill Length", y="Flipper Length") +
  ggtitle("Bill Length vs Flipper Length")

ggplot(df2, aes(bill_length_mm)) +
  geom_boxplot() + coord_flip()

ggplot(data=df2) +
  geom_point(mapping=aes(x=flipper_length_mm, bill_length_mm))

ggplot(data=df2) +
  geom_smooth(mapping=aes(x=flipper_length_mm, y=bill_length_mm)) +
  ggtitle("My Line Chart") 

mycounts<-df2 %>% group_by(species) %>%
  count() 

mycounts

ggplot(data=mycounts, aes(x=species,y=n)) +
  geom_bar(stat="identity") + ggtitle("Bar Chart")
