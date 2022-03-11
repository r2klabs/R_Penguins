library(tidyverse)
library(lubridate)

#More Plotting
ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy, color=class))
 
ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ, y=hwy), color="blue") +
  facet_wrap(~ class, nrow=2)

ggplot(data=mpg) +
  geom_smooth(mapping = aes(x=displ, y=hwy), linetype=4)

ggplot(data=mpg) +
  geom_point(mapping = aes(x=displ,  y=hwy, color=class)) +
  geom_smooth(mapping = aes(x=displ, y=hwy), linetype=4)

ggplot(data=mpg, mapping = aes(x=displ,  y=hwy)) +
  geom_point(mapping=aes(color=class)) +
  geom_smooth()

ggplot(data=mpg, mapping = aes(x=displ,  y=hwy)) +
  geom_point(mapping=aes(color=class)) +
  geom_smooth(data=filter(mpg, class=="subcompact"), se=FALSE)

ggplot(data=diamonds) +
  geom_bar(mapping=aes(x=cut))

demo <- tribble(
  ~cut,~freq,
  "Fair",1610,
  "Good",4096,
  "Very Good",12082,
  "Premium",13791,
  "Ideal",21551
)

ggplot(data=demo) +
  geom_bar(mapping=aes(x=cut, y  = freq), stat="identity")


library(nycflights13)
#Filtering
jan1<-filter(flights, month==1, day==1)
thirdq<-filter(flights, month==10 | month==11 | month==12)
thirdq<-filter(flights, month %in% c(10,11,12))

#Rearrange columns
arrange(flights, day, month)

#Select specific columns
select(flights, year, month, day)
select(flights, year:day)
select(flights, -(year:day))

#Rename a column
rename(flights, tail_num=tailnum)

select(flights, time_hour, air_time, everything())

flights_sml <- select(flights, 
                      year:day, 
                      ends_with("delay"), 
                      distance, 
                      air_time
)

colnames(flights_sml)

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       speed = distance / air_time * 60
       )

mutate(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain/hours
)

#Keeps only the new variables
transmute(flights_sml,
       gain = dep_delay - arr_delay,
       hours = air_time / 60,
       gain_per_hour = gain/hours
)

#Get summaries
summarise(flights, delay=mean(dep_delay, na.rm=TRUE))

delays<-flights %>%
  group_by(dest) %>%
  summarise(
    count = n(),
    dist = mean(distance, na.rm=TRUE),
    delay = mean(arr_delay, na.rm=TRUE)
    ) %>%
  filter(count > 20, dest != "HNL")

  ggplot(data=delays) +
    geom_freqpoly(mapping=aes(x=delay), binwidth=10)
  
  ggplot(data = delay, mapping = aes(x = dist, y = delay)) +
    geom_point(aes(size = count), alpha = 1/3) +
    geom_smooth(se = FALSE)
