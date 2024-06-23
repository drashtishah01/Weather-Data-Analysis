#Drashti Shah 

library(reader)
library(ggplot2)
library(dplyr)
library(gganimate)
library(graphics)
library(magrittr)

setwd("D:\\")
datasheet = read.csv("D:\\Hourly weather data.csv")
View(datasheet)

ggplot(datasheet,aes(wind_speed))+geom_bar()

#Analysis 1: Temperature is inversely proportional to wind speed.

avg1 = mean(datasheet$wind_speed, na.rm = TRUE)
datasheet$wind_speed = 
  ifelse(is.na(datasheet$wind_speed),avg1,datasheet$wind_speed)
#Assuming that the missing wind speed values are equal to the average value 
#of all present wind speed values, so replacing NA with the average value

one= datasheet %>%filter(origin == "JFK", month == 12, day >= 1 & day <= 10)%>% 
  ggplot(mapping = aes(x = day, y = wind_speed))+
  geom_point()+geom_point(aes(x = day, y = temp,colour = "temp"))+
  labs(title = "Relationship between Temperature and Wind Speed", 
       x="Day", y = "Wind speed and Temp")

#Analysis 2: what is the relationship between humidity and visibility?

two = datasheet %>% filter(month == 7)%>%
  ggplot(mapping = aes(x=day,y=visib))+geom_jitter()+
  geom_jitter(aes(x=day,y=humid, colour = "humid"))+
  facet_wrap(~origin)+
  labs(title = "Comparison of Humidity and Visibility by origins",
       x = "Days", y= "Humidity and Visibility")

#Analysis 3: relationship between visibility and precipitation.

three = datasheet %>% filter(month == 7)%>%
  ggplot(mapping = aes(x=day,y=visib))+geom_line()+
  geom_jitter(aes(x=day,y=precip,colour = "precip"))+
  facet_wrap(~origin)+
  labs(title = "Comparison of Precipitation and Visibility by origins",
       x = "Days", y= "Precipitation and Visibility")

#Analysis 4: relationship between humidity and temperature

four = datasheet %>% filter(origin == "LGA", month == 6,day >= 1 & day <= 10)%>%
  ggplot(mapping = aes(x = day, y= humid))+geom_smooth()+
  geom_point(aes(x=day, y = temp,colour = "temp"))+
  labs(title = "Comparison of Humidity and Temperature",
       x = "Days", y= "Humidity and Temperature")

#Analysis 5: relationship between visibility and pressure

avg5 = mean(datasheet$pressure, na.rm = TRUE)
datasheet$pressure = ifelse(is.na(datasheet$pressure),avg5,datasheet$pressure)
#Assuming that the missing pressure values were same as the average value
#of pressure and replacing NA with the average value

five = datasheet %>% filter(origin == "LGA",month == 9)%>%
  ggplot(mapping = aes(x = day, y= pressure))+geom_jitter()

five2 = datasheet %>% filter(origin == "LGA",month == 9)%>%
  ggplot(mapping = aes(x = day, y= visib))+geom_jitter()

#Analysis 6: comparing Precipitation and pressure

six = datasheet %>% filter(origin == "LGA",month == 9)%>%
  ggplot(mapping = aes(x = day, y= pressure))+geom_jitter()

six2 = datasheet %>% filter(origin == "LGA",month == 9)%>%
  ggplot(mapping = aes(x = day, y= precip))+geom_point()+
  geom_path()

#Analysis 7: comparing wind speed and wind gust

minim1 = min(datasheet$wind_gust, na.rm = TRUE)
datasheet$wind_gust = 
  ifelse(is.na(datasheet$wind_gust),minim1,datasheet$wind_gust)
#Assuming that wind gust were same as it's minimum value where
#data is not available so replacing NA with that minimum value

seven = datasheet %>%filter(origin == "JFK",month == 1,day >= 1 & day <= 10)%>%
  ggplot(mapping = aes(x = day, y= wind_speed))+geom_path()+
  geom_jitter(aes(x=day, y = wind_gust,colour = "wind gust"))+
  labs(title = "Comparison of Wind speed and Wind gust",
       x = "Days", y= "Wind speed and Wind gust")

#Analysis 8: connection between wind speed and humidity

eight = datasheet%>%filter(origin == "JFK", month == 6, day == 6)%>%
  ggplot(mapping = aes(x = hour, y = wind_speed))+geom_jitter()+
  geom_line()+annotate("segment",x =5,xend = 9, y= 7.5, yend = 16, 
      colour = "pink",size = 3, alpha = 0.6,arrow = arrow())

eight2 = datasheet%>%filter(origin == "JFK", month == 6, day == 6)%>%
  ggplot(mapping = aes(x = hour, y = humid))+geom_jitter()+geom_line()+
  annotate("segment",x =5,xend = 10, y= 87, yend = 45, 
           colour = "blue",size = 3, alpha = 0.6,arrow = arrow())


#Analysis 9: connection between visibility and temperature

nine = datasheet %>% filter(origin == "JFK",month == 4)%>%
  ggplot(mapping = aes(x = day, y= visib))+geom_point()+geom_step()+
  geom_text(aes(x= 15, y=0.5,label = "Lowest",size = 0.5))


nine2 = datasheet %>% filter(origin == "JFK",month == 4)%>%
  ggplot(mapping = aes(x = day, y= temp))+geom_point()+geom_step()+
  geom_text(aes(x = 13,y=83,label = "Highest",size = 0.5))+
  geom_text(aes(x= 22, y=68,label = "High",size = 0.5))

#Analysis 10: relationship between temperature and pressure

ten = datasheet %>% filter(origin == "JFK",month == 11)%>%
  ggplot(mapping = aes(x = day, y= temp))+geom_smooth()

ten2 = datasheet %>% filter(origin == "JFK",month == 11)%>%
  ggplot(mapping = aes(x = day, y= pressure))+geom_smooth()

#Analysis 11: relationship between humidity and precipitation

eleven = datasheet %>% filter(origin == "JFK",month == 5)%>%
  ggplot(mapping = aes(x = day, y= humid))+geom_point()+geom_step()

eleven2 = datasheet %>% filter(origin == "JFK",month == 5)%>%
  ggplot(mapping = aes(x = day, y= precip))+geom_point()+
  geom_segment(aes(x= day,xend = day,y=0, yend =precip))

#Analysis 12: connection between dewpoint and temperature

twelve = datasheet %>%filter(origin == "LGA", month == 2)%>% 
  ggplot(mapping = aes(x = day, y = temp))+
  geom_point()+geom_point(aes(x = day, y = dewp,colour = "dewp"))+
  labs(title = "Relationship between Temperature & Dewpoint", 
       x="Day", y = "Dewpoint & Temperature")+
  annotate("rect",xmin = c(9.5,20.5),xmax = c(18.5,29),ymin = c(2,17),
           ymax = c(53,50),alpha = 0.2,colour = "blue",fill = "blue")

#Analysis 13:	Connection between humidity and dewpoint

thirteen = datasheet %>%filter(origin == "JFK", month == 8)%>% 
  ggplot(mapping = aes(x = day, y = humid))+
  geom_point()+geom_point(aes(x = day, y = dewp,colour = "dewp"))+
  labs(title = "Relationship between Humidity & Dewpoint", 
       x="Day", y = "Dewpoint & Humidity")

#Analysis 14: Connection between wind gust and temperature

fourteen = datasheet%>%filter(origin == "JFK",month == 12)%>% 
  ggplot(mapping = aes(x = day, y = temp))+
  geom_point()+geom_point(aes(x = day, y = wind_gust,colour = "wind gust"))+
  labs(title = "Relationship between Temperature and Wind gust",
       x="Day", y = "Wind gust and Temp")+
  annotate("segment",x = 12,xend = 12, y=55,yend = 38,colour ="black",
           size = 2,alpha = 0.5,arrow = arrow())+
  annotate("segment",x = 21.5,xend = 21.5, y=19,yend = 40,colour ="red",
           size = 2,alpha = 0.5,arrow = arrow())

#Analysis 15: Temperature in the whole year

fifteen = ggplot(datasheet, aes(temp,col = "red"))+geom_histogram(bins = 40)+
  facet_wrap(~origin)+annotate("rect",xmin = c(35,25),xmax = c(80,90),
  ymin =c(350,140),ymax = c(475,250),alpha = 0.2,colour = "cyan",fill= "cyan")

datasheet%>%filter(origin == "JFK")%>%summary(temp)
datasheet%>%filter(origin == "LGA")%>%summary(temp)


#Analysis 16: relationship between dewpoint and precipitation

sixteen = datasheet %>%filter(origin == "LGA", month == 10)%>% 
  ggplot(mapping = aes(x = day, y = precip))+geom_point()+geom_step()+
  annotate("segment",x = 7,xend = 7, y=0.10,yend = 0.23,colour ="yellow",
           size = 2,alpha = 0.5,arrow = arrow())

sixteen2 = datasheet %>%filter(origin == "LGA", month == 10)%>% 
  ggplot(mapping = aes(x = day, y = dewp))+geom_point()+
  annotate("segment",x = 7,xend = 7, y=20,yend = 45,colour ="purple",
           size = 2,alpha = 0.5,arrow = arrow())


