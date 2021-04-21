#install.packages("tidyverse")
#library(tidyverse)
#first some play around with the participants_data
urlfile = "https://raw.githubusercontent.com/CWWhitney/teaching_R/master/participants_data.csv"
participants_data <- read.csv(url(urlfile))
head(participants_data)
head(participants_data,n = 4)
names(participants_data)
participants_data$age
mutate(participants_data, 
       commute = 
         ifelse(km_home_to_office > 10, 
                "commuter", "local"))
summarize(participants_data,
          mean(years_of_study),
          median(letters_in_first_name))
participants_data %>% 
  group_by(gender) %>% 
  summarize(mean(days_to_email_response), 
            median(letters_in_first_name), 
            max(years_of_study))
#dplyr####
#install.packages("dplyr")
#install.packages("ggplot2")
#library(dplyr)
#library(ggplot2)
#elect: carat and price
diamonds$carat
diamonds$price

#filter: only where carat is > 0.5
filter(diamonds,carat >0.5 )
#rename: rename price as cost
dia<-diamonds #crate new varable to change colum name
dia<-rename(diamonds, cost=price)#erst der neue Name dann der alte
#mutate: create a variable with ‘expensive’ if greater than mean of cost and ‘cheap’ otherwise

dia<-mutate(dia,#neue Spalte mit dem Namen "name" und wenn Preis des Diamanten>mean ist dann expensive sonst cheap
       status=ifelse(cost>mean(cost),#aber keine echte spalte kann nicht drauf zugreifen 
        "expensive","cheap"))
#group_by: split into cheap and expensive
dia%>%
  group_by(status)

#summarize: give some summary statistics of your choice

summarize(dia,
          mean(cost),
          mean(carat))

dia %>% 
  group_by(status) %>% #mean & median of the group cheap and expensive 
  summarize(mean(cost), 
            median(carat))

#plot####
ggplot(data = diamonds, 
       aes(x = color, 
           y = price, 
           alpha = I(0.1))) + 
  geom_boxplot()



