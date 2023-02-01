#Setting up environment
library(RColorBrewer) #Color Palettes
library(tidyverse) #For cleaning data
library(dplyr) #Data manipulation
library(skimr) #Data summary
library(lubridate) #Date manipulation
library(ggrepel) #Plot labeling
library(rmarkdown) #documentation
library(png) #reading png files

#ggpattern installation
# install.packages('scales')
# install.packages('grid')
# install.packages('glue')
# install.packages('rlang')
# install.packages('gridGeometry')
# install.packages('sf')
# install.packages('png')
# install.packages('magick')
# install.packages("remotes")
# install.packages("memoise")
remotes::install_github("coolbutuseless/ggpattern")
library(ggpattern) #for fill pattern

#Setting up the data
trip_data <- read_csv("trip_data.csv")
x <- c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday")
y <- c("January", "February", "March", "April", "May", "June",
       "July", "August", "September", "October", "November", "December")
trip_data$rideable_type <- as.factor(trip_data$rideable_type)
trip_data$member_casual <- ordered(trip_data$member_casual, levels = c("member", "casual"))
trip_data$start_station_id <- as.factor(trip_data$start_station_id)
trip_data$end_station_id <- as.factor(trip_data$end_station_id)
trip_data$day_of_week <- ordered(trip_data$day_of_week, levels = x)
trip_data$month_of_year <- ordered(trip_data$month_of_year, levels = y)
skim_without_charts(trip_data)

###Testing codes from the project guide
aggregate(trip_data$ride_length ~ trip_data$day_of_week + trip_data$member_casual, FUN = mean)
trip_data %>% 
  group_by(day_of_week, member_casual) %>%  #groups by usertype and weekday
  summarise(number_of_rides = n()							#calculates the number of rides and average duration 
            ,average_duration = mean(ride_length)) %>% 		# calculates the average duration
  arrange(day_of_week, member_casual)
#Summary of the cleaned dataset(can be included at the end of data processing)
summary_clean <- skim_without_charts(trip_data)

#Summary per membership type
trip_data %>% 
  group_by(member_casual) %>% 
  summarise(mean = mean(ride_length), median = median(ride_length),
          min = min(ride_length), max = max(ride_length))

#Number of rides

theme1 <- theme(axis.text = element_text(color = "#2B1703"),
                legend.direction = "vertical",
                legend.position = "right",
                legend.background = element_rect(fill = "#595959", linetype = "solid", linewidth = 1.4, color = "gray0"),
                legend.text = element_text(color = "#F0F0F0", size = 7.5),
                legend.title = element_text(color = "#F0F0F0", size = 10),
                legend.box.spacing = unit(12.5, "pt"),
                legend.justification = c(1,1),
                legend.key.size = unit(18, "pt"),
                plot.background = element_rect(fill = "aliceblue"),
                panel.background = element_rect(fill = "#BFBFBF",linetype = "solid", linewidth = 1.5, color = "gray0"),
                panel.spacing = unit(10,"pt"),
                plot.margin = margin(t=17.5, r=17.5, b=17.5, l=17.5, unit="pt"),
                plot.title = element_text(face = "bold", size = 17.5)
)
ggplot(trip_data, aes(x = member_casual, fill = rideable_type))+
  geom_bar(width = 0.8)+
  scale_fill_brewer(type = "qual",
                    palette = "Set2",
                    labels = c("classic", "docked", "electric"))+
  labs(title = "Number of rides per membership type",
       subtitle = "partitioned by bike types",
       x = "Membership type",
       y = "Count",
       fill = "bike types",
       caption = "Data from Lyft Bikes and Scooters, LLC"
  )+
  theme1+
  annotate("text", x = 2,
           y = 3000000,
           label = "no docked bike rides",
           size = 4)+
  annotate("segment", xend = 1.25,
           yend = 2000000,
           x = 1.8,
           y = 3000000,
           linewidth = 0.9,
           color = "#595959",
           arrow = arrow(length = unit(5, "pt"), type = "closed")
  )

#Mean ride duration every day(member vs casual)
trip_data_summ1 <- trip_data %>% 
  group_by(day_of_week, member_casual) %>% 
  summarise(mean_ride_length = mean(ride_length))

theme2 <- theme(axis.text.x = element_text(angle = 50, margin = margin(t=14, r=10, b=0, l=0, unit="pt")),
                legend.key = element_rect(linewidth = 1, linetype = "solid", color = "#F0F0F0")
                )

ggplot(trip_data_summ1, aes(x = day_of_week, y = mean_ride_length, fill = mean_ride_length, pattern = member_casual))+
  geom_col_pattern(position = "dodge", pattern_density =0.3)+
  scale_fill_distiller(type = "seq", palette = "PuBu")+
  labs(title = "Mean ride duration by day",
       subtitle = "measured in seconds",
       x = "Day of the week",
       y = "Ride length",
       fill = "mean ride time",
       pattern = "membership type",
       caption = "Data from Lyft Bikes and Scooters, LLC"
  )+
  #geom_smooth(aes(group = member_casual),method = lm, formula = y ~ splines::bs(x, 3), se = FALSE, color = "tomato1")+
  theme1+
  theme2

#Mean number of rides every day
trip_data_summ2 <- trip_data %>% 
  mutate(day_date_fac = as.factor(date(started_at))) %>% 
  group_by(day_of_week, member_casual) %>% 
  summarise(mean_count = n()/n_distinct(day_date_fac))

ggplot(trip_data_summ2, aes(x = day_of_week, y = mean_count, fill = mean_count, pattern = member_casual))+
  geom_col_pattern(position = "dodge", pattern_density =0.3)+
  scale_fill_distiller(type = "seq", palette = "OrRd", direction = 1)+
  labs(title = "Mean count of rides by day",
       x = "Day of the week",
       y = "Count",
       fill = "mean count",
       pattern = "membership type",
       caption = "Data from Lyft Bikes and Scooters, LLC"
  )+
  theme2+
  theme1

#Mean ride duration every month
##subject for consideration
trip_data_summ3 <- trip_data %>% 
  group_by(month_of_year, member_casual) %>% 
  summarise(mean_ride_length = mean(ride_length))

ggplot(trip_data_summ3, aes(x = month_of_year, y = mean_ride_length, fill = mean_ride_length, pattern = member_casual))+
  geom_col_pattern(position = "dodge")+
  scale_fill_distiller(type = "seq", palette = "PuBu")+
  labs(title = "Mean ride duration by month",
       subtitle = "measured in seconds",
       x = "Month of the year",
       y = "Ride length",
       fill = "mean ride time",
       pattern = "membership type",
       caption = "Data from Lyft Bikes and Scooters, LLC"
  )+
  theme2+
  theme1+
  annotate("segment", x = 9.2, y = 1308, 
           xend = 11.2, yend = 1035, 
           color = "darkorange2", linewidth = 1.5,
           arrow = arrow(length = unit(5, "pt"), type = "closed"))+
  annotate("rect",
           xmin = 2.85, xmax = 5.57,
           ymin = 0, ymax = 1750,
           alpha = 0.2, color = "springgreen2",
           linewidth = 1.5)

#Number of rides every month
trip_data_summ4 <- trip_data %>% 
  group_by(month_of_year, member_casual) %>% 
  summarise(count_by_m = n())
ggplot(trip_data_summ4, aes(x = month_of_year, y = count_by_m, fill = count_by_m, pattern = member_casual))+
  geom_col_pattern(position = "dodge")+
  scale_fill_distiller(type = "seq", palette = "OrRd", direction = 1)+
  labs(title = "Total count of rides by month",
       subtitle = "from 2021-12-01 up to 2022-11-30",
       x = "Month of the year",
       y = "Count",
       fill = "count",
       pattern = "membership type",
       caption = "Data from Lyft Bikes and Scooters, LLC"
  )+
  theme2+
  theme1+
  geom_smooth(aes(group = member_casual),method = lm,
              formula = y ~ splines::bs(x, 3),
              se = FALSE,
              color = "yellow", alpha = 0.8)


#data for tableau
trip_data_2 <- trip_data %>% 
  select(ride_id, start_station_id, end_station_id, start_lat, start_lng, end_lat, end_lng, member_casual)

trip_data_2_summ <- trip_data_2 %>% 
  group_by(start_station_id) %>% 
  summarise(n = n()) %>% 
  arrange(desc(n))
print(trip_data_2_summ, n=10)

trip_data_tableau <- trip_data_2 %>% 
  filter(start_station_id %in% as.character(as.matrix(trip_data_2_summ[1:10,1])))
n_unique(trip_data_tableau$start_station_id)

write_csv(trip_data_tableau, "trip_data_tableau.csv")

#second data for tableau

trip_data_tableau2 <- trip_data %>% 
  group_by(start_station_id, member_casual) %>% 
  summarise(id_count = n(), initial_lat = mean(start_lat), initial_lng = mean(start_lng))
write_csv(trip_data_tableau2, "trip_data_tableau2.csv")

trip_data_tableau3 <- trip_data %>% 
  group_by(start_station_id, end_station_id, member_casual) %>% 
  summarise(id_count = n(),
            initial_lat = mean(start_lat),
            initial_lng = mean(start_lng),
            final_lat = mean(end_lat),
            final_lng = mean(end_lng))

write_csv(trip_data_tableau3, "trip_data_tableau3.csv")
#trip_data_tableau3 was considered to use in tableau renaming it as trip_data_agg