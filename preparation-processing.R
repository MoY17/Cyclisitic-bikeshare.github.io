#Data Preparation
#Setting up the environment

library(tidyverse) #For cleaning data
library(dplyr) #Data manipulation
library(skimr) #Data summary
library(lubridate) #Date manipulation

tripdata_202112 <- read_csv("202112-divvy-tripdata.csv") #Dec, 2021
tripdata_202201 <- read_csv("202201-divvy-tripdata.csv") #Jan, 2022
tripdata_202202 <- read_csv("202202-divvy-tripdata.csv") #Feb, 2022
tripdata_202203 <- read_csv("202203-divvy-tripdata.csv") #Mar, 2022
tripdata_202204 <- read_csv("202204-divvy-tripdata.csv") #Apr, 2022
tripdata_202205 <- read_csv("202205-divvy-tripdata.csv") #May, 2022
tripdata_202206 <- read_csv("202206-divvy-tripdata.csv") #Jun, 2022
tripdata_202207 <- read_csv("202207-divvy-tripdata.csv") #Jul, 2022
tripdata_202208 <- read_csv("202208-divvy-tripdata.csv") #Aug, 2022
tripdata_202209 <- read_csv("202209-divvy-publictripdata.csv") #Sep, 2022
tripdata_202210 <- read_csv("202210-divvy-tripdata.csv") #Oct, 2022
tripdata_202211 <- read_csv("202211-divvy-tripdata.csv") #Nov, 2022

# Checking column types of the data frames
str(list(tripdata_202112,tripdata_202201, tripdata_202202))
str(list(tripdata_202203, tripdata_202204, tripdata_202205))
str(list(tripdata_202206, tripdata_202207, tripdata_202208))
str(list(tripdata_202209, tripdata_202210, tripdata_202211))

#Checking if `ride_id` is a primary key
nrow(tripdata_202112) == n_unique(tripdata_202112$ride_id)
nrow(tripdata_202201) == n_unique(tripdata_202201$ride_id)
nrow(tripdata_202202) == n_unique(tripdata_202202$ride_id)
nrow(tripdata_202203) == n_unique(tripdata_202203$ride_id)
nrow(tripdata_202204) == n_unique(tripdata_202204$ride_id)
nrow(tripdata_202205) == n_unique(tripdata_202205$ride_id)
nrow(tripdata_202206) == n_unique(tripdata_202206$ride_id)
nrow(tripdata_202207) == n_unique(tripdata_202207$ride_id)
nrow(tripdata_202208) == n_unique(tripdata_202208$ride_id)
nrow(tripdata_202209) == n_unique(tripdata_202209$ride_id)
nrow(tripdata_202210) == n_unique(tripdata_202210$ride_id)
nrow(tripdata_202211) == n_unique(tripdata_202211$ride_id)

# Joining the datasets
tripdata_all <- tripdata_202112 %>% 
  full_join(tripdata_202201) %>% 
  full_join(tripdata_202202) %>% 
  full_join(tripdata_202203) %>% 
  full_join(tripdata_202204) %>% 
  full_join(tripdata_202205) %>% 
  full_join(tripdata_202206) %>% 
  full_join(tripdata_202207) %>%
  full_join(tripdata_202208) %>%
  full_join(tripdata_202209) %>%
  full_join(tripdata_202210) %>%
  full_join(tripdata_202211)

#Checking if the number of rows of `tripdata_all` is the same
#as the number of unique values of the primary key `ride_id`
247540+103770+115609+284042+371249+634858+769204+823488+785932+701339++558685+337735
nrow(tripdata_all) == n_unique(tripdata_all$ride_id)
write_csv(tripdata_all, "tripdata_all.csv") #exporting the data
tripdata_all <- read_csv("tripdata_all.csv")

#Data Processing(data cleaning)
#Creating a summary statistics of the data

skim_without_charts(tripdata_all)

unique(tripdata_all2$rideable_type) #Finding the type of bikes
unique(tripdata_all2$member_casual) #Finding member types

#Converting `rideable_type` and `member_casual` as factors
tripdata_all2 <- tripdata_all #Creating an alternative data frame for the conversion of data types
tripdata_all2$rideable_type <- as.factor(tripdata_all2$rideable_type)
tripdata_all2$member_casual <- as.factor(tripdata_all2$member_casual)
#Checking the levels of the factors
levels(tripdata_all2$rideable_type)
levels(tripdata_all2$member_casual)

#Checking for rows with `NA` values
#`!is.na(start_station_name)&is.na(start_station_id)`
na_ss_id <- tripdata_all2 %>% 
  filter(!is.na(start_station_name)&is.na(start_station_id))
na_ss_id
##is.na(start_station_name)&!is.na(start_station_id)
na_ss_name <- tripdata_all2 %>% 
  filter(is.na(start_station_name)&!is.na(start_station_id))
na_ss_name
##`!is.na(end_station_name)&is.na(end_station_id)`
na_es_id <- tripdata_all2 %>% 
  filter(!is.na(end_station_name)&is.na(end_station_id))
na_es_id
##`is.na(end_station_name)&!is.na(end_station_id)`
na_es_name <- tripdata_all2 %>% 
  filter(is.na(end_station_name)&!is.na(end_station_id))
na_es_name

#Counting `rideable_type` in start station names with `NA` values
df1_na <- tripdata_all2 %>% 
  filter(is.na(start_station_name)) %>% 
  count(rideable_type, sort = TRUE)
df1_na
#Counting `rideable_type` in end station names with `NA` values
df2_na <- tripdata_all2 %>% 
  filter(is.na(end_station_name)) %>% 
  count(rideable_type, sort = TRUE)
df2_na

#is.na(end_lat)&is.na(end_lng)
na_lat_lng <- tripdata_all2 %>% 
  filter(is.na(end_lat)&is.na(end_lng))
na_lat_lng
###Filtering where `end_station_name` is not `NA`
na_lat_lng %>% 
  filter(!is.na(end_station_name))

#removal of rows with missing values
trip_clean1 <- drop_na(tripdata_all2)
skim_without_charts(trip_clean1)
write_csv(trip_clean1, "trip_clean1.csv")#exporting file

#Creating table for start station names with unique ids
dataframe1 <- trip_clean1 %>% 
  select(start_station_name, start_station_id) %>%
  group_by(start_station_name) %>% 
  summarise(ss_id_count = n_unique(start_station_id)) %>% 
  arrange(desc(ss_id_count))

dataframe1.1 <- trip_clean1 %>% 
  group_by(start_station_name) %>% 
  filter(n_unique(start_station_id)==2) %>% 
  summarise(start_station_id = unique(start_station_id))

dataframe1.2 <- trip_clean1 %>% 
  select(start_station_name, start_station_id) %>% 
  count(start_station_name, start_station_id)

dataframe1.3 <- inner_join(dataframe1.1, dataframe1.2) %>% 
  arrange(desc(n), .by_group = TRUE)

dataframe1.4 <- data.frame(dataframe1.3[seq(1,36,by=2),1:2])

dataframe1.5 <- trip_clean1 %>% 
  group_by(start_station_name) %>% 
  filter(n_unique(start_station_id)==1) %>% 
  summarise(start_station_id = unique(start_station_id))

dataframe1.6 <- full_join(dataframe1.4, dataframe1.5)
start_station_names_ids <- dataframe1.6
##Creating table for station ids with unique names
dataframe2 <- trip_clean1 %>% 
  select(start_station_id, start_station_name) %>%
  group_by(start_station_id) %>% 
  summarise(ss_name_count = n_unique(start_station_name)) %>% 
  arrange(desc(ss_name_count))

dataframe2.1 <- trip_clean1 %>% 
  group_by(start_station_id) %>% 
  arrange(desc(n_unique(start_station_name))) %>% 
  filter(n_unique(start_station_name)!=1) %>%
  summarise(start_station_name = unique(start_station_name))

dataframe2.1.1 <- trip_clean1 %>% 
  select(start_station_id, start_station_name) %>% 
  count(start_station_id, start_station_name)

dataframe2.2 <- trip_clean1 %>% 
  select(start_station_id, start_station_name) %>% 
  count(start_station_id, start_station_name)

dataframe2.3 <- inner_join(dataframe2.1, dataframe2.2) %>% 
  arrange(desc(n), .by_group = TRUE)


trip_clean2 <- left_join(trip_clean1, start_station_names_ids, by = "start_station_name", suffix = c(".x",""))

trip_clean2 <- trip_clean2 %>% 
  select(1:5,14,7:13)
write_csv(trip_clean2, "trip_clean2.csv")#exporting file

#Creating table for end station names with unique ids
dataframe3 <- trip_clean2 %>% 
  select(end_station_name, end_station_id) %>%
  group_by(end_station_name) %>% 
  summarise(es_id_count = n_unique(end_station_id)) %>% 
  arrange(desc(es_id_count))

dataframe3.1 <- trip_clean2 %>% 
  group_by(end_station_name) %>% 
  filter(n_unique(end_station_id)==2) %>% 
  summarise(end_station_id = unique(end_station_id))

dataframe3.2 <- trip_clean2 %>% 
  select(end_station_name, end_station_id) %>% 
  count(end_station_name, end_station_id)

dataframe3.3 <- inner_join(dataframe3.1, dataframe3.2) %>% 
  arrange(desc(n), .by_group = TRUE)

dataframe3.4 <- data.frame(dataframe3.3[seq(1,36,by=2),1:2])

dataframe3.5 <- trip_clean2 %>% 
  group_by(end_station_name) %>% 
  filter(n_unique(end_station_id)==1) %>% 
  summarise(end_station_id = unique(end_station_id))

dataframe3.6 <- full_join(dataframe3.4, dataframe3.5)
end_station_names_ids <- dataframe3.6


#Creating table for end station ids with unique names
dataframe4 <- trip_clean2 %>% 
  select(end_station_id, end_station_name) %>%
  group_by(end_station_id) %>% 
  summarise(es_name_count = n_unique(end_station_name)) %>% 
  arrange(desc(es_name_count))

dataframe4.1 <- trip_clean2 %>% 
  group_by(end_station_id) %>% 
  arrange(desc(n_unique(end_station_name))) %>% 
  filter(n_unique(end_station_name)!=1) %>%
  summarise(end_station_name = unique(end_station_name))

dataframe4.1.1 <- trip_clean2 %>% 
  select(end_station_id, end_station_name) %>% 
  count(end_station_id, end_station_name)

dataframe4.2 <- trip_clean2 %>% 
  select(end_station_id, end_station_name) %>% 
  count(end_station_id, end_station_name)

dataframe4.3 <- inner_join(dataframe4.1, dataframe4.2) %>% 
  arrange(desc(n), .by_group = TRUE)

trip_clean3 <- left_join(trip_clean2, end_station_names_ids, by = "end_station_name", suffix = c(".x",""))
trip_clean3 <- trip_clean3 %>% 
  select(1:7,14,9:13)
head(trip_clean3)
write_csv(trip_clean3, "trip_clean3.csv")#exporting file
skim_without_charts(trip_clean3)

##Conversion of station id columns to factors
trip_clean3$start_station_id  <- as.factor(trip_clean3$start_station_id)
trip_clean3$end_station_id  <- as.factor(trip_clean3$end_station_id)

##Removal of the station name columns
trip_clean4 <- trip_clean3 %>% 
  select(1:4,6,8:13)

##Plotting of the latitudes and longitudes
ggplot(trip_clean4, aes(x=start_lat, y=start_lng))+
  geom_jitter(alpha = 0.4)
ggsave("scatterplot_start_lat_lng.png")#exporting file

ggplot(trip_clean4, aes(x=end_lat, y=end_lng))+
  geom_jitter(alpha = 0.4)
ggsave("scatterplot_end_lat_lng.png")#exporting file

#Deleting outliers
trip_clean5 <- trip_clean4 %>% 
  filter(end_lat!=0) %>% 
  filter(between(start_lat, 41.5, 42.5))
head(trip_clean5)

#Addition of variables
##Adding the duration column which is the difference between `ended_at` and `start_at`
trip_new1 <- trip_clean5 %>% 
  mutate(ride_length = int_length(interval(started_at, ended_at)),
         .after = ended_at) %>% 
  filter(ride_length >= 0)
write_csv(trip_new1, "trip_new1.csv")
##Addition of two new variables day and month
trip_data <- trip_new1 %>% 
  mutate(day_of_week = wday(started_at, label = TRUE, abbr = FALSE),
         month_of_year = month(started_at, label = TRUE, abbr = FALSE))
write_csv(trip_data, "trip_data.csv")#exporting file
skim_without_charts(trip_data)
