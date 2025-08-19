# clear enevironment
rm(list = ls())

# Attach packages
library(tidyverse)
library(palmerpenguins)
library(lubridate) # help us work with dates

# Date wrangling refresher
# 1. only include penguins at Briscoe and Dream Islands
# 2. Remove the year and sex variables
# 3. add a new column called body_mass_kg with penguin mass convertred from grams to kg 
# 4. rename the island varaible to location

#names(penguins)
# group_by and summarize is used when you need to summarize something

penguins %>% 
filter (island %in% c("Briscoe", "Dream")) %>% 
  select(-year, -sex) %>% 
mutate("body_mass_kg" = body_mass_g / 1000) %>% 
  rename(location = island)

# 1. Limit to only Adelie Penguins
# 2. Remove any observation whre flipper_length_mm is NA
# 3. Group the data by sex
# 4. Find the mean, standard deviation, and sample size (n()) of flipper length for male and females

penguins %>% 
  filter(species == "Adelie") %>% # can add the second filter to this row as well
  filter(!is.na(flipper_length_mm),
         ! is.na(sex)) %>% # remove rows that are not NA
group_by(sex) %>% 
  summarise(mean_size = mean(flipper_length_mm),
            sd_size = sd(flipper_length_mm),
            sample_size = n())

# Practice with Joins

animals <- data.frame(
  stringsAsFactors = FALSE,
          location = c("lagoon", "bluff", "creek", "oaks", "bluff"),
           species = c("bobcat", "coyote", "fox", "squirrel", "bobcat"),
          maturity = c("adult", "juvenile", "adult", "juvenile", "adult")
)


sites <- data.frame(
           
  stringsAsFactors = FALSE,
          location = c("beach", "lagoon", "bluff", "oaks"),
    full_site_name = c("Goleta Beach","UCSB Lagoon",
                       "Ellwood Mesa","Fremont Campground"),
      jurisdiction = c("SB City", "UCSB", "SB City", "USFS")
)

# Mutating joins
# practice with full_join
# keeps all rows and adds all columns in this case 5 total
full_join(animals, sites)

# left_join()
left_join(animals, sites)


# right_join()
left_join(animals, sites)

# inner_join()
inner_join(animals, sites)


# Filtering Joins

semi_join(aniamls, sites)
# same as 
#animals %>% 
  #filter(location %in% sites$location)

anti_join(sites , animals)

#animals %>% 
  #filter(!location %in% sites$location)

# Practice with lubridate examples:

my_date <- "03-15-1998"
lubridate::mdy(my_date) # telling lubridate the date data is in mdy()= month, day, and year,  fixed date to ISO 8601


my_date <- "08-Jun-1974"
lubridate::dmy(my_date) #dmy() = date, month, year

my_date <- "19610518"
lubridate::ymd(my_date) # ymd = year, month, and day

# what happens if we give lubridate a date that doesnt make sense?
lubridate::mdy("1942-08-30")

lubridate::dmy("09/12/84") # comp doesnt know this is not correct, important to know date format

# working with date-times
time <- "2020-08-12 11:18"
time <- ymd_hm(time) # or in (time, tz = " America/Los_Angeles"), need to look up lubridate time zone names

time # Note that the default is UTC

# convert to PDT
with_tz(time, "America/Los_Angeles") # did not add a time zone it convert a time zone to pacific daylight time

# extra infor from dates
week(time)
year(time)
day(time)

Sys.time() # tells you current time in comp

start_time Sys.time()

end_time <- Sys.time()

end_time <- start_time
