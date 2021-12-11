Weather with Transportation
================

## Visualization

``` r
library(knitr)
library(tidyverse)
library(dplyr)
library(tidyverse)
library(data.table)
library(patchwork)

knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

``` r
# Read velocity dataset.   
test_df <- read_csv("./data/test_dt_V1.csv")
# Select data with time range
test_df <- setDT(test_df)
test_df <- test_df[, date := format(as.Date(tpep_pickup_datetime),"%Y-%m-%d")]
test_df <- test_df[date >= "2020-06"][date < "2021-06"]

# Read weather dataset. 
weather_df <- read_csv("./data/manhattan_weather.csv")

# Data clean
test_df <- 
  test_df %>% 
  mutate(date = format(tpep_pickup_datetime, format = '%Y-%m-%d')) %>% 
  mutate(date = as.Date(date))

# Join table
weather_transport_df <-
  left_join(test_df, weather_df, by = "date") %>%
  dplyr::select(trip_distance, type, month, velocity, date, awnd, prcp, snwd, tmax, tmin, tsun) %>% 
  mutate(type = as.factor(type))
```

``` r
# popular of transportation vs month
weather_transport_df %>% 
  mutate(month = as.factor(month)) %>% 
  group_by(type, month) %>% 
  summarise(n_obs = n()) %>% 
  ggplot(aes(x = month, y = n_obs, group = type, fill = type)) + 
    geom_bar(stat = "identity", alpha = 0.5) +
    geom_line(aes(color = type)) +
    facet_grid(.~type) +
    labs(title = "Popular of Transportation According to Month", 
           x = "Month", 
           y = "Trends of Transportation") +
    theme(plot.title = element_text(hjust = 0.5))
```

<img src="weather_taxi_bike_files/figure-gfm/transportation trends among months-1.png" width="90%" />

The trends for people choose to travel outside are generally similar no
matter using bikes or cabs. But it exists a more significant drop for
choosing taxi during the June and less people ride bikes during Dec and
Feb. After analyzing the possible casuals, June is the month for
students within NYU start their Summer Vacations. So the traffic
pressure has a visualized decrease with more people still live in NYU.
And for people, especially students, who still live in NYU, they do not
have the need to commute in a hurry. Also, with the temperature getting
colder and colder, people will prefer more stay at home comparing to
traveling and less possibility for them to riding bikes outside. The
temperature of the weather could be a confounder to transport choice and
month model, but not for special month June.

``` r
# wind speed (mm) vs velocity
weather_transport_df %>% 
  group_by(type, awnd) %>% 
  summarize(v_mean = mean(velocity)) %>% 
  ggplot(aes(x = awnd/10, y = v_mean, group = type, colour = type)) + 
    geom_point(aes(colour = type)) +
    geom_smooth(method = lm) +
    labs(title = "Velocity vs Wind Speed",
           x = "Wind Speed (m/s)", 
          y = "Mean Velocity (m/s)") +
    theme(plot.title = element_text(hjust = 0.5))
```

    ## Warning: Removed 2 rows containing non-finite values (stat_smooth).

    ## Warning: Removed 2 rows containing missing values (geom_point).

<img src="weather_taxi_bike_files/figure-gfm/wind speed(mm) vs velocity-1.png" width="90%" />

It’s interesting to find out the truth that following by the increasing
of wind speed, a linear regression model could be explored between the
velocity of bike riders and wind speed and velocity of bikes is
increasing as well. While at the same time, wind speed does no influence
to taxi drivers.

``` r
# Precipitation(mm) vs velocity
prcp_vel <-
  weather_transport_df %>% 
  group_by(type, prcp) %>% 
  summarize(v_mean = mean(velocity)) %>% 
  ggplot(aes(x = prcp/10, y = v_mean, group = type, colour = type)) + 
  geom_point(aes(colour = type)) +
  geom_smooth(method = lm) +
  labs(title = "Velocity vs Precipitation",
         x = "Precipitation (mm)", 
         y = "Mean Velocity (m/s)") +
  theme(plot.title = element_text(hjust = 0.5))

# Precipitation(mm) vs transportation choice
prcp_trans <-
  weather_transport_df %>% 
  group_by(type, prcp) %>% 
  summarise(n_avg = n()/n_distinct(date)) %>% 
    ggplot(aes(x = prcp/10, y = n_avg, group = type, colour = type)) + 
      geom_point(aes(color = type)) +
      geom_smooth(method = lm) +
      labs(title = "Transport Choice vs Precipitation", 
           x = "Precipitation (mm)", 
           y = "Avg Travels counts per day") +
      theme(plot.title = element_text(hjust = 0.5))
prcp_vel + prcp_trans
```

<img src="weather_taxi_bike_files/figure-gfm/precipitation with velocity and total number of specific transport-1.png" width="90%" />

Velocity could be regarded as one parameter to judge the flow of a
transportation. Obviously from the plots showing the connection between
the velocity and the mode of transport, the velocity of taxi are faster
to bike on average. And the light to moderate rain (&lt;30 mm),
precipitation seems have no impact to the velocity, especially while
taking a look at bike data. That might because though raining will make
road more slippery, it also has washing effect to environment and other
effects. The combination of all effects make velocity nearly stay the
same. Moreover, the more amount of rain is, the slower the traffic it
would be, the less people would choose to travel and it is more likely
to choose taxi while weather condition getting even worse compared to
riding. That might because a heavy rain might possibly affect drivers
and riders visibility, thus decreases the vehicle speed and affects
travel choice.

``` r
# High Temperature vs transportation choice. 
ht_trans <- 
  weather_transport_df %>% 
  group_by(type, tmax) %>% 
  summarise(n_avg = n()/n_distinct(date)) %>% 
  ggplot(aes(x = tmax/10, y = n_avg, group = type, colour = type)) + 
    geom_point(aes(color = type)) +
    geom_smooth() +
    labs(title = "Transit vs High T", 
          x = "High Temperature (°C)", 
          y = "Avg Travel Counts per Day") +
     theme(plot.title = element_text(hjust = 0.5)) 

# High Temperature vs velocity. 
ht_vel <- 
  weather_transport_df %>% 
  group_by(type, tmax) %>% 
  summarise(v_avg = mean(velocity)) %>% 
  ggplot(aes(x = tmax/10, y = v_avg, group = type, colour = type)) + 
    geom_point(aes(color = type)) +
    geom_smooth() +
    labs(title = "High T vs Velocity", 
           x = "High Temperature (°C)", 
           y = "Avg Velocity (m/s)") +
    theme(plot.title = element_text(hjust = 0.5))

ht_trans + ht_vel
```

<img src="weather_taxi_bike_files/figure-gfm/show high T connection-1.png" width="90%" />

``` r
# Low Temperature vs transportation choice. 
lt_trans <- 
  weather_transport_df %>% 
  group_by(type, tmin) %>% 
  summarise(n_avg = n()/n_distinct(date)) %>% 
  ggplot(aes(x = tmin/10, y = n_avg, group = type, colour = type)) + 
    geom_point(aes(color = type)) +
    geom_smooth() +
    labs(title = "Transit vs Low T", 
          x = "Low Temperature (°C)", 
           y = "Avg Travel Counts per Day") +
    theme(plot.title = element_text(hjust = 0.5))

# Low Temperature vs velocity. 
lt_vel <- 
  weather_transport_df %>% 
  group_by(type, tmin) %>% 
  summarise(v_avg = mean(velocity)) %>% 
  ggplot(aes(x = tmin/10, y = v_avg, group = type, colour = type)) + 
    geom_point(aes(color = type)) +
    geom_smooth() +
    labs(title = "Velocity vs Low Temperature", 
          x = "Low Temperature (°C)", 
           y = "Velocity (m/s)") +
    theme(plot.title = element_text(hjust = 0.5)) +
    scale_y_continuous(limits = c(1, 8))

lt_trans + lt_vel
```

<img src="weather_taxi_bike_files/figure-gfm/low temperature connection-1.png" width="90%" />

The distribution and trends of travel choice among taxi and bikes are
distint different. People tend to choose taxi travel plan while in
relatively low temperature and will change to bike preference when there
is significant increase in temperature. People have more possibility to
stay home when meet with extreme high temperature. It could be concluded
that people are more willing to travel outside under the moderate
temperature, the higher, lower or even extreme temperature the weather
is, the less potential likelihood that people will go outside and use
the transport. It also comes an interesting phenomenon that taxi tends
to run faster and bike runs slower with the temperature increase. The
faster of the taxi could because of the decrease of the traffic pressure
and the better road condition. At the same time, snow effect makes
riders drive quicker for getting avoid of staying outside and keep warm.
