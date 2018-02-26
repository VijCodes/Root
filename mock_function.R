# Mock function

library(dplyr)

setwd("C:/Users/katta/Desktop/data_analyst_work_sample")
sample_trip = readRDS('data/sample_trip.rds')

get_acceleration = function(time_vect, speed_vect, lag = 1) {
  dt = diff(time_vect, lag = lag)
  dv = diff(speed_vect, lag = lag)
  accel = dv/dt
  return(accel)
}
fix_speed = function(time_vect, speed_vect, 
                     accuracy_vect, accuracy_thresh = 25) {
  bad_accuracy = accuracy_vect > accuracy_thresh
  start_bad = min(which(bad_accuracy))
  end_bad = max(which(bad_accuracy))
  new_speed = approx(x = time_vect[c(start_bad-1, end_bad+1)],
                     y = speed_vect[c(start_bad-1, end_bad+1)],
                     xout = time_vect[bad_accuracy])$y
  speed_vect[bad_accuracy] = new_speed
  return(speed_vect)
}

sample_trip = sample_trip %>% 
  mutate(smooth_speed = fix_speed(timestamp, speed, accuracy))

summary = sample_trip %>%
  summarize(max_speed = max(speed),
            max_accel_lag1 = get_acceleration(timestamp, smooth_speed, lag = 1) %>% max,
            max_accel_lag2 = get_acceleration(timestamp, smooth_speed, lag = 2) %>% max,
            max_accel_lag3 = get_acceleration(timestamp, smooth_speed, lag = 3) %>% max)

print(summary)

