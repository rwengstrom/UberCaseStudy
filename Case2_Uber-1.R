# BUS AN 511: Programming Essentials
# Case 2: Uber

library(dplyr)
library(broom)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(stargazer)
library(data.table)
library(lfe)
library(readxl)
library(chron)


getwd()
setwd("C:/Data") # set working directory
rm(list = ls()) # clear environment
cat("\014") # clear console

# load data set into data frame
df <- data.table(read_excel("619702-XLS-ENG.xlsx", 3))

head(df)
str(df)
attach(df) 

# variable creation
df$date <- as.Date(period_start)
df$day <- wday(df$date)
df$time <- strftime(period_start, format="%H:%M:%S", tz=attr(time, "tzone")) # must set time zone of data set
df$total_trips <- trips_pool + trips_express_pool
df$cost_per_trip <- total_driver_payout / df$total_trips
df$total_single_matches <- total_matches - total_double_matches # those pool trips with one other person matched
df$single_rider <- df$total_trips - total_matches # the pool trips with only a single rider -- lost opportunity for multiple riders
df$single_rider_rate <- df$single_rider / df$total_trips 
df$match_rate <- total_matches / df$total_trips
df$single_match_rate <- df$total_single_matches / df$total_trips
df$double_match_rate <- total_double_matches / df$total_trips

detach(df)
attach(df)

summary <- df %>%  group_by() %>%  summarise_if(is.numeric, mean)
summary$wait_time <- "all"
summary <- rbind(summary,df %>% group_by(wait_time) %>% summarise_if(is.numeric, mean))
summary <- summary[, c("total_trips", "trips_pool", "trips_express_pool", "cost_per_trip", "single_rider_rate", "match_rate", "single_match_rate", "double_match_rate", "rider_cancellations")]
summary <- rbind(summary, summary[2,] - summary[3,])
summary$wait_time <- c("All", "2 mins", "5 mins", "Difference")
summary <- summary[, c("wait_time", "total_trips", "trips_pool", "trips_express_pool", "cost_per_trip", "single_rider_rate", "match_rate", "single_match_rate", "double_match_rate", "rider_cancellations")]

stargazer(summary,type = "text", summary=FALSE, flip=TRUE,colnames = FALSE,out = "summary_stats.txt")

p1 <- ggplot(df, aes(x = total_trips, fill = wait_time)) +
  geom_density(alpha = 0.4) + 
  ggtitle("Total Ride-Sharing Trips")
p2 <- ggplot(df, aes(x = trips_pool, fill = wait_time)) + 
  geom_density(alpha = 0.4) + theme_minimal() + 
  ggtitle("POOL Trips")
p3 <- ggplot(df, aes(x = trips_express_pool, fill = wait_time)) + 
  geom_density(alpha = 0.4) + theme_minimal() + 
  ggtitle("Express POOL Trips")
p4 <- ggplot(df, aes(x = rider_cancellations, fill = wait_time)) + 
  geom_density(alpha = 0.4) + theme_minimal() + 
  ggtitle("Rider Cancellations")
p5 <- ggplot(df, aes(x = single_rider, fill = wait_time)) +
  geom_density(alpha = 0.4) + theme_minimal() + 
  ggtitle("Ride-Sharing with 1 Rider")
p6 <- ggplot(df, aes(x = cost_per_trip, fill = wait_time)) +
  geom_density(alpha = 0.4) + theme_minimal() + 
  ggtitle("Avg Cost / Trip") 
p7 <- ggplot(df, aes(x = total_driver_payout, y = single_rider_rate, fill = wait_time)) +
  geom_point() + 
  geom_smooth(method=lm) + theme_minimal() + 
  ggtitle("Total Driver Payout vs. Single Rider Rate")


plotlist = list(p1,p2,p3,p4,p5,p6)   
p <- grid.arrange(grobs=plotlist,ncol=2)

ggsave("total_trips.png", p1, width = 6, height = 4)
ggsave("pool_trips.png", p2, width = 6, height = 4)
ggsave("express_pool_trips.png", p3, width = 6, height = 4)
ggsave("cancelled.png", p4, width = 6, height = 4)
ggsave("one_rider.png", p5, width = 6, height = 4)
ggsave("avg_cost_per_trip.png", p6, width = 6, height = 4)
ggsave("payout_by_single_rider_rate.png", p7, width = 6, height = 4)
ggsave("all_density_plots.png",p, width = 8, height = 8)

t1 <- t.test(df[wait_time=="5 mins"]$total_trips, df[wait_time=="2 mins"]$total_trips)
# p-val 0.3973 - not significant
t2 <- t.test(df[wait_time=="5 mins"]$trips_pool, df[wait_time=="2 mins"]$trips_pool)
# p-val 0.02127 - significant
t3 <- t.test(df[wait_time=="5 mins"]$trips_express_pool, df[wait_time=="2 mins"]$trips_express_pool)
# p-val 0.02957 - significant
t4 <- t.test(df[wait_time=="5 mins"]$rider_cancellations, df[wait_time=="2 mins"]$rider_cancellations)
# p-val 0.0066 - significant
t5 <- t.test(df[wait_time=="5 mins"]$cost_per_trip, df[wait_time=="2 mins"]$cost_per_trip)
# p-val 0.00074 - significant
t6 <- t.test(df[wait_time=="5 mins"]$match_rate, df[wait_time=="2 mins"]$match_rate)
# p-val 0.01312 - significant
t7 <- t.test(df[wait_time=="5 mins"]$double_match_rate, df[wait_time=="2 mins"]$double_match_rate)
# p-val 0.01439 - significant

list <- list(t1, t2, t3, t4, t5, t6, t7)

# Take the list of ttests and reorganize them into one vector
ttests <- t(sapply(list, function(x) {
  c(x$estimate[1],
    x$estimate[2],
    ci.lower = x$conf.int[1],
    ci.upper = x$conf.int[2],
    p.value = x$p.value)
}))
rownames(ttests) <- c("total_trips", "trips_pool", "trips_express_pool", "rider_cancellations", "cost_per_trip", "match_rate", "double_match_rate")
options("scipen"=100, "digits"=4)
View(ttests)

p8 <- ggplot(df, aes(cost_per_trip, color=wait_time)) +
  stat_ecdf(geom="step", pad=TRUE) + ggtitle("CDF of Cost Per Trip")
p9 <- ggplot(df, aes(total_trips, color=wait_time)) +
  stat_ecdf(geom="step", pad=TRUE) + ggtitle("CDF of Total Trips")
p10 <- ggplot(df, aes(trips_pool, color=wait_time)) +
  stat_ecdf(geom="step", pad=TRUE) + ggtitle("CDF of POOL Trips")
p11 <- ggplot(df, aes(trips_express_pool, color=wait_time)) +
  stat_ecdf(geom="step", pad=TRUE) + ggtitle("CDF of Express POOL Trips")
p12 <- ggplot(df, aes(rider_cancellations, color=wait_time)) +
  stat_ecdf(geom="step", pad=TRUE) + ggtitle("CDF of Rider Cancellations")
p13 <- ggplot(df, aes(match_rate, color=wait_time)) +
  stat_ecdf(geom="step", pad=TRUE) + ggtitle("CDF of Match Rate")
p14 <- ggplot(df, aes(double_match_rate, color=wait_time)) +
  stat_ecdf(geom="step", pad=TRUE) + ggtitle("CDF of Double Match Rate")

plotlist2 = list(p8,p9,p10,p11,p12,p13)   
p <- grid.arrange(grobs=plotlist2,ncol=2)

# Regression 
# Initial Results - REGULAR
r1 <- felm(data = df, (total_trips) ~ treat)
r2 <- felm(data = df, (trips_pool) ~ treat)
r3 <- felm(data = df, (trips_express_pool) ~ treat)
r4 <- felm(data = df, (rider_cancellations) ~ treat)
r5 <- felm(data = df, (cost_per_trip) ~ treat)
r6 <- felm(data = df, match_rate ~ treat)
r7 <- felm(data = df, double_match_rate ~ treat)
r8 <- felm(data = df, (single_rider) ~ treat)
stargazer(r1,r2,r3,r4,r5,r6,r7,r8,type = "text",title = "Initial Results - Trips/Cancellations/Costs/Single Rider",
          dep.var.labels = c('Total Trips', 'POOL Trips', 'Express POOL Trips', 'Cancellations', 'Cost/Trip', 'Match Rate', 'Double Match Rate', 'Single Rider'),
          out = "initial_results.txt")

# Initial Results - LOG
r1 <- felm(data = df, log(total_trips) ~ treat)
r2 <- felm(data = df, log(trips_pool) ~ treat)
r3 <- felm(data = df, log(trips_express_pool) ~ treat)
r4 <- felm(data = df, log(rider_cancellations) ~ treat)
r5 <- felm(data = df, log(cost_per_trip) ~ treat)
r6 <- felm(data = df, match_rate ~ treat)
r7 <- felm(data = df, double_match_rate ~ treat)
r8 <- felm(data = df, log(single_rider) ~ treat)
stargazer(r1,r2,r3,r4,r5,r6,r7,r8,type = "text",title = "Initial Log Results - Trips/Cancellations/Costs/Single Rider",
          dep.var.labels = c('Total Trips', 'POOL Trips', 'Express POOL Trips', 'Cancellations', 'Cost/Trip', 'Match Rate', 'Double Match Rate', 'Single Rider'),
          out = "initial_logs_results.txt")

# Commute-Only Results - REGULAR
r1 <- felm(data = df[commute=="TRUE"], (total_trips) ~ treat)
r2 <- felm(data = df[commute=="TRUE"], (trips_pool) ~ treat)
r3 <- felm(data = df[commute=="TRUE"], (trips_express_pool) ~ treat)
r4 <- felm(data = df[commute=="TRUE"], (rider_cancellations) ~ treat)
r5 <- felm(data = df[commute=="TRUE"], (cost_per_trip) ~ treat)
r6 <- felm(data = df[commute=="TRUE"], match_rate ~ treat)
r7 <- felm(data = df[commute=="TRUE"], double_match_rate ~ treat)
r8 <- felm(data = df[commute=="TRUE"], (single_rider) ~ treat)
stargazer(r1,r2,r3,r4,r5,r6,r7,r8,type = "text",title = "Commute-Only Results - Trips/Cancellations/Costs/Single Rider",
          dep.var.labels = c('Total Trips', 'POOL Trips', 'Express POOL Trips', 'Cancellations', 'Cost/Trip', 'Match Rate', 'Double Match Rate', 'Single Rider'),
          out = "commute_only_results.txt")

# Commute-Only Results - LOG
r1 <- felm(data = df[commute=="TRUE"], log(total_trips) ~ treat)
r2 <- felm(data = df[commute=="TRUE"], log(trips_pool) ~ treat)
r3 <- felm(data = df[commute=="TRUE"], log(trips_express_pool) ~ treat)
r4 <- felm(data = df[commute=="TRUE"], log(rider_cancellations) ~ treat)
r5 <- felm(data = df[commute=="TRUE"], log(cost_per_trip) ~ treat)
r6 <- felm(data = df[commute=="TRUE"], match_rate ~ treat)
r7 <- felm(data = df[commute=="TRUE"], double_match_rate ~ treat)
r8 <- felm(data = df[commute=="TRUE"], log(single_rider) ~ treat)
stargazer(r1,r2,r3,r4,r5,r6,r7,r8,type = "text",title = "Commute-Only Log Results - Trips/Cancellations/Costs/Single Rider",
          dep.var.labels = c('Total Trips', 'POOL Trips', 'Express POOL Trips', 'Cancellations', 'Cost/Trip', 'Match Rate', 'Double Match Rate', 'Single Rider'),
          out = "commute_only_logs_results.txt")

# NonCommute-Only Results - REGULAR
r1 <- felm(data = df[commute=="FALSE"], (total_trips) ~ treat)
r2 <- felm(data = df[commute=="FALSE"], (trips_pool) ~ treat)
r3 <- felm(data = df[commute=="FALSE"], (trips_express_pool) ~ treat)
r4 <- felm(data = df[commute=="FALSE"], (rider_cancellations) ~ treat)
r5 <- felm(data = df[commute=="FALSE"], (cost_per_trip) ~ treat)
r6 <- felm(data = df[commute=="FALSE"], match_rate ~ treat)
r7 <- felm(data = df[commute=="FALSE"], double_match_rate ~ treat)
r8 <- felm(data = df[commute=="FALSE"], (single_rider) ~ treat)
stargazer(r1,r2,r3,r4,r5,r6,r7,r8,type = "text",title = "NonCommute-Only Results - Trips/Cancellations/Costs/Single Rider",
          dep.var.labels = c('Total Trips', 'POOL Trips', 'Express POOL Trips', 'Cancellations', 'Cost/Trip', 'Match Rate', 'Double Match Rate', 'Single Rider'),
          out = "noncommute_only_results.txt")

# NonCommute-Only Results - LOG
r1 <- felm(data = df[commute=="FALSE"], log(total_trips) ~ treat)
r2 <- felm(data = df[commute=="FALSE"], log(trips_pool) ~ treat)
r3 <- felm(data = df[commute=="FALSE"], log(trips_express_pool) ~ treat)
r4 <- felm(data = df[commute=="FALSE"], log(rider_cancellations) ~ treat)
r5 <- felm(data = df[commute=="FALSE"], log(cost_per_trip) ~ treat)
r6 <- felm(data = df[commute=="FALSE"], match_rate ~ treat)
r7 <- felm(data = df[commute=="FALSE"], double_match_rate ~ treat)
r8 <- felm(data = df[commute=="FALSE"], log(single_rider) ~ treat)
stargazer(r1,r2,r3,r4,r5,r6,r7,r8,type = "text",title = "NonCommute-Only Log Results - Trips/Cancellations/Costs/Single Rider",
          dep.var.labels = c('Total Trips', 'POOL Trips', 'Express POOL Trips', 'Cancellations', 'Cost/Trip', 'Match Rate', 'Double Match Rate', 'Single Rider'),
          out = "noncommute_only_logs_results.txt")
check <- df[treat=="FALSE"]

dev.off() # clear plots
rm(list = ls()) # clear environment
cat("\014") # clear console