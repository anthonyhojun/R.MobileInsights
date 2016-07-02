###Introduction

#Loading libraries
library(jsonlite)
library(dplyr)
library(ggplot2)
library(tidyr)
library(ggthemes)

runOG <- tbl_df(fromJSON("/Users/antbae/OneDrive/R/Projects/Archive/SR/Datasets/20151218_AWS/combi.log", flatten=TRUE))
?fromJSON
### REMOVE UNNECESSARY COLUMNS NO ECHO      

run <- runOG %>%
  select( 
    app_version,
    game,
    type,
    parameters.name, 
    user,
    parameters.attributes.swrve.install_date,
    time
    )

run <- 
   run %>%
  mutate(
   user  = rank(user, ties.method="min")
  ) %>%
  rename(install.date = parameters.attributes.swrve.install_date,
         event = parameters.name)

#remove mentions of grepl
run <-
run %>%
  filter(!grepl("Swrve", event))

#Initial observations
head(run)
str(run)

###Data cleaning

#Change a few columns to factors
run <- 
run %>%
  mutate(
    game = ifelse(game == "3368", "iOS", "Android")
  ) %>%
  mutate_each(funs(as.factor),
            c(app_version,
              game,
              type,
              event)
  )

#Map install dates
matchrun <-
run %>%
  distinct(user, install.date) %>%
  filter(!is.na(install.date)) %>%
  select(user, install.date)

#Mapping to original dataset
run$installed <- matchrun$install.date[match(run$user, matchrun$user)]

#Verification of install.date and installed
run %>%
  select(user, installed, install.date) %>%
  filter(!is.na(install.date)) %>%
  distinct(user, installed) %>%
  print(n=50)

#Drop the old column 'install.date' and change the new column 'installed' to a date class. Filter dates to Dec 1 ~ Dec
run <-
  run %>%
  select(-install.date) %>%
  mutate(
    installed = as.Date(installed, format = "%Y%m%d")
  )

#Change time from unix time

#Convert unix to LA time
run <- 
run %>% 
  mutate_each(funs(as.POSIXct(run$time/1000, origin="1970-01-01", tz="AMERICA/LOS_ANGELES")), time)

#Separate date and time 
run <-
run %>%
  separate(time, c("date", "time"), sep = " ")

#Make date and time a date class
run$date <- as.Date(run$date)
run$installed <- as.Date(run$installed, format = "%Y%m%d")

#Filter only marketing days ECHO OFF 
run <-
  run %>%
  filter(installed >= "2015-12-05" & installed <= "2015-12-10")

### Retention

#Create columns marking if users are 0 or 1 for logging in that day
retentionOG <-
run %>% 
  mutate(
    day0 = as.integer(ifelse(date - installed == 0, 1, 0)),
    day1 = as.integer(ifelse(date - installed == 1, 1, 0)),
    day2 = as.integer(ifelse(date - installed == 2, 1, 0)),
    day3 = as.integer(ifelse(date - installed == 3, 1, 0)),
    day4 = as.integer(ifelse(date - installed == 4, 1, 0)),
    day5 = as.integer(ifelse(date - installed == 5, 1, 0)),
    day6 = as.integer(ifelse(date - installed == 6, 1, 0)),
    day7 = as.integer(ifelse(date - installed == 7, 1, 0))
  ) 
 
#Using summarise_each sum collapse the rows based on the installation date of user
retention <-  
retentionOG %>%
  group_by(installed) %>%
  distinct(user, date) %>%
  summarise_each(funs(sum), contains("day"))

#Change to percentages 
retention.p <-
  retention %>%
  mutate(
    d0 = round(day0 / day0, 2),
    d1 = round(day1 / day0, 2),
    d2 = round(day2 / day0, 2),
    d3 = round(day3 / day0, 2),
    d4 = round(day4 / day0, 2),
    d5 = round(day5 / day0, 2),
    d6 = round(day6 / day0, 2),
    d7 = round(day7 / day0, 2)
  ) %>%
  select(installed, day0, d1:d7)

#Changing nan to 0s. a[is.nan(a)] <- 0 doesn't work with dataframes
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

retention.p[is.nan(retention.p)] <- 0

#Plotting
retention.plot <-
retention.p %>%
  gather(day, retention, d1:d7) %>%
  arrange(installed, day)

#Bar graph
ggplot(retention.plot, aes(x = day, y = retention, label = paste(100*(retention), '%'))) +
  geom_bar(stat = "identity", fill = "light blue", color = "black") +
  facet_grid(~installed) +
  theme_few() +
  geom_text(vjust = 1.7, size = 2) +
  ggtitle("Retention %") +
  ylab("") +
  xlab("") 

#Line graph
ggplot(retention.plot, aes(x = day, y = retention, color = as.factor(installed), label = paste(100*retention, '%'), group = installed)) +
  geom_point(size = 3) +
  geom_line(size = .8) +
  theme_few() +
  scale_fill_few() +
  geom_text(size = 3, vjust = -1.5) +
  ggtitle("Retention %") +
  ylab("") +
  xlab("") +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

#Retention by platform and use additional parameter group_by for game
retention.by.platform <-  
  retentionOG %>%
  group_by(installed, game) %>%
  distinct(user, date) %>%
  summarise_each(funs(sum), contains("day"))

#Create percentages
retention.by.platform.p <-
  retention.by.platform %>%
  mutate(
    day0p = round(day0 / day0, 2),
    d1 = round(day1 / day0, 2),
    d2 = round(day2 / day0, 2),
    d3 = round(day3 / day0, 2),
    d4 = round(day4 / day0, 2),
    d5 = round(day5 / day0, 2),
    d6 = round(day6 / day0, 2),
    d7 = round(day7 / day0, 2)
  ) %>%
  select(installed, game, day0, d1:d7)

#Changing nan to 0s. a[is.nan(a)] <- 0 doesn't work with dataframes 
is.nan.data.frame <- function(x)
  do.call(cbind, lapply(x, is.nan))

retention.by.platform.p[is.nan(retention.by.platform.p)] <- 0

#Table of how many from each platform
retention.by.platform %>%
  group_by(game) %>%
  summarise(
    count = sum(day0)
  )

#Plotting
retention.by.platform.plot <-
  retention.by.platform.p %>%
  gather(day, retention, d1:d7) %>%
  arrange(installed, day)

ggplot(retention.by.platform.plot, aes(x = day, y = retention, fill = game, label = paste(100*(retention), '%'))) +
  geom_bar(stat = "identity") +
  facet_grid(game~installed) +
  theme_few() +
  scale_fill_few() +
  ggtitle("Retention %") +
  geom_text(size = 2.2, vjust = 1.2) +
  ylab("") +
  xlab("") 

#Line graph
ggplot(retention.by.platform.plot, aes(x = day, y = retention, color = as.factor(installed), label = paste(100*retention, '%'), group = installed)) +
  geom_point(size = 2) +
  geom_line(size = .8) +
  theme_few() +
  scale_fill_few() +
  facet_grid(~game) +
  geom_text(size = 3, vjust = -1.5) +
  ggtitle("Retention x platform") +
  ylab("") +
  xlab("") +
  theme(
    axis.ticks.y = element_blank(),
    axis.text.y = element_blank()
  )

###Tutorial bounce
#Extract 
tutorial.bounce <-
run %>%
  filter(grepl("tutorial", event))

#Line
tutorial.bounce.l <-
  tutorial.bounce %>%
  group_by(event) %>%
  mutate(
    count = n()
  ) %>%
  distinct(event) %>%
  filter(count >340)

#Create remaining, and bouncing users
tutorial.bounce.l <- 
tutorial.bounce.l %>%
  ungroup() %>%
  arrange(desc(count)) %>%
  mutate(
    remaining = lead(count), 
    remaining.p = lag(round(remaining / 475, 2)),
    bounce.p = 1 - remaining.p
  ) %>%
  select(event, count, remaining, remaining.p, bounce.p)

#Fill in NAs with 100% and 0
tutorial.bounce.l$bounce.p[is.na(tutorial.bounce.l$bounce.p)] <- 0
tutorial.bounce.l$remaining.p[is.na(tutorial.bounce.l$remaining.p)] <- 1

#Line plot with remaining percentages as labels 
ggplot(tutorial.bounce.l, aes(x = reorder(event, -count), y = count, label = remaining.p)) +
  geom_point(size = 1.5, color = "light blue") + 
  geom_line(group = 1, color = "light blue ") + 
  theme_few() +
  geom_text(size = 3, hjust = -.3, vjust = -.5) +
  theme(
    axis.text.x = element_text(angle = 90)
  ) +
  ggtitle("Tutorial bounce rate") +
  ylab("") +
  xlab("Tutorial step")
