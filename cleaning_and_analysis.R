---
title: "Bellabeat_Case_Study"
author: "Anthony"
date: "2025-05-16"
output: html_document
---

### Setting up environment 

```{r, message=FALSE, warning=FALSE}

library(sqldf)
library(here)
library(tidyverse)
library(conflicted)
library(skimr)
library(janitor)
conflict_prefer("filter","dplyr")
conflict_prefer("lag", "dplyr")
```

### Importing raw data from .csv files

```{r, message=FALSE}
daily_activity <- read_csv("C:/Users/18046/OneDrive/Desktop/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/dailyActivity_merged.csv")
hourly_calories <- read_csv("C:/Users/18046/OneDrive/Desktop/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/hourlyCalories_merged.csv")
weight_log <- read_csv("C:/Users/18046/OneDrive/Desktop/mturkfitbit_export_3.12.16-4.11.16/Fitabase Data 3.12.16-4.11.16/weightLogInfo_merged.csv")
daily_activity2 <- read_csv("C:/Users/18046/OneDrive/Desktop/mturkfitbit_export_4.12.16-5.12.16/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")  
```

### Preiewing the data

```{r}
skim_without_charts(daily_activity)
summary(daily_activity)
skim_without_charts(hourly_calories)
summary(hourly_calories)
skim_without_charts(daily_activity2)
summary(daily_activity2)
skim_without_charts(weight_log)
summary(weight_log)
```

We can see,from this, that *most* users only submitted one or two entries into the *Weight Log* over the course of data collection, and, of these entries,  31 of 33 entries are missing *fat* data.

### Combining datasets

```{r, message=FALSE}
all_activity <- bind_rows(daily_activity, daily_activity2) %>% 
  group_by(Id) %>% 
  arrange(Id)
```

### Checking # of occurences of each date entered for each user ID

```{r}
n_occur <- data.frame(table(all_activity$Id, all_activity$ActivityDate)) %>% 
  group_by(Freq) %>% 
  arrange(desc(Freq))
tibble(n_occur)
```

Here, we can see that most entries for 2020-04-12 have 2 entries. This is likely due to the data for that day being split among the two entries, possibly a glitch or a system reboot. Upon closer inspection, this should have no effect on the final analysis.

### Formatting date from character toPOSIX

```{r}
all_activity$ActivityDate = as.POSIXct(all_activity$ActivityDate, format= "%m/%d/%y",
                                       tz=Sys.timezone())
```


### Separating date by day of the week inorderto find daily averages later.

```{r, message=FALSE}
all_activity$day_of_week <- format(as.Date(all_activity$ActivityDate), "%a")
```

### Creating new dataframe with averages for all fields by day of the week

```{r}
summarized_data <- all_activity %>% 
  group_by(day_of_week) %>% 
  summarise(AvgDailySteps = mean(TotalSteps),
            AvgDailyCals = mean(Calories),
            AvgDailyDistance  = mean(TotalDistance),
            AvgSedentaryMinutes = mean(SedentaryMinutes),
            AvgLightlyActiveMinutes = mean(LightlyActiveMinutes),
            AvgFairlyActiveMinutes = mean(FairlyActiveMinutes),
            AvgVeryActiveMinutes = mean(VeryActiveMinutes))
tibble(summarized_data)
```

### Creating new dataframe totals for all fields

```{r}
activity_totals_long <- data.frame(
  GROUP=  c("Sedentary", "Light","Fair", "Very"),
  VALUE= c(sum(summarized_data$AvgSedentaryMinutes), sum(summarized_data$AvgLightlyActiveMinutes),
           sum(summarized_data$AvgFairlyActiveMinutes), sum(summarized_data$AvgVeryActiveMinutes)))
tibble(activity_totals_long)  
```

### Creating .csv files to import to Tableau for visualization

```{r, echo=FALSE}
write.csv(all_activity,  file = "Bellabeat Combined Activity.csv")
write.csv(activity_totals_long, file= "Bellabeat Activity Totals.csv")
write.csv(summarized_data, file = "Bellabeat Summarized Data.csv")
```
