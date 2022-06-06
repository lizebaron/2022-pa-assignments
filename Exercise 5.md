---
title: "Exercise 5"
output: github_document
---

```{r setup, include=FALSE}
rm(list = ls())
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
```

## 0. Loading data and preparing variables

Here, I'm using the steps from exercise 2 example to create the necessary variables.

```{r prep-steps}
# set path for R to find our data
data_path <- "/home/liz/USPTO_data/"
library(arrow) # to be able to load data in the .parquet format
# read application data
app_data_sample <- read_parquet(paste0(data_path,"app_data_sample.parquet"))
library(gender)
#install_genderdata_package() # only run this line the first time you use the package, to get data for it
# get a list of first names without repetitions
examiner_names <- app_data_sample %>% 
  distinct(examiner_name_first)
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )
# remove extra columns from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)
# joining gender back to the dataset
app_data_sample <- app_data_sample %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")
# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()
# Examiners' race
library(wru)
examiner_surnames <- app_data_sample %>% 
  select(surname = examiner_name_last) %>% 
  distinct()
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
examiner_race <- examiner_race %>% 
  mutate(max_race_p = pmax(pred.asi, pred.bla, pred.his, pred.oth, pred.whi)) %>% 
  mutate(race = case_when(
    max_race_p == pred.asi ~ "Asian",
    max_race_p == pred.bla ~ "black",
    max_race_p == pred.his ~ "Hispanic",
    max_race_p == pred.oth ~ "other",
    max_race_p == pred.whi ~ "white",
    TRUE ~ NA_character_
  ))
examiner_race <- examiner_race %>% 
  select(surname,race)
app_data_sample <- app_data_sample %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))
rm(examiner_race)
rm(examiner_surnames)
gc()
# Examiner's tenure
library(lubridate) # to work with dates
examiner_dates <- app_data_sample %>% 
  select(examiner_id, filing_date, appl_status_date) 
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date))) %>% 
  filter(year(end_date)<2018) %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    )
app_data_sample <- app_data_sample %>% 
  left_join(examiner_dates, by = "examiner_id")
rm(examiner_dates)
gc()
```
## Adding paygrade data

First, we load the paygrade file.

```{r load-paygrade-data}
examiner_gs <- read_csv(paste0(data_path,"examiner_gs.csv"))
examiner_ids <- read_csv(paste0(data_path,"examiner_ids.csv"))
```

### We need to replace various IDs with examiner ID

The ID fields in the `examiner_gs.csv` file don't match those in the application data file. Because we'll need to join these files later, we need to bring in the proper ID field, by using the cross-walk file `examiner_ids`.

```{r join-id}
examiner_gs <- examiner_gs %>% 
  left_join(examiner_ids) %>% 
  select(
    grade = examiner_grade,
    start_date,
    end_date,
    examiner_id = patex_id
  )
```

## Estimate time in grade

Now we need to estimate the average time each examiner spends in a given GS paygrade. Note that the less-biased way to do that is to exclude the latest or highest grade for each examiner. This is because after examiners reach the highest grade (which is usually grade 14 in this context), they cannot advance to the next grade. Imagine someone who advances through grades 11, 12 and 13 in just 18 months (so, the average for that examiner is 6 months per grade), but then stays in grade 14 and works at the agency for another ten years. If you were to average all grades, it would look like it took the examiner several years on average to get promoted through each grade. This is because the last grade biases the average positively. 

Similarly, for examiners who get promoted right before the end of our sample's time window, the very short time they stay in the highest observed grade will negatively bias the average. It will look like the examiner has progressed through the grades way faster than she did.

```{r get-time-in-grade}
time_in_grade <- examiner_gs %>% 
  mutate(
    start_date = mdy(start_date), # converting into proper date type
    end_date = mdy(end_date), # converting into proper date type
    days_in_grade = interval(start_date, end_date) %/% days(1)
  ) %>% 
  group_by(examiner_id) %>% 
  filter(grade!=max(grade, na.rm = TRUE)) %>% # dropping the highest grade record
  summarise(mean_days_in_grade = mean(days_in_grade, na.rm = TRUE))
time_in_grade
```

## Prepare application data

Let's get the measure of application processing time for each examiner. We'll do this by "collapsing" the dataframe from application level to examiner level (i.e., one record for each examiner, not multiple records).

```{r app-proc-time}
examiner_data <- app_data_sample %>% 
  filter(disposal_type!="PEND") %>% # here, we exclude in-process applications
  mutate(
    app_start_date = ymd(filing_date),
    app_end_date = case_when(
      disposal_type == "ISS" ~ ymd(patent_issue_date), # for issued patents
      disposal_type == "ABN" ~ ymd(abandon_date), # for abandoned applications
      TRUE ~ NA_Date_
    ),
    app_proc_days = interval(app_start_date, app_end_date) %/% days(1)) %>% 
  filter(app_proc_days>0 & app_proc_days < 3650) %>% # limit to 0-10 years
  group_by(examiner_id) %>% 
  summarise(
    app_count = n(),
    tc = min(tc, na.rm = TRUE),
    gender = first(gender),
    race = first(race),
    tenure_days = max(tenure_days, na.rm = TRUE),
    mean_app_proc_days = mean(app_proc_days, na.rm = TRUE)
  )
examiner_data
```

Now, let's join in the time in grade data.

```{r add-time-in-grade}
examiner_data <- examiner_data %>% 
  left_join(time_in_grade)
examiner_data
```
## Eliminating negative mean_days_in_grade

```{r add-time-in-grade}
b_examiner_data = examiner_data %>% filter(examiner_data$mean_days_in_grade > 0)
```

## Calculating Mean days in grade based on Gender & race

```{r}

gender_mean_advancement_time = b_examiner_data %>% group_by(gender) %>% summarise(Days_to_next_paygrade = mean(mean_days_in_grade, na.rm = TRUE)) %>% filter(!is.na(gender))

ggplot(data = gender_mean_advancement_time, mapping = aes(x = as_factor(gender), y = Days_to_next_paygrade)) + geom_bar(stat="identity")

race_mean_advancement_time = b_examiner_data %>% group_by(race) %>% summarise(Days_to_next_paygrade = mean(mean_days_in_grade, na.rm = TRUE))

ggplot(data = race_mean_advancement_time, mapping = aes(x = as_factor(race), y = Days_to_next_paygrade)) + geom_bar(stat="identity")

gen_race_mean_advancement_time = b_examiner_data %>% group_by(gender,race) %>% summarise(Days_to_next_paygrade = mean(mean_days_in_grade, na.rm = TRUE)) %>% filter(!is.na(gender))

ggplot(data = gen_race_mean_advancement_time, mapping = aes(x = as_factor(race), y = Days_to_next_paygrade, fill = as_factor(gender))) + geom_bar(stat="identity",position = "dodge") + geom_text(aes(label = round(Days_to_next_paygrade,1)),position = position_dodge(0.9), color="white",vjust = 1,hjust = 0.5)




```
## Plotting mean application prosecution time vs mean days in same paygrade

```{r}
ggplot(b_examiner_data) +
  geom_point(
    aes(x=mean_app_proc_days, y = mean_days_in_grade), 
    )

```


## Descriptive statistics and regressions

Let's run a couple of simple regressions.

```{r reg-sample}
library(modelsummary)
models <- list()
models[['m1']] <- lm(mean_days_in_grade ~ 1 + mean_app_proc_days, data = b_examiner_data) 
models[['m2']] <- lm(mean_days_in_grade ~ 1 + mean_app_proc_days + as_factor(gender), 
         data = b_examiner_data) 
models[['m3']] <- lm(mean_days_in_grade ~ 1 + as_factor(gender), data = b_examiner_data)
models[['m4']] <- lm(mean_days_in_grade ~ 1 + as_factor(race), data = b_examiner_data) 
models[['m5']] <- lm(mean_days_in_grade ~ 1 + mean_app_proc_days + as_factor(race), 
         data = b_examiner_data) 


modelsummary(models)


```

# R session aborting every time when running code on my own computer. 
From results observed with team, there does not appear to be a relationship between average time in grade and average application prosecution time.

This suggests that employees are being promoted through the grades in a fairly automatic manner, regardless of how quickly they process applications. This may be contributing to slow prosecution times and low motivation among staff, if employees know that this aspect of their performance is unlikely to have any bearing on their future path in the organization. 

More research could help determine whether there is a relationship between average time in grade and other factors such as the number of examiners in a given art unit. It would also be worth learning whether employees are explicitly told to expect promotions on a certain schedule with no exceptions or whether they could be motivated to try and receive promotions faster, perhaps by improving their prosecution times.

Team for this exercise: Hima, Shan, Adel, Ranvir, Patrick

