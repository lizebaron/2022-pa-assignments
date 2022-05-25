---
title: "Exercise 3"
output: github_document
---

```{r setup-1, include=FALSE}
#rm(list = ls()) # clear memory, step 1
#gc() # clear memory, step 2
knitr::opts_chunk$set(echo = TRUE)
# load the necessary packages ("extensions")
library(tidyverse)
```

## Code from Exercise 2 to be reused

## Setting up data and environment

## We first need to do a few things before we can manipulate the data. 
```{r setup-2}

library(arrow)
# set path for R to find our data
data_path <- "/home/liz/USPTO_data/"

```


## 1. Load data

We'll load application data only here (you are welcome to load the other three files as well). Because we are loading from a .parquet format file, we'll use library `arrow` and the functions `read_parquet()`. For the rest of the files, we can use function `read_csv()` which comes with a package `readr` (which is included in `tidyverse` set of packages, so if we are loading `tidyverse` there is no need to also load `readr`). Note that the path to the data file on my computer is defined above, in the `data_path` variable.

```{r load-data}
 # to be able to load data in the .parquet format
 # read application data
app_data_sample <- read_parquet(paste0(data_path,"app_data_sample.parquet"))
```

To inspect the top slice of the data, we can simply call it:

```{r show-data}
app_data_sample
```
### Get gender for examiners

We'll get gender based on the first name of the examiner, which is recorded in the field `examiner_name_first`. We'll use library `gender` for that, relying on a modified version of their own [example](https://cran.r-project.org/web/packages/gender/vignettes/predicting-gender.html).

Note that there are over 2 million records in the applications table -- that's because there are many records for each examiner, as many as the number of applications that examiner worked on during this time frame. Our first step therefore is to get all *unique* names in a separate list `examiner_names`. We will then guess gender for each one and will join this table back to the original dataset. So, let's get names without repetition:

```{r gender-1}
library(gender)
#install_genderdata_package() # only run this line the first time you use the package, to get data for it
# get a list of first names without repetitions
examiner_names <- app_data_sample %>% 
  distinct(examiner_name_first)
examiner_names
```

Now let's use function `gender()` as shown in the example for the package to attach a gender and probability to each name and put the results into the table `examiner_names_gender`

```{r gender-2}
# get a table of names and gender
examiner_names_gender <- examiner_names %>% 
  do(results = gender(.$examiner_name_first, method = "ssa")) %>% 
  unnest(cols = c(results), keep_empty = TRUE) %>% 
  select(
    examiner_name_first = name,
    gender,
    proportion_female
  )
examiner_names_gender
```

Finally, let's join that table back to our original applications data and discard the temporary tables we have just created to reduce clutter in our environment.

```{r gender-3}
# remove extra colums from the gender table
examiner_names_gender <- examiner_names_gender %>% 
  select(examiner_name_first, gender)
# joining gender back to the dataset
app_data_sample <- app_data_sample %>% 
  left_join(examiner_names_gender, by = "examiner_name_first")
# cleaning up
rm(examiner_names)
rm(examiner_names_gender)
gc()
```


### Guess the examiner's race

We'll now use package `wru` to estimate likely race of an examiner. Just like with gender, we'll get a list of unique names first, only now we are using surnames.

```{r race-1}
library(wru)
examiner_surnames <- app_data_sample %>% 
  select(surname = examiner_name_last) %>% 
  distinct()
examiner_surnames
```
We'll follow the instructions for the package outlined here [https://github.com/kosukeimai/wru](https://github.com/kosukeimai/wru).

```{r race-2}
examiner_race <- predict_race(voter.file = examiner_surnames, surname.only = T) %>% 
  as_tibble()
examiner_race
```

As you can see, we get probabilities across five broad US Census categories: white, black, Hispanic, Asian and other. (Some of you may correctly point out that Hispanic is not a race category in the US Census, but these are the limitations of this package.)

Our final step here is to pick the race category that has the highest probability for each last name and then join the table back to the main applications table. See this example for comparing values across columns: [https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-rowwise/](https://www.tidyverse.org/blog/2020/04/dplyr-1-0-0-rowwise/). And this one for `case_when()` function: [https://dplyr.tidyverse.org/reference/case_when.html](https://dplyr.tidyverse.org/reference/case_when.html).

```{r race-3}
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
examiner_race
```

Let's join the data back to the applications table.

```{r race-4}
# removing extra columns
examiner_race <- examiner_race %>% 
  select(surname,race)
app_data_sample <- app_data_sample %>% 
  left_join(examiner_race, by = c("examiner_name_last" = "surname"))
rm(examiner_race)
rm(examiner_surnames)
gc()
```


### Examiner's tenure 

To figure out the timespan for which we observe each examiner in the applications data, let's find the first and the last observed date for each examiner. We'll first get examiner IDs and application dates in a separate table, for ease of manipulation. We'll keep examiner ID (the field `examiner_id`), and earliest and latest dates for each application (`filing_date` and `appl_status_date` respectively). We'll use functions in package `lubridate` to work with date and time values.

```{r tenure-1}
library(lubridate) # to work with dates
examiner_dates <- app_data_sample %>% 
  select(examiner_id, filing_date, appl_status_date) 
examiner_dates
```

The dates look inconsistent in terms of formatting. Let's make them consistent. We'll create new variables `start_date` and `end_date`.

```{r tenure-2}
examiner_dates <- examiner_dates %>% 
  mutate(start_date = ymd(filing_date), end_date = as_date(dmy_hms(appl_status_date))) %>% 
  filter(year(end_date)<2018)
```

Let's now identify the earliest and the latest date for each examiner and calculate the difference in days, which is their tenure in the organization.

```{r tenure-3}
examiner_dates <- examiner_dates %>% 
  group_by(examiner_id) %>% 
  summarise(
    earliest_date = min(start_date, na.rm = TRUE), 
    latest_date = max(end_date, na.rm = TRUE),
    tenure_days = interval(earliest_date, latest_date) %/% days(1)
    )
examiner_dates
```

Joining back to the applications data.

```{r tenure-4}
app_data_sample <- app_data_sample %>% 
  left_join(examiner_dates, by = "examiner_id")
rm(examiner_dates)
gc()
```

## Linear Regression Models 

#### Libraries for the regression 

```{r}
library(modelsummary)
library(kableExtra)
```

### First we calculate art_unit_size and art_unit_gender_ratio variables

```{r}
## calculate art unit size
art_unit_size <- app_data_sample %>% group_by(examiner_art_unit) %>% 
  count()  %>% 
  select(
    art_unit_size = n,
    examiner_art_unit
  )

## joining art_unit_size back to dataset
Table_All_1 <- app_data_sample %>% 
  left_join(art_unit_size, by = "examiner_art_unit")

## calculate art_unit_gender_ratio (female to male ratio)

art_unit_gender_size = Table_All_1 %>% group_by(examiner_art_unit,gender) %>%
  count() %>%  filter(!is.na(gender)) %>% filter(gender == 'female')

Table_gender_size <- art_unit_gender_size %>% 
  left_join(art_unit_size, by = "examiner_art_unit")

Table_gender_size$female_to_male_ratio = Table_gender_size$n/(Table_gender_size$art_unit_size - Table_gender_size$n)

## joining gender_ratio back to dataset
 
Table_All <- Table_All_1 %>% 
  left_join(Table_gender_size %>% dplyr::select(female_to_male_ratio), by = "examiner_art_unit")

# remove unused columns
Table_gender_size = subset(Table_gender_size, select = -c(gender))

```

### OLS 1 - Using gender and race to explain Tenure_days

```{r}

app_data_sample$likely_race = as.factor(app_data_sample$race)
levels(app_data_sample$likely_race)

OLS_1 = lm(tenure_days ~ as.factor(gender) + likely_race, data=app_data_sample)
modelsummary(OLS_1)
```


```

```

### OLS 4 - Using gender,race and art_unit_size variabes to explain Tenure_days

```{r}

OLS_4 = lm(tenure_days ~ art_unit_size + as.factor(gender) + as.factor(race)  , data=Table_All)
modelsummary(OLS_4)

```{r}

### Ran regressions and discussed with team but am finding errors when attempting to recreate code on my own computer -- responses based on results obtained with team.

OLS 4 is a better fit than OLS 1 as it has a higher adjusted R squared value, and the difference in standard error between the two is negligible.

These results indicate that many variables have an effect on examiner tenure days and additional specific research may be needed to further isolate the primary causes of shorter tenures. It is also difficult to conclude exactly how these variables may be interacting with one another in order to form actionable goals for the office to work toward in improving tenure durations.   
   
Team for part of this exercise: Ranvir, Hima, Patrick, Adel, Shan
