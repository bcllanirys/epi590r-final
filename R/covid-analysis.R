
install.packages("pacman")
library(pacman)

# Install and load multiple packages
p_load(gtsummary,readr, dplyr, tidyverse, dplyr,broom.helpers,cardx, broom, cards)

library(readr)
library(tidyverse)
library(gtsummary)
library(dplyr)
library(broom.helpers)
library(cards)
library(cardx)
library(broom)

install.packages("medicaldata")
library(medicaldata)
data(package="medicaldata")
covid <- medicaldata::covid_testing
class(covid)
covid <- as.data.frame(covid)

write_csv(covid, here::here("Data", "raw", "covid.csv"))


#cleaning data
covid_clean <- covid |>
  mutate(clinic_name = case_when(str_detect(clinic_name, "onc") ~ "oncology",str_detect(clinic_name, "mri") ~ "MRI", str_detect(clinic_name, "onc") ~ "oncology", str_detect(clinic_name, "rad") ~ "diagnosis radiology",str_detect(clinic_name, "urg") ~ "urgent care",
           str_detect(clinic_name, "lab") ~ "laboratory",str_detect(clinic_name, "ntwk") ~ "care network",
           str_detect(clinic_name, "hosp") ~ "hospital",
           str_detect(clinic_name, "ward") ~ "ward",
           TRUE ~ clinic_name
         ),
         clinic_name = fct_lump(clinic_name, n = 10))

write_rds(covid_clean, here::here("Data", "clean", "covid_clean.rds"))
write_csv(covid_clean, here::here("Data", "clean", "covid_clean.csv"))


library(gtsummary,glue)
  table1 <- tbl_summary(
  covid_clean,
  by = gender,
  include = c(age, result, pan_day, test_id, clinic_name, payor_group, demo_group, patient_class),
  label = list(
    payor_group ~ "Paying method",
    pan_day ~ "# of days tested after pandemic start",
    test_id ~ "Type of test",
    clinic_name ~ "Clinic",
    demo_group ~ "Subject groups",
    patient_class ~ "Subject disposition"
  ),
  missing_text = "Missing")   |>
 	add_overall(col_label = "**Total**") |>
  bold_labels()
 table1
mean_age <-inline_text(table1,variable = "age", column="stat_0")


#| label: tbl-lm
#| tbl-cap: "This is a regression model"
regressionmodel <- tbl_uvregression(
  covid_clean,
  y=age,
  method = lm,
  include = c(age, clinic_name,result, gender, payor_group, demo_group, patient_class))
regressionmodel


#create function of sample std

stdfunction <- function(col_rec_tat) {
  n <- length(col_rec_tat)
  mean <- sum(col_rec_tat) / n
  diffsq <- (col_rec_tat - mean)^2
  variance <- sum(diffsq) / (n - 1)
  std_val <- sqrt(variance)

  return(std_val)
}

std <-sd(covid_clean$col_rec_tat, na.rm=TRUE)


hist(covid_clean$age,
     xlab = "Age",
     ylab = "Frequency",
     col = "coral",
     border = "black")


