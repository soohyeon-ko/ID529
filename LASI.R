rm(list = ls())

library(dplyr)
library(tidyr)
library(haven)

getwd()
setwd("/Users/soohyeonko/Library/CloudStorage/OneDrive-Personal/Research/data/LASI")

data <- read_dta("H_LASI_a3.dta")

data <- data %>% 
  mutate(
    hh1ctot_new = hh1ctot - (12 * hh1cohc1m + hh1cihc1y),
    hh1cperc_new = hh1ctot_new / hh1hhres,
    mpce5 = ntile(hh1cperc_new, 5)
  )

table(data$mpce5, useNA = "ifany")

data <- data %>%
  filter(r1agey >= 45)

data <- data %>%
  filter(if_all(c(r1imrc, r1dlrc, r1orient, r1orientp, r1ser7, 
                  r1compu, r1execu, r1draw, r1object, 
                  r1bwc20a, r1bwc100a), ~ !is.na(.))
         ) %>%
  mutate(
    cog = rowSums(select(., r1imrc, r1dlrc, r1orient, r1orientp, 
                         r1ser7, r1compu, r1execu, r1draw, 
                         r1object, r1bwc20a, r1bwc100a), na.rm = TRUE
  )) %>% 
  mutate(
    lowcog = case_when(
      cog > 16 ~ "No impairment",
      cog <= 16 ~ "Cognitive impairment"
    ))

table(data$lowcog, useNA = "ifany")
    
data <- data %>%
  filter(if_all(c(mpce5, rafinanch), ~ !is.na(.))
  ) %>%
  mutate(
    owealth = case_when(
      mpce5 %in% c(3:5) ~ "High",
      mpce5 %in% c(1:2) ~ "Low")
    ) %>% 
  mutate(
    cwealth = case_when(
      rafinanch %in% c(1, 2) ~ "High",
      rafinanch == 3 ~ "Low"
    ),
    lwealth = case_when(
      owealth == "High" & cwealth == "High" ~ "Consistently High",
      owealth == "Low" & cwealth == "High" ~ "Upward Mobility",
      owealth == "High" & cwealth == "Low" ~ "Downward Mobility",
      owealth == "Low" & cwealth == "Low" ~ "Consistently Low"
    )
  )

table(data$lwealth, useNA = "ifany")

data <- data %>%
    rename(eduyear = raedyrs) %>% 
  mutate(
    education = case_when(
      eduyear == 0 ~ "No education",
      eduyear %in% 1:5 ~ "Primary",
      eduyear %in% 6:9 ~ "Secondary",
      eduyear >= 10 ~ "Higher"
  ))

table(data$education, useNA = "ifany")
summary(data$eduyear)

data <- data %>%
  mutate(
    marital = case_when(
      r1mstat %in% c(3, 4, 5, 7, 8) ~ "Not married",
      r1mstat == 1 ~ "Married"
  ))

data <- data %>%
  mutate(
    gender = case_when(
      ragender == 1 ~ "Men", 
      ragender == 2 ~ "Women"),
    age = r1agey,
    age2 = r1agey^2,
    age_group = ifelse(floor(age / 5) * 5 >= 100, 100, floor(age / 5) * 5),
    religion = case_when(
      r1relig_l == 2 ~ "Hindu",
      r1relig_l == 3 ~ "Muslim",
      r1relig_l == 4 ~ "Christian",
      r1relig_l %in% c(1, 5:10) ~ "Others"
    ),
    social = case_when(
      r1caste == 1 ~ "SC", 
      r1caste == 2 ~ "ST", 
      r1caste == 3 ~ "OBC", 
      r1caste == 4 ~ "Others"),
    household_size = hh1hhres,
    work = case_when(
      r1worka == 0 & r1sayret_l == 1 ~ "Retirement",
      r1worka == 1 ~ "Working",
      r1worka == 0 ~ "Not-working"
    ),
    residence = case_when(
      hh1rural == 1 ~ "Rural",
      hh1rural == 0 ~ "Urban"),
    csrh = case_when(
      rachshlta %in% c(1,2) ~ "Poor", 
      rachshlta ==3 ~ "Fair",
      rachshlta %in% c(4,5) ~ "Good")
  ) %>%
  filter(!is.na(gender), !is.na(age), !is.na(marital), !is.na(work), !is.na(education),
         !is.na(mpce5), !is.na(religion), !is.na(social), !is.na(residence), !is.na(household_size), !is.na(csrh))

library(gtsummary)
library(tableone)

data %>%
  select(cog, lwealth, cwealth, owealth, marital, religion, social, education, work, residence, csrh, gender) %>%
  tbl_summary(by = gender)


