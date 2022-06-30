# Check compared to public files

library(tidyverse)
library(rio)


district_2011_2021 <- read_csv("C:/ORP_accountability/data/2011/2011-2021_district_assessment_file.csv")

## Hey Andrew, I went ahead and added 2010 and 2021 public data, switched out all the local data paths for TDOE website links. 
## I also added the state-level files in a similar section below. 


# 2010 ==================
public_2010 <- tibble(rio::import("https://www.tn.gov/content/dam/tn/education/data/data_2010_district_base.xlsx"))

# 2011 ==================
public_2011 <- tibble(rio::import("https://www.tn.gov/content/dam/tn/education/data/data_2011_district_base.xlsx"))

# 2012 ==================
public_2012 <- tibble(rio::import("https://www.tn.gov/content/dam/tn/education/data/data_2012_district_base.xlsx"))

# 2013 ===================
public_2013 <- tibble(rio::import("https://www.tn.gov/content/dam/tn/education/data/data_2013_district_base.xlsx"))

# 2014 ===================
public_2014 <- tibble(rio::import("https://www.tn.gov/content/dam/tn/education/data/data_2014_district_base.xlsx"))

# 2015 ===================
public_2015 <- tibble(rio::import("https://www.tn.gov/content/dam/tn/education/data/data_2015_district_base.xlsx"))

# 2016 ===================
public_2016 <- tibble(rio::import("https://www.tn.gov/content/dam/tn/education/data/data_2016_suppressed_district_base.xlsx"))

# 2017 ===================
public_2017 <- read_csv("https://www.tn.gov/content/dam/tn/education/data/data_2017_district_base.csv")

# 2018 ===================
public_2018 <- read_csv("https://www.tn.gov/content/dam/tn/education/data/data_2018_district_base.csv")

# 2019 ===================
public_2019 <- read_csv("https://www.tn.gov/content/dam/tn/education/accountability/2019/district_assessment_file_suppressed.csv")

# 2021 ===================
public_2021 <- read_csv("https://www.tn.gov/content/dam/tn/education/accountability/2021/district_assessment_file_suppressed_upd422.csv")

# 2022 ===================
public_2022 <- read_csv("N:/ORP_accountability/data/2022_final_accountability_files/district_assessment_file_suppressed.csv")

non_public_2022 <- read_csv("N:/ORP_accountability/data/2022_final_accountability_files/district_assessment_file.csv")

public_2022 <- left_join(
   public_2022 %>% select(
     "year","system","system_name","test" , "subject","grade","subgroup",
     "participation_rate", "pct_below" ,"pct_approaching", "pct_met_expectations", 
     "pct_exceeded_expectations" , "pct_met_exceeded"
   ), 
   non_public_2022 %>% select(
     'year','system','system_name','test','subject','grade','subgroup', "enrolled","tested", "valid_tests"
   ), 
   by = c('year','system','system_name','test','subject','grade','subgroup')
)

# Combine =================
combined_public <- bind_rows(
  public_2010 %>%
    transmute(
      year = 2010,
      system = `District ID`, system_name = `District Name`,
      subject = Subject, grade = Grade, subgroup = `Student Group`,
      enrolled = `Number Enrolled`,
      valid_tests = as.character(`Number of Valid Tests`),
      pct_met_exceeded = `Percent Proficient or Advanced`
    ) %>%
    filter(system != 0),
  public_2011 %>% 
    transmute(
      year = 2011,
      system = `District ID`, system_name = `District Name`,
      subject = Subject, grade = Grade, subgroup = `Student Group`,
      enrolled = `Number Enrolled`,
      valid_tests = as.character(`Number of Valid Tests`),
      pct_met_exceeded = `Percent Proficient or Advanced`
    ) %>% 
    filter(system != 0),
  public_2012 %>% 
    transmute(
      year = 2012,
      system = `District ID`, system_name = `District Name`,
      subject = Subject, grade = Grade, subgroup = `Student Group`,
      enrolled = `Number Enrolled`,
      valid_tests = as.character(`Number of Valid Tests`),
      pct_met_exceeded = `Percent Proficient or Advanced`
    ) %>% 
    filter(system != 0),
  public_2013 %>% 
    transmute(
      year,
      system, system_name,
      subject, grade, subgroup,
      # enrolled = `Number Enrolled`, 
      valid_tests,
      pct_met_exceeded = pct_prof_adv
    ) %>% 
    filter(system != 0),
  public_2014 %>% 
    transmute(
      year,
      system, system_name,
      subject, grade, subgroup,
      # enrolled = `Number Enrolled`, 
      valid_tests = as.character(valid_tests),
      pct_met_exceeded = pct_prof_adv
    ) %>% 
    filter(system != 0),
  public_2015 %>% 
    transmute(
      year,
      system, system_name,
      subject, grade, subgroup,
      # enrolled = `Number Enrolled`, 
      valid_tests = as.character(valid_tests),
      pct_met_exceeded = pct_prof_adv
    ) %>% 
    filter(system != 0),
  public_2017 %>% 
    transmute(
      year,
      system, system_name,
      subject, grade, subgroup,
      # enrolled = `Number Enrolled`, 
      valid_tests = as.character(valid_tests),
      pct_met_exceeded = pct_on_mastered
    ) %>% 
    filter(system != 0),
  public_2018 %>% 
    filter(test == "TNReady") %>%
    transmute(
      year,
      system, system_name,
      subject, grade, subgroup,
      # enrolled = `Number Enrolled`, 
      valid_tests = as.character(valid_tests),
      pct_met_exceeded = pct_on_mastered
    ) %>% 
    filter(system != 0),
  public_2019 %>% 
    filter(test == "TNReady") %>% 
    transmute(
      year,
      system, system_name,
      subject, grade, subgroup,
      # enrolled, 
      valid_tests = as.character(valid_tests),
      pct_met_exceeded = pct_on_mastered
    ) %>% 
    filter(system != 0),
  public_2021 %>% 
    filter(test == "TNReady") %>% 
    transmute(
      year,
      system, system_name,
      subject, grade, subgroup,
      # enrolled, 
      valid_tests = as.character(valid_tests),
      pct_met_exceeded = as.character(pct_on_mastered)
    ) %>% 
    filter(system != 0),
  public_2022 %>% 
    filter(test == "TNReady") %>% 
    transmute(
      year,
      system, system_name,
      subject, grade, subgroup,
      # enrolled, 
      valid_tests = as.character(valid_tests),
      pct_met_exceeded = as.character(pct_met_exceeded)
    ) %>% 
    filter(system != 0)
) %>% 
  mutate(
    subject = case_when(
      subject %in% c("Reading/Language", "RLA") ~ "ELA",
      TRUE ~ subject
    ),
    subgroup = case_when(
      subgroup == "Native American" ~ "American Indian or Alaska Native",
      subgroup == "All" ~ "All Students",
      subgroup == "Economically Disadvantaged (Free or Reduced Price Lunch)" ~ "Economically Disadvantaged",
      subgroup == "English Language Learners" ~ "English Learners",
      subgroup %in% c("English Learners with T1/T2", "English Language Learners with T1/T2") ~ "English Learners with Transitional 1-4",
      subgroup %in%  c("Hawaiian or PI", "Hawaiian or Pacific Islander") ~ "Native Hawaiian or Other Pacific Islander",
      subgroup %in% c(
        "Non-English Language Learners/T1 or T2", "Non-English Learners/T1 or T2"
      ) ~ "Non-English Learners/Transitional 1-4",
      subgroup == "Non-English Language Learners" ~ "Non-English Learners",
      subgroup == "Black" ~ "Black or African American",
      TRUE ~ subgroup
    )
  ) %>% 
  filter((grade == "3" & subject == "ELA") | (grade == "7" & subject == "Math") |
           (grade == "4" & subject == "ELA")) %>% 
  arrange(system, grade, subgroup, -year)

# write_csv(combined_public, "raw_district_assessment_data_downloads_2011-2019.csv", na = '')
write_csv(combined_public, "public_district_assessment_data_2010-2022.csv", na = '')



#####################  State Level Data File ###################

# 2010 ==================
public_2010 <- tibble(rio::import("https://www.tn.gov/content/dam/tn/education/data/data_2010_state_base.xlsx"))

# 2011 ==================
public_2011 <- tibble(rio::import("https://www.tn.gov/content/dam/tn/education/data/data_2011_state_base.xlsx"))

# 2012 ==================
public_2012 <- tibble(rio::import("https://www.tn.gov/content/dam/tn/education/data/data_2012_state_base.xlsx"))

# 2013 ===================
public_2013 <- tibble(rio::import("https://www.tn.gov/content/dam/tn/education/data/data_2013_state_base.xlsx"))

# 2014 ===================
public_2014 <- tibble(rio::import("https://www.tn.gov/content/dam/tn/education/data/data_2014_state_base.xlsx"))

# 2015 ===================
public_2015 <- tibble(rio::import("https://www.tn.gov/content/dam/tn/education/data/data_2015_state_base.xlsx"))

# 2016 ===================
public_2016 <- tibble(rio::import("https://www.tn.gov/content/dam/tn/education/data/data_2016_suppressed_state_base.xlsx"))

# 2017 ===================
public_2017 <- read_csv("https://www.tn.gov/content/dam/tn/education/data/data_2017_state_base.csv")

# 2018 ===================
public_2018 <- read_csv("https://www.tn.gov/content/dam/tn/education/data/data_2018_state_base_grade_level.csv")

# 2019 ===================
public_2019 <- read_csv("https://www.tn.gov/content/dam/tn/education/accountability/2019/state_assessment_file_suppressed.csv")

# 2021 ===================
public_2021 <- read_csv("https://www.tn.gov/content/dam/tn/education/accountability/2021/state_assessment_file_suppressed.csv")

# 2022 ===================
public_2022 <- read_csv("N:/ORP_accountability/data/2022_final_accountability_files/state_assessment_file_suppressed.csv")

non_public_2022 <- read_csv("N:/ORP_accountability/data/2022_final_accountability_files/state_assessment_file.csv")

public_2022 <- left_join(
  public_2022 %>% select(
    "year","system","system_name","test" , "subject","grade","subgroup",
    "participation_rate", "pct_below" ,"pct_approaching", "pct_met_expectations", 
    "pct_exceeded_expectations" , "pct_met_exceeded"
  ), 
  non_public_2022 %>% select(
    'year','system','system_name','test','subject','grade','subgroup', "enrolled","tested", "valid_tests"
  ), 
  by = c('year','system','system_name','test','subject','grade','subgroup')
)

# Combine =================
combined_public <- bind_rows(
  public_2010 %>%
    transmute(
      year = 2010,
      # system = `District ID`, system_name = `District Name`,
      subject = Subject, grade = Grade, subgroup = `Student Group`,
      enrolled = `Number Enrolled`,
      valid_tests = as.character(`Number of Valid Tests`),
      pct_met_exceeded = `Percent Proficient or Advanced`
    ),
  public_2011 %>% 
    transmute(
      year = 2011,
      # system = `District ID`, system_name = `District Name`,
      subject = Subject, grade = Grade, subgroup = `Student Group`,
      enrolled = `Number Enrolled`,
      valid_tests = as.character(`Number of Valid Tests`),
      pct_met_exceeded = `Percent Proficient or Advanced`
    ),
  public_2012 %>% 
    transmute(
      year = 2012,
      # system = `District ID`, system_name = `District Name`,
      subject = Subject, grade = Grade, subgroup = `Student Group`,
      enrolled = `Number Enrolled`,
      valid_tests = as.character(`Number of Valid Tests`),
      pct_met_exceeded = `Percent Proficient or Advanced`
    ),
  public_2013 %>% 
    transmute(
      year,
      # system, system_name,
      subject, grade, subgroup,
      # enrolled = `Number Enrolled`, 
      valid_tests,
      pct_met_exceeded = pct_prof_adv
    ),
  public_2014 %>% 
    transmute(
      year,
      # system, system_name,
      subject, grade, subgroup,
      # enrolled = `Number Enrolled`, 
      valid_tests = as.character(valid_tests),
      pct_met_exceeded = pct_prof_adv
    ),
  public_2015 %>% 
    transmute(
      year,
      # system, system_name,
      subject, grade, subgroup,
      # enrolled = `Number Enrolled`, 
      valid_tests = as.character(valid_tests),
      pct_met_exceeded = pct_prof_adv
    ),
  public_2017 %>% 
    transmute(
      year,
      # system, system_name,
      subject, grade = as.character(grade), 
      subgroup,
      # enrolled = `Number Enrolled`, 
      valid_tests = as.character(valid_tests),
      pct_met_exceeded = pct_on_mastered
    ),
  public_2018 %>% 
    filter(test == "TNReady") %>%
    transmute(
      year,
      # system, system_name,
      subject, grade, subgroup,
      # enrolled = `Number Enrolled`, 
      valid_tests = as.character(valid_tests),
      pct_met_exceeded = pct_on_mastered
    ),
  public_2019 %>% 
    filter(test == "TNReady") %>% 
    transmute(
      year,
      # system, system_name,
      subject, grade, subgroup,
      # enrolled, 
      valid_tests = as.character(valid_tests),
      pct_met_exceeded = pct_on_mastered
    ) ,
  public_2021 %>% 
    filter(test == "TNReady") %>% 
    transmute(
      year,
      # system, system_name,
      subject, grade, subgroup,
      # enrolled, 
      valid_tests = as.character(valid_tests),
      pct_met_exceeded = as.character(pct_on_mastered)
    ) ,
  public_2022 %>% 
    filter(test == "TNReady") %>% 
    transmute(
      year,
      # system, system_name,
      subject, grade, subgroup,
      # enrolled, 
      valid_tests = as.character(valid_tests),
      pct_met_exceeded = as.character(pct_met_exceeded)
    ) 
) %>% 
  mutate(
    subject = case_when(
      subject %in% c("Reading/Language", "RLA") ~ "ELA",
      TRUE ~ subject
    ),
    subgroup = case_when(
      subgroup == "Native American" ~ "American Indian or Alaska Native",
      subgroup == "All" ~ "All Students",
      subgroup == "Economically Disadvantaged (Free or Reduced Price Lunch)" ~ "Economically Disadvantaged",
      subgroup == "English Language Learners" ~ "English Learners",
      subgroup %in% c("English Learners with T1/T2", "English Language Learners with T1/T2") ~ "English Learners with Transitional 1-4",
      subgroup %in%  c("Hawaiian or PI", "Hawaiian or Pacific Islander") ~ "Native Hawaiian or Other Pacific Islander",
      subgroup %in% c(
        "Non-English Language Learners/T1 or T2", "Non-English Learners/T1 or T2"
      ) ~ "Non-English Learners/Transitional 1-4",
      subgroup == "Non-English Language Learners" ~ "Non-English Learners",
      subgroup == "Black" ~ "Black or African American",
      TRUE ~ subgroup
    )
  ) %>% 
  filter((grade == "3" & subject == "ELA") | (grade == "7" & subject == "Math") |
           (grade == "4" & subject == "ELA")) %>% 
  arrange(grade, subgroup, -year)

# write_csv(combined_public, "raw_district_assessment_data_downloads_2011-2019.csv", na = '')
write_csv(combined_public, "public_state_assessment_data_2010-2022.csv", na = '')









