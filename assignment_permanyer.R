# Loading all necessary packages
library(tidyverse)
library(readxl)
library(dineq)


####################
# EXERCISE 1

# Loading data of exercise 1

ineq <- read_xlsx("Income_data.xlsx")

glimpse(ineq)


# Transforming Sex and Household to readable categories
ineq <- ineq %>% 
          mutate(sex_cat = case_when(Sex == 1 ~ "M",
                                     Sex == 2 ~ "F",
                                     TRUE ~ "Check"),
                 house_cat = case_when(Hhtype == 1 ~ "01 - Unipersonal households",
                                       Hhtype == 2 ~ "02 - Households without family relationships",
                                       Hhtype == 3 ~ "03 - Couples without children",
                                       Hhtype == 4 ~ "04 - Couples with children",
                                       Hhtype == 5 ~ "05 - Single fathers",
                                       Hhtype == 6 ~ "06 - Single mothers",
                                       Hhtype == 11 ~ "11 - Other",
                                       TRUE ~ "Check")
                 )

# Quick plot to see what's going on with Income
# Using Sample_Weight to have correct distribution

# For Sex
ineq %>% 
  ggplot(aes(x = Income, weight = Sample_Weight)) + geom_density(aes(color = sex_cat))

# For Household
ineq %>% 
  ggplot(aes(x = Income, weight = Sample_Weight)) + geom_density(aes(color = house_cat))



# 1. What is the level of inequality in this country? 
# Use the different inequality measures presented in the course.

gini.wtd(ineq$Income, weights = ineq$Sample_Weight)
  
theil.wtd(ineq$Income, weights = ineq$Sample_Weight)
  


