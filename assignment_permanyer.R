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
  

# 2. What is the level of inequality for the different household types? 
# Use the different inequality measures presented in the course. 
# Are the different household types ranked consistently when using the different inequality indices?

gini_by_house <- gini_decomp(ineq$Income, ineq$house_cat, weights = ineq$Sample_Weight)

theil_by_house 


  
  
# 4. Let’s define the poverty threshold as 60% of the median (the standard definition used in the EU). 
# What is the level of poverty in that society according to FGT0, FGT1, and FGT2?

# Unweighted
poverty_z <- median(ineq$Income) * 0.6

# Weighted
ineq$poverty_z2 <- median(rep(ineq$Income, times=ineq$Sample_Weight)) * 0.6

# Calculating who is poor given Z, and calculating the poverty gap
ineq <- ineq %>% 
          mutate(is_poor = as.factor(case_when(Income < poverty_z2 ~ 1,
                                               TRUE ~ 0)),
                 poverty_gap = if_else(poverty_z2 - Income < 0, 0, poverty_z2 - Income))

