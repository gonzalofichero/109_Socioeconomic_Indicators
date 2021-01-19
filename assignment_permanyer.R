# Loading all necessary packages
library(tidyverse)
library(readxl)
library(dineq)
library(sf)
library(rworldmap)
library(naniar)


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

for 

ineq %>%
  filter(Hhtype == 2) %>% 
  mutate(veo = dineq::theil.wtd(Income, weights = Sample_Weight),
         )


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




####################
# EXERCISE 2

# Loading data of exercise 2

gender <- read_xlsx("Exercise_Day2.xlsx", sheet = "Indices")

glimpse(gender)


# Calculating Gaps between Women and Men for each feature
gender %>% 
  mutate(gap_LE = LE_male - LE_female,
         gap_EYS = EYS_male - EYS_female,
         gap_MYS = MYS_male - MYS_female,
         gap_GNI = GNIpc_male - GNIpc_female,
         qty_missing = is.na(gap_LE) + is.na(gap_EYS) + is.na(gap_MYS) + is.na(gap_GNI)) -> gender


# Check missing data by column
vis_miss(gender %>% select(gap_LE, gap_EYS, gap_MYS, gap_GNI))

# Distribution of missings
prop.table(table(gender$qty_missing))

gender %>% 
  ggplot(aes(x=qty_missing)) + geom_bar()



# Loading the ISO codes for mapping countries
un_areas <- read_csv("UN_areas_all.csv")


# Joining data with ISO codes
gender %>% 
  rename(iso2 = cty_code) %>% 
  left_join(un_areas, by = "iso2") -> gender_v2


# Calculating distribution for gaps by subregion
gender_v2 %>% 
  ggplot(aes(x=gap_LE, color = region)) + geom_density()

gender_v2 %>% 
  ggplot(aes(x=gap_EYS, color = region)) + geom_density()

gender_v2 %>% 
  ggplot(aes(x=gap_MYS, color = region)) + geom_density()

gender_v2 %>% 
  ggplot(aes(x=gap_GNI, color = region)) + geom_density()


# Oceania is always bi-modal distribution...
# For the other could find mean + std and assign random number in 2+- std interva?


# Calculating mean and std of each continent
gender_v2 %>% 
  group_by(region) %>% 
  summarize(mean_gap_LE = mean(gap_LE, na.rm = T),
            mean_gap_EYS = mean(gap_EYS, na.rm = T),
            mean_gap_MYS = mean(gap_MYS, na.rm = T),
            mean_gap_GNI = mean(gap_GNI, na.rm = T),
            sd_gap_GNI = sd(gap_GNI, na.rm = T),
            sd_gap_EYS = sd(gap_EYS, na.rm = T),
            sd_gap_MYS = sd(gap_MYS, na.rm = T),
            sd_gap_LE = sd(gap_LE, na.rm = T)) -> to_imput

# Imputing random value between -+2 SD from mean, by continent for each missing value
# and setting seed for random assignment
gender_imput <- gender_v2 %>% left_join(to_imput, by = "region")

gender_imput %>% 
  mutate(


# Generate sf data for plotting
joinCountryData2Map(gender, joinCode="ISO3", nameJoinColumn="iso3") -> gender_mapped

# Plotting
mapCountryData(gender_mapped, nameColumnToPlot='gap_GNI')




