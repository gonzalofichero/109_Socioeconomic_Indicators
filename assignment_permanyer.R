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

# Calculating theil for each subset
ineq %>% 
  group_by(house_cat) %>% 
  summarize(gini_group = dineq::gini.wtd(Income, weights = Sample_Weight)) %>% 
  ungroup() -> gini_group


# Calculating theil for each subset
ineq %>% 
  group_by(house_cat) %>% 
  summarize(theil_group = dineq::theil.wtd(Income, weights = Sample_Weight)) %>% 
  ungroup() -> theil_group

# DEPRECATED CODE! Now the looping is working
# 
# ineq %>% 
#   filter(Hhtype == 1) %>% 
#   mutate(theil_group = dineq::theil.wtd(Income, weights = Sample_Weight)) -> theil_1
# ineq %>% 
#   filter(Hhtype == 2) %>% 
#   mutate(theil_group = dineq::theil.wtd(Income, weights = Sample_Weight)) -> theil_2
# ineq %>% 
#   filter(Hhtype == 3) %>% 
#   mutate(theil_group = dineq::theil.wtd(Income, weights = Sample_Weight)) -> theil_3
# ineq %>% 
#   filter(Hhtype == 4) %>% 
#   mutate(theil_group = dineq::theil.wtd(Income, weights = Sample_Weight)) -> theil_4
# ineq %>% 
#   filter(Hhtype == 5) %>% 
#   mutate(theil_group = dineq::theil.wtd(Income, weights = Sample_Weight)) -> theil_5
# ineq %>%
#   filter(Hhtype == 6) %>%
#   mutate(theil_group = dineq::theil.wtd(Income, weights = Sample_Weight)) -> theil_6
# ineq %>% 
#   filter(Hhtype == 11) %>% 
#   mutate(theil_group = dineq::theil.wtd(Income, weights = Sample_Weight)) -> theil_11
# 
# # Binding all together
# theil_group <- rbind(theil_1,theil_2,theil_3,theil_4,theil_5,theil_6,theil_11) %>% 
#                 group_by(house_cat) %>% 
#                 summarize(theil_group = max(theil_group))


# Calculating variability coefficient by group
library(Hmisc)

ineq %>% 
  group_by(house_cat) %>% 
  mutate( group_mean = weighted.mean(Income, Sample_Weight),
          group_sd = sqrt(wtd.var(Income, Sample_Weight)),
          group_cv = group_sd/group_mean) %>% 
  slice(which.max(group_cv)) %>% 
  select(house_cat, group_cv) -> cv_by_group
  


# Binding both index into data.frame

# Arranging by Index and calcularint Rank for each
gini_group %>% 
  left_join(theil_group, by = "house_cat") %>% 
  left_join(cv_by_group, by = "house_cat") %>%
  arrange(group_cv) %>% 
  mutate(cv_rank = row_number()) %>%
  arrange(theil_group) %>% 
  mutate(theil_rank = row_number()) %>% 
  arrange(gini_group) %>% 
  mutate(gini_rank = row_number()) -> ineq_group


# Create table by rankings




# 3. Decompose total inequality in inequality within groups + inequality between groups 
# (use the (decomposable) inequality measure you prefer). 
# What is the contribution of each component to overall inequality?

# 1) Calculating weight and mean for whole sample
# 2) Calculating weight and mean for each House type group
# 3) Adding the theil index already calculated in point 2
# 4) Creating the within and between for each group
# 5) Summup the total for the whole sample: between + within
ineq %>% 
  mutate(total_w = sum(Sample_Weight),
         total_mu = weighted.mean(Income, Sample_Weight)) %>%
  group_by(house_cat) %>%
  summarise(g_w = sum(Sample_Weight),
            g_mu = weighted.mean(Income, Sample_Weight),
            total_w = max(total_w),
            total_mu = max(total_mu)) %>% 
  left_join(theil_group, by = "house_cat") %>% 
  mutate(theil_within = (g_w / total_w) * (g_mu / total_mu) * theil_group,
         theil_between = (g_w / total_w) * (g_mu / total_mu) * log(g_mu / total_mu)) -> theil_decomp

theil_decomp %>% 
  summarize(theil_w = sum(theil_within),
            theil_b = sum(theil_between))


  
# 4. Let’s define the poverty threshold as 60% of the median (the standard definition used in the EU). 
# What is the level of poverty in that society according to FGT0, FGT1, and FGT2?

# Unweighted
poverty_z <- median(ineq$Income) * 0.6

# Weighted
ineq$poverty_z2 <- median(rep(ineq$Income, times=ineq$Sample_Weight)) * 0.6

# Calculating who is poor given Z, and calculating the poverty gap
ineq <- ineq %>% 
          mutate(is_poor = case_when(Income < poverty_z2 ~ 1,
                                               TRUE ~ 0),
                 poverty_gap = if_else(poverty_z2 - Income < 0, 0, poverty_z2 - Income))

ineq %>% 
  mutate(total_w = sum(Sample_Weight),
         fgt_0i = is_poor*(Sample_Weight/total_w),
         fgt_1i = poverty_gap * (Sample_Weight/total_w),
         fgt_2i = (poverty_gap * poverty_gap) * (Sample_Weight/total_w)) %>%
  summarize(fgt_0 = sum(fgt_0i),
            fgt_1 = sum(fgt_1i),
            fgt_2 = sum(fgt_2i))
  

# 5. What is the level of poverty (according to FGT0, FGT1, and FGT2) 
# for the different household types? 
# Are the different household types ranked consistently when using the different poverty indices?

ineq %>% 
  group_by(house_cat) %>% 
  summarize(group_w = sum(Sample_Weight),
             fgt_0i = sum(is_poor*(Sample_Weight/group_w)),
             fgt_1i = sum(poverty_gap * (Sample_Weight/group_w)),
             fgt_2i = sum((poverty_gap * poverty_gap) * (Sample_Weight/group_w))) %>% 
  arrange(fgt_0i) %>% 
  mutate(fgt_0_rank = row_number()) %>% 
  arrange(fgt_1i) %>% 
  mutate(fgt_1_rank = row_number()) %>% 
  arrange(fgt_2i) %>% 
  mutate(fgt_2_rank = row_number()) -> poverty_group

# Create table by rankingsof poverty


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

# Countries by #Missing and Region
table(gender_v2$region, gender_v2$qty_missing)


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
# For the other could find mean + std and assign random number in 1+- std interva?


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

# Imputing random value between -+1 SD from mean, by continent for each missing value
# and setting seed for random assignment
# Standardizing MAX-MIN each gap feature
gender_imput <- gender_v2 %>% left_join(to_imput, by = "region")

set.seed(42)

# Own function for max-min normalization
normalize <- function(x)
{
  return((x- min(x)) /(max(x)-min(x)))
}


gender_imput %>% 
  mutate(
         # For each feature, I'm assigning a number +-1 standard deviation from the mean, group by region
         gap_LE = if_else(is.na(gap_LE), mean_gap_LE + sd_gap_LE * runif(1, min = -1, max = 1), gap_LE),
         gap_MYS = if_else(is.na(gap_MYS), mean_gap_MYS + sd_gap_MYS * runif(1, min = -1, max = 1), gap_MYS),
         gap_EYS = if_else(is.na(gap_EYS), mean_gap_EYS + sd_gap_EYS * runif(1, min = -1, max = 1), gap_EYS),
         gap_GNI = if_else(is.na(gap_GNI), mean_gap_GNI + sd_gap_GNI * runif(1, min = -1, max = 1), gap_GNI),
         # Normalizing with max-min normalization (own function, run)
         # All feature will be in [0,1] range
         std_gap_LE = normalize(gap_LE),
         std_gap_MYS = normalize(gap_MYS),
         std_gap_EYS = normalize(gap_EYS),
         std_gap_GNI = normalize(gap_GNI),
         # Adding weights for each feature
         # MYS and EYS are same dimension, hence 1/6 each, LE and GNI 1/3 => Index will still be in [0,1] range
         gender_index = (1/3) * std_gap_LE + (1/6) * std_gap_MYS + (1/6) * std_gap_EYS + (1/3) * std_gap_GNI
  ) -> gender_imput

# Creating small data frame for plotting
gender_imput %>% 
  select(Country, iso3, country_code, 
         std_gap_LE, std_gap_MYS, std_gap_EYS, std_gap_GNI,
         gender_index) -> gender_small


# Generate sf data for plotting
joinCountryData2Map(gender_small, joinCode="ISO3", nameJoinColumn="iso3") -> gender_mapped

# First map: DEPRECATED
# mapCountryData(gender_mapped, nameColumnToPlot='gender_index')


# Plotting
library(classInt)
library(RColorBrewer)

# getting class intervals
classInt <- classIntervals(gender_mapped[["gender_index"]], n=6, style = "jenks")
catMethod = classInt[["brks"]]

# getting colours: reverse pallete so purple (feminism) is better
colourPalette <- rev(brewer.pal(6,'RdPu'))

# https://www.r-bloggers.com/2016/07/creating-color-palettes-in-r/
mycols <- colors()[c(465, 542, 419, 520, 616, 29)]

# plot map
mapDevice() #create world map shaped window
mapParams <- mapCountryData(gender_mapped
                            ,nameColumnToPlot="gender_index"
                            ,addLegend=FALSE
                            ,catMethod = catMethod
                            ,colourPalette=mycols)

#adding legend
do.call(addMapLegend, c(mapParams, legendLabels="all", legendWidth=0.5, legendIntervals="data", legendMar = 2))




#####################################
# EXERCISE 3

# Assume we are working with a variable bounded between 0 and 100 (e.g. a percentage).
# Assume also that the mean of the distribution is mu.


# Q1. Among all possible distributions with mean value equal to 𝜇, which one done does maximize inequality?




# Q2. What is the value of the Gini index or the Variance (choose the one you prefer) when applied
# to the previous inequality-maximizing distribution?


# Q3. Plot the values of maximal inequality found in Q2 against all values of 𝜇 ranging between 0 and 100.




