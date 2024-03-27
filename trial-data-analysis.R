library(here)
library(tidyr)
library(stringr)
library(ggplot2)
library(dplyr)
library(tidyverse)


# this is to read in the dataframe
weed_time = read.csv(here::here("00-data", "b-prepared", "20230814_SCM-Weeding Labor-UMN.csv"))
str(weed_time)

# split the time between hours and minutes into different columns
split <- strsplit(weed_time$total.time, split = ":")
weed_time$hours <- sapply(split, `[`, 1)  
weed_time$minutes <- sapply(split, `[`, 2)

str(weed_time)
# convert hours and minutes into num instead of chr
weed_time$hours <- as.numeric(weed_time$hours)
weed_time$minutes <- as.numeric(weed_time$minutes)

# converting hours into minutes, multiplying by 60min per hour
weed_time$total_minutes = weed_time$hours*60 + weed_time$minutes

# creating aggregate, summing the total time spent weeding to one variable
wt_agg = aggregate(weed_time$total_minutes, list(weed_time$date_collected, weed_time$treatment, weed_time$crop), FUN = sum)

str(wt_agg)                   

#changing the date format from 00Jun23 to numeric/julien date
wt_agg$numeric_date = as.Date(wt_agg$Group.1, format = "%d%b%Y")

ggplot(wt_agg, aes(x = numeric_date, y = x, group = Group.2, color = Group.2)) + geom_line()

#changing column titles
colnames(wt_agg) <- c("og_date", "treatment", "crop", "total_minutes", "date")

ggplot(wt_agg, aes(x = date, y = total_minutes, color = treatment)) +
  geom_line() +
  facet_wrap(~crop, labeller = labeller(crop = c(AS = "Albion Strawberries", DB = "Dragon Tongue Beans", FC = "Flash Collards"))) +
  ###adding different crops, changing their labels
  labs( ###changing graph titles
    x = "Date",
    y = "Total Time Spend Removing Weeds (minutes)", 
    color = "Mulch Treatment",
    title = "How Mulch Treatments Influence Total Time Spent Weeding",
    caption = "Data source: University of Minnesota"
  )+
  scale_color_manual( ####changing plot titles, line colors
    labels = c("BM" = "Bare Soil",   
               "CM" = "Shredded Cardboard Mulch",
               "SM" = "Straw Mulch"),
    
    values = c("BM" = "brown",
               "CM" = "forestgreen",
               "SM" = "goldenrod")
  )+
  scale_x_date(date_labels = "%b", date_breaks = "3 weeks")

# now calculate cumulative weeding time
wt_plot_cum = aggregate(weed_time$total_minutes, list(weed_time$block, weed_time$treatment, weed_time$crop), FUN = sum)

boxplot(wt_plot_cum$x ~ wt_plot_cum$Group.2, col = c("brown", "forestgreen", "goldenrod"), xlab = "Mulch Treatment", ylab = "Total Time Spent Weeding (minutes)", main = "Total Time Spent Weeding by Mulch Treatment", ylim = c(0,150))

###### NOW DOING MOISTURE USING SAME STEPS FROM ABOVE
vwc = read.csv(here::here("00-data", "a-raw", "umn_scm_soilmoisture.csv"))
str(vwc)

# Filter out rows with "N/A" values in treatment or crop
vwc_filtered <- vwc[!(vwc$treatment == "N/A" | vwc$crop == "N/A"), ]

# Create vwc_agg from the filtered data
vwc_agg <- aggregate(vwc_filtered$VWC., list(vwc_filtered$field_date, vwc_filtered$treatment, vwc_filtered$crop), FUN = mean)

vwc_agg$numeric_date = as.Date(vwc_agg$Group.1, format = "%d%b%Y")

ggplot(vwc_agg, aes(x = numeric_date, y = x, group = Group.2, color = Group.2)) + geom_line()

#changing the column names
colnames(vwc_agg) <- c("og_date", "treatment", "crop", "volumetric_water_content", "date")

#creating plot showing all crops, excluding "N/A" treatments and crops. 
ggplot(vwc_agg, aes(x = date, y = volumetric_water_content, color = treatment)) +
  geom_line(data = subset(vwc_agg, treatment != "N/A" & crop != "N/A")) +
  facet_wrap(~crop, labeller = labeller(crop = c(AS = "Albion Strawberries", DB = "Dragon Tongue Beans", FC = "Flash Collards"))) +
 ###adding different crops, changing their labels
  labs( ###changing graph titles
    x = "Date",
    y = "Volumetric Water Content (%)", 
    color = "Mulch Treatment",
    title = "Effect of Mulch Treatments on Soil Volumetric Water Content",
    caption = "Data source: University of Minnesota"
  )+
  scale_color_manual( ####changing plot titles, line colors
    labels = c("BM" = "Bare Soil",   
               "CM" = "Shredded Cardboard Mulch",
               "SM" = "Straw Mulch"),
    
    values = c("BM" = "brown",
               "CM" = "forestgreen",
               "SM" = "goldenrod")
  )





###### NOW DOING SPAD GRAPH
spad = read.csv(here::here("00-data", "a-raw", "umn_scm_spad.csv"))
str(spad)

spad_agg = aggregate(spad$SPAD_reading, list(spad$date_collected, spad$treatment, spad$crop), FUN = mean)
str(spad_agg)

spad_agg$numeric_date = as.Date(spad_agg$Group.1, format = "%d%b%Y")

colnames(spad_agg) <- c("og_date", "treatment", "crop", "spad_reading", "date")

#creating multiple graphs to show different crops
ggplot(spad_agg, aes(x = date, y = spad_reading, color = treatment)) +
  geom_point() +
  geom_line(aes(group = treatment)) +
  facet_wrap(~crop) +
  labs(
    x = "Date",
    y = "SPAD Reading",
    color = "Treatment"
  ) 

#getting rid of NA data
spad_agg1 <- spad_agg %>% filter(!is.na(spad_reading) & !is.na(date))

#creating new graphs with clean data
ggplot(spad_agg1, aes(x = date, y = spad_reading, color = treatment)) +
  geom_line(aes(group = treatment)) + 
  facet_wrap(~crop, labeller = labeller (crop = c(AS = "Albion Strawberries", DB = "Dragon Tongue Beans", FC = "Flash Collards"))) + ###adding different crops, changing their labels
  labs( ###changing graph titles
    x = "Date",
    y = "Mean Leaf Clorophyll Concentrations (SPAD Units)", 
    color = "Mulch Treatment",
    title = "Soil Plant Analysis Development Reading Compared to Treatment and Crop",
    caption = "Data source: University of Minnesota"
  )+
  scale_color_manual( ####changing plot titles, line colors
    labels = c("BM" = "Bare Soil",   
               "CM" = "Shredded Cardboard Mulch",
               "SM" = "Straw Mulch"),
    
    values = c("BM" = "brown",
               "CM" = "forestgreen",
               "SM" = "goldenrod")
  )





####PLOTS FOR WEED BIOMASS- OD BIOMASS CALCULATED G
weed_biomass = read.csv(here::here("00-data", "a-raw", "umn_scm_weedbiomass.csv"))
str(weed_biomass)

biomass_agg = aggregate(weed_biomass$od_biomass_calculated_g, list(weed_biomass$date_collected, weed_biomass$treatment, weed_biomass$crop), FUN = mean)
str(biomass_agg)

biomass_agg$numeric_date = as.Date(biomass_agg$Group.1, format = "%d%b%Y")

colnames(biomass_agg) <- c("og_date", "treatment", "crop", "weed_biomass", "date")

biomass_agg <- biomass_agg %>% arrange(date)

#creating multiple graphs to show different crops
ggplot(biomass_agg, aes(x = date, y = weed_biomass, color = treatment)) +
geom_line(aes(group = treatment)) + 
  facet_wrap(~crop, labeller = labeller (crop = c(AS = "Albion Strawberries", DB = "Dragon Tongue Beans", FC = "Flash Collards"))) + ###adding different crops, changing their labels
  labs( ###changing graph titles
    x = "Date",
    y = "Weed Biomass (g)", 
    color = "Mulch Treatment",
    title = "Mulch Effects on Weed Biomass in Crops",
    caption = "Data source: University of Minnesota"
  )+
  scale_color_manual( ####changing plot titles, line colors
    labels = c("BM" = "Bare Soil",   
               "CM" = "Shredded Cardboard Mulch",
               "SM" = "Straw Mulch"),
    
    values = c("BM" = "brown",
               "CM" = "forestgreen",
               "SM" = "goldenrod")
  ) +
  scale_x_date(date_labels = "%b", date_breaks = "3.75 weeks")






####PLOTS FOR YIELD:MARKETABLE
yield_marketable = read.csv(here::here("00-data", "a-raw", "umn_scm_yield.csv"))
str(yield_marketable)

# Filter out rows where marketable_yield is "na" and treatment is "edge"
clean_yield <- yield_marketable[!(yield_marketable$marketable_yield_g == "na" | yield_marketable$treatment == "edge"), ]
str(clean_yield)

# Aggregate the clean data
yield_agg = aggregate(clean_data$marketable_yield_g, list(clean_data$date_collected, clean_data$treatment, clean_data$crop), FUN = mean)

str(yield_agg)

# Convert date to Date format
yield_agg$numeric_date = as.Date(yield_agg$Group.1, format = "%d%b%Y")

# Rename columns
colnames(yield_agg) <- c("og_date", "treatment", "crop", "marketable_yield", "date")

#Create plot
ggplot(yield_agg, aes(x = date, y = marketable_yield, color = treatment)) +
  geom_line(aes(group = treatment)) + 
  facet_wrap(~crop, labeller = labeller (crop = c(AS = "Albion Strawberries", DB = "Dragon Tongue Beans", FC = "Flash Collards"))) + ###adding different crops, changing their labels
  labs( ###changing graph titles
    x = "Date",
    y = "Marketable Yield (g)", 
    color = "Mulch Treatment",
    title = "Mulch Treatment's Impact on Marketable Yield in Crops",
    caption = "Data source: University of Minnesota"
  )+
  scale_color_manual( ####changing plot titles, line colors
    labels = c("BM" = "Bare Soil",   
               "CM" = "Shredded Cardboard Mulch",
               "SM" = "Straw Mulch"),
    
    values = c("BM" = "brown",
               "CM" = "forestgreen",
               "SM" = "goldenrod")
  )

##### Now soil parameters
soil = read.csv(here::here("00-data", "b-prepared", "SCM-soil-data.csv"))
str(soil)

# side-by-side boxplot of NO3 by treatment and date - here i want a grouped boxplot where dates are separated out

ggplot(soil, aes(x = Treatment, y = NO3_ppm, fill = Sample_Date)) +
  geom_boxplot() +
  labs(
    x = "Treatment",
    y = "NO3 (ppm)",
    fill = "Sample_Date"
  )

soil_mid_late <- soil[soil$Sample_Date != "23May2023", ]

ggplot(soil_mid_late, aes(x = Treatment, y = NO3_ppm, fill = Sample_Date)) +
  geom_boxplot() +
  labs(
    x = "Treatment",
    y = "NO3 (ppm)",
    fill = "Sample_Date"
  )

# anova for treatment differences bby date
no3_anova <- aov(NO3_ppm ~ Treatment * Sample_Date, data = soil)
summary(no3_anova)

# posthoc test Tukey's
TukeyHSD(no3_anova)

# anova for treatment differences bby date
nh4_anova <- aov(NH4_ppm ~ Treatment * Sample_Date, data = soil)
summary(nh4_anova)

# posthoc test Tukey's
TukeyHSD(nh4_anova)

soil_02oct = soil[soil$Sample_Date == "02Oct2023", ]

ggplot(soil_02oct, aes(x = Treatment, y = NO3_ppm)) +
  geom_boxplot() +
  labs(
    x = "Treatment",
    y = "NO3 (ppm)"
  )

no3_anova_02oct <- aov(NO3_ppm ~ Treatment, data = soil_02oct)
summary(no3_anova_02oct)

# posthoc test Tukey's
TukeyHSD(no3_anova_02oct)

q: how do i remove all rows with value of "23May2023" in Sample_Date?
  
a: soil <- soil[soil$Sample_Date != "23May2023", ]

NO3_agg = aggregate(soil$NO3_mgkg, list(soil$Sample_Date, soil$treatment), FUN = mean)