Preliminary

library(rmarkdown)
library(datasauRus)
library(lubridate)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(maps)
library(ggrepel)
library(ggtext)
library(tinytex)
library(knitr)
library(sf)
library(tmap)
library(RColorBrewer)
library(classInt)
library(tmaptools)
library(ggforce)
library(gghighlight)
library(viridis)


getwd()

  3/20
  


PowerData <- read.csv('global_power_plant_database.csv')
Khadija_data<-read.csv("global_power_plant_database.csv")

names(Khadija_data)

1. Desc the Data
##dimensions of the data frame (rows and columns) and what types of variables are included (numerical, categorical etc.)?

glimpse(PowerData1)
summary(PowerData)
colnames(PowerData1)

_Answer_: There are 34936 observations in the data and 36 column variables. There are NA values (how many?)... 
          and  lots of "missing values" (empty cells)!
The dataset contains information about power plants in various countries. The dataset includes the following columns:
  #country: The country where the power plant is located.
  #m name of the country where the power plant is located.
 #name: The name of the powercountry_long: The long for plant.
  #gppd_idnr: A unique identifier for the power plant.
  #capacity_mw: The capacity of the power plant in megawatts.
  #latitude: The latitude of the power plant's location.


2. Data Cleansing##############Rename the Column names#####################################

names(PowerData1)[names(PowerData1) %in% c('country','country_long','gppd_idnr','capacity_mw','primary_fuel','other_fuel1',
      'other_fuel2','other_fuel3','commissioning_year','geolocation_source','wepp_id','year_of_capacity_data','generation_gwh_2013',
      'generation_gwh_2014','generation_gwh_2015','generation_gwh_2016','generation_gwh_2017','generation_gwh_2018','generation_gwh_2019',
      'generation_data_source','estimated_generation_gwh_2013','estimated_generation_gwh_2014','estimated_generation_gwh_2015','estimated_generation_gwh_2016',
      'estimated_generation_gwh_2017')] <-
  c('countid','country','gppdidnr','capacity','pryfuel','ofuel1','ofuel2','ofuel3','commyear','geosource','weppid','yearofcapacity',
    'gen2013','gen2014','gen2015','gen2016','gen2017','gen2018','gen2019','gendatasource','estgen2013','estgen2014','estgen2015','estgen2016','estgen2017')
 ===INTRODUCTION: 
  
          
===============================================================================================
            
  ############Examine trends in the total capacity and generation by fuel type, which can provide insights into changes in the energy mix over time#################

  ===============================Calculate descriptive statistics ===============================
such as mean, median, mode, standard deviation, and range for numerical variables such as capacity_mw,
  latitude, longitude, commissioning_year, year_of_capacity_data, generation_gwh_2013-2019, and 
estimated_generation_gwh_2013-2017. 
This will help you understand the central tendency, variability, and distribution of the data
===================================================================================================
  

DONE==============CORRELATION COEFF BETWEEN THE PANTS CAPACITY AND EST GENERATION=============================

CorrCoef2_PowerData1 <- PowerData1 %>% 
  summarize(r = cor(capacity, estgen2017, use = 'pairwise.complete.obs'))

CorrCoef2_PowerData1 = 0.9489435     

The codes calculate the give you the correlation coefficient between the capacity(mw) and the 
estimated generation in the Power Plants in the year 2017 while excluding cases where either 
variable is missing or zero by using the use='pairwise.complete.obs'.
This calculation revealed a strong correlation between plants capacity and their corresponding 
estimated generation.




==============================  Geospatial Analysis:    ===========================================
  
select(PowerData1,pryfuel, longitude, latitude, capacity) %>%
  head()  

world <- map_data("world")


ggplot() +
  geom_map(data=world, map=world, aes(long, lat, map_id=region), color='red', fill='lightblue', size=.05) +
  geom_point(data = PowerData1, aes(x = longitude, y = latitude, color = pryfuel), alpha = .4) +
  theme_void() +
  theme(legend.position = 'right', plot.title = element_text(hjust = .5)) +
  labs(title="GLOBAL POWER PLANTS MAP BY PRIMARY FUEL", caption='Orange_ibrahim Power Plants Geospatial Vis.')


#The map plot is a visualization of global power plants by their primary fuel types. 
The plot consists of two geospatial layers: the base map layer and the power plant layer. 
The base map is a world map that is rendered in light blue color, with red outlines around 
the map boundaries. The power plant layer is plotted on top of the base map, and it contains 
a scatter plot of the power plants#' locations, with each point colored according to the primary fuel type. 
The legend is positioned on the right side of the plot, indicating the primary fuel types and their corresponding colors. The plot's title is "GLOBAL POWER PLANTS MAP BY PRIMARY FUEL," with a caption indicating the author's name.
  
#====================================================

===================USING LINE PLOT======================================================
  #######Overview the Dataset
  str(Data)
Summary(Data)

============Filter the data for specific country========================================
filteredData <-filter(data, country=='USA')

=====Filter the data for primary_fuel,Capacity_mw,Generation_gwh,EstimatedGeneration_gwh variables==========
Apply this:
  


=========================================================================================================
 view(TotalGen_by_fuel)

==OK OK OK OK OK==GIVE CHRISPAUL======######Line Plot of the Total Generation by Fuel Type######

ggplot(TotalGen_by_fuel, aes(x=pryfuel, y=totalGen2013, color=pryfuel)) +
  geom_line(linewidth=1) +
  geom_point(size=2) +
  labs(x='Primary Fuel', y='Total Generation (GWh)', title='Total generation by fuel type',
       caption='Orange_ibro Works') + 
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(), legend.position='right') +
  scale_y_continuous(limits=c(0, max(TotalGen_by_fuel$totalGen2013)*1.1))
         

============================
  ===================================SCATTER PLOT OK===========================================================
  ===========THe Big One ===GIVE CHRIS PAUL=====================
  ggplot(PowerData1, aes(capacity, gen2013)) + 
  annotate(geom = "rect", xmin = 1.5, xmax = 2, ymin = 40, ymax = 45, fill = "lightblue", alpha = 0.2) +
  geom_point(aes(colour = factor(pryfuel))) +
  geom_mark_ellipse(aes(label = pryfuel, group = pryfuel)) +
  scale_x_continuous(limits = c(0, 5), breaks = seq(0, 5, 0.5)) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 50, 5)) +
  labs(
    x = "Capacity (mw)", 
    y = "Generation_2013(gwh)", 
    colour = "Pry Fuel",
    title = 'Power Plant Database SCATTER PLOT',
    subtitle = 'Total Capacity by Generation In 2013'
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())



############Identifying any gaps or inconsistencies in the data that may require further investigation or data cleaning###
#####Visualizing Capacity and Generation by Fuel Type using line or Area plots to identify the relative contribution of different fuel types to the overall capacity and generation in different countries or regions#####
#####Using line or area plots to compare the trends in capacity and generation over time for different countries or regions. This can provide insights into the changes in energy production patterns and the growth of capacity and generation in different areas#######
######Using heat orchoropleth maps to visualize the spatial distr of capacity, generation, or other relevant metrics across different countries or regions. This can provide a geographical perspective on the energy production patterns and variations across different areas####
############## Country with the highest no of Power Plants ###############
############## Significant Variance in generation_gwh between 2013 and 2019 #####
############## Estimated_generation_gwh between 2013 and 2017 #####
############## The most desirable Power Plants ###############
############## Country with the most robust Power Plants ###############

############## Analysing the Dataset  ###############

#####Time Series Analysis: 
####to uncover patterns and trends over time.Use techniques such as time series ####decomposition,autocorrelation, and forecasting to analyze the historical generation of
######data and make future predictions.


       
       #######Filter the data for specific country and fuel type
       filteredData <-filter(data, country=='USA', primary_fuel=='coal')
       
       
3. Using EDA, Explore the distribution of variables, identify trends, patterns, and ##############################outliers, and analyze the relationships between variables
                     
4. Feature Engineering  
                     ######derive meaningful insights from existing features. e.g,calculate the age of each 
                     ###### power plant by subtracting the commissioning_year from the current year. 
                     ######You can also calculate the estimated generation percentage by dividing ######estimated_generation_gwh_2013-2017 by generation_gwh_2013-2019 to determine the ######accuracy of generation data.
                     
5. Data Visualization  
                     #####Use data visualization techniques to effectively communicate your findings. Create #####visualizations such as bar charts, pie charts, and maps to illustrate the distribution
                     ######of power plants by country, primary fuel type, and generation data.  
                     ######Use color coding or size coding to represent diff variables and make the ######Visualizations more informative and visually appealing.
                     
                     
6. Data Interpretation     ###############
                     #####Interpret the findings from your analysis and draw conclusions. Summarize the key 
                     #####insights, trends,and patterns identified from the data analysis 
                     
                     ############## Provide Recommendations ###############
                     
                     
7. Lastly#########It's important to thoroughly document the data visualization process, including the choice of plots, colours, scales, and interpretations of the visualizations. 
      #This documentation can help in explaining the insights obtained from the visualizations and supporting the conclusions drawn from the analysis.#########################################
                     
                     
                     
                     
