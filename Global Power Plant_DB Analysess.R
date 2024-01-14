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
 ===INTRODUCTION: density plot of the distribution of power plant capacity for different primary fuels=
            # ALSO DEFINE THE VARIABLES AND THIS =================================
          
          ggplot(PowerData1, aes(x=capacity, color=pryfuel, fill=pryfuel)) +
            geom_density(alpha=.45) + facet_wrap(~pryfuel) +
            theme_minimal() +
            labs(x = "Capacity (MW)", y = "Density", 
                 title = "Density Plot of Power Plant Capacity by Primary Fuel",
                 Caption = "Global Power Plant Database By OrangeIbro",
                 fill = "Primary Fuel", color = "Primary Fuel") +
            scale_color_viridis_d() + scale_fill_viridis_d()
          
          ##This code creates a density plot of the distribution of power plant capacity for different primary fuels using the ggplot2 package in R. The data is sourced from the Global Power Plant Database.
          The plot is faceted by the primary fuel type, with each facet showing the density curve for the corresponding fuel type. The capacity is displayed on the x-axis, and the density is displayed on the y-axis.
          The theme_minimal function is used to simplify the plot#s background and the labs function is used to label the x and y axes, as well as the title and subtitle of the plot. The fill and color of the 
          density curves are based on the primary fuel type, with the viridis_d color palette used to ensure distinct and visually appealing colors.         
          
===================================================================================================
############Identify the dominant fuel types used for power generation based on total capacity and generation#########
          summary(PowerData1)
          
          BiggestGen2019 <- PowerData1 %>%
            filter(gen2019 == max(gen2019, na.rm = TRUE) ) %>%
          select(country)
          
          BiggestCapacity <- PowerData1 %>%
            filter(capacity == max(capacity, na.rm = TRUE) ) %>%
            #elect(country)
          
          
          BigCountries<-PowerData1 %>%
            filter(capacity > 1500) 
          
          
=========================          GEOM COLUMN                =============================
            
            ggplot(BigCountries, aes(country, capacity))+ 
            geom_col() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 	
            gghighlight(capacity > 9900000000) 
          
          
          But in terms of Generation(GWh)
          
          ggplot(BigCountries, aes(country, gen2018))+ 
            geom_col() +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 	
            gghighlight(capacity > 9900000000) 
          
          Better like this###########################################Better like this 
          
          # # Define a color palette
          IbroPalette <- rainbow(length(BigCountries$country))
          
          # Create the column plot with highlighted big countries and color palette
          ggplot(BigCountries, aes(country, capacity, fill = capacity > 9900000000)) + 
            geom_col(color = "black", width = 0.8) +
            scale_fill_manual(values = c(IbroPalette[1], IbroPalette[length(BigCountries$country)])) +
            theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 	
            gghighlight(capacity > 9900000000) +
            labs(title = "Total Capacity by Country", 
                 subtitle = "Highlighting Countries with Capacity > 9.9 GW",
                 x = "", y = "Capacity (MW)") +
            theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
                  plot.subtitle = element_text(hjust = 0.5, size = 16),
                  axis.title.x = element_text(size = 14, vjust = -0.2),
                  axis.title.y = element_text(size = 14, vjust = 0.5),
                  legend.title = element_blank(),
                  legend.position = "bottom")
          
   ###In this code, fill = capacity > 9900000000 is added to the aes() function to fill the bars with 
          color based on whether the capacity of the country is greater than 9.9 GW. scale_fill_manual() is used to set the color palette with my_palette[1] for FALSE (countries with capacity <= 9.9 GW) and my_palette[5] for TRUE (countries with capacity > 9.9 GW).
          The resulting plot should have columns with two different colors - one for big countries (capacity > 9.9 GW) and another for small countries (capacity <= 9.9 GW).
          
          
  ====================BAR PLOTS==================================
   
          # Vector of bold country names
          BoldCountries <- c("China", "United States", "India", "Russia", "Japan", "Germany")
          
          # Create a color palette with a different color for each country
          Ibrahim <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a65628")
          
    
          # Filter the dataset to include only countries with generation >= 200 GWh in 2018
          Genfiltered200 <- PowerData1 %>%
            filter(gen2018 >= 200) %>%
            arrange(desc(gen2018))
          
          # Get the country with the highest generation in 2018
          BiggestGen2018 <- Genfiltered200 %>%
            filter(gen2018 == max(gen2018, na.rm = TRUE)) %>%
            select(country)
          
                 # Create the bar plot with different colors for each country
          ggplot(data = Genfiltered200, aes(x = country, y = gen2018, fill = country)) +
            geom_bar(stat = "identity") +
            scale_fill_manual(values = Ibrahim[1:length(unique(Genfiltered200$country))]) +
            xlab("") +
            ylab("Generation (GWh)") +
            ggtitle(paste("Country with highest generation in 2018:", BiggestGen2018$country)) +
            theme(axis.text.x = element_text(angle = 0, hjust = 1),
                  plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                  plot.margin = unit(c(1, 1, 1, 1), "cm"))
          
    ##   In this code, the filter() function is used to subset the PowerData1 dataset to include only 
          the countries with generation greater than or equal to 200 GWh in 2018 (dearth of data to work with in 2019). The resulting filtered 
          dataset is assigned to the PowerData_filtered object, which is then used as the data source for 
          the plot. By doing this, only the countries of interest are shown on the x-axis, helping to 
          decongest the plot.   
          
===============================================================================================
            
  ############Examine trends in the total capacity and generation by fuel type, which can provide insights into changes in the energy mix over time#################
          see
         
          
          ggplot(PowerData1, aes(x=capacity,color=pryfuel,fill=pryfuel)) +  
            geom_histogram(binwidth = .3)+ theme(legend.position='right') +
            labs(x='capacity(mw)',title ='HISTOGRAM OF THE GLOBAL POWER PLANT DATABASE:CAPACITY VS FUEL TYPE',
                 caption='Global Power Plant Database Visual. By Orange-ibro Viz')+
            facet_wrap(~pryfuel)
          
          ggplot(PowerData1, aes(x=gen2019,color=pryfuel
                                 ,fill=pryfuel))+
            geom_histogram(binwidth=.3)+facet_wrap(~pryfuel)        
          
          
          
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
  


==========================================================================================================

#####Total Capacity and Generation by fuel type 

TotalCapacity_by_fuel<-PowerData1 %>% 
  group_by(pryfuel) %>%
  summarize(TotalCapacity= sum(capacity,na.rm='TRUE'))

TotalGen_by_fuel2018<-PowerData1 %>% 
  group_by(pryfuel) %>%
  summarize(totalGen2018=sum(gen2013,na.rm='TRUE'))

######Line plot of the Total Capacity by Fuel Type


ggplot(PowerData1, aes(x = capacity, y = factor(pryfuel), color = pryfuel, group = pryfuel)) +
  geom_line(size = 1) +
  xlab('Total Capacity (MW)') +
  ylab('') +
  ggtitle('Total Capacity By Fuel Type') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_color_manual(values = viridis(15),
                     labels = levels(PowerData1$pryfuel)) +
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

#In this code, aes() is used to map the capacity variable to the x-axis, the pryfuel variable to the y-axis, and the pryfuel variable to both the color and group aesthetic. The group aesthetic is used to ensure that each line corresponds to a single fuel type, even though the pryfuel variable is a factor.
geom_line() is used to draw the lines, with size = 1 to make the lines more visible. 
scale_color_manual() is used to set the colors of the lines, with each color corresponding to a 
different fuel type. The labels argument is used to provide labels for the legend.

############Analyze generation data for specific power plants to understand their performance and contribution to total generation##########################

USAUKNIG_data <- filter(PowerData1, country %in% c('United States of America','United Kingdom','Nigeria'))

=====
  ggplot(USAUKNIG_data, aes(x=country, y=gen2017, color=country)) +
  geom_line(linewidth=.5) +
  geom_point(size=2) +
  labs(x='', y='Total Generation (GWh)', title='Total generation by fuel type',
       caption='Orange_ibro Works') +
  scale_y_continuous(limits= c(0, max(USAUKNIG_data$gen2017) * 1.1)) +
  theme_minimal() +
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), legend.position='right')
======
############Compare capacity and generation trends across different countries or regions to identify variations in energy production patterns#############
##This code produces a line plot of the total electricity generation (in GWh) for three countries 
(United States of America, United Kingdom, and Nigeria) using data from the USAUKNIG_data sub-dataset created from the 
world global power plant database. Each country is represented by a different color. The x-axis shows the country names, with USA taking 
the lead in generation in the year 2017, and the y-axis shows the total generation value in megaWatt hour.

The geom_line() function is used to connect the data points with a line, and geom_point() function is used to plot the data points themselves. The scale_y_continuous() function is used to set the limits of the y-axis. The theme_minimal() function sets a minimalistic theme for the plot, and theme() is used to remove the major gridlines on the x-axis and position the legend on the right side of the plot. The labs() function is used to add the title, subtitle, and caption to the plot. Overall, the plot shows the total electricity generation for each country in a clear and easy-to-understand way.
======================================THE SCATTER PLOT (2 VAR)========OK=============================================
  ggplot(TotalGen_by_fuel2018, aes(pryfuel, totalGen2018)) + 
  geom_point(aes(colour = pryfuel)) +
  labs(
    x = '', 
    y ='Total Generation (GWh)', 
    title = 'Power Plant Database SCATTER PLOT',
    subtitle = 'Total Capacity by Fuel Type')

then

# Define the plot


OR =========better=========
ggplot(TotalGen_by_fuel2018, aes(pryfuel, totalGen2018)) + 
  geom_line(aes(group = 1, color = pryfuel), size = 0.8) +
  geom_text(aes(label = pryfuel), hjust = 0.5, vjust = -0.5, size = 3) +
  labs(title = 'Global Power Plant Database Scatter Plot: Capacity Vs Fuel Type') +
  theme_minimal() +
  theme(legend.position = 'right', plot.title = element_text(hjust = .5)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())


#This plot is a scatter plot that visualizes the total generation of electricity by primary fuel type in the year 2018. 
#The scatter plot shows the relationship between pryfuel (primary fuel) and totalGen2018 (total electricity generated in 2018) for each fuel type as a point on the graph. The points are connected by lines of the same color to show trends in the data.
The scatter plot uses a minimal theme, with no labels on the x and y axis, and no background grid lines to draw attention to the data points. Additionally, the points are labeled with the pryfuel variable to help identify each data point. Overall, this plot is useful for comparing the total generation of electricity by primary fuel type in 2018 and for identifying trends in the data.
and changed the colors of the points and lines using the colour aesthetic. Finally, I customized the theme to make the plot more visually appealing.


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
         
====================================Lets checkout GGHIGHLIGHT===============================

  
   #or 

   
=========================          GEOM COLUMN                =============================
    
  ggplot(BigCountries, aes(country, capacity))+ 
    geom_col() +
    theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 	
    gghighlight(capacity > 9900000000) 
  
 
    But in terms of Generation(GWh)
  
    ggplot(BigCountries, aes(country, gen2018))+ 
      geom_col() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 	
      gghighlight(capacity > 9900000000) 
  
  #Aslo, see
  
  ggplot(airquality,
         aes(Day, Temp, group = Month, color = factor(Month))
  ) +
    geom_line() +
    scale_color_viridis_d() +
    gghighlight(max(Temp) > 93, label_key = Month) +
    labs(x = "Day of Month", y = "Temperature") +
    theme(legend.position = "top")
  
  _Answer_: The month for which the maximum temperature is > 93 is highlighted and labelled.
  ===============================
    TotalCapacity_by_Country<-PowerData1 %>% 
    group_by(country) %>%
    summarize(CountryCapacity= sum(capacity,na.rm='TRUE'))
 
========================================SCATTER PLOT OK===========================================================

ggplot(PowerData1, aes(capacity, gen2013)) + 
  annotate(geom = "rect", xmin = 1.5, xmax = 2,
           ymin = 40, ymax = 45,
           fill = "lightblue", alpha = 0.2) +
  geom_point(aes(colour = factor(pryfuel))) +
  geom_mark_ellipse(aes(label = pryfuel, group = pryfuel)) +
  
  labs(
    x = "Capacity (mw)", 
    y = "Generation_2013(gwh)", 
    colour = "Pry Fuel",
    title = 'Power Plant Database SCATTER PLOT',
    subtitle = 'Total Capacity by Generation In 2013'
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
============================

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
                     
                     
7. Lastly#########It's important to thoroughly document the data visualization process, including the choice of plots, colours, scales, and interpretations of the visualizations. This documentation can help in explaining the insights obtained from the visualizations and supporting the conclusions drawn from the analysis.#########################################
                     
                     
                     
                     
