

The Coursework assignment: 
  
In this report, R programming language is used to create a titivating, truthful and explorative report and 
visualisations of the Global Power Plant database statistical data analysis. 

The following libraries would be loaded for the purpose of this report:
  
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
library(tmaptools)
library(ggforce)
library(gghighlight)
library(viridis)

  
1. Importing the Dataset: 
  
  PowerData <- read.csv('global_power_plant_database.csv')

  This line of code reads the CSV file "global_power_plant_database.csv" and stores its contents in the variable "PowerData" as a data frame.

==========================================================================================================
  
2. The Global Power Plant Dataset: Description of the data variables. 
   
country: The country where the power plant is located.
country_long: The long form name of the country where the power plant is located.
name: The name of the power plant.
gppd_idnr: A unique identifier for the power plant.
capacity_mw: The capacity of the power plant in megawatts.
latitude: The latitude of the power plantss location.
longitude: The longitude of the power plantss location.
primary_fuel: The primary fuel source used by the power plant.
other_fuel1, other_fuel2, other_fuel3: Additional fuel sources used by the power plant.
commissioning_year: The year in which the power plant was commissioned.
owner: The owner of the power plant.
source: The source of the data for the power plant.
url: URL for the source of the data.
geolocation_source: The source of the geolocation data for the power plant.
wepp_id: The ID of the power plant in the World Electric Power Plants Database.
year_of_capacity_data: The year for which the capacity data is available.
generation_gwh_2013, generation_gwh_2014, generation_gwh_2015, generation_gwh_2016, generation_gwh_2017, generation_gwh_2018, generation_gwh_2019: Generation data for the respective years in gigawatt-hours (GWh).
generation_data_source: The source of the generation data.
estimated_generation_gwh_2013, estimated_generation_gwh_2014, estimated_generation_gwh_2015, estimated_generation_gwh_2016, estimated_generation_gwh_2017: Estimated generation data for the respective years in gigawatt-hours (GWh).
estimated_generation_note_2013, estimated_generation_note_2014, estimated_generation_note_2015, estimated_generation_note_2016, estimated_generation_note_2017: Notes or comments related to the estimated generation data for the respective years.

==========================================================================================================
  
3. Data Cleaning and Preprocessing: 
  
  names(PowerData1)[names(PowerData1) %in% c('country','country_long','gppd_idnr','capacity_mw','primary_fuel','other_fuel1','other_fuel2','other_fuel3','commissioning_year',
                                             'geolocation_source','wepp_id','year_of_capacity_data','generation_gwh_2013', 'generation_gwh_2014','generation_gwh_2015',
                                            'generation_gwh_2016','generation_gwh_2017','generation_gwh_2018','generation_gwh_2019','generation_data_source',
                                             'estimated_generation_gwh_2013','estimated_generation_gwh_2014','estimated_generation_gwh_2015','estimated_generation_gwh_2016',
                                             'estimated_generation_gwh_2017')] <-
                                c('countid','country','gppdidnr','capacity','pryfuel','ofuel1','ofuel2','ofuel3','commyear','geosource','weppid','yearofcapacity',
                                  'gen2013','gen2014','gen2015','gen2016','gen2017','gen2018','gen2019','gendatasource','estgen2013','estgen2014','estgen2015','estgen2016','estgen2017')

For the purpose of readability and idenfying the outliers, this report has been written by renaming the long columns in Global Power Plant dataset to shorter variables, 
bt creating the PowerData1 dataset. This has made this report to be more concise and easier to read and digest.

==========================================================================================================
  
4. Exploratory Data Analysis (EDA):


head(PowerData1) #This will display the first six rows of PowerData with all columns.
dim(PowerData1) #It returns the dimensions of the data frame: the number of rows and the number of columns.
glimpse(PowerData1) #It displays the first few rows of the dataset along with the variable names and data types. 
summary(PowerData1) #It provides a summary of the distribution of numerical variables in the dataset.
str(PowerData1) #It provides the structure of the object and the data types of its variables.

sum(sapply(PowerData1, is.numeric)) # count numerical variables
sum(sapply(PowerData1, is.character)) # count categorical variables
sum(sapply(PowerData1, is.double)) # count categorical variables
sum(is.na(PowerData1)) # Total Null values

There are 34936 observations in the global_power_plant_database (PowerData1) and 36 column variables with a toatl
of 17 numerical variables and 19 categorical variables. There are a total of 296603 N/A (missing values) in the dataset. 
============================================================
  
Also:
  
  4a.	Analysing Power Generation trends: Statistical Analysis: 
  
  BiggestGen2017 <- PowerData1 %>%
  filter(gen2017 == max(gen2017, na.rm = TRUE) ) 
#in 2017, France had the biggest Power Generation from a 'Nuclear station' of 5200 (MW) capacity, 
#generating 36,448.64 GWh of power.
  
  BiggestGen2018 <- PowerData1 %>%
  filter(gen2018 == max(gen2018, na.rm = TRUE) ) 
#in 2018, India had the biggest Power Generation from a 'Coal station' of 4760 (MW) capacity, 
#generating 35,136.00 GWh of power.
  
  BiggestGen2019 <- PowerData1 %>%
  filter(gen2019 == max(gen2019, na.rm = TRUE) ) 
#In 2019, United States Of America had the biggest Power Generation from a 'Nuclear station' of 4209.6 (MW) capacity, 
  #generating 31,920.37 GWh of power.
It is however, important to note that these data on power generation was available for these three big countries
    only in 2017 while France and India had missing values to showcase in 2019. 
    
 ####   Something Interesting about China.
  
BiggestCapacity <- PowerData1 %>%
  filter(capacity == max(capacity, na.rm = TRUE) )
#This code revealed that the country with the biggest capacity of Power plant is China from an 'Hydro'  primary fuel.
With the estimated power generation of 82,810.77 (GWh), we can feel free to deduce that China would have completed
favourable well with the other 'big countries' like USA, United Kingdom, France, India in terms of power generation,
if not better. But no single data was provided for China on power genetation in this dataset!
  
  BigCountries<-PowerData1 %>%
  filter(capacity > 1500)  # This code creates the 'BigCountries' showing the countries with Power plants 
                           # capacity greater than 1500 (MW).
BiggestGen2019 <- PowerData1 %>%
  filter(gen2019 == max(gen2019, na.rm = TRUE) ) %>%
  select(country)


4b.  ======THE BARPLOT OF COUNTRIES WITH POWER PLANT GENERATION EQUAL OR GREATER THAN 200GWh IN 2018=====================
  
  # Vector of bold country names
  BoldCountries <- c("China", "United States", "India", "Russia", "Japan", "Germany")

BoldCountries is a vector of character strings that contains the names of six countries: China, 
United States, India, Russia, Japan, and Germany. These countries are here referred to as 
'BoldCountries' countries, because they standout in our global power dataset in terms of capacity and generation. 

# Create a color palette with a different color for each country
Ibrahim <- c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a65628")

Ibrahim is a color palette that would allow different color for each country in the next visualisation.

Genfiltered200 <- PowerData1 %>%
  filter(gen2018 >= 200) #%>%
  #arrange(desc(gen2018))
# Filter the dataset to include only countries with generation >= 200 GWh in 2018

BiggestGen2019 <- Genfiltered200 %>%
  filter(gen2018 == max(gen2018, na.rm = TRUE)) %>%
  select(country)

This codes created a variable 'BiggestGen2019' which revealed countries with the highest generation in 2019

  ggplot(data = Genfiltered200, aes(x = country, y = gen2018, fill = country)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = Ibrahim[1:length(unique(Genfiltered200$country))]) +
  xlab("") +
  ylab("Generation (GWh)") +
  labs(x='', y='Total Generation (GWh)', title='Country with highest generation in 2019',
       caption='Orange_ibro Works') + 
  theme(axis.text.x = element_text(angle = 0, hjust = 1),
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
        plot.margin = unit(c(1, 1, 1, 1), "cm"))+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())

##   In this code, the filter() function is used to subset the PowerData1 dataset to include only 
the countries with generation greater than or equal to 200 GWh in 2019 (eventhough there is dearth of data to work with in 2019). The resulting filtered 
dataset is assigned to the BiggestGen2019 object, which is then used as the data source for 
the plot. By doing this, only the countries of interest are shown on the x-axis, helping to 
decongest the plot. 


4c.=THE COLUMN PLOT OF COUNTRIES WITH POWER PLANT CAPACITIES GREATER THAN 9,900 MW WITH RESPECT TO
     CAPACITY AND GENERATION==
  
  ggplot(BigCountries, aes(country, capacity))+ 
  geom_col() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 	#theme function is used to adjust the x-axis label's orientation.
  gghighlight(capacity > 9900000000) 

This code generates a column plot using ggplot2 package to visualize the capacity of power plants in different 
countries from the BigCountries dataset with China standing out, with the x-axis representing the countries and y-axis representing 
the capacity. The gghighlight function highlights columns corresponding to power plants with a capacity greater than 9,900 MW.

ggplot(BigCountries, aes(country, gen2018))+ 
  geom_col() + #This function is used to create the column bars.
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + #This theme rotates the x-axis labels by 90 degrees to avoid overlapping text.	
  gghighlight(capacity > 9900000000) 

This code creates a column plot using ggplot2 library to visualize the 'big countries' generation capacities, as created from the
world global power plant data source. The x-axis shows the country and the y-axis shows the generation in giga watts hour in the year 2018. 
The gghighlight(capacity > 9900000000) highlights the columns where the country power capacity is greater than 9900 MW, 
indicating the largest power plants in the dataset. The United States of America power plants has the highest generatoin (GWh) 
as earlier noted, followed by India and then, Australia.

  
BigTwo <- ifelse(BigCountries$country %in% c("China", "United States"), "highlight", "normal")

#Essentially, this code is creating a new categorical variable that distinguishes between the 
#countries of "China" and "United States" and all other countries in the BigCountries dataset.
This line of code creates a new variable called BigTwo in the BigCountries dataset. The ifelse 
function is used to check if the country variable in the BigCountries dataset matches any of the 
values "China" or "United States". If a country matches either of these values, then the value 
"highlight" is assigned to the corresponding row in the BigTwo variable. Otherwise, the value 
"normal" is assigned.

IbroPalette <- rainbow(length(unique(BigTwo))) #Defines a color palette

ggplot(BigCountries, aes(country, capacity, fill = highlight)) + 
  geom_col(color = "black", width = 0.8) +
  scale_fill_manual(values = IbroPalette) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 	
  gghighlight(highlight == "highlight") +
  labs(title = "Total Capacity by Country", 
       subtitle = "Highlighting China and United States",
       x = "", y = "Capacity (MW)") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold", size = 20),
        plot.subtitle = element_text(hjust = 0.5, size = 16),
        axis.title.x = element_text(size = 14, vjust = -0.2),
        axis.title.y = element_text(size = 14, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "bottom")

I said it, theress something unique about China! This code creates a new variable "BigTwo" in the 
dataset "BigCountries" with values "BigTwo" if the country is China or the United States, and "normal" otherwise.
The code then generates a column plot using ggplot2 with country on the x-axis, capacity on the y-axis, and 
the "BigTwo" variable used to fill the bars. The colors of the bars are set using a rainbow palette of colors (IbroPalette) 
based on the number of unique values in the "BigTwo" variable. Finally, the plot is customized with a title, 
subtitle, and axis labels, and the countries with "BigTwo" equal to "BigTwo" are highlighted using the gghighlight function.
Please note that every other country in the plots have Power plants capacity greater than 9.9 GW. 

============================================================================================================
  

  4d.======density plot of the distribution of power plant capacity for different primary fuels======

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

This visualization highlights Hydro Power Stations having the highest capacities in mega watts fo 
llowed by the Gas, Nuclear, Coal, Oil and then the Wind power plants. The capacities of the Biomass, 
Wave and Tidal and the Waste is relatively quite low which shows how much the bio-degradable energy 
has remained underunitilised!
  
  4d.======density plot of the distribution of power plant capacity for different primary fuels======
  ===for CHRIS====
  
  ggplot(PowerData1, aes(x=capacity, color=pryfuel, fill=pryfuel)) +
  geom_density(alpha=.45) + facet_wrap(~pryfuel) +
  theme_minimal() + #The theme_minimal function is used to simplify the plot's background
  labs(x = "Capacity (MW)", y = "Density",#used to label the x and y axes, as well as the title and subtitle of the plot 
       title = "Density Plot of Power Plant Capacity by Primary Fuel",
       Caption = "Global Power Plant Database By OrangeIbro",
       fill = "Primary Fuel", color = "Primary Fuel") + #The fill and color of the density curves are based on the primary fuel type
  scale_color_viridis_d() + scale_fill_viridis_d() #used to ensure distinct and visually appealing colors.  

##This code creates a density plot of the distribution of power plant capacity for different primary fuels using 
the ggplot2 package in R. The plot is faceted by the primary fuel type to represent the Power Plant Type, with each 
facet showing the density curve for the corresponding fuel type. The capacity is displayed on the x-axis, and the 
density is displayed on the y-axis. The density shows that the Solar Power stations are the most popular power
plants, followed by the the Waste, Storage and the Biomass power stations. The Hydro power stations seem to be loosing 
popularity while the Cogeneration, Geothermal and Gas plants are fast gaining popularity most especially in the United 
states of America and China. 

==========================================================================================================
 4e. ====Scatter Plot of the distribution of power plant capacity and generation with respect to the    primary fuels in 2013====
  
  ggplot(PowerData1, aes(capacity, gen2013)) + 
  annotate(geom = "rect", xmin = 1.5, xmax = 2,#nnotated rectangle in light blue to highlight a specific area of the plot
           ymin = 40, ymax = 45,
           fill = "lightblue", alpha = 0.2) +
  geom_point(aes(colour = factor(pryfuel))) +
  geom_mark_ellipse(aes(label = pryfuel, group = pryfuel)) + #to add labels for each primary fuel source group
  
  labs( #is used to add labels to the x-axis, y-axis and legend, as well as the title and subtitle of the plot
    x = "Capacity (mw)", 
    y = "Generation_2013(gwh)", 
    colour = "Pry Fuel",
    title = 'Power Plant Database SCATTER PLOT',
    subtitle = 'Total Capacity by Generation In 2013'
  ) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())#used to remove the grid lines from the plot
#This code creates a scatter plot of the capacity versus generation data in 2013 for power plants in the 
global power plant dataset. Each point on the scatter plot represents a power plant, with the capacity on the x-axis 
and gen2013 on the y-axis. The color of the point represents the primary fuel source used by the power plant, 
which is converted to a factor variable using factor(pryfuel).


4f.===============The Power Plants Total Capacity and Generation by fuel type=======================


TotalCapacity_by_fuel <- PowerData1 %>% 
  group_by(pryfuel) %>%
  summarize(TotalCapacity= sum(capacity,na.rm='TRUE'))

kable(TotalCapacity_by_fuel, align = c("l", "r"), col.names = c("Primary Fuel", "Total Capacity (MW)"))

TotalGen_by_fuel2018<-PowerData1 %>% 
  group_by(pryfuel) %>%
  summarize(totalGen2018=sum(gen2018,na.rm='TRUE'))

kable(TotalGen_by_fuel2018, align = c("l", "r"), col.names = c("Primary Fuel", "Total Generation 2018 (MWh)"))
=====
  
4g.===descriptive statistics =CORRELATION COEFF BETWEEN THE PANTS CAPACITY AND EST GENERATION================

  CorrCoef2_PowerData1 <- PowerData1 %>% 
  summarize(r = cor(capacity, estgen2017, use = 'pairwise.complete.obs'))

CorrCoef2_PowerData1 = 0.9489435 

Also, by Comparing the proportionality of primary fuel type in the two datasets 'TotalCapacity_by_fuel' 
and 'TotalGen_by_fuel2018', the data shows that there is a strong correlation (0.95) between Power Plants 
Capacity and the power generation.

Visualising the relativity of the Primary fuel type with respect to power generation,
Coal plants is number one, followed by Gas, Hydro and then, the Wind plant. Even though, generations 
from the Wave station, Tidal, Biomass and the Waste station (the bio-degradables) are at 
the second time quite low as seen in the next visualisation. 

4h. ============Line Plot of the total generation of electricity by primary fuel type in the year 2018====
  
    ggplot(TotalGen_by_fuel2018, aes(pryfuel, totalGen2018)) + 
  geom_line(aes(group = 1, color = pryfuel), size = 0.8) +
  geom_text(aes(label = pryfuel), hjust = 0.5, vjust = -0.5, size = 3) +
  labs(title = 'Global Power Plant Database Scatter Plot: Capacity Vs Fuel Type') +
  theme_minimal() +
  theme(legend.position = 'right', plot.title = element_text(hjust = .5)) +
  theme(axis.text.x = element_blank(), axis.text.y = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank())#no background grid lines

The line plot shows the relationship between primary fuel and total electricity generated in 2018 for each fuel type 
as a point on the graph. The points are connected by lines of the same color to show trends in the data.
The scatter plot uses a minimal theme, with no labels on the x and y axis, and no background grid lines to draw attention to the data points. 
Additionally, the points are labeled with the pryfuel variable to help identify each data point. Overall, this plot is useful for 
comparing the total generation of electricity by primary fuel type in 2018 and for identifying trends in the data.
 Finally, I customized the theme to make the plot more visually appealing and changed the colors of the points
 and lines using the colour aesthetic.

  
4i. ======The generation data for power plants in USA, UK and Nigeria in 2017 #########################

############The generation data for specific power plants to understand their performance and contribution to total generation##########################

USAUKNIG_data <- filter(PowerData1, country %in% c('United States of America','United Kingdom','Nigeria'))

The code creates a new data frame USAUKNIG_data by filtering the original PowerData1 data frame for rows 
where the country column is either 'United States of America', 'United Kingdom', or 'Nigeria'. The filter() 
function from the dplyr package is used to select only those rows that meet this condition. The %in% operator
is used to match values in the country column to the specified vector of country names.

  ggplot(USAUKNIG_data, aes(x=country, y=gen2017, color=country)) +
  geom_line(linewidth=.5) + #function is used to connect the data points with a line
  geom_point(size=2) +      #function is used to plot the data points themselves
  labs(x='', y='Total Generation (GWh)', title='Total generation by fuel type',
       caption='Orange_ibro Works') +
  scale_y_continuous(limits= c(0, max(USAUKNIG_data$gen2017) * 1.1)) + #function is used to set the limits of the y-axis
  theme_minimal() +         #function sets a minimalistic theme for the plot 
  theme(panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank(), legend.position='right')
  #theme() is used to remove the major gridlines on the x-axis and position the legend on the right side of the plot

Using the geom_line plots , the figure compares generation in GWh and the trends across 
different three countries to identify variations in energy production patterns
The line plot of the total power generation (in GWh) for three countries 
(United States of America, United Kingdom, and Nigeria) using data from the USAUKNIG_data sub-dataset created from the 
world global power plant database. Each country is represented by a different color. 
with USA taking  the lead in power generation in the year 2017. The x-axis shows the country names,  and the y-axis shows the total generation value in megaWatt hour.
Overall, the plot shows the total electricity generation for each country in a clear and easy-to-understand way.


4j.====Geospatial Analysis: The Global Power Plants map by primary fuel=====
  
OrangeWorld <- map_data("world")

'OrangeWorld' is a data object created by calling the map_data() function from the maps package with the 
argument "world". This function returns a data frame with coordinates for the boundaries of all countries 
in the world, which can be used to create a world map using the ggplot2 package. The OrangeWorld data frame 
contains columns such as long and lat, which correspond to the longitude and latitude values for the boundaries 
of each country. The OrangeWorld data frame will be used in this session to visualise the global power plants by their primary fuel types.

ggplot() +
  geom_map(data=OrangeWorld, map=OrangeWorld, 
           aes(long, lat, map_id=region), color='red', fill='lightblue', size=.05) +
  geom_point(data = PowerData1, aes(x = longitude, y = latitude, color = pryfuel), alpha = .4) +
  theme_void() +
  theme(legend.position = 'right', plot.title = element_text(hjust = .5)) +
  labs(title="GLOBAL POWER PLANTS MAP BY PRIMARY FUEL", 
       caption='Orange_ibrahim Power Plants Geospatial Vis.')


#The map plot is a visualization of global power plants by their primary fuel types. 
The plot consists of two geospatial layers: the base map layer and the power plant layer. 
The base map is a world map that is rendered in light blue color, with red outlines around 
the map boundaries. The power plant layer is plotted on top of the base map, and it contains 
a scatter plot of the power plantss locations, with each point colored according to the primary fuel type. 
The legend is positioned on the right side of the plot, indicating the primary fuel types and their corresponding colors. 
The plotss title is "GLOBAL POWER PLANTS MAP BY PRIMARY FUEL," with a caption indicating the authorss name.

With power station from bio-degradable fuels (in pink-like colors) sparingly spread across United States of America,
the Coal, Gas, Nuclear and Hydro power station as represented in Golden, light green, greena and light blue
and colors, painted most parts of the map as seen mostly in the North America, Asia, Europe, Australia and some 
part of africa.
#====================================================

================================================================================================
2.	Estimating power generation potential, and 
3.	Understanding the energy landscape in different countries.

 
  

Interpretation and Visualization of Results: 
  
  #Once you have conducted your statistical analyses, you can interpret the results and draw conclusions based on your findings. You can use functions like print(), summary(), and plot() to interpret and visualize the results of your analyses, and create visualizations such as bar charts, scatter plots, histograms, and more to present your findings visually.

Documentation: 
Finally, it's important to document your analysis, including the steps you followed, the functions you used, and the results you obtained. You can create a report or document using RMarkdown, a markup language that combines R code with text and generates reports in various formats such as HTML, PDF, or Word.






