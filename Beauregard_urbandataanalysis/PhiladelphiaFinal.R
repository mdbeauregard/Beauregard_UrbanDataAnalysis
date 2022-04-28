### FINAL URBAN DATA ANALYSIS PROJECT ###

library(tidyverse)
## For this analysis, we'll be working with 
## shapefiles, so we'll need to load the sf library.
library(sf)

#### POVERTY/CRASH RATE ANALYSIS

## For the first part of this project, I decided to examine the
## the relationship between the poverty rate of Philadelphia 
## census tracts and each tract's 'crash rates' for both
## crashes involving cyclists/pedestrians and crashes overall.

## First, we'll need to load our data.

## 'philly_bikeped_freq' contains the number of crashes between
## 2016 and 2020 involving cyclists/pedestrians and a vehicle
## in each Philadelphia census tract.

philly_bikeped_freq = read_csv("philly_bikeped_freq.csv")

## 'philly_all_freq' contains the number of *all* crashes 
## involving any type of vehicle in each Philadelphia census tract
## between 2016 and 2020.

philly_all_freq = read_csv("philly_all_freq.csv")

## This will read a shapefile exported from SimplyAnalytics
## containing the shapes of each of Philadelphia's census tracts
## as they were during the 2020 United States Census.

philly_census_map = read_sf("philly_all_census_data/SimplyAnalytics_Shapefiles_2022-04-05_18_25_57_9673ed352d35974c9920242edac18fa3.shp")

## 'philly_census_data' contains all of the relevant census data 
## we'll be looking at for this data analysis, as well as some data
## that I ended up not using.

philly_census_data = read_csv("philly_all_census_data/philly_all_census_data.csv")

## Now, we'll join these together to build a unified 'philly' dataset
## containing all of the aforementioned information.

philly = left_join(philly_census_map, philly_census_data, ON="name")
philly = left_join(philly, philly_all_freq, ON="name")
philly = left_join(philly, philly_bikeped_freq, ON="name")

## Next, we'll need to create a "crash rate" statistic for each 
## individual census tract. I've chosen to create the crash rate
## as a measure of incidents reported per 1,000 residents.
## The code below creates a new variable in the 'philly' dataset
## containing such a statistic.

philly$crash_rate = philly$`# Total Population, 2020 [Estimated]` / 1000
philly$bikeped_crash_rate = (philly$bikepedfreq) / (philly$crash_rate)
philly$all_crash_rate = (philly$allfreq) / (philly$crash_rate)

## Some census tracts contain data. From looking into these tracts, 
## it appears that they are mostly unpopulated industrial areas and
## government properties. Let's omit them before we continue with
## our analysis.

philly = na.omit(philly)

## Now, we'll create a plot comparing poverty rate to cyclist/pedestrian
## crash rate. I've decided to use the square root of the dependent
## variable as it makes the data more visually useful. I've also added
## a line of best of fit to the data.

ggplot(philly, aes(x=`% Poverty Status by Age | In Poverty, 2020 [Estimated]`, y=sqrt(bikeped_crash_rate))) +
  geom_point() +
  geom_smooth(method=glm, se=FALSE)

## As we can see from this plot, there are two massive outliers. From
## looking at the data, I found that both of these tracts are comprised
## of parkland. I have decided to remove these two datapoints, as I 
## believe they will be unhelpful in our analysis. We are mainly concerned
## with finding crash rates in neighborhoods, not parks where the population
## would be low and the number of crashes would be understandably high
## due to the higher number of pedestriansa and cyclists present.

philly = filter(philly, name != "CT980000, Philadelphia County, PA")
philly = filter(philly, name != "CT980100, Philadelphia County, PA")

## Now, we replot the data with the offending census tracts removed.

ggplot(philly, aes(x=`% Poverty Status by Age | In Poverty, 2020 [Estimated]`, y=sqrt(bikeped_crash_rate))) +
  geom_point() +
  geom_smooth(method=glm, se=FALSE)

## As we can see, there is a noticeable positive correlation
## between poverty rate and cyclist/pedestrian crash rate.

## Now let's see if there is any correlation between percentage of 
## residents identifying as Black or African American and crash
## rate.

ggplot(philly, aes(x=`% Race | Black or African American alone, 2020 [Estimated]`, y=sqrt(bikeped_crash_rate))) +
  geom_point() +
  geom_smooth(method=glm, se=FALSE)

## There seems to be a minor positive correlation between the two variables.

## We'll repeat the above but with Hispanic or Latino populations.

ggplot(philly, aes(x=`% Hispanic or Latino | Hispanic or Latino, 2020 [Estimated]`, y=sqrt(bikeped_crash_rate))) +
  geom_point() +
  geom_smooth(method=glm, se=FALSE)

## Again, there appears to be a very minor, positive correlation.

## Next, we'll plot poverty and the total crash rate, which 
## includes crashes involving all vehicle types.

ggplot(philly, aes(x=`% Poverty Status by Age | In Poverty, 2020 [Estimated]`, y=sqrt(all_crash_rate))) +
  geom_point() +
  geom_smooth(method=glm, se=FALSE)

## This plot shows a similar, positive correlation between 
## poverty rate and crash rate.

## Finally, we will write all of this into a shapefile.
## We can use this to make more visually interesting graphs
## in QGIS, as well as produce variables for the next section.

st_write(philly, "philly.shp", drive = "ESRI Shapefile")

#### BIKE INFRASTRUCTURE AND POVERTY/INCOME ANALYSIS

## First for this section, we'll load a dataset I created
## using QGIS that gives each census a score on whether or
## not it contains improved cycling infrastructure. My
## methodology for deciding what is and is not considered
## "improved" cycling infrastructure is contained in the paper.
## If a tract does contain improved cycling infrastructure,
## it received a score of '1'. If it did not, it received
## a score of '0'.

philly_bikeinfra = read_csv("philly_bikeinfra.csv")

## We'll now add this new variable to our existing
## 'philly' dataset.

philly$bikeinfra = philly_bikeinfra$imp_inf

## Now, we'll use the 'group_by' function to find the mean
## poverty rate of census tracts containing improved
## cycling infrastructure and those containing none.

philly_bikeinfra_poverty = 
  group_by(philly, bikeinfra) %>%
  summarize(bikeinfra_poverty = mean(`% Poverty Status by Age | In Poverty, 2020 [Estimated]`))

## Next, we'll create a barplot showing this data.

ggplot(philly_bikeinfra_poverty, aes(rownames(philly_bikeinfra_poverty), bikeinfra_poverty)) + 
  geom_bar(stat="identity")

## This plot shows that tracts without improved cycling infrastructure,
## on average, have a higher poverty rate than those without.

## Now, we'll do the same analysis but for the mean median income
## for tracts with and without improved cycling infrastructure.

philly_bikeinfra_income = 
  group_by(philly, bikeinfra) %>%
  summarize(bikeinfra_income = mean(`Median Household Income, 2020 [Estimated]`))
ggplot(philly_bikeinfra_income, aes(rownames(philly_bikeinfra_income), bikeinfra_income)) + 
  geom_bar(stat="identity")

## This yields similar results, with areas containing improved 
## bike infrastructure having a noticeably higher 
## mean median income than those without. 

## Next, we'll check to see the average percentage of black residents
## in areas with improved bike infrastructure and those without.

philly_bikeinfra_black= 
  group_by(philly, bikeinfra) %>%
  summarize(bikeinfra_black = mean(`% Race | Black or African American alone, 2020 [Estimated]`))

## And we'll repeat the process for Hispanic/Latino populations.

philly_bikeinfra_hispanic= 
  group_by(philly, bikeinfra) %>%
  summarize(bikeinfra_hispanic = mean(`% Hispanic or Latino | Hispanic or Latino, 2020 [Estimated]`))

## And finally, we'll find the average crash rate for all of Philadelphia.

philly_bikeinfra_crashrate= 
  group_by(philly, bikeinfra) %>%
  summarize(bikeinfra_crashrate = mean(bikeped_crash_rate))

philly_bikeped_average = sum(philly$`# Total Population, 2020 [Estimated]`) / 1000
philly_bikeped_average = sum(philly_bikeped_freq$bikepedfreq) / philly_bikeped_average
philly_bikeped_average
