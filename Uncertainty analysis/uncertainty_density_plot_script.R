library(dplyr)
library(tidyr)
library(ggplot2)
library(readr)
library(splines)
library(quantreg)
library(reshape2)
library(dplyr)
library(purrr)
library(viridis)
library(scico)
library(scales)

# morris_outcomes is the data from the Morris sensitivity sampling using ema_workbench in Python
# set file names and file paths according to your own local environment

# read in csv using read_r
data <- read_csv("E:/szetey_k/morris_outcomes.csv")

# total population variable
# total population high #E5243B line colour = #C95100 ylab = people ylim = (0,500)

# create plotting function
plotting_fun = function(var, data) {
  data %>%
    pivot_longer(!TIME, names_to = 'VARIABLE', values_to = 'VALUE')%>% # wide to long
    dplyr::group_by(`VARIABLE`,`TIME`)%>% # group by variable and time period
    dplyr::mutate(min = min(`VALUE`))%>% # find min for each variable and time period
    dplyr::mutate(max = max(`VALUE`))%>% # find max for each variable and time period
    dplyr::mutate(median = median(`VALUE`)) %>% #find median for each variable and time period
    dplyr::filter(`VARIABLE` %in% c(var))%>%
    ggplot(.) + 
    stat_density_2d(aes(x = `TIME`, y = `VALUE`, fill = ..density..), geom="raster", contour = FALSE) +
    scale_fill_gradient(low = "white", high = "#E5243B") +
    geom_line(aes(x=TIME, y = median, group = `VARIABLE`), colour = "#C95100") + # draw median line
    theme_bw() + # change theme to black and white
    theme(legend.position='none') +
    labs(x = "Year", y = "People") +
    ylim(0, 500) +
    ggtitle("Total Population") # add a title
}	

# call plotting function and draw plot	
popn <- plotting_fun("total population", data)
popn

# housing land variable
# housing land: high #FD6925 line colour = #CA3600 ylab = hectares ylim - (0, 1000)

# create plotting function
plotting_fun = function(var, data) {
  data %>%
    pivot_longer(!TIME, names_to = 'VARIABLE', values_to = 'VALUE')%>% # wide to long
    dplyr::group_by(`VARIABLE`,`TIME`)%>% # group by variable and time period
    dplyr::mutate(min = min(`VALUE`))%>% # find min for each variable and time period
    dplyr::mutate(max = max(`VALUE`))%>% # find max for each variable and time period
    dplyr::mutate(median = median(`VALUE`)) %>% #find median for each variable and time period
    dplyr::filter(`VARIABLE` %in% c(var))%>%
    ggplot(.) + 
    stat_density_2d(aes(x = `TIME`, y = `VALUE`, fill = ..density..), geom="raster", contour = FALSE) +
    scale_fill_gradient(low = "white", high = "#FD6925") +
    geom_line(aes(x=TIME, y = median, group = `VARIABLE`), colour = "#CA3600") + # draw median line
    theme_bw() + # change theme to black and white
    theme(legend.position='none') +
    labs(x = "Year", y = "Hectares") +
    ylim(0, 1000) +
    ggtitle("Housing Land") # add a title
}	

# call plotting function and draw plot	
hous <- plotting_fun("Housing Land", data)
hous

# housing demand variable
# housing demand high #FD9D24 line colour = #CA6A00 ylab = houses ylim = (0,300)

# create plotting function
plotting_fun = function(var, data) {
  data %>%
    pivot_longer(!TIME, names_to = 'VARIABLE', values_to = 'VALUE')%>% # wide to long
    dplyr::group_by(`VARIABLE`,`TIME`)%>% # group by variable and time period
    dplyr::mutate(min = min(`VALUE`))%>% # find min for each variable and time period
    dplyr::mutate(max = max(`VALUE`))%>% # find max for each variable and time period
    dplyr::mutate(median = median(`VALUE`)) %>% #find median for each variable and time period
    dplyr::filter(`VARIABLE` %in% c(var))%>%
    ggplot(.) + 
    stat_density_2d(aes(x = `TIME`, y = `VALUE`, fill = ..density..), geom="raster", contour = FALSE) +
    scale_fill_gradient(low = "white", high = "#FD9D24") +
    geom_line(aes(x=TIME, y = median, group = `VARIABLE`), colour = "#CA6A00") + # draw median line
    theme_bw() + # change theme to black and white
    theme(legend.position='none') +
    labs(x = "Year", y = "Houses") +
    ylim(0, 300) +
    ggtitle("Housing Demand") # add a title
}

# call plotting function and draw plot	
hses <- plotting_fun("housing demand", data)
hses

# total other productivity variable
# total other productivity high #A21942 line colour = #6F000F ylab = dollars ylim = (0,18000000)

# create plotting function
plotting_fun = function(var, data) {
  data %>%
    pivot_longer(!TIME, names_to = 'VARIABLE', values_to = 'VALUE')%>% # wide to long
    dplyr::group_by(`VARIABLE`,`TIME`)%>% # group by variable and time period
    dplyr::mutate(min = min(`VALUE`))%>% # find min for each variable and time period
    dplyr::mutate(max = max(`VALUE`))%>% # find max for each variable and time period
    dplyr::mutate(median = median(`VALUE`)) %>% #find median for each variable and time period
    dplyr::filter(`VARIABLE` %in% c(var))%>%
    ggplot(.) + 
    stat_density_2d(aes(x = `TIME`, y = `VALUE`, fill = ..density..), geom="raster", contour = FALSE) +
    scale_fill_gradient(low = "white", high = "#A21942") +
    geom_line(aes(x=TIME, y = median, group = `VARIABLE`), colour = "#6F000F") + # draw median line
    theme_bw() + # change theme to black and white
    theme(legend.position='none') +
    xlab("Year") +
    scale_y_continuous("Dollars (millions)", labels = comma_format(scale = 1/1000000)) +
    ggtitle("Total Productivity ('Other' Sector)") # add a title
}

# call plotting function and draw plot	
toprod <- plotting_fun("Total Other Productivity", data)
toprod

# species richness variable
# species richness high #56C02B line colour = #238D00 ylab = species ylim = (0, 12000)

# create plotting function
plotting_fun = function(var, data) {
  data %>%
    pivot_longer(!TIME, names_to = 'VARIABLE', values_to = 'VALUE')%>% # wide to long
    dplyr::group_by(`VARIABLE`,`TIME`)%>% # group by variable and time period
    dplyr::mutate(min = min(`VALUE`))%>% # find min for each variable and time period
    dplyr::mutate(max = max(`VALUE`))%>% # find max for each variable and time period
    dplyr::mutate(median = median(`VALUE`)) %>% #find median for each variable and time period
    dplyr::filter(`VARIABLE` %in% c(var))%>%
    ggplot(.) + 
    stat_density_2d(aes(x = `TIME`, y = `VALUE`, fill = ..density..), geom="raster", contour = FALSE) +
    scale_fill_gradient(low = "white", high = "#56C02B") +
    geom_line(aes(x=TIME, y = median, group = `VARIABLE`), colour = "#238D00") + # draw median line
    theme_bw() + # change theme to black and white
    theme(legend.position='none') +
    labs(x = "Year", y = "Species") +
    ylim(0, 10000) +
    ggtitle("Species Richness") # add a title
}

# call plotting function and draw plot	
SR <- plotting_fun("Species Richness", data)
SR

# inequality indicator variable
# inequality indicator high #DD1367 line colour #AA0034 ylab = dimensionless ylim = (0, 0.3)

# create plotting function
plotting_fun = function(var, data) {
  data %>%
    pivot_longer(!TIME, names_to = 'VARIABLE', values_to = 'VALUE')%>% # wide to long
    dplyr::group_by(`VARIABLE`,`TIME`)%>% # group by variable and time period
    dplyr::mutate(min = min(`VALUE`))%>% # find min for each variable and time period
    dplyr::mutate(max = max(`VALUE`))%>% # find max for each variable and time period
    dplyr::mutate(median = median(`VALUE`)) %>% #find median for each variable and time period
    dplyr::filter(`VARIABLE` %in% c(var))%>%
    ggplot(.) + 
    stat_density_2d(aes(x = `TIME`, y = `VALUE`, fill = ..density..), geom="raster", contour = FALSE) +
    scale_fill_gradient(low = "white", high = "#DD1367") +
    geom_line(aes(x=TIME, y = median, group = `VARIABLE`), colour = "#AA0034") + # draw median line
    theme_bw() + # change theme to black and white
    theme(legend.position='none') +
    labs(x = "Year", y = "Dimensionless") +
    ylim(0, 0.3) +
    ggtitle("Inequality Indicator") # add a title
}

# call plotting function and draw plot	
inind <- plotting_fun("inequality indicator", data)
inind

# safer healthier people variable
# safer healthier people high #4C9F38 line colour = #196C05 ylab = people ylim = (0,250)

# create plotting function
plotting_fun = function(var, data) {
  data %>%
    pivot_longer(!TIME, names_to = 'VARIABLE', values_to = 'VALUE')%>% # wide to long
    dplyr::group_by(`VARIABLE`,`TIME`)%>% # group by variable and time period
    dplyr::mutate(min = min(`VALUE`))%>% # find min for each variable and time period
    dplyr::mutate(max = max(`VALUE`))%>% # find max for each variable and time period
    dplyr::mutate(median = median(`VALUE`)) %>% #find median for each variable and time period
    dplyr::filter(`VARIABLE` %in% c(var))%>%
    ggplot(.) + 
    stat_density_2d(aes(x = `TIME`, y = `VALUE`, fill = ..density..), geom="raster", contour = FALSE) +
    scale_fill_gradient(low = "white", high = "#4C9F38") +
    geom_line(aes(x=TIME, y = median, group = `VARIABLE`), colour = "#196C05") + # draw median line
    theme_bw() + # change theme to black and white
    theme(legend.position='none') +
    labs(x = "Year", y = "People") +
    ylim(0, 200) +
    ggtitle("Safer Healthier People") # add a title
}

safhp <- plotting_fun("Safer Healthier People", data)
safhp

# internet service demand variable
# internet service demand high #C5192D line colour = #920000 ylab = connections ylim = (0,155)

# create plotting function
plotting_fun = function(var, data) {
  data %>%
    pivot_longer(!TIME, names_to = 'VARIABLE', values_to = 'VALUE')%>% # wide to long
    dplyr::group_by(`VARIABLE`,`TIME`)%>% # group by variable and time period
    dplyr::mutate(min = min(`VALUE`))%>% # find min for each variable and time period
    dplyr::mutate(max = max(`VALUE`))%>% # find max for each variable and time period
    dplyr::mutate(median = median(`VALUE`)) %>% #find median for each variable and time period
    dplyr::filter(`VARIABLE` %in% c(var))%>%
    ggplot(.) + 
    stat_density_2d(aes(x = `TIME`, y = `VALUE`, fill = ..density..), geom="raster", contour = FALSE) +
    scale_fill_gradient(low = "white", high = "#C5192D") +
    geom_line(aes(x=TIME, y = median, group = `VARIABLE`), colour = "#920000") + # draw median line
    theme_bw() + # change theme to black and white
    theme(legend.position='none') +
    labs(x = "Year", y = "Connections") +
    ylim(0, 170) +
    ggtitle("Internet Service Demand") # add a title
}

insedem <- plotting_fun("Internet Service Demand", data)
insedem

# bus frequency for travel equity variable
# travel equity high #BF8B2E line colour = #8C5800 ylab = trips per year ylim = (0, 1000)

# create plotting function
plotting_fun = function(var, data) {
  data %>%
    pivot_longer(!TIME, names_to = 'VARIABLE', values_to = 'VALUE')%>% # wide to long
    dplyr::group_by(`VARIABLE`,`TIME`)%>% # group by variable and time period
    dplyr::mutate(min = min(`VALUE`))%>% # find min for each variable and time period
    dplyr::mutate(max = max(`VALUE`))%>% # find max for each variable and time period
    dplyr::mutate(median = median(`VALUE`)) %>% #find median for each variable and time period
    dplyr::filter(`VARIABLE` %in% c(var))%>%
    ggplot(.) + 
    stat_density_2d(aes(x = `TIME`, y = `VALUE`, fill = ..density..), geom="raster", contour = FALSE) +
    scale_fill_gradient(low = "white", high = "#BF8B2E") +
    geom_line(aes(x=TIME, y = median, group = `VARIABLE`), colour = "#8C5800") + # draw median line
    theme_bw() + # change theme to black and white
    theme(legend.position='none') +
    labs(x = "Year", y = "Trips per year") +
    ylim(0, 1000) +
    ggtitle("Bus Trips For Travel Equity") # add a title
}

buseq <- plotting_fun("bus frequency required for travel equity", data)
buseq

# plot all on one plot

library(patchwork)

popn + hous + hses + toprod + SR + inind + safhp + insedem + buseq
