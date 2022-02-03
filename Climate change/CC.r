### CO2 data analysis
### Use datafile "CapeGrim_CO2_data_download.csv"

library(tidyverse)

co2 <- CapeGrim_CO2_data_download

co2 <- co2 %>% 
  filter(YYYY %in% (1976:2020))

co2

co2_plot <- ggplot(co2, aes(`DATE`, `CO2(ppm)`)) + 
  geom_point(na.rm = TRUE)
co2_plot

co2_trend <- co2_plot + stat_smooth(colour="green")
co2_trend

ggplot(co2, aes(x = `DATE`)) + 
  geom_point(aes(y = `CO2(ppm)`), col = "blue") +
  geom_smooth(aes(y = `CO2(ppm)`),  method = "lm", col = "blue") +
  ggtitle("CO2 observations from Cape Grim 1976-2020")

mean(co2$`CO2(ppm)`)

lm(co2$`CO2(ppm)`~co2$DATE)

### Temperature data analysis
### Use datafile "Cape_Otway_Daily.csv"

library(tidyverse)
library(lubridate)

Cape_Otway_Daily$date <-
  Cape_Otway_Daily %>% 
  mutate(date = make_datetime(Year, Month, Day))

str(Cape_Otway_Daily)

temp.year.sum <- Cape_Otway_Daily %>% 
  group_by(Year) %>% 
  dplyr::summarise(mean(tMax, na.rm = TRUE))

temp.year.sum

# filter data to 1990-2020

temp30yr <- temp.year.sum %>% 
  filter(between(Year, 1990, 2020 ))
temp30yr

ggplot(temp30yr, aes(x = Year)) + 
  geom_point(aes(y = `mean(tMax, na.rm = TRUE)`), col = "blue") +
  geom_smooth(aes(y = `mean(tMax, na.rm = TRUE)`),  method = "lm", col = "blue") +
  ggtitle("Mean annual max temperature at Cape Otway weather station") + 
  ylab("°C")

temp.lm <- lm(temp30yr$`mean(tMax, na.rm = TRUE)`~ temp30yr$Year)
temp.lm

### Rainfall data analysis
### Use datafile "Rainfall_Pennyroyal.csv"

library(tidyverse)
library(lubridate)

rain <- Rainfall_Pennyroyal

rain

rain_plot <- ggplot(rain, aes(Year, Annual)) + 
  geom_point(na.rm = TRUE)
rain_plot

rain_trend <- rain_plot + stat_smooth(colour="green")
rain_trend

# filter data to 1990-2020

rain30yr <- rain %>% 
  filter(between(Year, 1990, 2020 ))
rain30yr

ggplot(rain30yr, aes(x = Year)) + 
  geom_point(aes(y = Annual), col = "blue") +
  geom_smooth(aes(y = Annual),  method = "lm", col = "blue") +
  ggtitle("Annual rainfall at Pennyroyal weather station") + 
  ylab("Rainfall (mm)")

rain.lm <- lm(rain30yr$Annual ~ rain30yr$Year)
rain.lm

### FFDI data analysis
### Use datafile "FFDI.csv" (not available on Github)

library(tidyverse)
library(lubridate)

FFDI$year <- year(FFDI$date)

ffdi.year.sum <- FFDI %>% 
  group_by(year) %>%  # group by year
  dplyr::summarise(mean(ffdi_mean, na.rm=TRUE)) # find annual means

ffdi.year.sum

#filter data to 1990-2020

ffdi30yr <- ffdi.year.sum %>% 
  filter(between(year, 1990, 2020 ))
ffdi30yr

means30yr <- ffdi30yr$`mean(ffdi_mean, na.rm = TRUE)` #rename variable

ggplot(ffdi30yr, aes(x = year)) + 
  geom_point(aes(y = means30yr), col = "blue") +
  geom_smooth(aes(y = means30yr),  method = "lm", col = "blue") +
  ggtitle("Annual mean Forest Fire Danger Index for Forrest") +
  ylab("FFDI")

ffdi30.lm <- lm(means30yr ~ ffdi30yr$year)
ffdi30.lm
