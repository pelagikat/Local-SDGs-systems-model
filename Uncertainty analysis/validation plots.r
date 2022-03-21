# export data files from Vensim as csv
# ForrestSDGs_test is the base model run
# validation_outcomes is the data from the LHC sampling using ema_workbench in Python
# set file names and file paths according to your own local environment

setwd("E:/szetey_k/")

library(readr)
Historical_data <- read_csv("Historical_data.csv")
View(Historical_data)
ForrestSDGs_test <- read_csv("ForrestSDGs_test.csv")
View(ForrestSDGs_test)
uncertainties <- read_csv("validation_outcomes.csv")
View(uncertainties)
str(uncertainties)
head(uncertainties)

library(tidyverse)
library(patchwork)
library(scales)

# MUST CALL THESE THREE LINES OF CODE BEFORE STARTING
Historical_data$year <- Historical_data$VARIABLE
Time <- (seq(from = 2000, to = 2050, by = 0.25))
unc_summ <- data.frame(Time)

## Economy component validation plots

par(mfrow = c(3,3))
par(mar = c(0.5,4,2,2) + 0.1)
layout(matrix(c(1,2,0,3,4,7,5,6,0), nrow = 3, ncol = 3))

# Total Agricultural Productivity variable

#plot(Historical_data$`Total Agricultural Productivity` ~ Historical_data$year, 
#     main = "Total agricultural productivity", pch = 20,
#     xlim = c(1990, 2050), ylim = c(1500000, 2600000),
#     xlab = "Year", ylab = "dollars", xaxt = "n") 
#lines(ForrestSDGs_test$`Total Agricultural Productivity` ~ ForrestSDGs_test$Time, col = "#A21942")

# rename variables for ggplot to remove spaces 
Historical_data$totagprod <- Historical_data$`Total Agricultural Productivity`
ForrestSDGs_test$totagprod <- ForrestSDGs_test$`Total Agricultural Productivity`
uncertainties$totagprod <- uncertainties$`Total Agricultural Productivity`

# this code summarises min and max values for the model simulations
unc_summ$Mintotagprod <- tapply(uncertainties$totagprod, uncertainties$TIME, min)
unc_summ$Maxtotagprod <- tapply(uncertainties$totagprod, uncertainties$TIME, max)

#ggplot() +
#   geom_point(data = Historical_data, aes(x = year, y = totagprod)) +
#   geom_smooth(data = Historical_data, aes(x = year, y = totagprod), method = lm, se = FALSE)

TAP <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = totagprod)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = totagprod), color = "#A21942") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = Mintotagprod, ymax = Maxtotagprod),
               fill = "#A21942", alpha = 0.4) +
   theme_bw() +
#   ylim(0, 2300000) +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) +
   scale_y_continuous("dollars (millions)", limits = c(0, 2300000), 
                      labels = label_number(scale = 1/1000000)) + 
   ggtitle("Total agricultural\nproductivity")
TAP

# labour input ag variable (NB this is not a sensitivity variable)

#plot(Historical_data$`labour input ag` ~ Historical_data$year, 
#     main = "Labour input agriculture", pch = 20,
#     xlim = c(1990, 2050), ylim = c(0, 50),
#     xlab = "Year", ylab = "People") 
#lines(ForrestSDGs_test$`labour input ag` ~ ForrestSDGs_test$Time, col = "#A21942")

# rename variables for ggplot to remove spaces 
Historical_data$labinag <- Historical_data$`labour input ag`
ForrestSDGs_test$labinag <- ForrestSDGs_test$`labour input ag`
uncertainties$labinag <- uncertainties$`labour input ag`

# this code summarises min and max values for the model simulations
unc_summ$Minlabinag <- tapply(uncertainties$labinag, uncertainties$TIME, min)
unc_summ$Maxlabinag <- tapply(uncertainties$labinag, uncertainties$TIME, max)

LIA <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = labinag)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = labinag), color = "#A21942") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = Minlabinag, ymax = Maxlabinag),
               fill = "#A21942", alpha = 0.2) +
   theme_bw() +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) +
   ylab("people") + ylim(0, 70) + ggtitle("Labour input agriculture")

LIA

# Total Tourism Productivity variable

#plot(Historical_data$`Total Tourism Productivity` ~ Historical_data$year, 
#     main = "Total tourism productivity", pch = 20,
#     xlim = c(1990, 2050), ylim = c(0, 40000000),
#     xlab = "Year", ylab = "dollars", xaxt = "n") 
#lines(ForrestSDGs_test$`Total Tourism Productivity` ~ ForrestSDGs_test$Time, col = "#A21942") 

# rename variables for ggplot to remove spaces 
Historical_data$tottoprod <- Historical_data$`Total Tourism Productivity`
ForrestSDGs_test$tottoprod <- ForrestSDGs_test$`Total Tourism Productivity`
uncertainties$tottoprod <- uncertainties$`Total Tourism Productivity`

# this code summarises min and max values for the model simulations
unc_summ$Mintottoprod <- tapply(uncertainties$tottoprod, uncertainties$TIME, min)
unc_summ$Maxtottoprod <- tapply(uncertainties$tottoprod, uncertainties$TIME, max)

#ggplot() +
#   geom_point(data = Historical_data, aes(x = year, y = tottoprod)) +
#   geom_smooth(data = Historical_data, aes(x = year, y = tottoprod), method = lm, se = FALSE)

library(scales)

TTP <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = tottoprod)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = tottoprod), color = "#A21942") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = Mintottoprod, ymax = Maxtottoprod, na.rm = TRUE),
               fill = "#A21942", alpha = 0.2) +
   theme_bw() +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) +
   scale_y_continuous("dollars (millions)", limits = c(0, 25000000),
                      labels = comma_format(scale = 1/1000000)) +
   ggtitle("Total tourism productivity")

TTP


# labour input tourism variable (NB this is not a sensitivity variable)

#plot(Historical_data$`labour input tourism` ~ Historical_data$year, 
#     main = "Labour input tourism", pch = 20,
#     xlim = c(1990, 2050), ylim = c(0, 50),
#     xlab = "Year", ylab = "People", xaxt = "n") 
#lines(ForrestSDGs_test$`labour input tourism` ~ ForrestSDGs_test$Time, col = "#A21942")

# rename variables for ggplot to remove spaces 
Historical_data$labinto <- Historical_data$`labour input tourism`
ForrestSDGs_test$labinto <- ForrestSDGs_test$`labour input tourism`
uncertainties$labinto <- uncertainties$`labour input tourism`

# this code summarises min and max values for the model simulations
unc_summ$Minlabinto <- tapply(uncertainties$labinto, uncertainties$TIME, min)
unc_summ$Maxlabinto <- tapply(uncertainties$labinto, uncertainties$TIME, max)

LIT <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = labinto)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = labinto), color = "#A21942") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = Minlabinto, ymax = Maxlabinto, na.rm = TRUE),
               fill = "#A21942", alpha = 0.2) +
   theme_bw() +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) +
   ylab("people") + ylim(0, 110) + ggtitle("Labour input tourism")

LIT


# Total Other Productivity variable

#plot(Historical_data$`Total Other Productivity` ~ Historical_data$year, 
#     main = "Total other productivity", pch = 20,
#     xlim = c(1990, 2050), ylim = c(1000000, 9000000),
#     xlab = "Year", ylab = "dollars", xaxt = "n") 
#lines(ForrestSDGs_test$`Total Other Productivity` ~ ForrestSDGs_test$Time, col = "#A21942")

# rename variables for ggplot to remove spaces 
Historical_data$tototprod <- Historical_data$`Total Other Productivity`
ForrestSDGs_test$tototprod <- ForrestSDGs_test$`Total Other Productivity`
uncertainties$tototprod <- uncertainties$`Total Other Productivity`

# this code summarises min and max values for the model simulations
unc_summ$Mintototprod <- tapply(uncertainties$tototprod, uncertainties$TIME, min)
unc_summ$Maxtototprod <- tapply(uncertainties$tototprod, uncertainties$TIME, max)

#ggplot() +
#   geom_point(data = Historical_data, aes(x = year, y = tototprod)) +
#   geom_smooth(data = Historical_data, aes(x = year, y = tototprod), method = lm, se = FALSE)

TOP <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = tototprod)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = tototprod), color = "#A21942") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = Mintototprod, ymax = Maxtototprod, na.rm = TRUE),
               fill = "#A21942", alpha = 0.2) +
   xlab("year") +
   scale_y_continuous("dollars (millions)", limits = c(0, 16000000),
                      labels = comma_format(scale = 1/1000000)) +
   theme_bw() +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) +
   ggtitle("Total other productivity")

TOP

# labour input other variable (NB this is not a sensitivity variable)

#plot(Historical_data$`labour input other` ~ Historical_data$year, 
#     main = "Labour input other", pch = 20,
#     xlim = c(1990, 2050), ylim = c(0, 75),
#     xlab = "Year", ylab = "People") 
#lines(ForrestSDGs_test$`labour input other` ~ ForrestSDGs_test$Time, col = "#A21942")

# rename variables for ggplot to remove spaces 
Historical_data$labinot <- Historical_data$`labour input other`
ForrestSDGs_test$labinot <- ForrestSDGs_test$`labour input other`
uncertainties$labinot <- uncertainties$`labour input other`

# this code summarises min and max values for the model simulations
unc_summ$Minlabinot <- tapply(uncertainties$labinot, uncertainties$TIME, min)
unc_summ$Maxlabinot <- tapply(uncertainties$labinot, uncertainties$TIME, max)

LIO <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = labinot)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = labinot), color = "#A21942") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = Minlabinot, ymax = Maxlabinot, na.rm = TRUE),
               fill = "#A21942", alpha = 0.2) +
   theme_bw() + ylim(0, 80) +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) +
   ylab("people") + xlab("year") + ggtitle("Labour input other")

LIO

# profitability increase from regenerative agriculture variable

#par(mar = c(2,4,2.5,2) + 0.1)
#plot(Historical_data$`profitability increase from regenerative agriculture` ~ Historical_data$year, 
#     main = "Profitability increase from \nregenerative agriculture", pch = 20,
#     xlim = c(1990, 2050), ylim = c(0, 2),
#     xlab = "Year", ylab = "multiplier") 
#lines(ForrestSDGs_test$`profitability increase from regenerative agriculture` ~ ForrestSDGs_test$Time, 
#      col = "#A21942")

# rename variables for ggplot to remove spaces 
Historical_data$regagprof <- Historical_data$`profitability increase from regenerative agriculture`
ForrestSDGs_test$regagprof <- ForrestSDGs_test$`profitability increase from regenerative agriculture`
uncertainties$regagprof <- uncertainties$`profitability increase from regenerative agriculture`

# this code summarises min and max values for the model simulations
unc_summ$Minregagprof <- tapply(uncertainties$regagprof, uncertainties$TIME, min)
unc_summ$Maxregagprof <- tapply(uncertainties$regagprof, uncertainties$TIME, max)

PIRA <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = regagprof)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = regagprof), color = "#A21942") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = Minregagprof, ymax = Maxregagprof, na.rm = TRUE),
               fill = "#A21942", alpha = 0.2) +
   theme_bw() + ylim(0, 2.5) +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) +
   ylab("multiplier") + xlab("year") + ggtitle("Profitability increase from\nregenerative agriculture")
PIRA

layout <- "
AB#
CDG
EF#
"
TAP + LIA + TTP + LIT + TOP + LIO + PIRA + plot_layout(design = layout)

par(mfrow = c(1,1))
par(mar = c(5,3,1,1) + 0.1)

## climate change component validation plots

par(mfrow = c(2,2))
par(mar = c(2,4,2,2) + 0.1)

# FFDI Variable

#plot(Historical_data$`FFDI model` ~ Historical_data$year, main = "Forest Fire Danger Index", 
#     pch = 20,
#     xlim = c(1990, 2050), ylim = c(1, 3),
#     xlab = " ", ylab = "Index", xaxt = "n") 
#lines(ForrestSDGs_test$'FFDI model' ~ ForrestSDGs_test$Time, col = "#3F7E44") 

# rename variables for ggplot to remove spaces 
Historical_data$ffdi <- Historical_data$`FFDI model`
ForrestSDGs_test$ffdi <- ForrestSDGs_test$`FFDI model`
uncertainties$ffdi <- uncertainties$`FFDI model`

# this code summarises min and max values for the model simulations
unc_summ$Minffdi <- tapply(uncertainties$ffdi, uncertainties$TIME, min)
unc_summ$Maxffdi <- tapply(uncertainties$ffdi, uncertainties$TIME, max)

FFD <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = ffdi)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = ffdi), color = "#3F7E44") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = Minffdi, ymax = Maxffdi, na.rm = TRUE),
               fill = "#3F7E44", alpha = 0.2) +
   theme_bw() + theme(plot.title = element_text(size=14)) +
   ylab("index") + xlab("year") + ylim(0, 3.5) + ggtitle("Forest Fire Danger Index")
FFD

# rainfall variable

#plot(Historical_data$`rainfall linear model` ~ Historical_data$year, main = "Rainfall", 
#     pch = 20,
#     xlim = c(1990, 2050), ylim = c(500, 1050),
#     xlab = " ", ylab = "mm", xaxt = "n") 
#lines(ForrestSDGs_test$`rainfall linear model` ~ ForrestSDGs_test$Time, col = "#3F7E44") 

# rename variables for ggplot to remove spaces 
Historical_data$rain <- Historical_data$`rainfall linear model`
ForrestSDGs_test$rain <- ForrestSDGs_test$`rainfall linear model`
uncertainties$rain <- uncertainties$`rainfall linear model`

# this code summarises min and max values for the model simulations
unc_summ$Minrain <- tapply(uncertainties$rain, uncertainties$TIME, min)
unc_summ$Maxrain <- tapply(uncertainties$rain, uncertainties$TIME, max)

RF <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = rain)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = rain), color = "#3F7E44") +
#   geom_ribbon(data = unc_summ, 
#               aes(x = Time, ymin = Minrain, ymax = Maxrain, na.rm = TRUE),
#              fill = "#3F7E44", alpha = 0.2) +
   theme_bw() + theme(plot.title = element_text(size=14)) +
   ylab("mm") + xlab("year") + ylim(0, 1200) + ggtitle("Annual rainfall")
RF

# temperature variable

#plot(Historical_data$`temperature linear model` ~ Historical_data$year, main = "Temperature", 
#     pch = 20,
#     xlim = c(1990, 2050), ylim = c(16, 19),
#     xlab = "Year", ylab = "Â°C") 
#lines(ForrestSDGs_test$`temperature linear model` ~ ForrestSDGs_test$Time, col = "#3F7E44") 

# rename variables for ggplot to remove spaces 
Historical_data$temp <- Historical_data$`temperature linear model`
ForrestSDGs_test$temp <- ForrestSDGs_test$`temperature linear model`
uncertainties$temp <- uncertainties$`temperature linear model`

# this code summarises min and max values for the model simulations
unc_summ$Mintemp <- tapply(uncertainties$temp, uncertainties$TIME, min)
unc_summ$Maxtemp <- tapply(uncertainties$temp, uncertainties$TIME, max)

TMP <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = temp)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = temp), color = "#3F7E44") +
#   geom_ribbon(data = unc_summ, 
#               aes(x = Time, ymin = Mintemp, ymax = Maxtemp, na.rm = TRUE),
#               fill = "#3F7E44", alpha = 0.2) +
   theme_bw() + theme(plot.title = element_text(size=14)) +
   ylab("°C") + xlab("year") + ylim(0, 19) + ggtitle("Average max temperature")
TMP

# CO2 variable

#plot(Historical_data$`CO2 linear model` ~ Historical_data$year, 
#     main = expression("CO"[2]), 
#     pch = 20,
#     xlim = c(1990, 2050), ylim = c(350, 460),
#     xlab = "Year", ylab = "ppm") 
#lines(ForrestSDGs_test$`CO2 linear model` ~ ForrestSDGs_test$Time, col = "#3F7E44") 

# rename variables for ggplot to remove spaces 
Historical_data$CO2 <- Historical_data$`CO2 linear model`
ForrestSDGs_test$CO2 <- ForrestSDGs_test$`CO2 linear model`
uncertainties$CO2 <- uncertainties$`CO2 linear model`

# this code summarises min and max values for the model simulations
unc_summ$MinCO2 <- tapply(uncertainties$CO2, uncertainties$TIME, min)
unc_summ$MaxCO2 <- tapply(uncertainties$CO2, uncertainties$TIME, max)

PPM <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = CO2)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = CO2), color = "#3F7E44") +
#   geom_ribbon(data = unc_summ, 
#               aes(x = Time, ymin = MinCO2, ymax = MaxCO2, na.rm = TRUE),
#               fill = "#3F7E44", alpha = 0.2) +
   theme_bw() + ylim(0, 450) + theme(plot.title = element_text(size=14)) +
   ylab("ppm") + xlab("year") + ggtitle(expression("CO"[2]))
PPM

FFD + RF + TMP + PPM


## Land Use component validation plots

par(mfrow = c(3,2))
par(mar = c(2,4,2,2) + 0.1)

# Housing Land Variable

#plot(Historical_data$`Housing Land` ~ Historical_data$year, main = "Housing land", 
#     pch = 20,
#     xlim = c(1990, 2050), ylim = c(0, 900),
#     xlab = " ", ylab = "hectares", xaxt = "n") 
#lines(ForrestSDGs_test$`Housing Land` ~ ForrestSDGs_test$Time, col = "#FD9D24") 

# rename variables for ggplot to remove spaces 
Historical_data$HL <- Historical_data$`Housing Land`
ForrestSDGs_test$HL <- ForrestSDGs_test$`Housing Land`
uncertainties$HL <- uncertainties$`Housing Land`

# this code summarises min and max values for the model simulations
unc_summ$MinHL <- tapply(uncertainties$HL, uncertainties$TIME, min)
unc_summ$MaxHL <- tapply(uncertainties$HL, uncertainties$TIME, max)

HLan <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = HL)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = HL), color = "#FD9D24") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = MinHL, ymax = MaxHL, na.rm = TRUE),
               fill = "#FD9D24", alpha = 0.2) +
   theme_bw() + ylim(0, 1000) +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) +
   ylab("hectares") + xlab("year") + ggtitle("Housing land")
HLan

# Forest Land variable

#plot(Historical_data$`Forest Land` ~ Historical_data$year, main = "Forest land", 
#     pch = 20,
#     xlim = c(1990, 2050), ylim = c(4000, 4500),
#     xlab = " ", ylab = "hectares", xaxt = "n") 
#lines(ForrestSDGs_test$`Forest Land` ~ ForrestSDGs_test$Time, col = "#FD9D24") 

# rename variables for ggplot to remove spaces 
Historical_data$FL <- Historical_data$`Forest Land`
ForrestSDGs_test$FL <- ForrestSDGs_test$`Forest Land`
uncertainties$FL <- uncertainties$`Forest Land`

# this code summarises min and max values for the model simulations
unc_summ$MinFL <- tapply(uncertainties$FL, uncertainties$TIME, min)
unc_summ$MaxFL <- tapply(uncertainties$FL, uncertainties$TIME, max)

FLan <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = FL)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = FL), color = "#FD9D24") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = MinFL, ymax = MaxFL, na.rm = TRUE),
               fill = "#FD9D24", alpha = 0.2) +
   theme_bw() + ylim(0, 7000) +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) +
   ylab("hectares") + xlab("year") + ggtitle("Forest land")
FLan


# Agriculture Land variable

#plot(Historical_data$`Agriculture Land` ~ Historical_data$year, main = "Agriculture land", 
#     pch = 20,
#     xlim = c(1990, 2050), ylim = c(2400, 3700),
#     xlab = "Year", ylab = "hectares", xaxt = "n") 
#lines(ForrestSDGs_test$`Agriculture Land` ~ ForrestSDGs_test$Time, col = "#FD9D24") 

# rename variables for ggplot to remove spaces 
Historical_data$AL <- Historical_data$`Agriculture Land`
ForrestSDGs_test$AL <- ForrestSDGs_test$`Agriculture Land`
uncertainties$AL <- uncertainties$`Agriculture Land`

# this code summarises min and max values for the model simulations
unc_summ$MinAL <- tapply(uncertainties$AL, uncertainties$TIME, min)
unc_summ$MaxAL <- tapply(uncertainties$AL, uncertainties$TIME, max)

ALan <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = AL)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = AL), color = "#FD9D24") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = MinAL, ymax = MaxAL, na.rm = TRUE),
               fill = "#FD9D24", alpha = 0.2) +
   theme_bw() + ylim(0,5000) +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) +
   ylab("hectares") + xlab("year") + ggtitle("Agriculture land")
ALan


# Protected Land variable

#plot(Historical_data$`Protected Land` ~ Historical_data$year, main = "Protected land", 
#     pch = 20,
#     xlim = c(1990, 2050), ylim = c(1200, 12000),
#     xlab = "Year", ylab = "hectares", xaxt = "n") 
#lines(ForrestSDGs_test$`Protected Land` ~ ForrestSDGs_test$Time, col = "#FD9D24") 

# rename variables for ggplot to remove spaces 
Historical_data$PL <- Historical_data$`Protected Land`
ForrestSDGs_test$PL <- ForrestSDGs_test$`Protected Land`
uncertainties$PL <- uncertainties$`Protected Land`

# this code summarises min and max values for the model simulations
unc_summ$MinPL <- tapply(uncertainties$PL, uncertainties$TIME, min)
unc_summ$MaxPL <- tapply(uncertainties$PL, uncertainties$TIME, max)

PLan <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = PL)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = PL), color = "#FD9D24") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = MinPL, ymax = MaxPL, na.rm = TRUE),
               fill = "#FD9D24", alpha = 0.2) +
   theme_bw() + ylim(0, 12000) +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) +
   ylab("hectares") + xlab("year") + ggtitle("Protected land")
PLan


# Fertilisation Intensity variable

#plot(Historical_data$`Fertilisation Intensity` ~ Historical_data$year, main = "Fertilisation intensity", 
#     pch = 20,
#     xlim = c(1990, 2050), ylim = c(0.05, 0.26),
#     xlab = "Year", ylab = "Tons per hectare per year") 
#lines(ForrestSDGs_test$`Fertilisation Intensity` ~ ForrestSDGs_test$Time, col = "#FD9D24") 

# rename variables for ggplot to remove spaces 
Historical_data$FI <- Historical_data$`Fertilisation Intensity`
ForrestSDGs_test$FI <- ForrestSDGs_test$`Fertilisation Intensity`
uncertainties$FI <- uncertainties$`Fertilisation Intensity`

# this code summarises min and max values for the model simulations
unc_summ$MinFI <- tapply(uncertainties$FI, uncertainties$TIME, min)
unc_summ$MaxFI <- tapply(uncertainties$FI, uncertainties$TIME, max)

FInt <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = FI)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = FI), color = "#FD9D24") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = MinFI, ymax = MaxFI, na.rm = TRUE),
               fill = "#FD9D24", alpha = 0.2) +
   theme_bw() + ylim(0, 0.275) +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) +
   ylab("tons per hectare per year") + xlab("year") + ggtitle("Fertilisation intensity")
FInt


# fertiliser consumption variable

#plot(Historical_data$`fertiliser consumption` ~ Historical_data$year, main = "Fertiliser consumption", 
#     pch = 20,
#     xlim = c(1990, 2050), ylim = c(170, 920),
#     xlab = "Year", ylab = "Tons per year") 
#lines(ForrestSDGs_test$`fertiliser consumption` ~ ForrestSDGs_test$Time, col = "#FD9D24") 

# rename variables for ggplot to remove spaces 
Historical_data$FC <- Historical_data$`fertiliser consumption`
ForrestSDGs_test$FC <- ForrestSDGs_test$`fertiliser consumption`
uncertainties$FC <- uncertainties$`fertiliser consumption`

# this code summarises min and max values for the model simulations
unc_summ$MinFC <- tapply(uncertainties$FC, uncertainties$TIME, min)
unc_summ$MaxFC <- tapply(uncertainties$FC, uncertainties$TIME, max)

FCom <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = FC)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = FC), color = "#FD9D24") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = MinFC, ymax = MaxFC, na.rm = TRUE),
               fill = "#FD9D24", alpha = 0.2) +
   theme_bw() + ylim(0, 1000) +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank() , plot.title = element_text(size=14)) +
   ylab("tons per year") + xlab("year") + ggtitle("Fertiliser consumption")
FCom

ALan + FLan + PLan + HLan + FCom + FInt

## demographic component validation plots

par(mfrow = c(2,3))
par(mar = c(2,4,2,2) + 0.1)

# 0-14 Variable

#plot(Historical_data$`0-14` ~ Historical_data$year, main = "Population 0-14", 
#     pch = 20,
#     xlim = c(1990, 2050), ylim = c(20, 60),
#     xlab = " ", ylab = "people", xaxt = "n") 
#lines(ForrestSDGs_test$`0-14` ~ ForrestSDGs_test$Time, col = "#E5243B") 

# rename variables for ggplot to remove spaces 
Historical_data$young <- Historical_data$`0-14`
ForrestSDGs_test$young <- ForrestSDGs_test$`Young`
uncertainties$young <- uncertainties$`Young`

# this code summarises min and max values for the model simulations
unc_summ$Minyoung <- tapply(uncertainties$young, uncertainties$TIME, min)
unc_summ$Maxyoung <- tapply(uncertainties$young, uncertainties$TIME, max)

plot_young <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = young)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = young), color = "#E5243B") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = Minyoung, ymax = Maxyoung, na.rm = TRUE),
               fill = "#E5243B", alpha = 0.2) +
   theme_bw() + 
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) + 
   ylim(0, 50) +
   ylab("people") + xlab("year") + ggtitle("Population 0-14")

plot_young
# 15-64 Variable

#plot(Historical_data$`15-64` ~ Historical_data$year, main = "Population 15-64", 
#     pch = 20,
#     xlim = c(1990, 2050), ylim = c(100, 600),
#     xlab = " ", ylab = "people", xaxt = "n") 
#lines(ForrestSDGs_test$`15-64` ~ ForrestSDGs_test$Time, col = "#E5243B") 

# rename variables for ggplot to remove spaces 
Historical_data$mid <- Historical_data$`15-64`
ForrestSDGs_test$mid <- ForrestSDGs_test$`Mid`
uncertainties$mid <- uncertainties$`Mid`

# this code summarises min and max values for the model simulations
unc_summ$Minmid <- tapply(uncertainties$mid, uncertainties$TIME, min)
unc_summ$Maxmid <- tapply(uncertainties$mid, uncertainties$TIME, max)

plot_mid <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = mid)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = mid), color = "#E5243B") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = Minmid, ymax = Maxmid, na.rm = TRUE),
               fill = "#E5243B", alpha = 0.2) +
   theme_bw() + ylim(0, 400) +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) +
   ylab("people") + xlab("year") + ggtitle("Population 15-64")
plot_mid

# 65+ Variable

#plot(Historical_data$`65+` ~ Historical_data$year, main = "Population 65+", 
#     pch = 20,
#     xlim = c(1990, 2050), ylim = c(0, 130),
#     xlab = " ", ylab = "people") 
#lines(ForrestSDGs_test$`65+` ~ ForrestSDGs_test$Time, col = "#E5243B")

# rename variables for ggplot to remove spaces 
Historical_data$old <- Historical_data$`65+`
ForrestSDGs_test$old <- ForrestSDGs_test$`Old`
uncertainties$old <- uncertainties$`Old`

# this code summarises min and max values for the model simulations
unc_summ$Minold <- tapply(uncertainties$old, uncertainties$TIME, min)
unc_summ$Maxold <- tapply(uncertainties$old, uncertainties$TIME, max)

plot_old <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = old)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = old), color = "#E5243B") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = Minold, ymax = Maxold, na.rm = TRUE),
               fill = "#E5243B", alpha = 0.2) +
   theme_bw() + ylim(0, 110) +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) +
   ylab("people") + xlab("year") + ggtitle("Population 65+")
plot_old

# total population variable

#plot(Historical_data$`total population` ~ Historical_data$year, main = "Total population", 
#     pch = 20,
#     xlim = c(1990, 2050), ylim = c(100, 750),
#     xlab = " ", ylab = "people") 
#lines(ForrestSDGs_test$'total population' ~ ForrestSDGs_test$Time, col = "#E5243B")

# rename variables for ggplot to remove spaces 
Historical_data$totpop <- Historical_data$`total population`
ForrestSDGs_test$totpop <- ForrestSDGs_test$`total population`
uncertainties$totpop <- uncertainties$`total population`

# this code summarises min and max values for the model simulations
unc_summ$Mintotpop <- tapply(uncertainties$totpop, uncertainties$TIME, min)
unc_summ$Maxtotpop <- tapply(uncertainties$totpop, uncertainties$TIME, max)

plot_all <- ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = totpop)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = totpop), color = "#E5243B") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = Mintotpop, ymax = Maxtotpop, na.rm = TRUE),
               fill = "#E5243B", alpha = 0.2) +
   theme_bw() + ylim(0, 600) +
   theme(axis.title.x=element_blank(), axis.text.x=element_blank(), plot.title = element_text(size=14)) +
   ylab("people") + xlab("year") + ggtitle("Total population")
plot_all

library(patchwork)

plot_young + plot_mid + plot_old + plot_all



# road accidents variable

#plot(Historical_data$`road accidents` ~ Historical_data$year, main = "Road accidents", 
#     pch = 20,
#     xlim = c(1990, 2050), ylim = c(0, 20),
#     xlab = " ", ylab = "people") 
#lines(ForrestSDGs_test$'road accidents' ~ ForrestSDGs_test$Time, col = "#E5243B")

# rename variables for ggplot to remove spaces 
Historical_data$roadac <- Historical_data$`road accidents`
ForrestSDGs_test$roadac <- ForrestSDGs_test$`road accidents`
uncertainties$roadac <- uncertainties$`road accidents`

# this code summarises min and max values for the model simulations
unc_summ$Minroadac <- tapply(uncertainties$roadac, uncertainties$TIME, min)
unc_summ$Maxroadac <- tapply(uncertainties$roadac, uncertainties$TIME, max)

ggplot() +
   geom_point(data = Historical_data, aes(x = year, y = roadac)) +
   geom_line(data = ForrestSDGs_test, aes(x = Time, y = roadac), color = "#E5243B") +
   geom_ribbon(data = unc_summ, 
               aes(x = Time, ymin = Minroadac, ymax = Maxroadac, na.rm = TRUE),
               fill = "#E5243B", alpha = 0.2) +
   ylab("people") + xlab("year") + ggtitle("Road accidents")


horiz_layout <- "
ABCD
EFGH
IJKL
MNOP
QRST
"
plot_young + plot_mid + plot_old + plot_all +
   TAP + TTP + TOP + PIRA + LIA + LIT + LIO + FCom + ALan + FLan + PLan + HLan +
   FFD + RF + TMP + PPM +
   plot_layout(design = horiz_layout)
