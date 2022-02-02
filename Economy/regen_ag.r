
x_year <- c(2007, 2008, 2009, 2010, 2011, 2012, 2013, 2014, 2015, 2016)
y_fraction <- c(1.80, 1.34, 1.46, 1.58, 1.00, 1.31, 1.81, 1.25, 1.45, 1.09)

### Data taken from "Regen Ag.xlsx" data file

production <- data.frame(x_year, y_fraction)

### Linear model

library(tidyverse)
library(ggpubr)

ggplot(production, aes(x = x_year, y = y_fraction)) + 
  geom_point(col = "blue") +
  geom_smooth(method = "lm", col = "blue") +
  xlab("Year") + ylab("Productivity fraction over baseline") +
  ggtitle("Regenerative agriculture impact on productivity")


plot(y_fraction~x_year)
lm(y_fraction~x_year)
