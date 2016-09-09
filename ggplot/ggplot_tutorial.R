# ggplot


housing <- read.csv("dataSets/landdata-states.csv")
head(housing[1:5])

# Histogram
ggplot(housing, aes(x = Home.Value)) + geom_histogram()

# Colored scatterplot
ggplot(subset(housing, State %in% c("MA","TX")),
       aes(x = Date,
           y = Home.Value,
           color = State ))+
  geom_point()

# Scatterplot
hp2001q1 = subset(housing, Date == 20011)
ggplot(hp2001q1, aes(y = Structure.Cost, x= Land.Value)) +
  geom_point()

# Lines
hp2001q1$pred.SC = predict(lm(Structure.Cost ~ Land.Value, data = hp2001q1))

p1 = ggplot(hp2001q1, aes(x = Land.Value, y = Structure.Cost))

p1 + geom_point(aes(color = Home.Value)) +
  geom_line(aes(y = pred.SC))

# Smoothers
p1 + geom_point(aes(color = Home.Value)) + geom_smooth()

# Text as label points
p1 + geom_text(aes(label = State), size = 3)

install.packages("ggrepel")
library(ggrepel)
p1 + geom_point() + geom_text_repel(aes(label = State) , size = 3)

# aes() have to do with variables.  Fixed aesthetics are done outside of aes()
p1 + geom_point(aes(color = Home.Value, shape = region))


# Economist data
dat = read.csv("dataSets/EconomistData.csv")
# Exercises
cpi.hdi = ggplot(data = dat, aes(x = CPI, y = HDI))
cpi.hdi + geom_point()
# adding some color
cpi.hdi + geom_point(color = "blue")
# Color by region
cpi.hdi + geom_point(aes(color = Region))
# Box plots of CPI
ggplot(data = dat, aes(x = Region, y = CPI))+
  geom_boxplot() +geom_point()

# Transformations
p2 = ggplot(housing, aes(x = Home.Value))
p2 + geom_histogram()
p2 + geom_histogram(stat = "bin", binwidth = 4000)

housing.sum = aggregate(housing["Home.Value"],housing["State"], FUN = mean)
rbind(head(housing.sum), tail(housing.sum))

ggplot(housing.sum, aes(x = State, y = Home.Value )) +
  geom_bar(stat = "identity")

## Exercises
cpi.hdi = ggplot(data = dat, aes(x = CPI, y = HDI))
cdi.hdi + geom_point()

cpi.hdi + geom_point()+
  geom_smooth(method = "lm")
# Previous method
dat$pred.hdi.cpi = predict(lm(HDI ~ CPI, data = dat))

cpi.hdi + geom_point() +
  geom_line(aes(y = pred.hdi.cpi))

# OR
cpi.hdi + geom_point() +
  geom_smooth()

#
ggplot(dat, aes(x = CPI, y = HDI)) +
  geom_point() +
  geom_smooth(span = .4)

### Scale Modification

p3 = ggplot(housing, aes(x = State,
                         y = Home.Price.Index)) +
  theme(legend.position = "top",
        axis.text = element_text(size = 6))
p4 = p3 + geom_point(aes(color = Date),
                     alpha = .5,
                     size = 1.5,
                     position = position_jitter(width = .25, height = 0))

p4 + scale_x_discrete(name = "State Abrreviation") +
  scale_color_continuous(name = "",
                         breaks = c(19751,19941,20131),
                         labels = c(1971,1994,2013))

#Changing high and low colors
p4 + scale_x_discrete(name = "State Abrreviation") +
  scale_color_continuous(name = "",
                         breaks = c(19751,19941,20131),
                         labels = c(1971,1994,2013),
                         low = "blue" , high = "red")

p4 + scale_x_discrete(name = "State Abrreviation") +
  scale_color_continuous(name = "",
                         breaks = c(19751,19941,20131),
                         labels = c(1971,1994,2013),
                         low = muted("blue") , high = muted("red"))

p4 + scale_x_discrete(name = "State Abrreviation") +
  scale_color_gradient2(name = "",
                         breaks = c(19751,19941,20131),
                         labels = c(1971,1994,2013),
                         low = "blue" , 
                         mid = "gray60",
                         high = "red",
                        midpoint = 19941)
# Exercises III
cpi.hdi = ggplot(data = dat, aes(x = CPI, y = HDI, color = Region))
cpi.hdi + geom_point()
# Changing Labels
cpi.hdi + geom_point() + scale_x_continuous(name = "Corruption Perception Index") +
  scale_y_continuous(name = "Human Development Index") +
  scale_color_discrete(name = "Region of the World")

# Changing Color Scale
ggplot(dat, aes(x = CPI, y = HDI, color = Region)) +
  geom_point() +
  scale_x_continuous(name = "Corruption Perception Index") +
  scale_y_continuous(name = "Human Development Index") +
  scale_color_manual(name = "Region of the world",
                     values = c("#24576D",
                                "#099DD7",
                                "#28AADC",
                                "#248E84",
                                "#F2583F",
                                "#96503F"))


# Faceting
p5 = ggplot(housing, aes(x = Date, y = Home.Value))
p5 + geom_line(aes(color = State))

# Facet
p5 = p5 + geom_line() + facet_wrap(~State, ncol = 10)
p5

## Themes
p5 + theme_linedraw()
p5 + theme_light()
p5 + theme_minimal() +
  theme(text = element_text(color = "turquoise"))


library(tidyr)
housing.byyear <- aggregate(cbind(Home.Value, Land.Value) ~ Date, data = housing, mean)
home.land.byyear = gather(housing.byyear,
                          value = "value",
                          key = "type",
                          Home.Value, Land.Value)

ggplot(home.land.byyear,
       aes(x = Date,
           y = value,
           color = type)) +
  geom_line()


# Challenge Solution
pc1 <- ggplot(dat, aes(x = CPI, y = HDI, color = Region))
pc1 + geom_point()

pc2 = pc1 +
  geom_smooth(aes(group = 1),
              method = "lm",
              formula = y ~ log(x),
              se = FALSE,
              color = "red") +
  geom_point()

pc2 + geom_point(shape = 1, size = 4)

(pc3 <- pc2 +
    geom_point(size = 4.5, shape = 1) +
    geom_point(size = 4, shape = 1) +
    geom_point(size = 3.5, shape = 1))


pointsToLabel <- c("Russia", "Venezuela", "Iraq", "Myanmar", "Sudan",
                   "Afghanistan", "Congo", "Greece", "Argentina", "Brazil",
                   "India", "Italy", "China", "South Africa", "Spane",
                   "Botswana", "Cape Verde", "Bhutan", "Rwanda", "France",
                   "United States", "Germany", "Britain", "Barbados", "Norway", "Japan",
                   "New Zealand", "Singapore")
library(ggrepel)
pc4 = pc3 + geom_text_repel(aes(label = Country),
                      color = "gray20",
                      data= subset(dat, Country %in% pointsToLabel),
                      force = 10)

dat$Region <- factor(dat$Region,
                     levels = c("EU W. Europe",
                                "Americas",
                                "Asia Pacific",
                                "East EU Cemt Asia",
                                "MENA",
                                "SSA"),
                     labels = c("OECD",
                                "Americas",
                                "Asia &\nOceania",
                                "Central &\nEastern Europe",
                                "Middle East &\nnorth Africa",
                                "Sub-Saharan\nAfrica"))
pc4$data <- dat
pc4

library(grid)
(pc5 <- pc4 +
    scale_x_continuous(name = "Corruption Perceptions Index, 2011 (10=least corrupt)",
                       limits = c(.9, 10.5),
                       breaks = 1:10) +
    scale_y_continuous(name = "Human Development Index, 2011 (1=Best)",
                       limits = c(0.2, 1.0),
                       breaks = seq(0.2, 1.0, by = 0.1)) +
    scale_color_manual(name = "",
                       values = c("#24576D",
                                  "#099DD7",
                                  "#28AADC",
                                  "#248E84",
                                  "#F2583F",
                                  "#96503F")) +
    ggtitle("Corruption and Human development"))

library(grid) # for the 'unit' function
(pc6 <- pc5 +
    theme_minimal() + # start with a minimal theme and add what we need
    theme(text = element_text(color = "gray20"),
          legend.position = c("top"), # position the legend in the upper left 
          legend.direction = "horizontal",
          legend.justification = 0.1, # anchor point for legend.position.
          legend.text = element_text(size = 11, color = "gray10"),
          axis.text = element_text(face = "italic"),
          axis.title.x = element_text(vjust = -1), # move title away from axis
          axis.title.y = element_text(vjust = 2), # move away for axis
          axis.ticks.y = element_blank(), # element_blank() is how we remove elements
          axis.line = element_line(color = "gray40", size = 0.5),
          axis.line.y = element_blank(),
          panel.grid.major = element_line(color = "gray50", size = 0.5),
          panel.grid.major.x = element_blank()
    ))

mR2 <- summary(lm(HDI ~ log(CPI), data = dat))$r.squared

pc6 
grid.text("Sources: Transparency International; UN Human Development Report",
          x = .02, y = .03,
          just = "left",
          draw = TRUE)
grid.segments(x0 = 0.81, x1 = 0.825,
              y0 = 0.90, y1 = 0.90,
              gp = gpar(col = "red"),
              draw = TRUE)
grid.text(paste0("R² = ",
                 as.integer(mR2*100),
                 "%"),
          x = 0.835, y = 0.90,
          gp = gpar(col = "gray20"),
          draw = TRUE,
          just = "left")

