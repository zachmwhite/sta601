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
