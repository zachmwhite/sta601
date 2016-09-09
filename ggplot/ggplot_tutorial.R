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
