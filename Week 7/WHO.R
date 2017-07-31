WHO = read.csv("WHO.csv")
str(WHO)

plot(WHO$GNI, WHO$FertilityRate)

library(ggplot2)
scatterplot = ggplot(WHO, aes(x = GNI, y = FertilityRate))
scatterplot + geom_point()
scatterplot + geom_line()
scatterplot + geom_point()

scatterplot + geom_point(color="blue", size = 3, shape = 17)
scatterplot + geom_point(color="darkred", size = 3, shape = 8)
scatterplot + geom_point(color="darkred", size = 3, shape = 8) + ggtitle("Fertility Rave vs. GNI")
fertilityGNIplot = scatterplot + geom_point(color="darkred", size = 3, shape = 8) + ggtitle("Fertility Rave vs. GNI")
pdf("Myplot.pdf")
print(fertilityGNIplot)
dev.off()

scatterplot + geom_point(color="blue", size = 3, shape = 15)

ggplot(WHO, aes(x=GNI, y=FertilityRate, color=Region)) + geom_point()

ggplot(WHO, aes(x=GNI, y=FertilityRate, color=LifeExpectancy)) + geom_point()

ggplot(WHO, aes(x = FertilityRate, y = Under15, color=Region)) + geom_point()
