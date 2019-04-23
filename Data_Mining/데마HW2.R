### Exercise 2.9

library(ISLR)
data(Auto)
?Auto
Auto <- Auto[complete.cases(Auto),]

#(a)
Auto$origin <- as.factor(Auto$origin)

#(b)
apply(Auto[,-c(8, 9)], 2, range)

#(c)
colMeans(Auto[,-c(8, 9)])
apply(Auto[,-c(8, 9)], 2, sd)

#(d)
Auto2 <- Auto[-(10:85),]
apply(Auto2[,-c(8, 9)], 2, range)
colMeans(Auto2[,-c(8, 9)])
apply(Auto2[,-c(8, 9)], 2, sd)

#(e), (f)
pairs(Auto[,-c(8,9)])

library(ggplot2)

origin_name <- c("American", "European", "Japanese")
ggplot(Auto) + geom_boxplot(aes(x=origin, y=mpg)) + scale_x_discrete(labels=origin_name)
ggplot(Auto) + geom_boxplot(aes(x=origin, y=displacement)) + scale_x_discrete(labels=origin_name)
ggplot(Auto) + geom_boxplot(aes(x=origin, y=horsepower)) + scale_x_discrete(labels=origin_name)
ggplot(Auto) + geom_boxplot(aes(x=origin, y=weight)) + scale_x_discrete(labels=origin_name)
ggplot(Auto) + geom_boxplot(aes(x=origin, y=acceleration)) + scale_x_discrete(labels=origin_name)
ggplot(Auto) + geom_boxplot(aes(x=origin, y=year)) + scale_x_discrete(labels=origin_name)





### Exercise 2.10

#(a)
library(MASS)
Boston
?Boston
summary(Boston)

#(b)
pairs(Boston[,-4])

#(c)
ggplot(Boston) + geom_point(aes(x=zn, y=crim))
ggplot(Boston) + geom_point(aes(x=indus, y=crim))
ggplot(Boston) + geom_point(aes(x=rad, y=crim))
ggplot(Boston) + geom_point(aes(x=tax, y=crim))
ggplot(Boston) + geom_point(aes(x=ptratio, y=crim))

#(d)
library(dplyr)
mutate(Boston, n=c(1:nrow(Boston))) %>% arrange(desc(crim))
mutate(Boston, n=c(1:nrow(Boston))) %>% arrange(desc(tax))
mutate(Boston, n=c(1:nrow(Boston))) %>% arrange(desc(ptratio))

#(e)
sum(as.numeric(Boston$chas))

#(f)
median(Boston$ptratio)

#(g)
Boston[which.min(Boston$medv),]
nrow(filter(Boston, zn==0))/nrow(Boston)

#(h)
sum(Boston$rm > 7)
sum(Boston$rm > 8)
Boston[Boston$rm > 8,]
