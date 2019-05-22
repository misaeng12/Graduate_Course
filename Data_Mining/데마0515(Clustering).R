?hclust
hc <- hclust(dist(USArrests), "ave") 
plot(hc, hang=-1)

cutree(hc, 2)


## Homework (Chapter10)

# Lab
# Exercise 7, 10
# A quick tour of mclust


library(ggplot2)

### Exercise 10.7

r <- cor((t(USArrests)))
Euclidean <- as.matrix(dist(t(scale(t(USArrests)))))^2
Euclidean/(1-r)



### Exercise 10.10

#(a)
set.seed(1)
X <- matrix(rnorm(60*50), ncol=50)
X[1:20,] <- X[1:20,] + 1
X[41:60,] <- X[41:60,] - 1
Class <- as.factor(rep(c("Class1", "Class2", "Class3"), each=20))
ggplot() + geom_point(aes(1:60, rowMeans(X), color=Class, shape=Class), size=3) +
  labs(title="Simulated Data", x="Observation Number", y="Mean")

#(b)
PCA <- prcomp(X)
PC <- data.frame(PC1=PCA$x[,1], PC2=PCA$x[,2])
ggplot(PC) + geom_point(aes(PC1, PC2, color=Class, shape=Class), size=3)

#(c)
Kmeans1 <- kmeans(X, 3)
table(Class, Kmeans1$cluster)

#(d)
Kmeans2 <- kmeans(X, 2)
table(Class, Kmeans2$cluster)

#(e)
Kmeans3 <- kmeans(X, 4)
table(Class, Kmeans3$cluster)

#(f)
Kmeans4 <- kmeans(PC, 3)
table(Class, Kmeans4$cluster)

#(g)
Kmeans5 <- kmeans(scale(X), 3)
table(Class, Kmeans5$cluster)
