library(ggplot2)
library(dplyr)
library(GGally)
library(gridExtra)
library(BBmisc)
#read data
scores <- readRDS("fifth_grade_tested.rds")
scores <- scores[, c("Economic.Need.Index", "Average.Math.Proficiency")] %>%
  normalize(method = "range", 
            margin = 2, 
            range = c(0, 1), 
            on.constant = "quiet")%>%
  as.data.frame()
ggpairs(scores)
#elbow method! Plots within sum of squares vs k
#more clustering means less variance left unexplained by clusters
set.seed(42)
max <- 20
within_ss <- sapply(1:max, function(k)kmeans(scores, k, nstart=50,iter.max = 30)$tot.withinss)
print(within_ss)
ggplot(mapping = aes(1:max, within_ss)) + 
  geom_point() +
  geom_line()+scale_x_continuous(breaks = 1:20) +
  xlab("k")
#looks like k=3 or k=4 is appropriate
kmeans.fit.3 <- kmeans(scores, 3, nstart=50,iter.max = 30)
kmeans.fit.4 <- kmeans(scores, 4, nstart=50,iter.max = 30)
scores <- cbind(scores, cat3 = kmeans.fit.3$cluster, cat4 = kmeans.fit.4$cluster)
p1 <- ggplot(aes(Economic.Need.Index, Average.Math.Proficiency), data = scores) +
  geom_point(aes(shape=as.factor(cat3), color=as.factor(cat3))) + 
  ggtitle("Cluster Analysis with K = 3")
p2 <- ggplot(aes(Economic.Need.Index, Average.Math.Proficiency), data = scores) + 
  geom_point(aes(shape=as.factor(cat4), color=as.factor(cat4))) + 
  ggtitle("Cluster Analysis with K = 4")
grid.arrange(p1, p2, nrow=1)
#neato, good visualization for 3/4 classes of schools based on affluence and performance
saveRDS(scores, file = "scores_cat.rds")
