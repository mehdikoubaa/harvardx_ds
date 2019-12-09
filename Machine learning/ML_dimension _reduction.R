library(tidyverse)
data("tissue_gene_expression")
dim(tissue_gene_expression$x)
#select 2 first predictors
sample_x <- tissue_gene_expression$x[,c(1,2)]
sample_x <- cbind (sample_x,tissue_gene_expression$y)
d <- dist(sample_x)
plot(d)

#solution given
pc <- prcomp(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#Q2
options(digits = 3)
avg <- rowMeans(pc$x)
data.frame(pc_1 = pc$x[,1], avg = avg, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(y=pc_1, x=avg, color = tissue)) +
  geom_point()
cor(avg, pc$x[,1])

#♥solution given
avgs <- rowMeans(tissue_gene_expression$x)
data.frame(pc_1 = pc$x[,1], avg = avgs, 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(avgs, pc_1, color = tissue)) +
  geom_point()
cor(avgs, pc$x[,1])

#Q3
x <- with(tissue_gene_expression, sweep(x, 1, rowMeans(x)))
pc <- prcomp(x)
data.frame(pc_1 = pc$x[,1], pc_2 = pc$x[,2], 
           tissue = tissue_gene_expression$y) %>%
  ggplot(aes(pc_1, pc_2, color = tissue)) +
  geom_point()

#Q4
data.frame(pc$x[,1:10],tissue=tissue_gene_expression$y) %>% 
  gather(comp,value,-tissue)%>%
  ggplot(aes(comp,value,col=tissue))+
  geom_boxplot()
#solution given
for(i in 1:10){
  boxplot(pc$x[,i] ~ tissue_gene_expression$y, main = paste("PC", i))
}

plot(summary(pc)$importance[3,])
