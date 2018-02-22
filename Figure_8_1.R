df1 <- data.frame(Diet=factor(c("Skim milk protein", "Whey", "Casein"), levels = c("Skim milk protein", "Whey", "Casein")), Control = c(.70, .93, 2.11), Cu_deficient = c(1.28, 1.87, 2.53))

library(reshape2); library(ggplot2)

# Reshaping the data frame to plot conveniently
df2 <- melt(df1, id.vars='Diet', variable.name='Condition', value.name='Iron')

p <- ggplot(df2, aes(x=Diet, y=Iron, group=Condition, label=Condition))
p2 <- p + geom_line(aes(lty=Condition))+theme_bw()+
  theme(panel.grid=element_blank(),
        axis.text.x = element_text(size=13),
        axis.text.y = element_text(size=13),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15))+
  xlab('Milk Diet')+
  scale_y_continuous(breaks=seq(0, 2.5, by=0.2))

p2
