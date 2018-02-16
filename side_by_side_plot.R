# Plotting a side-by-side plot with resin data(p. 54)
# The textbook's plot has been made with MacAnova, so I thought it would be a good practice
# to draw a same plot with R using ggplot2.

library(cfcdae);library(ggplot2)

# resin data
resin <- read.table('http://users.stat.umn.edu/~corbett/classes/5303/RDataFiles/exmpl3.2', header=T)

# make trt as factors
resin$temp <- as.factor(resin$temp)

# set a linear model
out <- lm(y~temp, resin) 

# prepare a dataframe that stores effects and residuals
dt1 <- data.frame(effects = c(as.numeric(model.effects(out, "temp")), resid(out)),
                  labels = factor(c(rep("Temp",5), rep("Residuals", 37)),
                                  levels=c("Temp", "Residuals")))

# A basic side-by-side plot
p <- ggplot(dt1, aes(x=labels, y=effects))+geom_boxplot(data = dt1[dt1$labels=='Residuals',], outlier.shape=8)+
  geom_point(data = dt1[dt1$labels=="Temp",], shape=32) + 
  geom_text(data = dt1[dt1$labels=="Temp",], aes(x=labels, y=effects), 
            label=c(1,2,3,4,5), size = 6) + 
  scale_x_discrete(limits=levels(dt1$labels))

# Background theme to a black and white
p2 <- p + theme_bw()

# Remove grids and make the background blank
p3 <- p2 + theme(axis.text.x = element_text(size = 14, face = 'bold'),
               axis.text.y = element_text(size = 14, face = 'bold'),
               axis.title.x = element_blank(),
               axis.title.y = element_text(size = 16, face = 'bold'),
               axis.ticks.y = element_line(),
               plot.background = element_blank(),
               panel.grid.major = element_blank(),
               panel.grid.minor = element_blank())+
  scale_y_continuous('Effects or residuals', breaks = seq(-0.5, 0.6, 0.1))

p3