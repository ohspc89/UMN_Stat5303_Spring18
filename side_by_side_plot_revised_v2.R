pfdata <- read.table(
  'http://www.stat.umn.edu/~corbett/classes/5303/RDataFiles/exmpl8.8',
  header=T)

# Factorizing
pfdata$alg <- as.factor(pfdata$alg)
pfdata$seq <- as.factor(pfdata$seq)
pfdata$psize <- as.factor(pfdata$psize)
pfdata$ram <- as.factor(pfdata$ram)

# This will include the main effects and
# two-way interaction terms.
out_pf <- aov(y~.*., pfdata)
anova(out_pf)

# Include three-way interaction terms
out_pf2 <- aov(y~.*.*., pfdata)
anova(out_pf2)

# Check if transformation is required. log-trans recommended
car::powerTransform(out_pf2)

# Refit a model
out_pf2_log <- aov(log(y)~.*.*., pfdata)
anova(out_pf2_log)


# A function that calculates SS excluding the SS of
# a factor and its interactions with other factors
ssq_exclude <- function(model, factor)
{
  anova_result <- anova(model)
  res_vec <- rep(0, nrow(anova_result))
  for (i in 1:nrow(anova_result))
  {
    # 'pmatch' examines the partial match of a character
    # to a series of characters
    res_vec[i] <- pmatch(factor, rownames(anova_result)[i])
  }
  
  # SS without the SS{factor of interest}
  return(sum(anova_result$`Sum Sq`[-which(res_vec == 1)]))
}

# page 190, bottom paragraph
# "The full model explains 173.6 SS;of that, 170.9 is
# explained by allocation, size, load sequence, and their
# interactions. Thus while algorithm and some of its
# interactions may be significant, their effects are
# tiny compared to the other effects.

# Returns 170.9
ssq_exclude(out_pf2_log, "alg")


# Updated side_by_side plot function
# An aov model is give as the input of the function
# boxplot_threshod: when terms have elements beyond this number,
# corresponding boxplots will be drawn.
# exclude_term: designate terms that would be removed from the plot.
side_by_side <- function(model, boxplot_threshold = 20, exclude_term = NULL)
{
  # get the term names
  term_labels <- attr(model$terms, 'term.labels')
  
  if (!is.null(exclude_term))
  {
    term_labels <- term_labels[!term_labels %in% exclude_term]
  }
  
  list1 <- list()
  for (i in 1:length(term_labels))
  {
    # fill an empty list with effects of each term.
    list1[[term_labels[i]]] <- as.vector(cfcdae::model.effects(model, term_labels[i]))
  }
  
  # Residuals are not counted as an explicit term. Add it manually.
  list1[['Residuals']] <- model$residuals
  
  # ldply: 'l'ist to 'd'ata frame
  df1 <- plyr::ldply(list1, rbind)
  
  # set factor levels: from main effects to high order interaction terms, left to right
  # trick 1 : main effect terms will be shorter in terms of their names and
  # higher order interaction terms will have longer names.
  # therefore, count the number of characters in each term, sort them in an
  # ascending order, and store the names of the terms in that order.
  # trick 2: factor level 'Residuals', according to this method, 
  # will not be placed in the last. Move that factor level to the end.  
  factor_labels <- names(sort(sapply(df1$.id[!df1$.id=='Residuals'], FUN=nchar), decreasing=F))
  factor_labels <- c(factor_labels, "Residuals")
  df1$.id <- factor(df1$.id, levels=factor_labels)
  
  # adjust it to apply ggplot
  df1_long <- na.omit(reshape2::melt(df1, id.var = '.id'))
  
  # choose which term(s) to make boxplots
  boxplot_terms <- df1_long[df1_long$variable == boxplot_threshold, 1]
  
  library(ggplot2)
  p <- ggplot(df1_long, aes(x=.id, y=value))+geom_boxplot(data = df1_long[df1_long$.id %in% boxplot_terms, ], outlier.shape = 8)
  p <- p + geom_text(data=df1_long[!df1_long$.id %in% boxplot_terms,], 
                label=df1_long[!df1_long$.id %in% boxplot_terms, 2])
  p <- p + scale_x_discrete(limits = levels(df1_long$.id)) + theme_bw()
  
  p <- p + theme(axis.text.x = element_text(size=12, angle = 45, hjust=.5),
                 axis.text.y = element_text(size=12, face = 'bold'),
                 axis.title.x = element_blank(),
                 axis.ticks.y = element_line(),
                 plot.background = element_blank(),
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank())
  
    p + scale_y_continuous('Effects or residuals')
}

# sample execution
side_by_side(model=out_pf2_log, boxplot_threshold=19, exclude_term=c('alg'))

sample_dt <- read.table('/home/jinseok/users.stat.umn.edu/~corbett/classes/5303/RDataFiles/exmpl8.10', header=T)

sample_dt$atemp <- as.factor(sample_dt$atemp)
sample_dt$gtemp <- as.factor(sample_dt$gtemp)
sample_dt$variety <- as.factor(sample_dt$variety)

fit1 <- lm(log(y)~atemp*gtemp*variety, sample_dt)

side_by_side(fit1, boxplot_threshold = 15)
