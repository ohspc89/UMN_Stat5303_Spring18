# Chapter 11, Carton experiment
# This project intends to implement the below goals in R

# 1. a function that calculates satterthwaite degrees of freedom
# 2. a function that calculates approximate F value
# 3. a function that calculates estimated variances of the random effects
# 4. a function that re-calculates EMS, based on the result from function #3

# setting up the data frame
carton <- data.frame(DF = c(9, 9, 1, 81, 9, 9, 81, 200),SS =c(2706, 8887, 2376, 1683, 420.4, 145.3, 1650, 4646))
carton$MS <- carton$SS/carton$DF
rownames(carton) <- c('m', 'o', 'g', 'm.o', 'm.g', 'o.g', 'm.o.g', 'error')

carton

# a function to find the satterthwaite degrees of freedom
satterthwaite_df <- function(MS = list(), df = list()){
  df <- sum(MS)^2 / sum(MS^2/(df))
  return(df)
}

# a function that calculates how many main effects are in each term
term_lengths <- function(terms){
  return(sapply(strsplit(terms, split='[.]'), length))
}

# a function to calculate approximate F-value, degree of freedoms and the corresponding p-values up to three-way interaction effects
# could think of some way to deal with n-way interaction effects later..
approx_f <- function(target_effect, data.frame){
  # don't run the function if a user tries to get F-value of an error term
  try(if(target_effect %in% c('error', 'residual', 'Error', 'Residual')) stop("Residuals do not have F-values")
  else{
    terms <- rownames(data.frame)
    N <- nrow(data.frame)
    length_of_target <- length(strsplit(target_effect, split="[.]")[[1]])
    if(length_of_target == 3){
      numer <- target_effect; denom <- terms[N]
    }
    else if(length_of_target == 2){
      numer <- target_effect; denom <- terms[(N-1)]
    }
    else{
      numer <- c(target_effect, terms[(N-1)])

    # this part needs to be imporved...or not
      index <- c()
      for(i in 1:N){
        if(target_effect %in% strsplit(terms[i], split='[.]')[[1]]){
          if(term_lengths(terms)[i] == (length_of_target+1)){
            index <- c(index, i)
          }
        }
      }
      denom <- terms[index]
    }
    F_target <- sum(data.frame$MS[which(terms %in% numer)])/sum(data.frame$MS[which(terms %in% denom)])

    if(length(numer) > 1){
      df1 <- satterthwaite_df(data.frame$MS[which(terms %in% numer)], data.frame$DF[which(terms %in% numer)])
      df2 <- satterthwaite_df(data.frame$MS[which(terms %in% denom)], data.frame$DF[which(terms %in% denom)])
    }
    else{
      df1 <- data.frame$DF[terms == numer]; df2 <- data.frame$DF[terms == denom]
    }
    return(data.frame(F.approx = F_target, F.df1 = df1, F.df2 = df2, p_val = pf(F_target, df1, df2, lower=F)))
  })
}

# a function to augment an original data frame with critical F-value related columns
# the function makes use of the previous function, approx_f
add_F_cols <- function(data.frame)
{
  # prepare a data frame that just has the shell
  Ftable <- data.frame(F.approx = NULL, F.df1 = NULL, F.df2 = NULL, p_val=NULL)
# (nrow(data.frame)-1): Error term does not have F-value!!!
  for (i in 1:(nrow(data.frame)-1)){
    Ftable <- rbind(Ftable, approx_f(rownames(data.frame)[i], data.frame))
  }
  Ftable <- rbind(Ftable, data.frame(F.approx = NA, F.df1 = NA, F.df2 = NA, p_val=NA))
  rownames(Ftable) <- rownames(data.frame)
  return(cbind(data.frame, Ftable))
}

# testing the function
augF <- add_F_cols(carton)
augF

# a function to find MS's for a specific effect
 find_MSs <- function(target_effect, terms){
  if(which(terms == target_effect) == (length(terms)-1)){
    index <- c(length(terms)-1, length(terms))
  }
  else{
    splitted <- sapply(terms, strsplit, split="[.]")
    index <- c()
    for (i in 1:length(splitted)){
      if(prod(strsplit(target_effect, split="[.]")[[1]] %in% splitted[[i]]) == 1){
        index <- c(index, i)
      }
    }
  }
  return(index)
}

# a function to estimate the variance of a target 'random' effect
# the structure of this function is very similar to approx_f
# this function makes use of find_MSs function
random_effect_var_estimate <- function(target_effect, data.frame, obs.num){
  terms <- rownames(data.frame)
  N <- nrow(data.frame)

  if(target_effect == 'error'){
    # error variance requires no calculation
    var_est <- data.frame$MS[terms == target_effect]
    df_var_est <- data.frame$DF[terms == target_effect]
  }
  else{
    length_of_target <- length(strsplit(target_effect, split="[.]")[[1]])
    num_of_terms <- term_lengths(terms)
    main_effects <- terms[which(num_of_terms[-N] == 1)]

    MS <- terms[find_MSs(target_effect, terms)]
    # estimate variances of the main effects
    if(length_of_target == 1){
      weight_var <- c(1, -1, -1, 1)
      var_est <- sum(weight_var*data.frame$MS[terms %in% MS])/(obs.num*prod(data.frame$DF[terms %in% main_effects[which(main_effects != target_effect)]]+1))
    }
    # two-way interaction effects
    else if(length_of_target == 2){
      weight_var <- c(1, -1)
      var_est <- sum(weight_var*data.frame$MS[terms %in% MS]) / (obs.num*(data.frame$DF[terms == main_effects[which(!(main_effects %in% strsplit(target_effect, split="[.]")[[1]]))]]+1))
    }
    # three-way interaction effect
    else{
      weight_var <- c(1, -1)
      var_est <- sum(weight_var*data.frame$MS[terms %in% MS])/obs.num
    }
    # degree of freedom is calculated in the same way for all three effects.
    df_var_est <- satterthwaite_df(c(weight_var*data.frame$MS[terms %in% MS]), c(data.frame$DF[terms %in% MS]))
  }
  return(data.frame(var.est = var_est, var.est.df = df_var_est))
}

# testing the function
# it seems like the book has a typo...(p.266 Exampl 11.3: DF of the estimated variance for 'm.g' is 2.80)
random_effect_var_estimate('m.o', carton, 2)

# a function to bind hte columns of variance estimate related values
add_var_cols <- function(data.frame)
{
  # needs to be changed
  obs.num <- 2
  N <- nrow(data.frame)
  terms <- row.names(data.frame)
  var_cols <- data.frame(var.est = NULL, var.est.df = NULL)
  for(i in 1:N){
    var_cols <- rbind(var_cols, random_effect_var_estimate(terms[i], data.frame, obs.num))
  }
  return(cbind(data.frame, var_cols))
}

# testing the function
augvar <- add_var_cols(augF)
augvar

# Expected mean square calculation with variance estimates
# Just calculating the previous MS values, based on var.est values.
calc_ems <- function(target_effect, data.frame, obs.num){
  N <- nrow(data.frame)
  ems_error <- data.frame$var.est[N]
  length_of_target <- length(strsplit(target_effect, split="[.]")[[1]])

  # a function to calculate the EMS's of two-way interaction effects(the portion excluding ems_error and ems_three_way_interaction effect)
  tw_ems_estimation <- function(target_effect, data.frame, obs.num){
    # number of observation x 'n' of the main effect that is not in the target effect x variance estimate of the target effect
    terms <- rownames(data.frame)
    num_of_terms <- term_lengths(terms)
    target_effect_split <- strsplit(target_effect, split="[.]")[[1]]
    main_effects <- terms[which(num_of_terms[-nrow(data.frame)] == 1)]
    tw_ems <- obs.num*(data.frame$DF[which(terms %in% main_effects[!main_effects %in% target_effect_split])]+1)*data.frame$var.est[terms == target_effect]
    return(tw_ems)
  }

  # a function to calculate the EMS's of the main effect(the portion excluding ems_error and ems_three_way_interaction effect; uses tw_ems_estimation)
  thw_ems_estimation <- function(target_effect, data.frame, obs.num){
    # think of the case of \alpha
    # this portion should include "obs.num x (DF_{\beta} + 1) x var_est_{\alpha, \gamma} + obs.num x (DF_{\gamma} + 1) x var_est_{\alpha, \beta}
    # + obs.num x (DF_{beta} + 1) x (DF_{gamma} + 1) x var_est_{\alpha}
    terms <- rownames(data.frame)
    num_of_terms <- term_lengths(terms)
    main_effects <- terms[which(num_of_terms[-nrow(data.frame)] == 1)]

    # we will find what the two_way interaction var_ests are first.
    # the two_way interactions should include the target_effect
    # obs.num x (DF_{\beta} + 1) x var_est_{\alpha, \gamma} + obs.num x (DF_{\gamma} + 1) x var_est_{\alpha, \beta}
    index <- c()
    for (i in 1:nrow(data.frame)){
      if (num_of_terms[i] == 2){
        if (target_effect %in% strsplit(terms[i], split='[.]')[[1]])
        {
          index <- c(index, i)
        }
      }
    }

    # then we need to multiply them one by one
    thw_ems <- 0
    for (i in 1:length(index)){
      thw_ems <- thw_ems + tw_ems_estimation(terms[index[i]], data.frame, obs.num)
    }

    # + obs.num x (DF_{beta} + 1) x (DF_{gamma} + 1) x var_est_{\alpha}
    thw_ems <- thw_ems + obs.num*prod(data.frame$DF[which(terms %in% main_effects[!main_effects %in% target_effect])]+1)*data.frame$var.est[terms == target_effect]
    return(thw_ems)
  }

  if(target_effect %in% c('error', 'Error', 'residual', 'Residual')){
    ems <- ems_error
  }
  else if(length_of_target == 3){
    ems <- ems_error + obs.num*data.frame$var.est[(N-1)]
  }
  else if(length_of_target == 2){
    ems <- ems_error + obs.num*data.frame$var.est[(N-1)] + tw_ems_estimation(target_effect, data.frame, obs.num)
  }
  else{
    ems <- ems_error + obs.num*data.frame$var.est[(N-1)] + thw_ems_estimation(target_effect, data.frame, obs.num)
  }
  return(ems)
}

# Test if the calculated values are valid
# The function calc_ems seems to work fairly well!
recalculated <- plyr::ldply(rownames(augvar), calc_ems, data.frame=augvar, obs.num=2)
cbind(carton$MS, recalculated)
