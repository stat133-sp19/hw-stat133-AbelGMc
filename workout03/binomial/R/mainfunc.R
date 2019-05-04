library(roxygen2)
library(ggplot2)
#' @title choose function
#' @description calculate n!/(k!(n-k)!)
#' @param n positive integer, n for trials
#' @param k integer, k for success, 0<=k<=n
#' @return how many cases of choosing such success from given trials
#' @export
#' @examples
#' bin_choose(n=5,k=2)
#' bin_choose(5,0)
#' bin_choose(5,1:3)
bin_choose <- function(n,k)
{
  if(sum(k>n)==length(sum)){stop('k cannot be greater than n')}
  return(factorial(n)/(factorial(k)*factorial(n-k)))
}
#' @title Binomial Probability
#' @description calculate Binomial Probability
#' @param success number of success(may be a vector)
#' @param trials  number of trials
#' @param prob Probablility that a success occurs
#' @return conresponding probability
#' @export
#' @examples
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#' bin_probability(success = 0:2, trials = 5, prob = 0.5)
#' bin_probability(success = 55, trials = 100, prob = 0.45)
bin_probability<-function(success,trials,prob)
{
  check_trials(trials)
  check_prob(prob)
  check_success(success, trials)
  return(bin_choose(trials,success)*prob^success*(1-prob)^(trials-success))
}

#' @title Binomial Distribution
#' @description calculate Binomial distribution
#' @param trials number of trials
#' @param prob Probability that a success occurs
#' @return data frame with: success; probability distribution
#' @export
#' @examples
#' bin_distribution(trials = 5, prob = 0.5)
bin_distribution<-function(trials,prob)
{
  bindis<-data.frame(success=0:trials,probability=bin_probability(0:trials,trials,prob))
  class(bindis)=c('bindis','data.frame')
  return(bindis)
}

#plot binomial distribution
#' @export
plot.bindis<-function(bindis)
{
  ggplot(bindis,aes(x=success,y=probability))+
    geom_col()+labs(x='successes',y='probability')
}

#' @title Binomial Cumulative function
#' @description calculate binomial probability and cumulative function
#' @param trials number of trials
#' @param prob Probability that a success occurs
#' @return data frame with : success;probability distribution;cumulative probabilities
#' @export
#' @examples
#' bin_cumulative(trials = 5, prob = 0.5)
bin_cumulative <- function(trials, prob)
{
  bindis<-data.frame(success=0:trials,probability=bin_probability(0:trials,trials,prob),
                     cumulative=cumsum(bin_probability(0:trials,trials,prob)))
  class(bindis)=c("bincum",'data.frame')
  return(bindis)
}


#plot bincum
#' @export
plot.bincum<-function(bincum)
{
  ggplot(bincum,aes(x=success,y=cumulative))+
    geom_line()+
    geom_point()+labs(x='successes',y='probability')
}

#' @title Binomial Variable
#' @description  generate a binomial variable
#' @param trials number of trials
#' @param prob Probability that a success occurs
#' @return a list of binomial variable with trials and probability of success
#' @export
#' @examples
#' bin_variable(trials=5,prob=0.5)
bin_variable<-function(trials,prob)
{
  check_prob(prob)
  check_trials(trials)
  binvar<-list(trials=trials,prob=prob)
  class(binvar)=c('binvar','data.frame')
  return(binvar)
}

#print class binvar
#' @export
print.binvar<-function(binvar)
{
  if(!'binvar'%in%class(binvar)){stop('must be class binvar')}
  cat("\"Binomial Variable\"\n\n'Parameter\n")
  cat(paste('- number of trials:',binvar[[1]],'\n'))
  cat(paste('- prob of success :',binvar[[2]]))
}

#Summary class binvar
#' @export
summary.binvar<-function(binvar)
{
  if(!'binvar'%in%class(binvar)){stop('must be class binvar')}
  ls<-list(
    trials=binvar[[1]],
    prob=binvar[[2]],
    mean=aux_mean(binvar[[1]],binvar[[2]]),
    variance=aux_variance(binvar[[1]],binvar[[2]]),
    mode=aux_mode(binvar[[1]],binvar[[2]]),
    skewness=aux_skewness(binvar[[1]],binvar[[2]]),
    kurtosis=aux_kurtosis(binvar[[1]],binvar[[2]])
  )
  class(ls)<-c('summary.binvar','data.frame')
  return(ls)
}

# print for summary.binvar
#' @export
print.summary.binvar<-function(binvar)
{
  cat("\"Summary Binomial\"\n\n")
  cat('Parameter\n')
  cat(paste('- number of trials:',binvar$trials),'\n')
  cat(paste('- prob of success :',binvar$prob),'\n\n')
  cat('Measure\n')
  cat(paste('- mean    :',binvar$mean,'\n'))
  cat(paste('- variance:',binvar$variance,'\n'))
  cat(paste('- mode    :',binvar$mode,'\n'))
  cat(paste('- skewness:',binvar$skewness,'\n'))
  cat(paste('- kurtosis:',binvar$kurtosis))
}

#' @title Binomial Mean
#' @description calulate mean of the binomial variable
#' @param trials number of trials
#' @param prob Probability that a success occurs
#' @return mean of the binomial variable
#' @export
#' @examples
#' bin_mean(10, 0.3)
bin_mean<-function(trials,prob)
{
  check_trials(trials)
  check_prob(prob)
  return(aux_mean(trials,prob))
}


#' @title Binomial Variance
#' @description calculate the variance of the binomial variable
#' @param trials number of trials
#' @param prob Probability that a success occurs
#' @return variance of the binomial bino variable
#' @export
#' @examples
#' bin_variance(10, 0.3)
bin_variance<-function(trials,prob)
{
  check_trials(trials)
  check_prob(prob)
  return(aux_variance(trials,prob))
}

#' @title Binomial Mode
#' @description calculate the mode of the binomial variable
#' @param trials number of trials
#' @param prob Probability that a success occurs
#' @return mode of Binomial variable
#' @export
#' @examples
#' bin_mode(10, 0.3)
bin_mode<-function(trials,prob)
{
  check_trials(trials)
  check_prob(prob)
  return(aux_mode(trials,prob))
}

#' @title Binomial Skewness
#' @description calculate the skewness of binomial variable
#' @param trials number of trials
#' @param prob Probability that a success occurs
#' @return skewness of binomial variable
#' @export
#' @examples
#' bin_skewness(10, 0.3)
bin_skewness<-function(trials,prob)
{
  check_trials(trials)
  check_prob(prob)
  return(aux_skewness(trials,prob))
}

#' @title Binomial Kurtosis
#' @description calculate the kurtosis of binomial variable
#' @param trials number of trials
#' @param prob Probability that a success occurs
#' @return Kurtosis of binomial variable
#' @export
#' @examples
#' bin_kurtosis(10, 0.3)
bin_kurtosis<-function(trials,prob)
{
  check_trials(trials)
  check_prob(prob)
  return(aux_kurtosis(trials,prob))
}
