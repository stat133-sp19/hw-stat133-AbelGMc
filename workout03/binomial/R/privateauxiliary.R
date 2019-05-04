aux_mean <- function(trials,prob)
{
  return(trials*prob)
}
aux_variance <- function(trials,prob)
{
  return(trials*prob*(1-prob))
}
aux_mode <- function(trials,prob)
{
  if((trials%%2==1)&prob==0.5)
  {return(c(trials*prob+prob,trials*prob+prob-1))}
  return(floor(trials*prob+prob))
}
aux_skewness <- function(trials,prob)
{
  return((1-2*prob)/sqrt(trials*prob*(1-prob)))
}
aux_kurtosis <- function(trials,prob)
{
  return((1-6*prob*(1-prob))/(trials*prob*(1-prob)))
}

