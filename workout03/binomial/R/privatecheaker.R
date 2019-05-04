#Private_1
check_prob <- function(prob)
{
  if(length(prob)>1)
  {stop('invalid prob value')}
  if(prob<=1&prob>=0)
  {
    return(TRUE)
  }
  else
  {
    stop('invalid prob value')
  }
}
#Private 2
check_trials <- function(trials)
{
  if(length(trials)>1)
  {stop('invalid trials value')}
  if(trials>=0&(round(trials)==trials))
  {
    return(TRUE)
  }
  else{stop('invalid trials value')}
}
#3:check_success
check_success <- function(success,trials)
{
  if(sum(success<=trials&success>=0)==length(success))
  {return(TRUE)}
  else{stop('invalid success value')}
}
