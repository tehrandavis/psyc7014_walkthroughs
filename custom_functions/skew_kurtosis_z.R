# Skewness and kurtosis and their standard errors as determined 
# using boostrapped confidence intervals

# Arguments:
#   - x: vector of data you wish to analyze
#   - calc_method: method of calculating skew and kurtosis, used by DescTools, defaults to "2"
#   - calc_ci: how to derive conf. intervals, for DescTools, default "bca"
#   - reps: How many bootstrap repetitions, recommend at least 1000

# Result: a 2x2 matrix with standardized skew and kurtosis (z) 
# as well as critical values to compare against. 

# Values as recommended by Kim (2013) "Statistical notes for clinical 
# researchers: assessing normal distribution using skewness and kurtosis."


skew_kurtosis_z <- function(x, calc_method = 2, calc_ci = "bca", reps = 1000){
  
  # get skew and kurt and their cis 
  skew_ci <- DescTools::Skew(x, method = calc_method, ci.type = calc_ci, 
                             conf.level = 0.95, R = reps)
  kurt_ci <- DescTools::Kurt(x, method = calc_method, ci.type = calc_ci, 
                             conf.level = 0.95, R = reps)
  
  # calculate ses from ci
  skew_ses <- (skew_ci[3] - skew_ci[2])/3.92 # range of 95% CI
  kurt_ses <- (kurt_ci[3] - kurt_ci[2])/3.92 # range of 95% CI
  
  # calculate standardized values
  skew_z<- psych::skew(x)/skew_ses 
  kurt_z<- psych::kurtosi(x)/kurt_ses
  
  # remove names and ci columns
  #skew_z <- psych::skew(x)
  #kurt_z <- psych::kurtosi()
  
  values <- data.frame("values" = rbind(skew_z,kurt_z))
  
  # what are the critical values?
  N <- length(x)
  crit_vals<- ifelse(N<50, 1.96,
                     ifelse(N<300, 3.29)
  )
  
  round(cbind(values,crit_vals),digits = 2) # round to 2 digits
  
}