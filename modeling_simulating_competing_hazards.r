# The model function takes as input arguments
# (in the following order): time, state and parameters

cohort_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)))
  
  # The differential equations 
     dI <- -gamma * I - mu * I
     dR <- gamma * I 
     dM <- mu * I 
     
# Return the number of people in each compartment at each time step (in the same order as the input same variables)
  return(list(c(dI, dR, dM)))     
}

# Defining model input and timesteps

initial_state_values <- c(I = 1000000,
                          R = 0, 
                          M = 0)
parameters <- c(gamma = 0.1, 
                mu = 0.2)
times <- seq(from = 0, to = 4*7, by = 1)

# After 4 weeks, we expect more people/subjects to die rather than to recover because the mortality rate (0.2) is higher
#  than the recovery rate (0.1), so people move more quickly from I to M than from I to R

# Loading the deSolve package to use it to solve diferential equations:

library(deSolve)
library(reshape2)
library(ggplot2)

output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = cohort_model, 
                            parms = parameters))

# Plotting the output over time shows that ore people have died than recovered:

output_long <- melt(as.data.frame(output), id = "time")
ggplot(data = output_long, 
       aes(x = time, 
           y = value,
           colour = variable,
           group = variable)) +
  geom_line() +
  xlab("Time (days)") +
  ylab('number of people') + 
  labs(colour = "Compartment")

# 