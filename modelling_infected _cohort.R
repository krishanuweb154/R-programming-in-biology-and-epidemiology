
#Modeling an infected cohort


# loading libraries
library(deSolve)
library(reshape2)
library(ggplot2)

initial_number_infected <- 1000000
initial_number_recovered <- 0  

recovery_rate <- 1/10 
follow_up_duration <- 4*7 

# Combine into the model input vectors:
initial_state_values <- c(I = initial_number_infected, 
                          R = initial_number_recovered)
parameters <- c(gamma = recovery_rate) 
times <- seq(from = 0, to = follow_up_duration, by = 1) 


cohort_model <- function(time, state, parameters) {
  with(as.list(c(state, parameters)), {  
    # Differential equations
    dI <- -gamma * I
    dR <- gamma * I
    return(list(c(dI, dR)))
  })
}

# Solving the model equations using the ode() function in the deSolve package and saving them as a dataframe object:
output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = cohort_model,
                            parms = parameters))
# Printing the model output
output
 
#Based on output, how many people have recovered after 4 weeks
output[output$time == 28, c("time","R")] 

#What proportion of the total population does this correspond to?
output[output$time == 28,"R"]/(output[output$time == 28,"I"]+
                                 output[output$time == 28,"R"])

#Plotting the graph

output_long <- melt(as.data.frame(output), id = "time")      
ggplot(data = output_long, 
       aes(x = time, y = value, colour = variable, group = variable)) +  
  geom_line() + 
  xlab("Time (days)")+
  ylab("Number of people") +  
  labs(title = paste("Number infected and recovered over time when gamma =",
                     parameters["gamma"],"days^-1"))

#Based on the plot, at what timepoint were infected and recovered individuals equal in number 

output[output$time == 7,]

#  Varying $\gamma$
# Solving the model
output <- as.data.frame(ode(y = initial_state_values, 
                            times = times, 
                            func = cohort_model,
                            parms = parameters))

# Varying gamma

# Average duration of infection = 2 days so the recovery rate = 1/2 = 0.5 days
parameters <- c(gamma = 0.5)


#Solving the model 
output <- as.data.frame(ode(y = initial_state_values,
                            times = times,
                            func = cohort_model,
                            parms = parameters))

# Plotting the output
output_long <- melt(as.data.frame(output), id = "time")
# turn output dataset in long format

ggplot(data = output_long, 
       aes(x = time, 
           y = value,
           colour = variable,
           group =variable)) +
  geom_line() +
  xlab("Time (days)") +
  ylab("Numberof people") +
  labs(title = paste("Number infected and recovered over time when gamma =",
                     parameters["gamma"], "days^-1")) +
  scale_color_brewer(palette = "Set1")

# Average duration of infection = 20days so the recovery rate = 1/20 = 0.05days^-1

parameters <- c(gamma = 0.05)

# Solving the model
output <- as.data.frame(ode(y = initial_state_values,
                            times = times,
                            func = cohort_model,
                            parms = parameters))
# plotting the output

output_long <- melt(as.data.frame(output), id = "time")

ggplot(data = output_long,
       aes(x = time,
           y = value, 
           colour = variable,
           group = variable)) +
  geom_line() +
  xlab("Time (days)") +
  ylab("Number of people") +
  labs(title = paste("Number infected and recovered over time when gamma =", 
                     parameters["gamma"],"days^-1"))
