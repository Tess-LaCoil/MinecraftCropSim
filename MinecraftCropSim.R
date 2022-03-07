#Simulates a bunch of plants in Minecraft to estimate crop yields, growth rates, survival
#Intended for use with the PwnPlantGrowth plugin
#Theresa O'Brien 2022

#Parameters
sample_size <- 10000 #How many plants
turns <- 200 #How many ticks to simulate for
prob_grow <- 0.2 #The probability that a plant will grow
prob_die <- 0.05 #The probability that a plant that hasn't grown will die
times_grow_to_full_size <- 4 #How many times a plant has to grow to reach full size


#Data structures
alive <- rep(TRUE, times=sample_size) #Tracks which plants are alive
size <- rep(1, times=sample_size) #Tracks the size
time_to_fully_grown <- rep(1, times=sample_size) #How many turns until a plant reaches maturity
size_rec <- data.frame(size) #Data frame to track the plants over time if desired

for(i in 1:turns){ #Iterates through the turns
  for(j in 1:sample_size){ #Iterates through the simulated plants
    
    #If the plant is still alive and not full size, check if it grows. 
    #If it doesn't grow, check if it dies
    
    if((alive[j] && (size[j] < times_grow_to_full_size))){ 
      grow <- rbinom(1, 1, prob_grow) #Pr(1) = prob_grow
      die <- rbinom(1, 1, prob_die) #Pr(1) = prob_die
      
      if(grow == 1){ #Does the plant grow
        size[j] <- size[j] + 1 #Increase the size
        time_to_fully_grown[j] <- time_to_fully_grown[j] +1
      } else { #Does the plant survive if it doesn't grow
        if(die == 1){
          alive[j] <- FALSE
          time_to_fully_grown[j] <- -1 #Flags plants that die so they can be discarded.
        } else {
          time_to_fully_grown[j] <- time_to_fully_grown[j] +1
        }
      }
    }
  }
  
  size_rec[,i+1] <- size #Add the updated size to the simulation record
}

#Construct data object with results
plants <- data.frame(size = size, 
                     time_to_fully_grown = time_to_fully_grown,
                     alive = alive)

fully_grown <- subset(plants, plants$size==4)
died <- subset(plants, alive==FALSE)

#Proportion of the plants which survived
sum(plants$alive)/sample_size

#Summary statistics for the time to maturity of fully-grown survivors
summary(fully_grown$time_to_fully_grown)

#Proportion of the plants that made it to maturity
nrow(fully_grown)/sample_size

