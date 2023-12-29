### R Script
### GRAD C23: Mathematics for Data Science
### Problem Set 1
### Hertie School Fall 2023
### Prof. Magazinnik
# Giorgio Coppola, Group 3.
# 
# 
### Question 1
#
# a.
p_unique <- 1
for(i in 0:59) {
  p_unique <- p_unique * (600 - i) / 600
}
round(p_unique, 4) # = 0.0472

# b.
result_b <- choose(600, 60)
result_b # = 2.774267e+83, approx 2.77e+83

# c.
result_c <- choose(400, 60) / choose(600, 60)
result_c # = 5.438e-12

# d.
result_d <- choose(400, 40) * choose(50, 5) * choose(150, 15)
result_d # = 6.779343e+81

# e.
result_e_1 <- factorial(9) * choose(600, 10)
result_e_1 # = 5.607472e+26

result_e_2 <- factorial(10) * choose(600, 10)
result_e_2 # = 5.607472e+27

# f.
result_f_1 <- choose(400, 5) * choose(200, 5)
result_f_1 # = 2.110132e+20

result_f_2 <- (factorial(5) * choose(400, 5) * factorial(5) * choose(200, 5)) / (factorial(9) * choose(600, 10))
round(result_f_2, 5) # = 0.00542

result_f_3 <- (factorial(5) * choose(400, 5) * factorial(6) * choose(200, 5)) / (factorial(10) * choose(600, 10))
round(result_f_3, 5) # = 0.00325

result_f_4 <- (factorial(5) * choose(400, 5) * factorial(5) * choose(200, 5)) / (factorial(9) * choose(600, 10))
round(result_f_4, 5) # = 0.00542

### Question 3
#
# b.
set.seed(123) # Setting a seed for reproducibility

simulate_monty_hall_revisited <- function(p, n_simulations = 10000) {
  success_count <- 0
  
  for (i in 1:n_simulations) {
    car_position <- sample(1:3, 1) # Simulating the position of the car
    
    # Simulate Monty's decision based on the coin flip
    monty_decision <- ifelse(runif(1) < p, "goat", "random")
    
    # If contestant chooses door 1
    if (monty_decision == "goat") {
      if (car_position == 1) {
        monty_opens <- sample(2:3, 1)
      } else if (car_position == 2) {
        monty_opens <- 3
      } else {
        monty_opens <- 2
      }
    } else { # Monty's decision is random
      if (car_position == 1) {
        monty_opens <- sample(2:3, 1)
      } else if (car_position == 2) {
        monty_opens <- sample(c(2, 3), 1)
      } else {
        monty_opens <- sample(c(2, 3), 1)
      }
    }
    
    # Check if switching to door 3 is a success
    if (monty_opens == 2 && car_position == 3) {
      success_count <- success_count + 1
    }
  }
  
  return(success_count / n_simulations)
}

# Testing the function
p_value <- 0.5
estimated_probability <- simulate_monty_hall_revisited(p_value)
formula_probability <- 1/2 + p_value * 1/6

list(estimated_probability = estimated_probability, formula_probability = formula_probability)


### Question 4
#
# a.
# /


# b.
check_PMF_validity <- function(p1, p2, p3, p4, p5, p6) {
  # Calculate the probabilities for X = 1 to X = 7
  p_values <- c(1 - p1,
                p1 * (1 - p2),
                p1 * p2 * (1 - p3),
                p1 * p2 * p3 * (1 - p4),
                p1 * p2 * p3 * p4 * (1 - p5),
                p1 * p2 * p3 * p4 * p5 * (1 - p6),
                p1 * p2 * p3 * p4 * p5 * p6)
  
  # Sum the probabilities
  total <- sum(p_values)
  
  # Check if they sum to 1 (accounting for potential floating point inaccuracies)
  is_valid <- abs(total - 1) < 1e-9 
  # abs compute the absolute value of x, and define if it is sufficiently closed to 1 (accounting for potential floating point inaccuracies)
  
  return(list(sum = total, is_valid = is_valid))
}

### Test the function: giving random values to probabilities
p1 <- runif(1) # generates n random numbers between 0 and 1 from a uniform distribution
p2 <- runif(1)
p3 <- runif(1)
p4 <- runif(1)
p5 <- runif(1)
p6 <- runif(1)

# Print the random probabilities
cat("p1:", p1, "\np2:", p2, "\np3:", p3, "\np4:", p4, "\np5:", p5, "\np6:", p6, "\n")
# p1: 0.8714131 
# p2: 0.2548088 
# p3: 0.4642005 
# p4: 0.5919258 
# p5: 0.9023716 
# p6: 0.3368228 

# Result:
check_PMF_validity(p1, p2, p3, p4, p5, p6)

# $sum
# [1] 1

# $is_valid
# [1] TRUE