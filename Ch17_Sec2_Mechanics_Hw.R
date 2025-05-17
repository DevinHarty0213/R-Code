# Chapter 17 Section 2 Mechanics
#
if(!require(lpSolve)) install.packages("lpSolve")
library(lpSolve)
# C Contract: Ex. 17.2 Mechanics 20 
# B Contract: Ex. 17.2 Mechanics 19 
# A Contract: Ex. 17.2 Mechanics 21 
# Supplement: Ex. 17.2 Applications 22 
#
# C Contract: Ex. 17.2 Mechanics 2 
CloseEnough <- function(v1, v2, places){
  m1 <- round(v1 * 10^places, 0) # binary would be better, but harder to understand
  m2 <- round(v2 * 10^places, 0)
  return(m1 == m2)
}
# ========================================================================
# C Contract: Ex. 17.2 Mechanics 20
# 20. Consider the following LP problem.
#
#     Minimize z = 12x1 + 10x2
#
# subject to
#     Constraint 1: 8x1 +  6x2 >= 70
#     Constraint 2: 4x1 + 10x2 >= 80
#     Constraint 3: x1, x2 >= 0
#
# where x1 and x2 represent the decision variables. Solve the LP
#  problem to answer the following questions.
#
# ________________________________________________________________________
# a. What are the values of x1 and x2 at the optimal solution?
#   x1 (3.929), x2 (6.4286) What is the minimum value of z? (111.4286)
# Objective coefficients
costs <- c(12, 10)

# Constraint matrix
constraints <- matrix(c(8, 6,
                        4, 10),
                      nrow = 2, byrow = TRUE)

# RHS
rhs <- c(70, 80)

# Directions
direction <- c(">=", ">=")

# Solve LP
solution_a <- lp("min", costs, constraints, direction, rhs)

# Output
cat("Part A:\n")
cat("x1 =", round(solution_a$solution[1], 4), "\n")
cat("x2 =", round(solution_a$solution[2], 4), "\n")
cat("Minimum z =", round(solution_a$objval, 4), "\n\n")

# ________________________________________________________________________
#  b. Identify the binding and nonbinding constraints and
#      report the surplus values, as applicable.
# Calculate actual LHS values for constraints
actuals <- constraints %*% solution_a$solution
surplus <- actuals - rhs

cat("Part B:\n")
for (i in 1:2) {
  status <- ifelse(abs(surplus[i]) < 1e-5, "Binding", "Non-binding")
  cat("Constraint", i, ":", status)
  if (status == "Non-binding") {
    cat(" (Surplus:", round(surplus[i], 4), ")")
  }
  cat("\n")
}
cat("\n")


# ________________________________________________________________________
#  c. Report the shadow price and the range of feasibility of
#      each binding constraint. Interpret the results.
# Install if needed
if (!require(lpSolveAPI)) install.packages("lpSolveAPI", dependencies = TRUE)

# Load it
library(lpSolveAPI)

# Create LP model with lpSolveAPI to access duals

lp_model <- make.lp(0, 2)
set.objfn(lp_model, costs)
add.constraint(lp_model, c(8, 6), ">=", 70)
add.constraint(lp_model, c(4, 10), ">=", 80)
set.bounds(lp_model, lower = c(0, 0))
lp.control(lp_model, sense = "min")
solve(lp_model)

# Get shadow prices and feasibility ranges
duals <- get.sensitivity.rhs(lp_model)
shadow_prices <- duals$duals
ranges_rhs <- duals$duals.from.to

cat("Part C:\n")
for (i in 1:2) {
  cat("Constraint", i, "\n")
  cat("  Shadow price:", round(shadow_prices[i], 4), "\n")
  cat("  Feasibility range: [", round(ranges_rhs[i,1], 4), ",", round(ranges_rhs[i,2], 4), "]\n")
}
cat("\n")


# ________________________________________________________________________
#  d. What is the range of optimality for the two objective
#      function coefficients? Interpret the results.
obj_sens <- get.sensitivity.obj(lp_model)

cat("Part D:\n")
cat("x1 coefficient range: [", round(obj_sens$objfrom[1], 4), ",", round(obj_sens$objtill[1], 4), "]\n")
cat("x2 coefficient range: [", round(obj_sens$objfrom[2], 4), ",", round(obj_sens$objtill[2], 4), "]\n")


# ========================================================================

# ========================================================================
# B Contract: Ex. 17.2 Mechanics 19 
# 19. Consider the following LP problem.
#
#      Minimize z = 9x1 + 6x2
#
# subject to
#
#      Constraint 1: 5x1 + 3x2 >= 30
#      Constraint 2: 2x1 + 5x2 >= 33
#      Constraint 3: x1, x2 >= 0
#
# where x1 and x2 represent the decision variables. Solve the LP
#  problem to answer the following questions.
#
# ________________________________________________________________________
# a. What are the values of x1 and x2 at the optimal solution?
#   What is the minimum value of z? 
# Objective function: Minimize z = 9x1 + 6x2
costs <- c(9, 6)

# Constraint matrix:
# Constraint 1: 5x1 + 3x2 >= 30
# Constraint 2: 2x1 + 5x2 >= 33
constraints <- matrix(c(5, 3,
                        2, 5),
                      nrow = 2, byrow = TRUE)

rhs <- c(30, 33)
directions <- c(">=", ">=")

# Solve LP
solution_a <- lp("min", costs, constraints, directions, rhs)

# Output results
cat("Part A:\n")
cat("x1 =", round(solution_a$solution[1], 4), "\n")
cat("x2 =", round(solution_a$solution[2], 4), "\n")
cat("Minimum z =", round(solution_a$objval, 4), "\n\n")

# ________________________________________________________________________
#  b. Identify the binding and nonbinding constraints and
#      report the surplus values, as applicable.
# Actual LHS values
actuals <- constraints %*% solution_a$solution
surplus <- actuals - rhs

cat("Part B:\n")
for (i in 1:2) {
  status <- ifelse(abs(surplus[i]) < 1e-5, "Binding", "Non-binding")
  cat("Constraint", i, ":", status)
  if (status == "Non-binding") {
    cat(" (Surplus:", round(surplus[i], 4), ")")
  }
  cat("\n")
}
cat("\n")


# ________________________________________________________________________
#  c. Report the shadow price and the range of feasibility of
#      each binding constraint. Interpret the results.
# Set up LP with lpSolveAPI
lp_model <- make.lp(0, 2)
set.objfn(lp_model, costs)
add.constraint(lp_model, c(5, 3), ">=", 30)
add.constraint(lp_model, c(2, 5), ">=", 33)
set.bounds(lp_model, lower = c(0, 0))
lp.control(lp_model, sense = "min")
solve(lp_model)

# Get shadow prices and feasibility ranges
duals <- get.sensitivity.rhs(lp_model)
shadow_prices <- duals$duals
ranges_rhs <- duals$duals.from.to

cat("Part C:\n")
for (i in 1:2) {
  cat("Constraint", i, "\n")
  cat("  Shadow Price:", round(shadow_prices[i], 4), "\n")
  cat("  Feasibility Range: [", round(ranges_rhs[i,1], 4), ",", round(ranges_rhs[i,2], 4), "]\n")
}
cat("\n")


# ________________________________________________________________________
#  d. What is the range of optimality for the two objective
#      function coefficients? Interpret the results.
obj_sens <- get.sensitivity.obj(lp_model)

cat("Part D:\n")
cat("x1 coefficient range: [", round(obj_sens$objfrom[1], 4), ",", round(obj_sens$objtill[1], 4), "]\n")
cat("x2 coefficient range: [", round(obj_sens$objfrom[2], 4), ",", round(obj_sens$objtill[2], 4), "]\n")
