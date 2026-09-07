# APM1110---FA
```{r}
# Problem 5: compute overall percentage of relevant images
supply_percent <- c(15, 20, 25, 40)      # sensor supply percentages
relevance_percent <- c(50, 60, 80, 85)   # relevance percentages per sensor

# Convert to proportions and compute weighted average
P_S <- supply_percent / 100
P_R_given_S <- relevance_percent / 100
P_R <- sum(P_R_given_S * P_S)

# Print numeric results
print(P_R)               
cat(sprintf("%.1f%%\n", 100 * P_R))  

```
```{r}

# Problem 6: two fair coin tosses
outcomes <- c("HH", "HT", "TH", "TT")
p_outcome <- rep(1/4, length(outcomes))  # fair coin, each outcome 1/4

# Define events as logical membership vectors
E1 <- outcomes %in% c("HH", "TT")  # both tosses same
E2 <- outcomes %in% c("HH", "HT")  # first toss is head
E3 <- outcomes %in% c("HH", "TH")  # second toss is head

# Helper to compute probability of an event
prob <- function(event_logical) {
  if (!is.logical(event_logical) || length(event_logical) != length(p_outcome)) {
    stop("Event must be a logical vector matching the outcomes length.")
  }
  sum(p_outcome[event_logical])
}

# Single-event probabilities
P_E1 <- prob(E1)
P_E2 <- prob(E2)
P_E3 <- prob(E3)

# Pairwise intersections
P_E1E2 <- prob(E1 & E2)
P_E1E3 <- prob(E1 & E3)
P_E2E3 <- prob(E2 & E3)

# Triple intersection
P_E1E2E3 <- prob(E1 & E2 & E3)

# Checks for independence (numeric comparisons)
pairwise_checks <- c(
  E1_E2 = (abs(P_E1E2 - P_E1 * P_E2) < 1e-12),
  E1_E3 = (abs(P_E1E3 - P_E1 * P_E3) < 1e-12),
  E2_E3 = (abs(P_E2E3 - P_E2 * P_E3) < 1e-12)
)
mutual_check <- (abs(P_E1E2E3 - P_E1 * P_E2 * P_E3) < 1e-12)

# Return numeric results and logical checks 
list(
  P_E1 = P_E1,
  P_E2 = P_E2,
  P_E3 = P_E3,
  P_E1E2 = P_E1E2,
  P_E1E3 = P_E1E3,
  P_E2E3 = P_E2E3,
  P_E1E2E3 = P_E1E2E3,
  pairwise_checks = pairwise_checks,
  mutual_check = mutual_check
)
```
