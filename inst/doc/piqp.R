## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, echo = FALSE------------------------------------------------------
library(piqp)

## -----------------------------------------------------------------------------
P <- matrix(c(6, 0, 0, 4), nrow = 2)
c <- c(-1, -4)
A <- matrix(c(1, -2), nrow = 1)
b <- 1
G <- matrix(c(1, 2, -1, 0), nrow = 2)
h <- c(0.2, -1)
x_lb <- c(-1, -Inf)  ## 2 variables
x_ub <- c(1, Inf)    ## 2 variables

## -----------------------------------------------------------------------------
sol <- solve_piqp(P, c, A, b, G, h, x_lb = x_lb, x_ub = x_ub, backend = "auto")
cat(sprintf("(Solution status, description): = (%d, %s)\n",
            sol$status, sol$info$status_desc))
cat(sprintf("Objective: %f, solution: (x1, x2) = (%f, %f)\n", sol$info$primal_obj, sol$x[1], sol$x[2]))

## -----------------------------------------------------------------------------
status_description(sol$status)

## -----------------------------------------------------------------------------
model <- piqp(P, c, A, b, G, h, x_lb = x_lb, x_ub = x_ub)
sol2 <- model$solve()
identical(sol, sol2)

## -----------------------------------------------------------------------------
model$update(x_lb = c(0, 0))
sol3 <- model$solve()
cat(sprintf("(Solution status, description): = (%d, %s)\n",
            sol3$status, status_description(sol3$status)))

## ---- error = TRUE------------------------------------------------------------
## Try to give an inappropriate b value
model$update(b = c(5, 2))

## -----------------------------------------------------------------------------
model$get_dims()

## -----------------------------------------------------------------------------
sparse_sol <- solve_piqp(P, c, A, b, G, h, x_lb, x_ub, backend = "sparse")
str(sparse_sol)

## -----------------------------------------------------------------------------
P <- matrix(2 * c(3, 0, 0, 2), nrow = 2, ncol = 2)
c <- c(-1, -4)
A <- matrix(c(1, -2), ncol = 2)
b <- 0
x_lb <- rep(-1.0, 2)
x_ub <- rep(1.0, 2)
sol <- solve_piqp(P = P, c = c, A = A, b = b, x_lb = x_lb, x_ub = x_ub)
cat(sprintf("(Solution status, description): = (%d, %s)\n",
            sol$status, sol$info$status_desc))
cat(sprintf("Objective: %f, solution: (x1, x2) = (%f, %f)\n", sol$info$primal_obj, sol$x[1], sol$x[2]))

## -----------------------------------------------------------------------------
G <- diag(2)
h <- c(1, 1)
sol <- solve_piqp(P = P, c = c, A = A, b = b, G = G, h = h,
                  x_lb = c(-1, -1), x_ub = c(Inf, Inf))
cat(sprintf("(Solution status, description): = (%d, %s)\n",
            sol$status, sol$info$status_desc))
cat(sprintf("Objective: %f, solution: (x1, x2) = (%f, %f)\n", sol$info$primal_obj, sol$x[1], sol$x[2]))

## -----------------------------------------------------------------------------
G <- Matrix::Matrix(c(1, 0, -1, 0, 0, 1, 0, -1), byrow = TRUE,
                    nrow = 4, sparse = TRUE)
h <- rep(1, 4)

sol <- solve_piqp(A = A, b = b, c = c, P = P, G = G, h = h)
cat(sprintf("(Solution status, description): = (%d, %s)\n",
            sol$status, status_description(sol$status)))
cat(sprintf("Objective: %f, solution: (x1, x2) = (%f, %f)\n", sol$info$primal_obj, sol$x[1], sol$x[2]))

## -----------------------------------------------------------------------------
s <- solve_piqp(P = P, c = c, A = A, b = b, G = G, h = h,
          settings = list(max_iter = 3)) ## Reduced number of iterations
cat(sprintf("(Solution status, description): = (%d, %s)\n",
            s$status, s$info$status_desc))
cat(sprintf("Objective: %f, solution: (x1, x2) = (%f, %f)\n", s$info$primal_obj, s$x[1], s$x[2]))

