cellwise_matrix_sol <- function(v, nlayers = 100, variables = 3) {
  X <- cbind(1, matrix(v[-c(1:nlayers)], ncol = variables))
  solve(t(X) %*% X) %*% t(X) %*% v[1:nlayers]
}