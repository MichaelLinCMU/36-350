generate_data = function(n, p){
  covariates = matrix(rnorm(n*p,0,1), n,p)
  responses = vector("numeric", length = n)
  for (i in 1:n){
    responses[i] = rnorm(1,0,1)
  }
  return(list(covariates, responses))
}
generate_data(5,5)
