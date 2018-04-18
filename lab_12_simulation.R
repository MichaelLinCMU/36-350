generate_data = function(n, p){
  covariates = matrix(rnorm(n*p,0,1), n,p)
  responses = vector("numeric", length = n)
  for (i in 1:n){
    responses[i] = rnorm(1,0,1)
  }
  return(list(covariates=covariates, responses=responses))
}

model_select = function(covariates, responses, cutoff){
  responses = responses[which(covariates <cutoff)]
  covariates = covariates[which(covariates <cutoff)]
  if(length(covariates)==0) return(vector("numeric", length = 0))
  return(summary(lm(responses ~ covariates))[4])
}
model_select(generate_data(5,5)$covariates, generate_data(5,5)$responses, .5)

run_simulation = function(n_trials, n, p, cutoff){
  for (i in 1:n_trials){
    hist(model_select(generate_data(n,p)$covariates, generate_data(n,p)$responses, cutoff, breaks = 5))
  }
}

run_simulation(3,3,5,.5)
