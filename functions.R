prior <- function(param,prior1,prior2,prior3){
  a = param[1]
  b = param[2]
  sd = param[3]

  if(prior1$dist=="normal"){
    aprior = dnorm(a, mean=prior1$mean,sd=prior1$sd, log = T)
  }
  if(prior1$dist=="uniform"){
    aprior = dunif(a, min=prior1$min,max=prior1$max, log = T)
  }
  if(prior2$dist=="normal"){
    bprior = dnorm(b, mean=prior2$mean,sd=prior2$sd, log = T)
  }
  if(prior2$dist=="uniform"){
    bprior = dunif(b, min=prior2$min,max=prior2$max, log = T)
  }

  if(prior3$dist=="normal"){
    sdprior = dnorm(sd, mean=prior3$mean,sd=prior3$sd, log = T)
  }
  if(prior3$dist=="uniform"){
    sdprior = dunif(sd, min=prior3$min,max=prior3$max, log = T)
  }

  return(aprior+bprior+sdprior)
}




likelihood <- function(param,x,y){
  a = param[1]
  b = param[2]
  sd = param[3]

  pred = a*x + b
  singlelikelihoods = dnorm(y, mean = pred, sd = sd, log = T)
  sumll = sum(singlelikelihoods)
  return(sumll)
}



posterior <- function(param,x,y,prior1,prior2,prior3){
  return (likelihood(param,x,y) + prior(param,prior1,prior2,prior3))
}

proposalfunction <- function(param){
  return(rnorm(3,mean = param, sd= c(0.1,0.5,0.3)))
}

run_metropolis_MCMC <- function(startvalue, iterations,x,y,prior1,prior2,prior3){
  chain = array(dim = c(iterations+1,3))
  chain[1,] = startvalue
  for (i in 1:iterations){
    proposal = proposalfunction(chain[i,])

    probab = exp(posterior(proposal,x,y,prior1,prior2,prior3) - posterior(chain[i,],x,y,prior1,prior2,prior3))
    if (runif(1) < probab){
      chain[i+1,] = proposal
    }else{
      chain[i+1,] = chain[i,]
    }
  }

  return(chain)
}
