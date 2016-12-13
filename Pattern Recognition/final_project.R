set.seed(01131996)
setwd("/Users/jackliang 1/Desktop/School Work/Pattern Recognition/Final Project")
load('digits.RData')


#windows()
#image(t(1 - training.data[4,73,,])[,20:1], col=gray(seq(0, 1, length.out=256)), axes=FALSE, asp=1)
#image(t(1 - training.data[9,404,,])[,20:1], col=gray(seq(0, 1, length.out=256)), axes=FALSE, asp=1)


##tuning lambda
nums = 1:500
lambdas = c(1:100)/200
all_errors = rep(0,length(lambdas))

for(count in 1:length(lambdas)){
  lambda = lambdas[count]
  error_total = 0
  for(rep in 1:5){
    n = 400
    Sigmahat = mat.or.vec(400,400)
    samples= sample.int(500, 400)
    mu_0 = (colMeans(training.data[1,samples,,]))
    mu_1 = (colMeans(training.data[2,samples,,]))
    mu_2 = (colMeans(training.data[3,samples,,]))
    mu_3 = (colMeans(training.data[4,samples,,]))
    mu_4 = (colMeans(training.data[5,samples,,]))
    mu_5 = (colMeans(training.data[6,samples,,]))
    mu_6 = (colMeans(training.data[7,samples,,]))
    mu_7 = (colMeans(training.data[8,samples,,]))
    mu_8 = (colMeans(training.data[9,samples,,]))
    mu_9 = (colMeans(training.data[10,samples,,]))
    
    for(i in samples){
      Sigmahat = Sigmahat + 1/(10*n) * ((as.vector(training.data[1, i, , ]) - as.vector(mu_0)) %*% t(as.vector(training.data[1, i, , ]) - as.vector(mu_0)) +
                                          (as.vector(training.data[2, i, , ]) - as.vector(mu_1)) %*% t(as.vector(training.data[2, i, , ]) - as.vector(mu_1)) +
                                          (as.vector(training.data[3, i, , ]) - as.vector(mu_2)) %*% t(as.vector(training.data[3, i, , ]) - as.vector(mu_2)) +
                                          (as.vector(training.data[4, i, , ]) - as.vector(mu_3)) %*% t(as.vector(training.data[4, i, , ]) - as.vector(mu_3)) +
                                          (as.vector(training.data[5, i, , ]) - as.vector(mu_4)) %*% t(as.vector(training.data[5, i, , ]) - as.vector(mu_4)) +
                                          (as.vector(training.data[6, i, , ]) - as.vector(mu_5)) %*% t(as.vector(training.data[6, i, , ]) - as.vector(mu_5)) +
                                          (as.vector(training.data[7, i, , ]) - as.vector(mu_6)) %*% t(as.vector(training.data[7, i, , ]) - as.vector(mu_6)) +
                                          (as.vector(training.data[8, i, , ]) - as.vector(mu_7)) %*% t(as.vector(training.data[8, i, , ]) - as.vector(mu_7)) +
                                          (as.vector(training.data[9, i, , ]) - as.vector(mu_8)) %*% t(as.vector(training.data[9, i, , ]) - as.vector(mu_8)) +
                                          (as.vector(training.data[10, i, , ]) - as.vector(mu_9)) %*% t(as.vector(training.data[10, i, , ]) - as.vector(mu_9))
                                        ) 
    }
    Sigmalambda = (1- lambda) * Sigmahat + lambda/4 *diag(400)
    Sigmainv = solve(Sigmalambda)
    #print(sum(Sigmahat))
    
    w = data.frame(Sigmainv %*% as.vector(mu_0), Sigmainv %*% as.vector(mu_1), Sigmainv %*% as.vector(mu_2),
                   Sigmainv %*% as.vector(mu_3), Sigmainv %*% as.vector(mu_4), Sigmainv %*% as.vector(mu_5),
                   Sigmainv %*% as.vector(mu_6), Sigmainv %*% as.vector(mu_7),
                   Sigmainv %*% as.vector(mu_8), Sigmainv %*% as.vector(mu_9))
    
    w0 = c(-1/2* t(as.vector(mu_0))%*%Sigmainv%*%as.vector(mu_0), 
           -1/2* t(as.vector(mu_1))%*%Sigmainv%*%as.vector(mu_1), 
           -1/2* t(as.vector(mu_2))%*%Sigmainv%*%as.vector(mu_2),
           -1/2* t(as.vector(mu_3))%*%Sigmainv%*%as.vector(mu_3),
           -1/2* t(as.vector(mu_4))%*%Sigmainv%*%as.vector(mu_4),
           -1/2* t(as.vector(mu_5))%*%Sigmainv%*%as.vector(mu_5),
           -1/2* t(as.vector(mu_6))%*%Sigmainv%*%as.vector(mu_6),
           -1/2* t(as.vector(mu_7))%*%Sigmainv%*%as.vector(mu_7),
           -1/2* t(as.vector(mu_8))%*%Sigmainv%*%as.vector(mu_8),
           -1/2* t(as.vector(mu_9))%*%Sigmainv%*%as.vector(mu_9))
    
    results = data.frame(rep(0,10), rep(0,10), rep(0,10), rep(0,10), rep(0,10),
                         rep(0,10), rep(0,10), rep(0,10), rep(0,10), rep(0,10))
    for(j in 1:10){
      for(i in nums[is.na(pmatch(nums, samples))]){
        #for(i in 1:1000){
        test = training.data[j,i,,]
        vals = c( unlist(w[1])%*% as.vector(test) + w0[1],
                  unlist(w[2])%*% as.vector(test) + w0[2], unlist(w[3])%*% as.vector(test) + w0[3],
                  unlist(w[4])%*% as.vector(test) + w0[4], unlist(w[5])%*% as.vector(test) + w0[5],
                  unlist(w[6])%*% as.vector(test) + w0[6], unlist(w[7])%*% as.vector(test) + w0[7],
                  unlist(w[8])%*% as.vector(test) + w0[8], unlist(w[9])%*% as.vector(test) + w0[9],
                  unlist(w[10])%*% as.vector(test) + w0[10])
        results[which.max(vals),j] = results[which.max(vals),j]+1}}
    
    error = rep(0,10)
    for(j in 1:10){
      error[j] = 100 - results[j,j]
    }
    
    #print(error)
    #print(sum(error))
    error_total = error_total + sum(error)
    
  }
  all_errors[count] = error_total
}








##Running code over the test data
##Jack Liang
n = 500 ##size of training data
ptm <- proc.time() ##timing the process
Sigmahat = mat.or.vec(400,400)##defining our matrix sigma

mu_0 = (colMeans(training.data[1,1:500,,]))##calculating the means
mu_1 = (colMeans(training.data[2,1:500,,]))##for each digit
mu_2 = (colMeans(training.data[3,1:500,,]))
mu_3 = (colMeans(training.data[4,1:500,,]))
mu_4 = (colMeans(training.data[5,1:500,,]))
mu_5 = (colMeans(training.data[6,1:500,,]))
mu_6 = (colMeans(training.data[7,1:500,,]))
mu_7 = (colMeans(training.data[8,1:500,,]))
mu_8 = (colMeans(training.data[9,1:500,,]))
mu_9 = (colMeans(training.data[10,1:500,,]))

##storing all the mu's together
mu = data.frame(as.vector(mu_0), as.vector(mu_1), as.vector(mu_2),
                as.vector(mu_3), as.vector(mu_4), as.vector(mu_5),
                as.vector(mu_6), as.vector(mu_7), as.vector(mu_8),
                as.vector(mu_9))

for(i in 1:500){
  c = 0
  ##calculating the actual value of sigmahat, as per the formula provided
  for(j in 1:10){
    ##make training data from true false to 1/0, 
    ##then stack the 20x20 matrix as a vector, then use matrix multiplication
    c = c+ as.matrix((as.vector(as.numeric(training.data[j, i, , ])) - (mu[j]))) %*%
              as.matrix(t(as.vector(as.numeric(training.data[j, i, , ])) - (mu[j])))
  }
  Sigmahat = Sigmahat + 1/(10*n) * c

}

##computing sigma hat lambda
##using the given perturbation, lambda was calculated previously
lambda = .11
Sigmalambda = (1- lambda) * Sigmahat + lambda/4 *diag(400)
Sigmainv = solve(Sigmalambda)

##defining our classifier paramters w, w0
w = data.frame(Sigmainv %*% as.vector(mu_0), Sigmainv %*% as.vector(mu_1), 
               Sigmainv %*% as.vector(mu_2), Sigmainv %*% as.vector(mu_3), 
               Sigmainv %*% as.vector(mu_4), Sigmainv %*% as.vector(mu_5),
               Sigmainv %*% as.vector(mu_6), Sigmainv %*% as.vector(mu_7),
               Sigmainv %*% as.vector(mu_8), Sigmainv %*% as.vector(mu_9))

w0 =  c(-1/2* t(as.vector(mu_0))%*%Sigmainv%*%as.vector(mu_0), 
       -1/2* t(as.vector(mu_1))%*%Sigmainv%*%as.vector(mu_1), 
       -1/2* t(as.vector(mu_2))%*%Sigmainv%*%as.vector(mu_2),
       -1/2* t(as.vector(mu_3))%*%Sigmainv%*%as.vector(mu_3),
       -1/2* t(as.vector(mu_4))%*%Sigmainv%*%as.vector(mu_4),
       -1/2* t(as.vector(mu_5))%*%Sigmainv%*%as.vector(mu_5),
       -1/2* t(as.vector(mu_6))%*%Sigmainv%*%as.vector(mu_6),
       -1/2* t(as.vector(mu_7))%*%Sigmainv%*%as.vector(mu_7),
       -1/2* t(as.vector(mu_8))%*%Sigmainv%*%as.vector(mu_8),
       -1/2* t(as.vector(mu_9))%*%Sigmainv%*%as.vector(mu_9))

##creating a vector of where each digit was classified
##this matrix is 10x10, with (i,j) componenet
##defined as how many digit i's were classified as j's
##the error is the sum of the off-diagonal terms. 
results = data.frame(rep(0,10), rep(0,10), rep(0,10), rep(0,10), rep(0,10),
                     rep(0,10), rep(0,10), rep(0,10), rep(0,10), rep(0,10))
for(j in 1:10){
  for(i in 1:1000){
    test = test.data[j,i,,]
    
    ##computing the values of the classifier for a given test digit
    vals = c( unlist(w[1])%*% as.vector(test) + w0[1],
              unlist(w[2])%*% as.vector(test) + w0[2], 
              unlist(w[3])%*% as.vector(test) + w0[3],
              unlist(w[4])%*% as.vector(test) + w0[4], 
              unlist(w[5])%*% as.vector(test) + w0[5],
              unlist(w[6])%*% as.vector(test) + w0[6], 
              unlist(w[7])%*% as.vector(test) + w0[7],
              unlist(w[8])%*% as.vector(test) + w0[8], 
              unlist(w[9])%*% as.vector(test) + w0[9],
              unlist(w[10])%*% as.vector(test) + w0[10])
    ##adding 1 to where the digit was classified
    results[which.max(vals),j] = results[which.max(vals),j]+1}}

##computing the error for each digit
error = rep(0,10)
for(j in 1:10){
  error[j] = 1000 - results[j,j]
}


print(error)##computing error rates
print(sum(error)/10000)

proc.time() - ptm ##stoping the clock





##EM part
set.seed(01131996) ##setting a seed, for consistency
ptm <- proc.time() ##timing
M = 4 ##fixing a value of M
      ##note that for different values of M, we also have to change
      ##a few things, such as the number of ells

##we actually compile all the data into one large list of matrices
##but we end up only needing one matrix, for one particular digit
all_dat0 = mat.or.vec(M, 500)
all_dat1 = mat.or.vec(M, 500)
all_dat2 = mat.or.vec(M, 500)
all_dat3 = mat.or.vec(M, 500)
all_dat4 = mat.or.vec(M, 500)
all_dat5 = mat.or.vec(M, 500)
all_dat6 = mat.or.vec(M, 500)
all_dat7 = mat.or.vec(M, 500)
all_dat8 = mat.or.vec(M, 500)
all_dat9 = mat.or.vec(M, 500)

all_dat = list(all_dat0, all_dat1, all_dat2, all_dat3, all_dat4, 
               all_dat5, all_dat6, all_dat7, all_dat8, all_dat9)

##randomly assigning the training data points
for(i in 1:500){
  rand = sample(1:M, 10, replace = TRUE)
  for(j in 1:10){
    all_dat[[j]][rand[j],i] = 1 
  }}

##calculating pi for our random assignment
##pi is acutally a matrix, but again for one digit we only need one column
pi = mat.or.vec(M, 10)
for(i in 1:M){
  for(j in 1:10){
    pi[i,j] = sum(all_dat[[j]][i,])/500
  }
}

##setting up our matrix mu
##with rows as categories
mu3 = mat.or.vec(M, 400)
for(i in 1:M){
  total = mat.or.vec(400,1)
  for(j in 1:500){
    if(all_dat[[4]][i,j] ==1){
     total = total + training.data[4,j,,]
  }}
  mu3[i,] = total/sum(all_dat[[4]][i,])
}

pi3 = as.vector(pi[,4])

##looping through the EM algorithm
for(iterations in 1:5){ ## the number of iterations is arbitrary
                         ## but in general convergence happens very fast
                         ## so we only need about 10 iterations
  
  ##defining l_m(x) as in the assignment
  ell1 = mat.or.vec(1,500)
  ell2 = mat.or.vec(1,500)
  ell3 = mat.or.vec(1,500)
  ell4 = mat.or.vec(1,500)
  ellstar = mat.or.vec(1,500)
  
  ##computing l_m(x_i) for each training data point
  ##note that the log of the product is the sum of the logs
  ##doing it in this order helps prevent truncation at bad places
  for(k in 1:500){
    ell1[k] = sum(log(mu3[1,]^as.vector(training.data[4,k, ,]) * 
                        as.vector((1-mu3[1,])^
                                    (1- as.vector(training.data[4,k, ,])) )))
    ell2[k] = sum(log(mu3[2,]^as.vector(training.data[4,k, ,]) * 
                        as.vector((1-mu3[2,])^
                                    (1- as.vector(training.data[4,k, ,])) )))
    ell3[k] = sum(log(mu3[3,]^as.vector(training.data[4,k, ,]) * 
                        as.vector((1-mu3[3,])^
                                    (1- as.vector(training.data[4,k, ,])) )))
    ell4[k] = sum(log(mu3[4,]^as.vector(training.data[4,k, ,]) * 
                        as.vector((1-mu3[4,])^
                                    (1- as.vector(training.data[4,k, ,])) )))

    ##adding in the log(pi_m)
    ell1[k] = ell1[k] + log(pi3[1])
    ell2[k] = ell2[k] + log(pi3[2])
    ell3[k] = ell3[k] + log(pi3[3])
    ell4[k] = ell4[k] + log(pi3[4])

    ##computing the max of the ell_m(x_i)s for a fixed i
    ellstar[k] = max(ell1[k],ell2[k],ell3[k],ell4[k])
  }
  
  ##storing all the ells in one big list of vectors
  ell = list(ell1, ell2, ell3, ell4)
  
  ##computing the gammas
  gamma = mat.or.vec(M,500)
  for(i in 1:M){
    for(j in 1:500){
      ##first getting the denominator for a fixed i
      denom = exp(ell[[1]][j] - ellstar[j]) +
        exp(ell[[2]][j] - ellstar[j]) + exp(ell[[3]][j] - ellstar[j]) + 
        exp(ell[[4]][j] - ellstar[j])
      
      ##plugging in
      gamma[i,j] = exp(ell[[i]][j] - ellstar[j])/denom
    }
  }
  
  ##updating mu given the gammas
  mu3 = mat.or.vec(M, 400)
  for(i in 1:M){
    for(j in 1:400){
      for(k in 1:500){
        mu3[i,j] = mu3[i,j] + gamma[i,k] *
          as.numeric(as.vector(training.data[4,k,,])[j])
      }
      mu3[i,j] = (1+ mu3[i,j])/(2 + sum(gamma[i,]))
    }
  }
  
  ##updating pi given the gammas
  pi3 = c(1 + sum(gamma[1,]), 1 + sum(gamma[2,]),
          1 + sum(gamma[3,]), 1 + sum(gamma[4,])) * (1/(500 + M))
}





##EM part 2
set.seed(01131996) ##setting a seed, for consistency
M = 4 ##fixing a value of M
##note that for different values of M, we also have to change
##a few things, such as the number of ells

##we actually compile all the data into one large list of matrices
##but we end up only needing one matrix, for one particular digit
all_dat0 = mat.or.vec(M, 500)
all_dat1 = mat.or.vec(M, 500)
all_dat2 = mat.or.vec(M, 500)
all_dat3 = mat.or.vec(M, 500)
all_dat4 = mat.or.vec(M, 500)
all_dat5 = mat.or.vec(M, 500)
all_dat6 = mat.or.vec(M, 500)
all_dat7 = mat.or.vec(M, 500)
all_dat8 = mat.or.vec(M, 500)
all_dat9 = mat.or.vec(M, 500)

all_dat = list(all_dat0, all_dat1, all_dat2, all_dat3, all_dat4, 
               all_dat5, all_dat6, all_dat7, all_dat8, all_dat9)

##randomly assigning the training data points
for(i in 1:500){
  rand = sample(1:M, 10, replace = TRUE)
  for(j in 1:10){
    all_dat[[j]][rand[j],i] = 1 
  }}

##calculating pi for our random assignment
##pi is acutally a matrix, but again for one digit we only need one column
pi = mat.or.vec(M, 10)
for(i in 1:M){
  for(j in 1:10){
    pi[i,j] = sum(all_dat[[j]][i,])/500
  }
}

##setting up our matrix mu
##with rows as categories
mu4 = mat.or.vec(M, 400)
for(i in 1:M){
  total = mat.or.vec(400,1)
  for(j in 1:500){
    if(all_dat[[5]][i,j] ==1){
      total = total + training.data[5,j,,]
    }}
  mu4[i,] = total/sum(all_dat[[5]][i,])
}

pi4 = as.vector(pi[,5])

##looping through the EM algorithm
for(iterations in 1:5){ ## the number of iterations is arbitrary
  ## but in general convergence happens very fast
  ## so we only need about 10 iterations
  
  ##defining l_m(x) as in the assignment
  ell1 = mat.or.vec(1,500)
  ell2 = mat.or.vec(1,500)
  ell3 = mat.or.vec(1,500)
  ell4 = mat.or.vec(1,500)
  ellstar = mat.or.vec(1,500)
  
  ##computing l_m(x_i) for each training data point
  ##note that the log of the product is the sum of the logs
  ##doing it in this order helps prevent truncation at bad places
  for(k in 1:500){
    ell1[k] = sum(log(mu4[1,]^as.vector(training.data[5,k, ,]) * 
                        as.vector((1-mu4[1,])^
                                    (1- as.vector(training.data[5,k, ,])) )))
    ell2[k] = sum(log(mu4[2,]^as.vector(training.data[5,k, ,]) * 
                        as.vector((1-mu4[2,])^
                                    (1- as.vector(training.data[5,k, ,])) )))
    ell3[k] = sum(log(mu4[3,]^as.vector(training.data[5,k, ,]) * 
                        as.vector((1-mu4[3,])^
                                    (1- as.vector(training.data[5,k, ,])) )))
    ell4[k] = sum(log(mu4[4,]^as.vector(training.data[5,k, ,]) * 
                        as.vector((1-mu4[4,])^
                                    (1- as.vector(training.data[5,k, ,])) )))

    ##adding in the log(pi_m)
    ell1[k] = ell1[k] + log(pi4[1])
    ell2[k] = ell2[k] + log(pi4[2])
    ell3[k] = ell3[k] + log(pi4[3])
    ell4[k] = ell4[k] + log(pi4[4])

    ##computing the max of the ell_m(x_i)s for a fixed i
    ellstar[k] = max(ell1[k],ell2[k],ell3[k],ell4[k])
  }
  
  ##storing all the ells in one big list of vectors
  ell = list(ell1, ell2, ell3, ell4)
  
  ##computing the gammas
  gamma = mat.or.vec(M,500)
  for(i in 1:M){
    for(j in 1:500){
      ##first getting the denominator for a fixed i
      denom = exp(ell[[1]][j] - ellstar[j]) +
        exp(ell[[2]][j] - ellstar[j]) + exp(ell[[3]][j] - ellstar[j]) + 
        exp(ell[[4]][j] - ellstar[j])
      
      ##plugging in
      gamma[i,j] = exp(ell[[i]][j] - ellstar[j])/denom
    }
  }
  
  ##updating mu given the gammas
  mu4 = mat.or.vec(M, 400)
  for(i in 1:M){
    for(j in 1:400){
      for(k in 1:500){
        mu4[i,j] = mu4[i,j] + gamma[i,k] *
          as.numeric(as.vector(training.data[5,k,,])[j])
      }
      mu4[i,j] = (1+ mu4[i,j])/(2 + sum(gamma[i,]))
    }
  }
  
  ##updating pi given the gammas
  pi4 = c(1 + sum(gamma[1,]), 1 + sum(gamma[2,]),
          1 + sum(gamma[3,]), 1 + sum(gamma[4,]))* (1/ (500 + M))
}

proc.time() - ptm ##end timing

par(mfrow=c(4,2))
image(t(1-matrix(mu3[1,], 20, 20))[,20:1], 
      col=gray(seq(0, 1, length.out=256)), axes=FALSE, asp=1)
image(t(1-matrix(mu3[2,], 20, 20))[,20:1], 
      col=gray(seq(0, 1, length.out=256)), axes=FALSE, asp=1)
image(t(1-matrix(mu3[3,], 20, 20))[,20:1], 
      col=gray(seq(0, 1, length.out=256)), axes=FALSE, asp=1)
image(t(1-matrix(mu3[4,], 20, 20))[,20:1], 
      col=gray(seq(0, 1, length.out=256)), axes=FALSE, asp=1)
image(t(1-matrix(mu4[1,], 20, 20))[,20:1], 
      col=gray(seq(0, 1, length.out=256)), axes=FALSE, asp=1)
image(t(1-matrix(mu4[2,], 20, 20))[,20:1], 
      col=gray(seq(0, 1, length.out=256)), axes=FALSE, asp=1)
image(t(1-matrix(mu4[3,], 20, 20))[,20:1], 
      col=gray(seq(0, 1, length.out=256)), axes=FALSE, asp=1)
image(t(1-matrix(mu4[4,], 20, 20))[,20:1], 
      col=gray(seq(0, 1, length.out=256)), axes=FALSE, asp=1)



##last part
##we assume we already have mu3 and mu4, from prior code
##with M=4
vals = mat.or.vec(9,2000) ##calculating the gamma(z_{nm})
                          ##these vals are essentially the ells
for(i in 4:5){
  for(j in 1:1000){
    test = as.vector(test.data[i,j,,])
    a = 1000*(i-4) + j ##we index over the 2000 test samples, 1-1000 are
                       ##from digit class 3, 1001-2000 are 
                       ##from digit class 4
    vals[1,a] = sum(log(mu3[1,]^test * 
                          as.vector((1-mu3[1,])^
                                      (1- test))))
    vals[2,a] = sum(log(mu3[2,]^test * 
                          as.vector((1-mu3[2,])^
                                      (1- test))))
    vals[3,a] = sum(log(mu3[3,]^test * 
                          as.vector((1-mu3[3,])^
                                      (1- test))))
    vals[4,a] = sum(log(mu3[4,]^test * 
                          as.vector((1-mu3[4,])^
                                      (1- test))))
    vals[5,a] = sum(log(mu4[1,]^test * 
                          as.vector((1-mu4[1,])^
                                      (1- test))))
    vals[6,a] = sum(log(mu4[2,]^test * 
                          as.vector((1-mu4[2,])^
                                      (1- test))))
    vals[7,a] = sum(log(mu4[3,]^test * 
                          as.vector((1-mu4[3,])^
                                      (1- test))))
    vals[8,a] = sum(log(mu4[4,]^test * 
                          as.vector((1-mu4[4,])^
                                      (1- test))))
    
    vals[1,a] = vals[1,a] + log(pi3[1])
    vals[2,a] = vals[2,a] + log(pi3[2])
    vals[3,a] = vals[3,a] + log(pi3[3])
    vals[4,a] = vals[4,a] + log(pi3[4])
    vals[5,a] = vals[5,a] + log(pi4[1])
    vals[6,a] = vals[6,a] + log(pi4[2])
    vals[7,a] = vals[7,a] + log(pi4[3])
    vals[8,a] = vals[8,a] + log(pi4[4])
    
    ##calculating ellstar
    vals[9,a] = max(vals[1,a], vals[2,a], vals[3,a], vals[4,a],
                    vals[5,a], vals[6,a], vals[7,a], vals[8,a])
  }
}

##storing all the ells for each digit class
digit3 = list(vals[1,], vals[2,], vals[3,], vals[4,])
digit4 = list(vals[5,], vals[6,], vals[7,], vals[8,])

##creating two matrices of the gammas, one for each digit class
gamma3 = mat.or.vec(4,2000)
gamma4 = mat.or.vec(4,2000)
for(i in 1:4){
  for(j in 1:2000){
    ##first getting the denominator for a fixed i
    denom = exp(digit3[[1]][j] - vals[9,j]) +
      exp(digit3[[2]][j] - vals[9,j]) + exp(digit3[[3]][j] - vals[9,j]) +
      exp(digit3[[4]][j] - vals[9,j]) + exp(digit4[[1]][j] - vals[9,j]) +
      exp(digit4[[2]][j] - vals[9,j]) + exp(digit4[[3]][j] - vals[9,j]) +
      exp(digit4[[4]][j] - vals[9,j])
    
    ##plugging in
    gamma3[i,j] = exp(digit3[[i]][j] - vals[9,j])/denom
    gamma4[i,j] = exp(digit4[[i]][j] - vals[9,j])/denom
    
  }
}

##calculating error
error = 0
for(j in 1:2000){
  
  ##we misclassify if a digit in the 4 class has its highest gamma
  ##in one of the M classes associated with digit class 3
  if(max(gamma3[,j]) > max(gamma4[,j]) && j > 1000){
    error = error+1
    print(j)
  }
  
  ##and vice versa
  if(max(gamma3[,j]) < max(gamma4[,j]) && j < 1001){
    error = error+1
    print(j)
  }
}

##printing total error
print(error/2000)






















##EM part
set.seed(01131996) ##setting a seed, for consistency
ptm <- proc.time() ##timing
M = 2 ##fixing a value of M
##note that for different values of M, we also have to change
##a few things, such as the number of ells

##we actually compile all the data into one large list of matrices
##but we end up only needing one matrix, for one particular digit
all_dat0 = mat.or.vec(M, 500)
all_dat1 = mat.or.vec(M, 500)
all_dat2 = mat.or.vec(M, 500)
all_dat3 = mat.or.vec(M, 500)
all_dat4 = mat.or.vec(M, 500)
all_dat5 = mat.or.vec(M, 500)
all_dat6 = mat.or.vec(M, 500)
all_dat7 = mat.or.vec(M, 500)
all_dat8 = mat.or.vec(M, 500)
all_dat9 = mat.or.vec(M, 500)

all_dat = list(all_dat0, all_dat1, all_dat2, all_dat3, all_dat4, 
               all_dat5, all_dat6, all_dat7, all_dat8, all_dat9)

##randomly assigning the training data points
for(i in 1:500){
  rand = sample(1:M, 10, replace = TRUE)
  for(j in 1:10){
    all_dat[[j]][rand[j],i] = 1 
  }}

##calculating pi for our random assignment
##pi is acutally a matrix, but again for one digit we only need one column
pi = mat.or.vec(M, 10)
for(i in 1:M){
  for(j in 1:10){
    pi[i,j] = sum(all_dat[[j]][i,])/500
  }
}

##setting up our matrix mu
##with rows as categories
mu3 = mat.or.vec(M, 400)
for(i in 1:M){
  total = mat.or.vec(400,1)
  for(j in 1:500){
    if(all_dat[[4]][i,j] ==1){
      total = total + training.data[4,j,,]
    }}
  mu3[i,] = total/sum(all_dat[[4]][i,])
}

pi3 = as.vector(pi[,4])

##looping through the EM algorithm
for(iterations in 1:10){ ## the number of iterations is arbitrary
  ## but in general convergence happens very fast
  ## so we only need about 10 iterations
  
  ##defining l_m(x) as in the assignment
  ell1 = mat.or.vec(1,500)
  ell2 = mat.or.vec(1,500)
  ellstar = mat.or.vec(1,500)
  
  ##computing l_m(x_i) for each training data point
  ##note that the log of the product is the sum of the logs
  ##doing it in this order helps prevent truncation at bad places
  for(k in 1:500){
    ell1[k] = sum(log(mu3[1,]^as.vector(training.data[4,k, ,]) * 
                        as.vector((1-mu3[1,])^
                                    (1- as.vector(training.data[4,k, ,])) )))
    ell2[k] = sum(log(mu3[2,]^as.vector(training.data[4,k, ,]) * 
                        as.vector((1-mu3[2,])^
                                    (1- as.vector(training.data[4,k, ,])) )))

    ##adding in the log(pi_m)
    ell1[k] = ell1[k] + log(pi3[1])
    ell2[k] = ell2[k] + log(pi3[2])

    ##computing the max of the ell_m(x_i)s for a fixed i
    ellstar[k] = max(ell1[k],ell2[k])
  }
  
  ##storing all the ells in one big list of vectors
  ell = list(ell1, ell2)
  
  ##computing the gammas
  gamma = mat.or.vec(M,500)
  for(i in 1:M){
    for(j in 1:500){
      ##first getting the denominator for a fixed i
      denom = exp(ell[[1]][j] - ellstar[j]) +
        exp(ell[[2]][j] - ellstar[j])
      
      ##plugging in
      gamma[i,j] = exp(ell[[i]][j] - ellstar[j])/denom
    }
  }
  
  ##updating mu given the gammas
  mu3 = mat.or.vec(M, 400)
  for(i in 1:M){
    for(j in 1:400){
      for(k in 1:500){
        mu3[i,j] = mu3[i,j] + gamma[i,k] *
          as.numeric(as.vector(training.data[4,k,,])[j])
      }
      mu3[i,j] = (1+ mu3[i,j])/(2 + sum(gamma[i,]))
    }
  }
  
  ##updating pi given the gammas
  pi3 = c(1 + sum(gamma[1,]), 1 + sum(gamma[2,])) * (1/ 500 + M)
  
  ##computing log-likelihood
  ##as per the formula
  loglik = 0
  for(i in 1:5){
    for(j in 1:500){
      test = as.vector(training.data[4,j,,])
      if(which.max(gamma[,j]) == i){ ##only adding 1 if z_{nm} = 1
                                     ## i.e., when we classify sample n in
                                     ##caategory m
        loglik = loglik + log(pi3[i])
        for(k in 1:400){
          const =  as.numeric(test[k]) * log(mu3[i,k]) +
            (1-as.numeric(test[k])) * (log(1- mu3[i,k]))
          if(is.finite(const)){ ##we get odd errors, where sometimes
                                ##we get a log lik that is -infinity
                                ##this gets rid of these cases
            loglik = loglik+ const
          }
      }
    }
    }}
  ##displaying the log likelihood
  print(loglik)
  
}

##displaying the images
par(mfrow=c(3,2))
image(t(1-matrix(mu3[1,], 20, 20))[,20:1], 
      col=gray(seq(0, 1, length.out=256)), axes=FALSE, asp=1)
image(t(1-matrix(mu3[2,], 20, 20))[,20:1], 
      col=gray(seq(0, 1, length.out=256)), axes=FALSE, asp=1)
proc.time() - ptm ##end timing




