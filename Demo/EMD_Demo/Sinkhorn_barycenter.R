#####EMD solvers
pkgs <- c("einsum")
invisible(lapply(pkgs, library, character.only = T))
options(matprod = "internal")
shannon_entropy <- function(vec){
  log_vec <- log(vec)
  log_vec[which(log_vec == -Inf)] <- 0
  return(-sum((vec*log_vec-1)))
}

geometricMean <- function(matrix){
  log_mat <- log(matrix)
  rowmeans <- rowMeans(log_mat)
  return(exp(rowmeans))
}
geometricBar <- function(weights, matrix){
  log_mat <- log(matrix)
  dot_prod <- log_mat %*% weights
  return(c(exp(dot_prod)))
}
sd_pop <- function(vec, ddof = 0){
     dev <- (vec-mean(vec))^2
     N <- length(vec)-ddof
     return(sqrt(sum(dev)/N))
 }
sinkhorn <- function(p,q,M, eps, numItermax = 1000, stopThr = 1e-22,
                     verbose = FALSE, log = FALSE, warn = TRUE, warmstart = FALSE){

  if(length(p) == 0){
    p <- rep(1/nrow(M), length = nrow(M))
  }
  if(length(q) == 0){
    q <- rep(1/ncol(M), length = ncol(M))
  }
  
  
  dim_p <- length(p)
  if(is.vector(q)){
    dim_q <- length(q)
  }else if(is.matrix(q)){
    dim_q <- dim(q)[1]
  }else{
    print("error, q is not vector or matrix")
    break
  }
  
  
  if(!is.null(dim(q))){
    n_hists <- dim(q)[2]
  }else{
    n_hists <- 0
  }
  if(log == TRUE){
    log_list <- vector(mode = "list")
  }
  if(warmstart == FALSE){
    if(n_hists > 0){
      u <- matrix(1,nrow = dim_p, ncol = n_hists)/dim_p
      v <- matrix(1,nrow = dim_q, ncol = n_hists)/dim_q
    }else{
      u <- rep(1, dim_p)/dim_p
      v <- rep(1, dim_q)/dim_q
    }
  }  else{
    u <- exp(warmstart[[1]])
    v <- exp(warmstart[[2]])
  }

  
  # K <- exp(m - eps*shannon_entropy(c(m)))
  K <- exp(M/(-eps))
  Kp <- (1/p)*K
  
  
  err <- 1
  break_check <- FALSE
  for(ii in 0:(numItermax-1)){
    uprev <- u
    vprev <- v
    KtransposeU <- t(K) %*% u
    v <- q/ KtransposeU
    u <- 1/(Kp%*%v)
    
    
    if(any(KtransposeU == 0)|
       any(is.na(u))| any(is.na(v))| any(is.infinite(u))|any(is.infinite(v))){
      print(paste0("Warning:, numerical errors at iteration ",ii))
      u <- uprev
      v <- vprev
      break_check <- TRUE
      break
    }
    if(ii %% 10 == 0){
      if(n_hists >0){
        tmp2 <- einsum('ik,ij,jk -> jk', u, K, v)
      }else{
        tmp2 = einsum('i,ij,j -> j', c(u), K, c(v))
      }
      err = sqrt(sum(abs(tmp2-q)^2))
      if(log == TRUE){
        log_list$err <- append(log_list$err, err)
      }
      
      if(err < stopThr){
        break_check <- TRUE
        break
      }
      if (verbose){
        if (ii %% 200 == 0){
          print(paste0("iter: ", ii," Error: ", err))
        }
      }
    }
  } 
  if(break_check == FALSE){
    if(warn){
      print("Sinkhorn did not converge. You might want to
            increase the number of iterations `numItermax` 
            or the regularization parameter `reg`.")
    }
  }
  if(log == TRUE){
    log_list$niter = ii
    log_list$u = u
    log_list$v = v
  }
  if(n_hists>0){  # return only loss
    res = einsum('ik,ij,jk,ij->k', u, K, v,M)
    if(log == TRUE){
      return(list(res = res, log = log_list))
    }else{
      return(list(res = res))
  }
  }else{  # return OT matrix
    u_k <- c(u)*K
    res_calc <- sweep(u_k, MARGIN = 2, c(v), '*')  
    if(log == TRUE){
    return (list(res = res_calc, log = log_list))
    }else{
    return(list(res = res_calc))
    }
  }
}

#####sinkhorn(p,q,m, 1, log_check = T)
##### note, if your distributions have 0's , add 1e-6 
##### (or any small number) so that you don't get instant
##### "solution" because we check for infinities that can ruin results

barycenter_sinkhorn <- function(A, M, reg, weights = NULL,numItermax = 1000,
                    stopThr = 1e-4, verbose = FALSE, log = FALSE, warn = TRUE){
  if(is.null(weights)){
    weights <- rep(1/dim(A)[2], length = dim(A)[2])
  }else if(length(weights) != dim(A)[1]){
    print("Error, weights not equal to A")
    break
  }
  if(log == TRUE){
    log_list <- vector(mode = "list")
  }
  K <- exp(-M/reg)
  err = 1
  Ksum <- rowSums(K)
  UKv = K %*% (A/Ksum)
  u <- geometricMean(UKv)/UKv
  break_check <- FALSE
  for(ii in 0:(numItermax-1)){
    UKv <- u * (t(K) %*% (A/(K %*% u)))
    u <- (u * geometricBar(weights, UKv))/UKv
    if(ii %% 10 == 0){
      
      err = sum(apply(UKv, MARGIN = 1, sd_pop))
      if(log == TRUE){
        log_list$err <- append(log_list$err, err)
      }
      
      if(err < stopThr){
        break_check <- TRUE
        break
      }
      if (verbose){
        if (ii %% 200 == 0){
          print(paste0("iter: ", ii," Error: ", err))
        }
      }
    }
  }
  if(break_check == FALSE){
    if(warn){
      print("Sinkhorn did not converge. You might want to
            increase the number of iterations `numItermax` 
            or the regularization parameter `reg`.")
    }
  }
  if(log == TRUE){
    log_list$niter <- ii
    return(list(res = geometricBar(weights,UKv), log = log))
  }else{
    return(list(res = geometricBar(weights,UKv)))
  }
}
#results <- barycenter_sinkhorn(A,M,1, log = T)
