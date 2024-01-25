
#' @title: sample_clt()
#' @description: function to sample from a distribution and estimate the 
#' sample mean, standard deviation, n and standard error.
#' @param pop_data - vector of data-points from the population
#' @param n - size of the sample to be taken
#' @param n_samples - number of indepedent samples of size n to take

sample_clt <- function(pop_data, n, n_samples) {
  
  # test input data
  assertthat::assert_that(assertthat::is.count(n), 
                          msg = "n must be an integer")
  
  assertthat::assert_that(assertthat::is.count(n_samples), 
                         msg = "n_samples must be an integer")
  
  assertthat::assert_that(n < length(pop_data),
                          msg = "size of the sample cannot be greater than
                          the number of samples in the population distribution")
  
  dist_clt <- vector("list", length = length(1:n_samples))
  for (i in seq_along(1:n_samples)) {
    
    samp_dat <- sample(pop_data, size = n, replace = FALSE, prob = NULL)
    m_dist <- mean(samp_dat)
    sd_dist <- sd(samp_dat)
    n_dist <- length(samp_dat)
    se_dist <- sd(samp_dat)/sqrt(length(samp_dat))
    dist_clt[[i]] <- dplyr::bind_cols(sample_mean = m_dist,
                                      sample_sd = sd_dist,
                                      sample_n = n_dist,
                                      sample_se = se_dist)
  }
  
  # combine the independent samples
  dist_clt <- dplyr::bind_rows(dist_clt)
  
  # combine the different samples
  dplyr::bind_cols( dplyr::tibble(sample_size = as.character(rep(n, times = n_samples))),
                    dplyr::select(dist_clt, sample_mean, sample_sd, sample_n, sample_se))
  
}

#' @title: sim_dist()
#' @description: function to generate a population distribution from one of
#' four distributions: normal, lognormal, poisson and uniform.
#' @param dist - distribution to sample from (string: normal, lognorm, poisson, uniform)
#' @param n - size of the sample to be taken (integer)
#' @param par1 - parameter 1 of the distribution:
#' - normal: par1 = mean
#' - lognorm: par1 = meanlog
#' - poisson: par1 = lambda
#' - uniform: par1 = min
#' @param par2 - parameter 2 of the distribution:
#' - normal: par2 = sd
#' - lognorm: par2 = sdlog
#' - poisson: par2 = NULL
#' - uniform: par2 = max

sim_dist <- function(dist, n, par1, par2) {
  
  # test the inputs
  assertthat::assert_that(assertthat::is.string(dist), 
                          msg = "dist must be a string")
  assertthat::assert_that(dist %in% c("normal", "lognorm", "poisson", "uniform"), 
                          msg = "dist must be an accepted distribution")
  
  assertthat::assert_that(assertthat::is.count(n), 
                          msg = "n must be an integer")
  
  assertthat::assert_that(assertthat::is.number(par1), 
                          msg = "par1 must be an number")
  assertthat::assert_that(assertthat::is.number(par2), 
                          msg = "par2 must be an number")
  
  pop_data <- 
    
    if(dist == "normal") {
      
      assertthat::assert_that(par2 > 0, msg = "par2 must be greater than zero")
      rnorm(n = n, mean = par1, sd = par2)
      
    } else if(dist == "lognorm") {
      
      assertthat::assert_that(par2 > 0, msg = "par2 must be greater than zero")
      rlnorm(n = n, meanlog = par1, sdlog = par2)
      
    } else if(dist == "poisson") {
      
      assertthat::assert_that(par1 > 0, msg = "par1 must be greater than zero")
      rpois(n = n, lambda = par1)
      
    } else if(dist == "uniform") {
      
      runif(n = n, min = par1, max = par2)
      
    } 
    
  pop_data
  
}






