# generate some multilevel binary data
# arguments: n_subj = number of subjects
# n_obs = total number of trials per subject
# alpha = veridical grand mean log odds performance
# beta1 = veridical effect of cond1
# beta2 = veridical effect of cond2
# beta3 = veridical cond1 * cond2 interaction effect
# subj_corrs = list of correlations between random effects
# subj_tau = list of SDs of random effects

generate_bin <- function(n_subj, n_obs, alpha, beta1, beta2, beta3, subj_corrs, subj_tau) {
  # make data frame where number of rows = number of subjects * number of trials per subject
  data <- data.frame(matrix(0, ncol=0, nrow = n_subj * n_obs))
  # make subject vector and add to this data frame
  data$subject <- as.factor(rep(seq(1:n_subj), each = n_obs))
  # make condition 1 vector - within subjects
  # half trials one value, half trials the other
  data$cond1 <- as.factor(rep(c(0,1), each = n_obs/2))
  # make centred version
  data$c_cond1 <- as.numeric(data$cond1) - mean(as.numeric(data$cond1))
  # make condition 2 vector - also within subjects
  # 1/4 trials one value, 1/4 trials the other, then repeat
  # this ensures cond1 and cond2 are not identical
  data$cond2 <- as.factor(rep(c(0,1), each = n_obs/4))
  # make centred version
  data$c_cond2 <- as.numeric(data$cond2) - mean(as.numeric(data$cond2))
  # for subject effects
  # first, we put the correlations between the random effects in a matrix
  # * if changing to simulate fewer random effects & hence fewer correlations, 
  # this will need to be adjusted
  corr_matrix <- matrix(c(1, subj_corrs[1], subj_corrs[2], subj_corrs[3],
                          subj_corrs[1], 1, subj_corrs[4], subj_corrs[5],
                          subj_corrs[2], subj_corrs[4], 1, subj_corrs[6],
                          subj_corrs[3], subj_corrs[5], subj_corrs[6], 1), nrow = 4)
  # next, construct variance covariance matrix for subject effects
  # We multiply the subject effect sds (in matrix form) by the correlation matrix
  # and then again by the subject effect sds
  # so we end up with the sds squared (on the diagonal) = variance, 
  # and covariances between each pair of subject effects on the off-diagonal
  # * if changing to simulate fewer random effects, this should still work fine,
  # assuming corr_matrix has been adjusted appropriately
  subj_v_cov <- diag(subj_tau) %*% corr_matrix %*% diag(subj_tau)
  # Create the correlated subject effects, using mvrnorm to sample from multivariate normal distribution
  # means of subject intercepts and slopes are 0
  u <- mvrnorm(n = n_subj, c(0,0,0,0), subj_v_cov)
  # check the correlation - this should be fairly accurate in large samples
  # print(cor(u))
  # check the SDs - again, should be fairly accurate in large samples
  # print(sd(u[,1]))
  # print(sd(u[,2]))
  # print(sd(u[,3]))
  # print(sd(u[,4]))
  # finally, generate data on the basis of these parameters
  data <- data %>%
    mutate(
      # We first calculate the linear predictor eta for each row in the data frame
      # = overall intercept + subject intercept +
      eta = alpha + u[data$subject,1] +
        # cond1 value * (cond1 fixed effect + cond1 random slope) +
        data$c_cond1 * (beta1 + u[data$subject,2]) + 
        # cond2 value * (cond2 fixed effect + cond2 random slope) +
        data$c_cond2 * (beta2 + u[data$subject,3]) +
        # cond1 * cond2 value * (interaction fixed effect + interaction random slope) +
        (data$c_cond1 * data$c_cond2) * (beta3 + u[data$subject,4]),
      # then transform by inverse logit to a probability (for this combo of subject/condition)
      mu = inv.logit(eta),
      # finally, generate a 0 or 1 for this row based on the probability for this subject/condition
      y = rbinom(nrow(data),1,mu))
  return(data)
}