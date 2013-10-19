parcor <- function(x, y, data, covariates) {
  covariates <- subset(covariates, covariates != x & covariates != y)
  if (is.null(covariates)) {return(cor(data[x], data[y]))}
  
  x.resid <- lm(reformulate(response = x, 
                            termlabels = covariates),
                data = data)$residuals
  
  y.resid <- lm(reformulate(response = y,
                            termlabels = covariates),
                data = data)$residuals
  
  return(cor(x.resid, y.resid))
}

stepforward <- function(y, x, data, alpha) {
  count <- 0
  # Initialization with the null model
  new.fit <- lm(reformulate(response = y,
                            termlabels = '1'),
                data = data)
  
  # Start monitoring the steps
  steps <- list()
  next.ups <- list(next.up = c())
  terms <- c()
  
  # Add next-most correlated variable and see if it helps the model
  p <- 0
  while (p < alpha) {
    count <- count + 1
    current.fit <- new.fit
    correlations <- sapply(setdiff(x, terms),
                           parcor,
                           y = y,
                           covariates = terms,
                           data = data)
    next.up <- setdiff(x, terms)[which.max(abs(correlations))]
    terms <- c(terms, next.up)
    new.fit <- lm(reformulate(response = y,
                              termlabels = terms),
                  data = data)
    p <- anova(current.fit, new.fit)$'Pr(>F)'[2]
    
    # Monitor the steps
    if (p < alpha) {
      steps <- rbind(steps, 
                 list(list('Iteration' = count,
                           'Beta' = coef(new.fit)[-1],
                           'Coefficient' = names(coef(new.fit)[-1]))))
      next.ups <- c(next.ups, next.up)
    }
    
    if (length(setdiff(x, terms)) == 0) {break} # Manually break if you ended up using all your variables
  }
  
  return(
    list(do.call(rbind, lapply(steps, as.data.frame)),
         next.ups))
}