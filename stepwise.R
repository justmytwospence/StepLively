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
  count <- 1
  p <- 0
  terms <- NULL
  
  # Initialization with the null model
  new.fit <- lm(reformulate(response = y,
                            termlabels = '1'),
                data = data)
  correlations <- sapply(setdiff(x, terms),
                         parcor,
                         y = y,
                         covariates = terms,
                         data = data)
  next.up <- x[which.max(abs(correlations))]
  
  # Trace the steps
  trace[[count]] <- list(steps = data.frame('Beta' = coef(new.fit),
                                            'Coefficient' = names(coef(new.fit))),
                         next.up = next.up,
                         p.value = p,
                         model = summary(new.fit))
  
  # Prep for next iteration
  terms <- next.up
  old.fit <- new.fit
  count <- count + 1
  
  # Add next-most correlated variable and see if it helps the model
  while (p < alpha) {
    new.fit <- lm(reformulate(response = y,
                              termlabels = terms),
                  data = data)
    p <- anova(old.fit, new.fit)$'Pr(>F)'[2]
    correlations <- sapply(setdiff(x, terms),
                           parcor,
                           y = y,
                           covariates = terms,
                           data = data)
    next.up <- setdiff(x, terms)[which.max(abs(correlations))]
    
    # Monitor the steps
    trace[[count]] <- list(steps = data.frame('Beta' = coef(new.fit),
                                              'Coefficient' = names(coef(new.fit))),
                           next.up = next.up,
                           p.value = p,
                           model = summary(new.fit))
    
    # Prep for next iteration
    terms <- c(terms, next.up)
    old.fit <- new.fit
    count <- count + 1
    
    # Manually break if you ended up using all your variables
    if (length(setdiff(x, terms)) == 0) {break}
  }
  return(trace)
}