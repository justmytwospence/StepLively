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
  f.p <- 0
  terms <- NULL
  
  # Initialization with the null model
  new.fit <- lm(reformulate(response = y,
                            termlabels = '1'),
                data = data)
  model.p <- anova(new.fit)$'Pr(>F)'[2]
  correlations <- sapply(setdiff(x, terms),
                         parcor,
                         y = y,
                         covariates = terms,
                         data = data)
  next.up <- x[which.max(abs(correlations))]
  
  # Trace the steps
  traced <- list()
  traced[[count]] <- list(steps = data.frame('Beta' = coef(new.fit),
                                            'Coefficient' = names(coef(new.fit))),
                         next.up = next.up,
                         model.p = model.p,
                         f.p = model.p,
                         model = summary(new.fit))
  
  # Prep for next iteration
  terms <- next.up
  old.fit <- new.fit
  count <- count + 1
  
  # Add next-most correlated variable and see if it helps the model
  while (f.p < alpha) {
    new.fit <- lm(reformulate(response = y,
                              termlabels = terms),
                  data = data)
    f.p <- anova(old.fit, new.fit)$'Pr(>F)'[2]
    model.p <- anova(new.fit)$'Pr(>F)'[2]
    correlations <- sapply(setdiff(x, terms),
                           parcor,
                           y = y,
                           covariates = terms,
                           data = data)
    next.up <- setdiff(x, terms)[which.max(abs(correlations))]
    
    # Monitor the steps
    traced[[count]] <- list(steps = data.frame('Beta' = coef(new.fit),
                                              'Coefficient' = names(coef(new.fit))),
                           next.up = next.up,
                           model.p = model.p,
                           f.p = f.p,
                           model = summary(new.fit))
    
    # Prep for next iteration
    terms <- c(terms, next.up)
    old.fit <- new.fit
    count <- count + 1
    
    # Manually break if you ended up using all your variables
    if (length(setdiff(x, terms)) == 0) {break}
  }
  return(traced)
}