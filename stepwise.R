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

stepforward <- function(y, x, data, alpha, intercept = FALSE) {
  terms <- NULL
  count <- 1
  
  # Initialization with the null model
  new.fit <- lm(reformulate(response = y,
                            termlabels = '1'),
                data = data)
  model.p <- NA
  anova.p <- NA
  correlations <- sapply(setdiff(x, terms),
                         parcor,
                         y = y,
                         covariates = terms,
                         data = data)
  next.up <- x[which.max(abs(correlations))]
  
  # Trace the steps
  traced <- list()
  if (intercept == TRUE) {
    steps <- data.frame('Beta' = coef(new.fit),
                        'Coefficient' = names(coef(new.fit)))
    traced[[count]] <- list(steps = steps,
                            next.up = next.up,
                            model.p = model.p,
                            anova.p = anova.p,
                            model = summary(new.fit))
    count <- 2
  }
  
  # Prep for next iteration
  terms <- next.up
  old.fit <- new.fit
  
  # Add next-most correlated variable and see if it helps the model
  while (is.na(anova.p) | anova.p < alpha) {
    new.fit <- lm(reformulate(response = y,
                              termlabels = terms),
                  data = data)
    anova.p <- anova(old.fit, new.fit)$'Pr(>F)'[2]
    model.p <- 1 - pnorm(summary(new.fit)$fstatistic[[1]], summary(new.fit)$fstatistic[[2]])
    correlations <- sapply(setdiff(x, terms),
                           parcor,
                           y = y,
                           covariates = terms,
                           data = data)
    next.up <- setdiff(x, terms)[which.max(abs(correlations))]
    
    # Monitor the steps
    if (intercept == TRUE) {
      steps <- data.frame('Beta' = coef(new.fit),
                          'Coefficient' = names(coef(new.fit)))
    } else {
      steps <- data.frame('Beta' = coef(new.fit)[-1],
                          'Coefficient' = names(coef(new.fit)[-1]))
    }
    traced[[count]] <- list(steps = steps,
                            next.up = next.up,
                            model.p = model.p,
                            anova.p = anova.p,
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