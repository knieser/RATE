#based on code from Generalizing causal inferences from individuals in randomized trials to all trial-eligible individuals
estOM <- function(df, Ymodel){
  # get predicted values under treatment
  df_A1 <- df[df$trt==1,]
  model_A1 <- lm(formula = Ymodel, data=df_A1)
  p1 <- predict(model_A1, newdata=df, type="response")
  df$p1 <- p1
  
  # get predicted values under control
  df_A0 <- df[df$trt==0,]
  model_A0 <- lm(formula = Ymodel, data=df_A0)
  p0 <- predict(model_A0, newdata=df, type="response")
  df$p0 <- p0
  
  # compute ATE
  ATE = mean(df$p1) - mean(df$p0)
  
  output <- list(mu1=mean(df$p1), mu0=mean(df$p0), ATE=ATE,
                 p1=p1, p0=p0, model_A1=model_A1, model_A0=model_A0)
  return(output)
}