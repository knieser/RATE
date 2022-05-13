# make forest plot

makeForestPlot = function(estimates) {

  marg_tau = estimates$marg_tau
  strat = estimates$strat 
  re = estimates$re 
  minimax1 = estimates$minimax[1,,]
  minimax2 = estimates$minimax[2,,]
  minimax3 = estimates$minimax[3,,]
  grps = 1:ncol(strat)
  grp_names = paste("Group", grps)
  
  
  fig <- forestplot(
    legend = c(paste0("d=",estimates$parameters$delta[1],''),
               paste0("d=",estimates$parameters$delta[2],''),
               paste0("d=",estimates$parameters$delta[3],''),
               "Random Effects",
               "Stratified"),
    grp_names,
    mean = cbind(
      apply(minimax1,2,mean),
      apply(minimax2,2,mean),
      apply(minimax3,2,mean),
      apply(re,2,mean),
      apply(strat,2,mean)
    ),
    lower = cbind(
      apply(minimax1,2,mean) - 2*apply(minimax1,2,sd),
      apply(minimax2,2,mean) - 2*apply(minimax2,2,sd),
      apply(minimax3,2,mean) - 2*apply(minimax3,2,sd),
      apply(re,2,mean) - 2*apply(re,2,sd),
      apply(strat,2,mean) - 2*apply(strat,2,sd)),
    upper = cbind(
      apply(minimax1,2,mean) + 2*apply(minimax1,2,sd),
      apply(minimax2,2,mean) + 2*apply(minimax2,2,sd),
      apply(minimax3,2,mean) + 2*apply(minimax3,2,sd),
      apply(re,2,mean) + 2*apply(re,2,sd),
      apply(strat,2,mean) + 2*apply(strat,2,sd)
    ),
    boxsize=0.1,
    line.margin = 0.8,
    lwd.ci = c(2,2,2),
    lty.ci = c(2,2,2),
    xlab = "Average Treatment Effect",
    col = fpColors(box=c("#FF2525","#C80000","#8A0000","#9F9B9E","black"),
                   line=c("#FF2525","#C80000","#8A0000","#9F9B9E","black")),
    txt_gp =
      fpTxtGp(
        label = gpar(fontfamily = "sans", cex = 1.5),
        ticks = gpar(fontfamily = "sans", cex = 1.5),
        xlab  = gpar(fontfamily = "sans", cex = 1.5)),
    grid = structure(marg_tau, 
                     gp = gpar(lty=3,lwd=2,col="#6B6B6B"))
  )
  
  return(fig)
 
}