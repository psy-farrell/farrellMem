getTransMat <- function(indat, ll){

  if (is.null(indat$cue)){
    transmat <- ddply(indat[indat$serpos>0 & indat$serpos<=ll,], .(outpos), function(x){
      hist(x$serpos, breaks = (0:ll)+0.5, plot=F)$density
    })
  } else {
    transmat <- ddply(indat[indat$serpos>0 & indat$serpos<=ll,], .(cue), function(x){
      hist(x$serpos, breaks = (0:ll)+0.5, plot=F)$density
    })
  }
  return(transmat[,-1]) # rows are output positions
                        # columns are input positions
}

getTransGradient <- function(indat, ll, abstrans=FALSE){

  indat$tdisp <- indat$outpos-indat$serpos
  indat$tdisp[indat$recalled==0 | indat$recalled==-1] <- NA
  indat$tdist <- abs(indat$tdisp)

  if (abstrans){
    lags <- 0:(ll-1)
    kk <- hist(indat$tdist, breaks = c(lags,ll)-0.5, plot=F)
    olist <- list(trans=lags, counts=kk$counts, density=kk$density)
  } else {
    lags <- (-ll+1):(ll-1)
    kk <- hist(indat$tdisp, breaks = c(lags,ll)-0.5, plot=F)
    olist <- list(trans=lags, counts=kk$counts, density=kk$density)
  }

  return(olist)
}

getLatencySPC <- function(res, ll){
  # this only caluclates latency SPC for correct responses
  kk <- ddply(res[(res$recalled==1) &
                    (res$outpos==res$serpos) &
                    (res$outpos<=ll),],
            .(outpos),
            summarise, meanRT=mean(rt))
  return(kk)
}
