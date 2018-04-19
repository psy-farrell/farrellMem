# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Cmd + Shift + B'
#   Check Package:             'Cmd + Shift + E'
#   Test Package:              'Cmd + Shift + T'

# FRP and acc fn assume that repetitions are coded as -1
getFRP <- function(indat, ll, otherVars=NULL){

  if (!is.null(otherVars)){
    otherList <- {}
    for (kk in otherVars){
      otherList[[kk]] <- ddply(indat, .(serpos), function(x) getmode(x[,kk]))
    }
  }

  if (!is.null(indat$outpos)){
    indat <- indat[indat$outpos==1,]
  }

  indat <- indat[indat$serpos>0 & indat$serpos<=ll & indat$recalled==1,]

  histres <- hist(indat$serpos, breaks = (0:ll)+0.5, plot=F)

  retdat <- data.frame(serpos=1:ll,serposf=factor(1:ll),
                       prob=histres$density, counts=histres$counts)

  if (!is.null(otherVars)){
    for (kk in otherVars){
      retdat <- merge(retdat, otherList[[kk]], by="serpos")
    }
    names(retdat)[5:(4+length(otherVars))] <- otherVars
  }

  return(retdat)
}

getAccFree <- function(indat, ll, nTrials=NULL, otherVars=NULL){

  if (is.null(nTrials)){
    nTrials <- length(unique(indat$trial))
  }

  outdf <- ddply(indat, c("serpos",otherVars), summarise, ncor=sum(recalled==1))
  outdf$pcor <- outdf$ncor/nTrials

  return(outdf)
}

getlagCRP <- function (indat, ll, doGroup=NULL, posStruct){

  # note change in gpos name to be consistent with serpos
  # needs trial, outpos (cannot be a factor), serpos (cannot be a factor)
  # optional: gpos (not a factor)

  indat <- indat[indat$recalled!=0,] # filter out omissions here--don't rely on user to do this

  # the function checks for repetitions CHECK

  numer <- rep(0,ll*2-1)
  denom <- numer
  numerw <- numer
  denomw <- numer
  numerb <- numer
  denomb <- numer

  ll1 <- ll - 1

  trials <- unique(indat$trial)

  if (is.null(doGroup)){
    doGroup <- ifelse(is.null(indat$gpos), FALSE, TRUE)
  } else {
    if (is.null(indat$gpos) && (doGroup==TRUE)){
      stop("doGroup was set to TRUE, but no gpos was provided in data frame")
    }
  }

  for (trial in trials){

    tdat <- indat[indat$trial==trial,]
    tdat <- tdat[order(tdat$outpos),]

    inseq <- tdat$serpos

    if (!all(diff(tdat$outpos) == 1)){
      warning("Output positions not sequential on a trial")
    }

    if (doGroup){
      error("Not yet set up to do by group")
      # It would make sense to make whole thing condition on being in same/diff group?       # So make conditional on group of i being different to i+1, and numerator should
      # be all items from pool in taht condition (ie *given* I make a same/different
      #group transition...)
      gposseq <- indat$gpos[indat$trial==trial]
    }

    if (length(inseq)>1){

      pool = setdiff(1:ll, inseq[1])
      prevWasRep <- FALSE

      for (i in 2:length(inseq)){
        if (any(pool==inseq[i]) &
            (inseq[i]>0) & (inseq[i]<=ll) &
            (inseq[i-1]>0) & (inseq[i-1]<=ll) &
            !prevWasRep){

          numer[inseq[i]-inseq[i-1]+ll] <- numer[inseq[i]-inseq[i-1]+ll]+1
          denom[pool-inseq[i-1]+ll] <- denom[pool-inseq[i-1]+ll]+1

          if (doGroup){
            tpool <- intersect(which(posStruct==posStruct[inseq[i-1]]),pool)
            denomw[tpool-inseq[i-1]+ll] <- denomw[tpool-inseq[i-1]+ll]+1

            tpool <- intersect(which(posStruct!=posStruct[inseq[i-1]]),pool)
            denomb[tpool-inseq[i-1]+ll] <- denomb[tpool-inseq[i-1]+ll]+1

            if (gposseq[i]==gposseq[i-1]){
              numerw[inseq[i]-inseq[i-1]+ll] <- numerw[inseq[i]-inseq[i-1]+ll]+1
            } else {
              numerb[inseq[i]-inseq[i-1]+ll] <- numerb[inseq[i]-inseq[i-1]+ll]+1
            }
          }

          pool <- setdiff(pool,inseq[i])

        } else {
          if (!any(pool==inseq[i])){
            prevWasRep = TRUE
          } else {
            prevWasRep = FALSE
          }
        }
      }
    }
  }
  numer[ll] <- NA
  numerw[ll] <- NA
  numerb[ll] <- NA
  return(data.frame(lag=c(-(ll-1):0,1:(ll-1)),
                    lagrec=numer/denom,
                    lagrecw=numerw/denomw,
                    lagrecb=numerb/denomb))
}

# getlagCRPbySP <- function (indat, ll){
#
#   # note change in gpos name to be consistent with serpos
#   # needs trial, outpos (cannot be a factor), serpos (cannot be a factor)
#   # optional: gpos (not a factor)
#
#   # only pass in obtained responses (ie don;t pass in filled responses, where recalled==0)
#   # the function checks for repetitions CHECK
#
#   numer <- matrix(0,ll,ll*2-1)
#   denom <- numer
#
#   ll1 <- ll - 1
#
#   trials <- unique(indat$trial)
#
#   for (trial in trials){
#
#     tdat <- indat[indat$trial==trial,]
#     tdat <- tdat[order(tdat$outpos),]
#
#     inseq <- tdat$serpos
#
#     if (!all(diff(tdat$outpos) == 1)){
#       warning("Output positions not sequential on a trial")
#     }
#
#     if (length(inseq)>1){
#
#       pool = setdiff(1:ll, inseq[1])
#       prevWasRep <- FALSE
#
#       for (i in 2:length(inseq)){
#         if (any(pool==inseq[i]) &
#             (inseq[i]>0) & (inseq[i]<=ll) &
#             (inseq[i-1]>0) & (inseq[i-1]<=ll) &
#             !prevWasRep){
#
#           numer[inseq[i-1], inseq[i]-inseq[i-1]+ll] <- numer[inseq[i-1], inseq[i]-inseq[i-1]+ll]+1
#           denom[inseq[i-1], pool-inseq[i-1]+ll] <- denom[inseq[i-1], pool-inseq[i-1]+ll]+1
#
#           pool <- setdiff(pool,inseq[i])
#
#         } else {
#           if (!any(pool==inseq[i])){
#             prevWasRep = TRUE
#           } else {
#             prevWasRep = FALSE
#           }
#         }
#       }
#     }
#   }
#   numer[ll] <- NA
#   return(data.frame(lag=c(-(ll-1):0,1:(ll-1)),
#                     lagrec=numer/denom))
# }

TCscoreTrial <- function(dat, ll, crit){
  # make sure repetitions have been excluded from the data
  prlag <- dat$outpos + (ll-dat$serpos)
  prim <- sum((prlag <= crit) & dat$recalled==1)
  rec <- sum((prlag>crit) & dat$recalled==1)
  return(data.frame(prim=prim,rec=rec))
}

getCondRec <- function(indat, ll){
  numer <- rep(0,ll)
  denom <- rep(0,ll)

  trials <- unique(indat$trial)

  for (trial in trials){
    tdat <- indat[indat$trial==trial,]
    tdat <- tdat[order(tdat$outpos),]

    inseq <- tdat$serpos

    if (!all(diff(tdat$outpos) == 1)){
      warning("Output positions not sequential on a trial")
    }

    for (i in 1:length(inseq)){
      denom[i] <- denom[i] + 1
      if (inseq[i]==ll){
        numer[i] <- numer[i] + 1
        break;
      }
    }
  }
  return(list(numer=numer,denom=denom,condRec=numer/denom))
}

getGolomb <- function(dat, ll){

  tdat <- ddply(dat, .(trial), function(x){
    tx <- x[order(x$outpos),]
    outSeq <- tx$serpos[(tx$serpos>0) & (tx$recalled==1)]
    n <- length(outSeq)
    if (n>1){
      pcor <- c(1,outSeq[2:n] > outSeq[1:(n-1)])
    } else {
      pcor <- 1
    }
    tx$pcor <- rep(0, dim(tx)[1])
    # print(sum(tx$serpos>0))
    # print(length(pcor))
    tx$pcor[tx$serpos>0 & (tx$recalled==1)] <- pcor
    return(tx)
  })

  summ <- ddply(tdat, .(serpos), summarise, pcor=mean(pcor))

  return(summ[summ$serpos<=ll,])
}
