#' Obtain first recall probability function for a set of trials
#'
#' @param indat data frame containing recalls. This must contain an integer variable named `serpos`
#' @param ll List length (number).
#' @param otherVars A vector of strings specifying other variables from the data frame (e.g., condition labels) to carry in to output.
#' @return A data frame containing the following:
#' \describe{
#' \item{serpos}{serial position (as integer)}
#' \item{serposf}{serial position (as factor)}
#' \item{prob}{probability of first recall}
#' \item{counts}{counts of recall}
#' }
#' @examples
#' (requires dplyr and magrittr):
#' freerec %>% filter(listlen==10) %>% group_by(ID) %>% do(getFRP(.,ll=10))
#' @importFrom dplyr group_by_at summarise arrange_
#' @importFrom magrittr '%>%'
#' @export
getFRP <- function(indat, ll, otherVars=NULL){

  if (!is.null(otherVars)){

    otherdf <- indat %>%
      filter(serpos>0 & serpos<=ll) %>%
      select(serpos,otherVars) %>%
      group_by(serpos) %>%
      summarise_all(getmode)
  }

  if (!is.null(indat$outpos)){
    indat <- indat[indat$outpos==1,]
  }

  indat <- indat[indat$serpos>0 & indat$serpos<=ll & indat$recalled==1,]

  histres <- hist(indat$serpos, breaks = (0:ll)+0.5, plot=F)

  retdat <- data.frame(serpos=1:ll,serposf=factor(1:ll),
                       prob=histres$density, counts=histres$counts)

  if (!is.null(otherVars)){
    return(merge(retdat, otherdf, by="serpos"))
  } else {
    return(retdat)
  }
}

getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

#' Obtain accuracy serial position function for a set of trials
#'
#' @param indat data frame containing recalls.
#' @param ll List length (number).
#' @param nTrials Number of trials to use in denominator; if NULL, this is worked out from unique(trial_id).
#' @param serposcol String giving name of column to use as serial position, typically "serpos" or "serposf"
#' @param otherVars A vector of strings specifying other variables from the data frame (e.g., condition labels) to carry in to output. If these don't vary by serpos, best to put them in as grouping variables prior to calling getAccFree
#' @return A data frame containing the following:
#' \describe{
#' \item{serpos}{serial position (as integer)}
#' \item{ncor}{numer of items recalled}
#' \item{ntrials}{numer of trials}
#' \item{prob}{probability of recall of each item (nTrials as denominator)}
#' \item{...}{other variables passed in to otherVars}
#' }
#' @examples
#' (requires dplyr and magrittr):
#' freerec %>% filter(listlen==10) %>% group_by(ID) %>% do(getAccFree(.,ll=10))
#' @importFrom dplyr group_by_at summarise arrange_
#' @importFrom magrittr '%>%'
#' @importFrom tidyr complete_
#' @export
getAccFree <- function(indat, ll, nTrials=NULL, serposcol="serpos", otherVars=NULL){

  if (is.null(nTrials)){
    nTrials <- length(unique(indat$trial))
  }

  # outdf <- ddply(indat, c("serpos",otherVars), summarise, ncor=sum(recalled==1))
  outdf <- indat %>%
    group_by_at(c(serposcol,otherVars)) %>%
    summarise(ncor=sum(recalled==1),
              ntrials=nTrials)

  # # fill in any missing serpos
  # if (is.factor(outdf[,serposcol])){
  #   outdf <- complete(outdf, serposcol, fill=list(ncor=0,ntrials=nTrials))
  # } else {
  #   missed <- setdiff(1:ll,outdf$serpos)
  #   for (sp in missed){
  #     outdf <- rbind(outdf, data.frame(serpos=sp,ncor=0,ntrials=nTrials))
  #   }
  # }

  # we now just generate a warning; user should do this outside
  if (is.factor(outdf[,serposcol])){
    missed <- setdiff(as.character(1:ll),outdf$serpos)
  } else {
    missed <- setdiff(1:ll,outdf$serpos)
  }
  if (length(missed)>0){
    warning("Not all serial positions were present in the data set. Use complete from the tidyr package to fill in missing SPs.")
  }

  outdf$pcor <- outdf$ncor/nTrials

  return(dplyr::arrange_(outdf,serposcol))
}

#' Obtain lag conditional response probability (lag-CRP) function for a set of trials
#'
#' @param indat data frame containing recalls, with variables for trial, outpos (cannot be a factor), serpos (cannot be a factor). Recalls within each trial should be ordered by output position.
#' @param ll List length (number).
#' @param otherVars A vector of strings specifying other variables from the data frame (e.g., condition labels) to carry in to output.
#' @return A data frame containing the following:
#' \describe{
#' \item{lag}{lag [from -(ll-1) to (ll-1)]}
#' \item{prob}{probability at each lag}
#' \item{lagrec}{probability at each lag (same as prob; for compatability)}
#' \item{numer}{numerator of lag-CRP function}
#' \item{denom}{denominator of lag-CRP function}
#' }
#' @examples
#' (requires dplyr and magrittr):
#' freerec %>% filter(listlen==10) %>% group_by(ID) %>% do(getlagCRP(.,ll=10))
#' @importFrom dplyr group_by_at summarise arrange_
#' @importFrom magrittr '%>%'
#' @export
getlagCRP <- function (indat, ll, doGroup=NULL, posStruct=NULL){

  indat <- indat[indat$recalled!=0,] # filter out omissions here--don't rely on user to do this

  numer <- rep(0,ll*2-1)
  denom <- numer
  numerw <- numer
  denomw <- numer
  numerb <- numer
  denomb <- numer

  ll1 <- ll - 1

  trials <- unique(indat$trial)

  # this is coded up but not officially implemented (hasn't been checked)
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
                    prob=numer/denom,
                    numer=numer,
                    denom=denom))
                    # lagrecw=numerw/denomw,
                    # lagrecb=numerb/denomb))
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
