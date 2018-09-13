#' Example free recall data.
#'
#' A dataset containing example free recall data from an experiment in the
#' Farrell lab.
#'
#' @format A data frame with 15166 rows and 6 variables:
#' \describe{
#'   \item{ID}{Participant ID}
#'   \item{trial}{An identified for each trial. trial_id's do not correspond between participants, and are simply used to associate all data from a trial for a participant}
#'   \item{listlen}{List length}
#'   \item{outpos}{Output position}
#'   \item{serpos}{Serial position of item recalled. >100 indicates an intrusion, the intrusion code being 100+outpos to distinguish between intrusions within a trial}
#'   \item{recalled}{Status of recall: 1=correct recall; 0=no recall; -1=intrusion; -2=repetition error}
#' }
#' @source Unpublished experiment from Farrell lab
"freerec"
