#' @title Load Pmetrics NPAG or IT2B output
#' @description
#' `r lifecycle::badge("superseded")`
#' 
#' Loads all the data from an \emph{NPAG} or \emph{IT2B} run.
#' This function has been superseded by [PM_load], which returns objects.
#' In contrast, *PMload* loads them directly into the Global environment, which
#' is not best-practice programming.
#' @param run The numerical value of the folder number containing the run results.  This
#' number will also be used to name objects uniquely by appending \dQuote{.\code{run}},
#' e.g. NPdata.1 or ITdata.1 if run=1. This parameter is \code{1} by default.
#' @param \dots Additional runs to load if desired.
#' @param remote Default is \code{FALSE}.  Set to \code{TRUE} if loading results of an NPAG run on remote server.
#' See \code{\link{NPrun}}. Currently remote runs are not configured for IT2B or the Simulator.
#' @param server_address If missing, will use the default server address returned by getPMoptions().
#' Pmetrics will prompt the user to set this address the first time the \code{remote} argument is set to \code{TRUE}
#' in \code{\link{NPrun}}.
#' @return The following objects are loaded into R.
#' \item{NPdata/ITdata }{List with all output from NPAG/IT2B}
#' \item{pop }{ NPAG only: Population predictions for each output equation}
#' \item{post }{ NPAG only: Individual posterior predictions for each output equation}
#' \item{final }{Final cycle population support points and parameter summary statistics}
#' \item{cycle }{Cycle log-likelihood, AIC, BIC, Gamma/lambda, and normalized parameter means, medians and SDs}
#' \item{op }{List of observed vs. population and posterior predicted plots for each output equation}
#' \item{cov }{Data frame of subject ID, covariate values, and Bayesian posterior parameter estimates}
#' \item{mdata }{The original .csv data file used in the run}
#' \item{valid }{If \code{\link{makeValid}} has been executed after a run, this object will be added to
#' the save data.  It contains the information required to plot visual predictive checks and normalized prediction
#' error discrepancies via the npde code developed by Comets et al}
#' @author Michael Neely
#' @seealso  \code{\link{NPparse}}, \code{\link{ITparse}},
#' \code{\link{makeFinal}}, \code{\link{makeCycle}}, \code{\link{makeOP}}, \code{\link{makeCov}},
#' \code{\link{makePop}}, \code{\link{makePost}}
#' @export

PMload <- function(run = 1, ..., remote = F, server_address) {
  cat("This function is for legacy Pmetrics.\nPlease see documentation for R6 Pmetrics
      and PM_load\n")
  # declare variables to avoid R CMD Check flag
  NPAGout <- NULL
  IT2Bout <- NULL
  
  if (missing(server_address)) server_address <- getPMoptions("server_address")
  addlruns <- list(...)
  if (length(addlruns) > 0) {
    allruns <- c(run, unlist(addlruns))
  } else {
    allruns <- run
  }
  
  for (thisrun in allruns) {
    # check for NPAG output file
    filename <- "NPAGout.Rdata"
    outfile <- paste(thisrun, "outputs", filename, sep = "/")
    
    if (remote) { # only look on server
      status <- .remoteLoad(thisrun, server_address)
      if (status == "finished") {
        .splitOut(thisrun, NPAGout)
      } else {
        sprintf("Warning: Remote run #%d has not finished yet.\nCurrent status: \"%s\"\n", thisrun, status) %>%
          cat()
      }
    } else if (file.exists(outfile)) { # remote F, so look locally
      # load(outfile, .GlobalEnv)
      load(outfile)
      .splitOut(thisrun, get("NPAGout"))
    } else {
      # check for IT2B output file
      filename <- "IT2Bout.Rdata"
      outfile <- paste(thisrun, "outputs", filename, sep = "/")
      if (file.exists(outfile)) {
        load(outfile)
        .splitOut(thisrun, get("IT2Bout"))
      } else {
        cat(paste(outfile, " not found in ", getwd(), "/", thisrun, "/outputs or ", getwd(), ".\n", sep = ""))
        return(invisible(F)) # error, abort
      }
    }
  }
  # end thisrun loop
  
  
  return(invisible(T)) # no errors
}


.splitOut <- function(run, Out) {
  newNames <- paste(names(Out), ".", as.character(run), sep = "")
  for (i in 1:length(newNames)) {
    assign(newNames[i], Out[[i]], pos = .GlobalEnv)
  }
}

.remoteLoad <- function(run, server_address) {
  status <- ""
  rid <- .getRemoteId(run)
  status <- .PMremote_check(rid = rid, server_address = server_address)
  if (status == "finished") {
    sprintf("Remote run #%d finished successfuly.\n", run) %>%
      cat()
    .PMremote_outdata(run, server_address)
  }
  return(status)
}

.getRemoteId <- function(run) {
  run <- toString(run)
  fileName <- paste(run, "inputs", "id.txt", sep = "/")
  if (file.exists(fileName)) {
    return(readChar(fileName, file.info(fileName)$size) %>% gsub("\n", "", .data))
  } else {
    stop(sprintf("File id.txt not found in /%s/outputs.\n", run))
    return(NULL)
  }
}


#' @title Compare NPAG or IT2B runs
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' Compare NPAG or IT2B runs. This function is superseded by [PM_compare].
#' @details
#' For backwards compatibility, objects can be specified separated by commas, e.g. PMcompare(1,2,3) followed by
#' any arguments you wish to \code{\link{plot.PMop}}, \code{\link{mtsknn.eq}}. P-values are based on comparison using the nearest neighbors
#' approach if all models are non-parametrics.  Models may only be compared on parameters that are included
#' in the first model.  The P-value is the comparison between each model and the first model in
#' the list.  Missing P-values are when a model has no parameter names in common with the first
#' model, and for the first model compared to itself, or when models from IT2B runs are included.  Significant P-values indicate that the null
#' hypothesis should be rejected, i.e. the joint distributions between the two compared models are
#' significantly different.
#'
#' @param x The run number of the first object you wish to compare. This should be a folder in your
#' working directory. To avoid confusion, this function does not use objects
#' already loaded with \code{\link{PMload}}.
#' This will serve as the reference output for P-value testing (see details).
#' @param y The run number of the second object to compare.
#' @param \dots Additional run numbers to compare.  See details.  Also, parameters to be passed to \code{\link{plot.PMop}}
#' if \code{plot} is true as well as to \code{\link{mtsknn.eq}}.  Order does not matter.
#' @param icen Can be either "median" for the predictions based on medians of \code{pred.type} parameter value
#' distributions, or "mean".  Default is "median".
#' @param outeq Number of the output equation to compare; default is 1
#' @param plot Boolean operator selecting whether to generate observed vs. predicted plots for each data object
#' as in \code{\link{plot.PMop}}
#' @return A data frame with the following objects for each model to analyze:
#'  \item{run }{The run number of the data}
#'  \item{type }{NPAG or IT2B data}
#'  \item{nsub }{Number of subjects in the model}
#'  \item{nvar }{Number of random parameters in the model}
#'  \item{par }{Names of random parameters}
#'  \item{cycles }{Number of cycles run}
#'  \item{converge }{Boolean value if convergence occurred.}
#'  \item{ll }{Final cycle -2*Log-likelihood }
#'  \item{aic }{Final cycle Akaike Information Criterion}
#'  \item{bic }{Final cycle Bayesian (Schwartz) Information Criterion }
#'  \item{popBias }{Bias, or mean weighted prediction error of predictions based on population parameters minus observations}
#'  \item{popImp }{Imprecision, or bias-adjusted mean weighted squared error of predictions based on population parameters minus observations }
#'  \item{popPerRMSE}{Percent root mean squared error of predictions based on population parameters minus observations}
#'  \item{postBias }{Bias, or mean weighted prediction error of predictions - observations  based on posterior parameters}
#'  \item{postImp }{Imprecision, or bias-adjusted mean weighted squared error of predictions - observations based on posterior parameters}
#'  \item{postPerRMSE}{Percent root mean squared error of predictions based on posterior parameters minus observations}
#'  \item{pval }{P-value for each model compared to the first. See details.}
#' @author Michael Neely
#' @seealso \code{\link{PMload}}, \code{\link{plot.PMop}}, \code{\link{mtsknn.eq}}
#' @export


PMcompare <- function(x, y, ..., icen = "median", outeq = 1, plot = F) {
  if (missing(x) | missing(y)) stop("You must specify at least two run numbers for PMcompare.\n")
  if (inherits(x, c("NPdata", "ITdata"))) stop("You should specify your objects by run number.  See help.\n")
  
  # parse dots
  arglist <- list(...)
  namesPlot <- names(formals(plot.PMop))
  namesMTSKNN <- names(formals(mtsknn.eq))
  # get the args to plot.PMop and set defaults if missing
  plotArgs <- which(names(arglist) %in% namesPlot)
  argsPlot <- arglist[plotArgs]
  if (!"cex.stat" %in% names(argsPlot)) argsPlot$cex.stat <- 0.8
  if (!"x.stat" %in% names(argsPlot)) argsPlot$x.stat <- 0.5
  # get the args to mtsknn.eq and set defaults if missing
  MTSKNNargs <- which(names(arglist) %in% namesMTSKNN)
  argsMTSKNN <- arglist[MTSKNNargs]
  if (!"k" %in% names(argsMTSKNN)) argsMTSKNN$k <- 3
  if (!"print" %in% names(argsMTSKNN)) argsMTSKNN$print <- FALSE
  # get the others if there and assume that they are PMdata objects for now
  if ((length(arglist) - length(c(plotArgs, MTSKNNargs))) > 0) {
    if (length(c(plotArgs, MTSKNNargs)) == 0) {
      argsPM <- 1:length(arglist)
    } else {
      argsPM <- (1:length(arglist))[-c(plotArgs, MTSKNNargs)]
    }
  } else {
    argsPM <- NULL
  }
  
  if (length(argsPM) == 0) obj <- list(x, y)
  if (length(argsPM) >= 1) obj <- c(list(x, y), arglist[argsPM])
  
  # declare global variables to avoid problems with R CMD Check
  NPAGout <- NULL
  IT2Bout <- NULL
  
  # get each obj
  nobj <- length(obj)
  allObj <- list()
  for (thisobj in 1:nobj) {
    # find  objects
    if (!file.exists(as.character(obj[thisobj]))) {
      cat(paste(obj[thisobj], " is not a folder in the current working directory.\n", sep = ""))
    } else {
      ITfile <- list.files(paste(obj[thisobj], "outputs", sep = "/"), pattern = "IT2Bout.Rdata", full.names = T)
      NPfile <- list.files(paste(obj[thisobj], "outputs", sep = "/"), pattern = "NPAGout.Rdata", full.names = T)
      load(c(ITfile, NPfile))
      if (length(ITfile) > 0) {
        allObj[[thisobj]] <- IT2Bout$ITdata
      } else {
        allObj[[thisobj]] <- NPAGout$NPdata
      }
    }
  }
  
  
  
  
  objClass <- mapply(class, allObj)
  # check for non-Pmetrics data objects and remove them if found
  yesPM <- which(objClass %in% c("NPAG", "IT2B"))
  allObj <- allObj[yesPM]
  objClass <- objClass[yesPM]
  
  
  # check for zero cycle objects
  cycles <- unlist(sapply(allObj, function(x) x$icyctot))
  if (any(cycles == 0)) stop(paste("Do not include 0-cycle runs: item(s) ", paste(which(cycles == 0), collapse = ", "), "\n", sep = ""))
  op <- apply(mapply(makeOP, allObj), 2, function(x) {
    data.frame(x)
  })
  op <- lapply(op, function(x) {
    class(x) <- c("PMop", "data.frame")
    x
  })
  if (plot) {
    if (!"resid" %in% names(argsPlot)) {
      if (nobj <= 3) {
        par(mfrow = c(nobj, 2))
      } else {
        par(mfrow = c(3, 2))
        devAskNewPage(ask = T)
      }
      for (i in 1:length(op)) {
        do.call(plot.PMop, args = c(list(
          x = op[[i]], pred.type = "pop", icen = icen, outeq = outeq,
          main = paste("Model", i, "Population")
        ), argsPlot))
        do.call(plot.PMop, args = c(list(
          x = op[[i]], pred.type = "post", icen = icen, outeq = outeq,
          main = paste("Model", i, "Posterior")
        ), argsPlot))
      }
    } else {
      devAskNewPage(ask = T)
      for (i in 1:length(op)) {
        do.call(plot.PMop, args = c(list(
          x = op[[i]], pred.type = "post", icen = icen, outeq = outeq,
          main = paste("Model", i)
        ), argsPlot))
      }
    }
    par(mfrow = c(1, 1))
    devAskNewPage(ask = F)
  }
  
  
  # get summaries of op for outeq
  sumobjPop <- mapply(summary.PMop, op, MoreArgs = list(outeq = outeq, pred.type = "pop", icen = icen), SIMPLIFY = F)
  sumobjPost <- mapply(summary.PMop, op, MoreArgs = list(outeq = outeq, pred.type = "post", icen = icen), SIMPLIFY = F)
  
  
  popBias <- sapply(sumobjPop, function(x) ifelse(is.na(x$pe[1]), NA, x$pe$mwpe))
  postBias <- sapply(sumobjPost, function(x) ifelse(is.na(x$pe[1]), NA, x$pe$mwpe))
  popImp <- sapply(sumobjPop, function(x) ifelse(is.na(x$pe[1]), NA, x$pe$bamwspe))
  postImp <- sapply(sumobjPost, function(x) ifelse(is.na(x$pe[1]), NA, x$pe$bamwspe))
  popPercent_RMSE <- sapply(sumobjPop, function(x) ifelse(is.na(x$pe[1]), NA, x$pe$percent_rmse))
  postPercent_RMSE <- sapply(sumobjPost, function(x) ifelse(is.na(x$pe[1]), NA, x$pe$percent_rmse))
  
  # if all NPAG, calculate nearest neighbors p-value compared to first
  if (all(sapply(allObj, function(x) inherits(x, "NPAG")))) {
    # get population points
    final <- mapply(makeFinal, allObj)
    # find intersecting parameters
    popPointsRef <- final[, 1]$popPoints
    namesRef <- names(popPointsRef)
    popPointsOther <- lapply(2:nobj, function(x) final[, x]$popPoints)
    t <- sapply(2:nobj, function(x) {
      thisPopPoints <- popPointsOther[[x - 1]]
      namesThis <- names(thisPopPoints)
      intersect <- namesRef[namesRef %in% namesThis]
      if (length(intersect) > 0) {
        popPoints1 <- popPointsRef[, intersect]
        popPoints2 <- thisPopPoints[, intersect]
        t <- do.call(mtsknn.eq, args = c(list(x = popPoints1, y = popPoints2), argsMTSKNN))$pval
      } else {
        t <- NA
      }
      signif(t, 3)
    })
    
    t <- c(NA, t)
  } else {
    t <- NA
  }
  
  results <- data.frame(
    run = unlist(obj),
    type = objClass,
    nsub = mapply(function(x) x$nsub, allObj),
    nvar = mapply(function(x) x$nvar, allObj),
    par = mapply(function(x) paste(x$par, collapse = " "), allObj),
    converge = mapply(function(x) x$converge == 1, allObj),
    ll = mapply(function(x) -2 * x$ilog[length(x$ilog)], allObj),
    aic = mapply(function(x) tail(x$iic[, 1], 1), allObj),
    bic = mapply(function(x) tail(x$iic[, 2], 1), allObj),
    popBias = popBias,
    popImp = popImp,
    popPer_RMSE = popPercent_RMSE,
    postBias = postBias,
    postImp = postImp,
    postPer_RMSE = postPercent_RMSE,
    pval = t
  )
  names(results)[7] <- "-2*LL"
  results[, 7:15] <- format(results[, 7:15], digits = 4)
  row.names(results) <- 1:nobj
  results
}



#' @title Create a Pmetrics validation object
#' @description
#' `r lifecycle::badge("superseded")`
#'
#' This function is largely a legacy function, replaced by [make_valid], which is
#' typically called with the `$validate` method for a [PM_result] object.
#'
#' @details
#' `makeValid` will create an object suitable for plotting visual predictive
#' checks (VPCs) and prediction-corrected visual
#' predictive checks (pcVPCs). The function will guide the user
#' through appropriate clustering of doses, covariates and sample times for
#' prediction correction using the methods of Bergstrand et al (2011).
#' *NOTE:* Including TAD is only
#' valid if steady state conditions exist for each patient.  This means that dosing is stable and regular
#' for each patient, without changes in amount or timing, and that sampling occurs after the average concentrations
#' are the same from dose to dose.  Otherwise observations are *NOT* superimposable and `tad` should
#' *NOT* be used, i.e. should be set to `FALSE`.
#'
#' @param run When the current working directory is the Runs folder, the folder name of a previous run that you wish to use for the npde,
#' which will typically be a number, e.g. 1.
#' @param tad `r template("tad")`
#' @param binCov A character vector of the names of covariates which are included in the model, i.e. in the
#' model equations and which need to be binned.  For example `binCov='wt'` if "wt" is included in a
#' model equation like V=V0*wt, or `binCov=c( 'wt', 'crcl')` if both "wt" and "crcl"
#' are included in model equations.
#' @param doseC An integer with the number of dose/covariate bins to cluster, if known from a previous run of
#' this function.  Including this value will skip the clustering portion for doses/covariates.
#' @param timeC An integer with the number of observation time bins to cluster, if known from a previous run of
#' this function.  Including this value will skip the clustering portion for observation times.
#' @param tadC An integer with the number of time after dose bins to cluster, if known from a previous run of
#' this function.  Including this value will skip the clustering portion for time after dose. This argument
#' will be ignored if `tad=FALSE`.
#' @param limits Limits on simulated parameters. See [SIMrun].
#' @param \dots Other parameters to be passed to [SIMrun], especially `limits`.
#' @return The output of `makeValid` is a list of class `PMvalid`, which is a list with the following.
#' * simdata The combined, simulated files for all subjects using the population mean values and each subject
#' as a template. See [SIMparse]. This object will be automatically saved to the run, to be loaded with
#' [PMload] next time.
#' * timeBinMedian A data frame with the median times for each cluster bin.
#' * tadBinMedian A data frame with the median time after dose (tad) for each cluster bin.  This will be `NA` if
#' `tad = FALSE`.
#' * opDF A data frame with observations, predicitons, and bin-corrected predictions for each subject.
#' @author Michael Neely
#' @seealso [SIMrun], [plot.PMvalid]
#' @export

makeValid <- function(run, tad = F, binCov, doseC, timeC, tadC, limits, ...) {
  # verify packages used in this function
  if(!requireNamespace("mclust", quietly = TRUE)){
    stop("Install mclust package to perform clustering for validation.\n")
  }
  
  # save current wd
  currwd <- getwd()
  
  # get the run
  if (missing(run)) run <- readline("Enter the run number: ")
  PMload(run)
  
  getName <- function(x) {
    return(get(paste(x, run, sep = ".")))
  }
  
  # parse dots
  arglist <- list(...)
  namesSIM <- names(formals(SIMrun))
  # namesNPDE <- names(formals(autonpde))
  argsSIM <- arglist[which(names(arglist) %in% namesSIM)]
  
  # Cluster raw data --------------------------------------------------------
  
  # grab raw data file
  mdata <- getName("data")
  # remove missing observations
  missObs <- obsStatus(mdata$out)$missing
  if (length(missObs) > 0) mdata <- mdata[-missObs, ]
  
  # #get input and output max
  # maxInput <- max(mdata$input,na.rm=T)
  # maxOuteq <- max(mdata$outeq,na.rm=T)
  # if(outeq > maxOuteq){
  #   stop("You entered an output equation number greater than the number of output equations.\n")
  # }
  # if(input > maxInput){
  #   stop("You entered a drug input number greater than the number of drug inputs.\n")
  # }
  #
  # filter to include/exclude subjects
  if ("include" %in% names(argsSIM)) {
    includeID <- argsSIM$include
    mdata <- mdata[mdata$id %in% includeID, ]
    argsSIM[[which(names(argsSIM) == "include")]] <- NULL
  } else {
    includeID <- NA
  }
  if ("exclude" %in% names(argsSIM)) {
    excludeID <- argsSIM$exclude
    mdata <- mdata[!mdata$id %in% excludeID, ]
    argsSIM[[which(names(argsSIM) == "exclude")]] <- NULL
  } else {
    excludeID <- NA
  }
  
  # get time after dose
  if (tad) {
    valTAD <- calcTAD(mdata)
  }
  
  # number of subjects
  nsub <- length(unique(mdata$id))
  
  # define covariates in model to be binned
  covData <- getCov(mdata)
  if (covData$ncov > 0) { # if there are any covariates...
    if (missing(binCov)) {
      covInData <- getCov(mdata)$covnames
      cat(paste("Covariates in your data file: ", paste(getCov(mdata)$covnames, collapse = ", ")))
      binCov <- readline("Enter any covariates to be binned, separated by commas (<Return> for none): ")
      binCov <- unlist(strsplit(binCov, ","))
      # remove leading/trailing spaces
      binCov <- gsub("^[[:space:]]|[[:space:]]$", "", binCov)
    }
    if (!all(binCov %in% names(mdata))) {
      stop("You have entered covariates which are not valid covariates in your data.")
    }
    # ensure binCov has covariates in same order as data file
    covSub <- covData$covnames[covData$covnames %in% binCov]
    binCov <- covSub
  } else { # there are no covariates
    binCov <- NULL
  }
  
  # set up data for clustering
  # fill in gaps for cluster analysis only for binning variables (always dose and time)
  dataSub <- mdata[, c("id", "evid", "time", "out", "dose", "out", binCov)]
  # add time after dose
  if (tad) {
    dataSub$tad <- valTAD
  } else {
    dataSub$tad <- NA
  }
  dataSub <- dataSub %>% select(c("id", "evid", "time", "tad", "out", "dose", binCov))
  
  
  # restrict to doses for dose/covariate clustering (since covariates applied on doses)
  dataSubDC <- dataSub %>%
    filter(evid > 0) %>%
    select(c("id", "dose", binCov))
  
  # set zero doses (covariate changes) as missing
  dataSubDC$dose[dataSubDC$dose == 0] <- NA
  for (i in 1:nrow(dataSubDC)) {
    missingVal <- which(is.na(dataSubDC[i, ]))
    if (2 %in% missingVal) { # dose is missing
      if (i == 1 | (dataSubDC$id[i - 1] != dataSubDC$id[i])) { # first record for patient has zero dose
        j <- 0
        while (is.na(dataSubDC$dose[i + j])) { # increment until non-zero dose is found
          j <- j + 1
        }
        dataSubDC$dose[i] <- dataSubDC$dose[i + j] # set dose equal to first non-zero dose
        missingVal <- missingVal[-which(missingVal == 3)] # take out missing flag for dose as it has been dealt with
      }
    }
    dataSubDC[i, missingVal] <- dataSubDC[i - 1, missingVal]
  }
  # restrict to observations for time clustering
  dataSubTime <- dataSub$time[dataSub$evid == 0]
  # restrict to observations for tad clustering
  if (tad) {
    dataSubTad <- dataSub$tad[dataSub$evid == 0]
  }
  
  # ELBOW PLOT for clustering if used
  elbow <- function(x) {
    set.seed(123)
    # Compute and plot wss for k = 2 to k = 15.
    # set k.max
    if (is.null(dim(x))) {
      k.max <- min(length(unique(x)), 15)
    } else {
      k.max <- min(nrow(unique(x)), 15)
    }
    
    wss <- sapply(
      2:k.max,
      function(k) {
        val <- kmeans(x, k, nstart = 50, iter.max = 15)
        val$tot.withinss
      }
    )
    wss
    plot(2:k.max, wss,
         type = "b", pch = 19, frame = FALSE,
         xlab = "Number of clusters",
         ylab = "Total within-clusters sum of squares (WSS)"
    )
  }
  
  
  if (missing(doseC)) {
    # DOSE/COVARIATES
    cat("Now optimizing clusters for dose/covariates.\n")
    cat("First step is a Gaussian mixture model analysis, followed by an elbow plot.\n")
    readline(paste("Press <Return> to start cluster analysis for ",
                   paste(c("dose", binCov), collapse = ", ", sep = ""), ": ",
                   sep = ""
    ))
    cat("Now performing Gaussian mixture model analysis.")
    mod1 <- mclust::Mclust(dataSubDC)
    cat(paste("Most likely number of clusters is ", mod1$G, ".", sep = ""))
    readline("Press <Return> to see classification plot: ")
    plot(mod1, "classification")
    readline("Press <Return> to see elbow plot: ")
    elbow(dataSubDC)
    doseC <- as.numeric(readline(paste("Specify your dose/covariate cluster number, <Return> for ", mod1$G, ": ", sep = "")))
    if (is.na(doseC)) doseC <- mod1$G
  } # end if missing doseC
  
  # function to cluster by time or tad
  timeCluster <- function(timevar) {
    if (timevar == "time") {
      use.data <- dataSubTime
      timeLabel <- "Time"
      timePlot <- as.formula(out ~ time)
    } else {
      use.data <- dataSubTad
      timeLabel <- "Time after dose"
      timePlot <- as.formula(out ~ tad)
    }
    readline("Press <Return> to start cluster analysis for sample times: ")
    mod <- mclust::Mclust(use.data)
    cat(paste("Most likely number of clusters is ", mod$G, ".\n", sep = ""))
    readline("Press <Return> to see classification plot: ")
    plot(mod, "classification")
    readline("Press <Return> to see cluster plot: ")
    
    timeClusterPlot <- function() {
      plot(timePlot, dataSub, xlab = timeLabel, ylab = "Observation", xlim = c(min(use.data), max(use.data)))
    }
    
    # plot for user to see
    timeClusterPlot()
    timeClusters <- stats::kmeans(use.data, centers = mod$G, nstart = 50)
    abline(v = timeClusters$centers, col = "red")
    
    # allow user to override
    readline("Press <Return> to see elbow plot: ")
    elbow(use.data)
    ans <- readline(cat(paste("Enter:\n<1> for ", mod$G, " clusters\n<2> for a different number of automatically placed clusters\n<3> to manually specify cluster centers ", sep = "")))
    if (ans == 1) {
      TclustNum <- mod$G
    }
    if (ans == 2) {
      confirm <- 2
      while (confirm != 1) {
        TclustNum <- readline("Specify your sample time cluster number \n")
        mod <- mclust::Mclust(use.data, G = TclustNum)
        timeClusterPlot()
        timeClusters <- kmeans(use.data, centers = mod$G, nstart = 50)
        abline(v = timeClusters$centers, col = "red")
        confirm <- readline(cat("Enter:\n<1> to confirm times\n<2> to revise number of times\n<3> to manually enter times"))
        if (confirm == 3) {
          ans <- 3
          confirm <- 1
        }
      }
    }
    if (ans == 3) {
      confirm <- 2
      while (confirm != 1) {
        timeClusterPlot()
        timeVec <- readline("Specify a comma-separated list of times, e.g. 1,2,8,10: ")
        timeVec <- as.numeric(strsplit(timeVec, ",")[[1]])
        abline(v = timeVec, col = "red")
        confirm <- readline(cat("Enter:\n<1> to confirm times\n<2> to revise times "))
      }
      TclustNum <- timeVec
    }
    if (all(is.na(TclustNum))) TclustNum <- mod$G
    return(as.numeric(TclustNum))
  } # end timeCluster function
  
  # cluster by time and tad if appropriate
  if (missing(timeC)) {
    cat("Now clustering for actual sample times...\n")
    timeC <- timeCluster("time")
  } # end if missing timeC
  if (tad & missing(tadC)) {
    cat("Now clustering for time after dose...\n")
    tadC <- timeCluster("tad")
  }
  
  # now set the cluster bins
  dcClusters <- stats::kmeans(dataSubDC, centers = doseC, nstart = 50)
  dataSub$dcBin[dataSub$evid > 0] <- dcClusters$cluster # m=dose,covariate bins
  
  timeClusters <- stats::kmeans(dataSubTime, centers = timeC, nstart = 50)
  dataSub$timeBin[dataSub$evid == 0] <- sapply(timeClusters$cluster, function(x) which(order(timeClusters$centers) == x)) # n=ordered time bins
  
  if (tad) {
    tadClusters <- stats::kmeans(dataSubTad, centers = tadC, nstart = 50)
    dataSub$tadBin[dataSub$evid == 0] <- sapply(tadClusters$cluster, function(x) which(order(tadClusters$centers) == x)) # n=ordered time bins
  } else {
    dataSub$tadBin <- NA
  }
  
  # Simulations -------------------------------------------------------------
  
  # create /vpc
  if (!file.exists(paste(run, "/vpc", sep = ""))) dir.create(paste(run, "/vpc", sep = ""))
  
  # get model file
  instrfile <- suppressWarnings(tryCatch(readLines(paste(run, "etc/instr.inx", sep = "/")), error = function(e) NULL))
  if (length(grep("IVERIFY", instrfile)) == 0) { # not updated instruction file
    modelfile <- readline("Your run used an old instruction file. Enter model name: ")
  } else { # ok we are using updated instruction file
    if (length(instrfile) > 0) { # ok we got one
      # model.for file name
      modelfile <- instrfile[5]
      # convert to original name
      modelfile <- basename(Sys.glob(paste(run, "/inputs/", strsplit(modelfile, "\\.")[[1]][1], "*", sep = "")))
      if (length(modelfile) > 1) {
        modelfile <- modelfile[grep(".txt", modelfile)]
      }
    } else {
      stop("Model file not found.\n")
    }
  }
  
  # copy this modelfile to new /vpc folder
  invisible(file.copy(from = paste(run, "/inputs/", modelfile, sep = ""), to = paste(run, "/vpc", sep = "")))
  
  # now get the data file
  RFfile <- suppressWarnings(tryCatch(readLines(Sys.glob(paste(run, "outputs/??_RF0001.TXT", sep = "/"))), error = function(e) NULL))
  if (length(RFfile) > 0) {
    datafileName <- tail(RFfile, 1)
    # remove trailing spaces
    datafileName <- sub(" +$", "", datafileName)
    file.copy(from = paste(run, "inputs", datafileName, sep = "/"), to = paste(run, "/vpc", sep = ""))
    datafile <- datafileName
  } else {
    stop("Data file not found\n")
  }
  
  # change wd to new /vpc folder which now contains data and model files
  setwd(paste(run, "/vpc", sep = ""))
  
  # simulate PRED_bin from pop icen parameter values and median of each bin for each subject
  # first, calculate median of each bin
  dcMedian <- aggregate(dataSub[, c("dose", binCov)], by = list(dataSub$dcBin), FUN = median, na.rm = T)
  names(dcMedian)[1] <- "bin"
  timeMedian <- aggregate(dataSub$time, by = list(dataSub$timeBin), FUN = median)
  names(timeMedian) <- c("bin", "time")
  
  if (tad) {
    tadMedian <- aggregate(dataSub$tad, by = list(dataSub$tadBin), FUN = median)
    names(tadMedian) <- c("bin", "time")
  } else {
    tadMedian <- NA
  }
  
  # create  datafile based on mdata, but with covariates and doses replaced by medians
  # and sample times by bin times
  mdataMedian <- mdata
  mdataMedian$dcBin <- dataSub$dcBin
  mdataMedian$timeBin <- dataSub$timeBin
  # no need for tadBin as we don't simulate with tad
  mdataMedian$dose <- dcMedian$x[match(mdataMedian$dcBin, dcMedian$bin)]
  mdataMedian$time[mdataMedian$evid == 0] <- timeMedian$time[match(mdataMedian$timeBin[mdataMedian$evid == 0], timeMedian$bin)]
  covCols <- which(names(mdataMedian) %in% binCov)
  if (length(covCols) > 0) {
    for (i in covCols) {
      dcMedianCol <- which(names(dcMedian) == names(mdataMedian[i]))
      mdataMedian[, i] <- dcMedian[match(mdataMedian$dcBin, dcMedian$bin), dcMedianCol]
    }
  }
  # write median file
  MedianDataFileName <- paste(substr(paste("m_", strsplit(datafileName, "\\.")[[1]][1], sep = ""), 0, 8), ".csv", sep = "")
  PMwriteMatrix(mdataMedian[, 1:(ncol(mdataMedian) - 2)], MedianDataFileName, override = T)
  
  # remove old files
  invisible(file.remove(Sys.glob("sim*.txt")))
  
  # get poppar and make one with zero covariance
  poppar <- getName("final")
  popparZero <- poppar
  popparZero$popCov[popparZero$popCov != 0] <- 0
  # do the simulation for each subject using the median dose, median covariates and pop parameters
  if ("seed" %in% names(argsSIM)) {
    seed.start <- argsSIM$seed
    argsSIM[[which(names(argsSIM) == "seed")]] <- NULL
  } else {
    seed.start <- -17
  }
  set.seed(seed.start)
  if ("nsim" %in% names(argsSIM)) {
    nsim <- argsSIM$nsim
    argsSIM[[which(names(argsSIM) == "nsim")]] <- NULL
  } else {
    nsim <- 1000
  }
  if ("limits" %in% names(argsSIM)) {
    limits <- argsSIM$limits
    argsSIM[[which(names(argsSIM) == "limits")]] <- NULL
  } else {
    limits <- NA
  }
  argsSIM1 <- c(list(
    poppar = popparZero, data = MedianDataFileName, model = modelfile, nsim = 1,
    seed = runif(nsub, -100, 100), outname = "simMed"
  ), limits = limits, argsSIM)
  cat("Simulating outputs for each subject using population means...\n")
  flush.console()
  do.call("SIMrun", argsSIM1)
  
  # read and format the results of the simulation
  PRED_bin <- SIMparse("simMed*", combine = T, quiet = T)
  
  # make tempDF subset of PMop for subject, time, non-missing obs, outeq, pop predictions (PREDij)
  tempDF <- getName("op")
  tempDF <- tempDF[tempDF$pred.type == "pop", ]
  tempDF <- tempDF[obsStatus(tempDF$obs)$present, ] %>% filter(time > 0)
  if (!is.na(includeID[1])) {
    tempDF <- tempDF[tempDF$id %in% includeID, ]
  }
  if (!is.na(excludeID[1])) {
    tempDF <- tempDF[!tempDF$id %in% excludeID, ]
  }
  
  if (tad) {
    tempDF$tad <- rep(dataSub$tad[dataSub$evid == 0], 2)
  } else {
    tempDF$tad <- NA
  }
  
  
  # add PRED_bin to tempDF
  tempDF$PRED_bin <- rep(PRED_bin$obs$out[!is.na(PRED_bin$obs$out)], times = 2) # one for icen="median" and icen="mean"
  
  # add pcYij column to tempDF as obs * PREDbin/PREDij
  tempDF$pcObs <- tempDF$obs * tempDF$PRED_bin / tempDF$pred
  
  # #take out observations at time 0 (from evid=4 events)
  # tempDF <- tempDF[tempDF$time>0,]
  
  # bin pcYij by time and add to tempDF
  tempDF$timeBinNum <- rep(dataSub$timeBin[dataSub$evid == 0], times = 2) # one for each icen
  tempDF$timeBinMedian <- timeMedian$time[match(tempDF$timeBinNum, timeMedian$bin)]
  if (tad) {
    tempDF$tadBinNum <- rep(dataSub$tadBin[dataSub$evid == 0], times = 2)
    tempDF$tadBinMedian <- tadMedian$time[match(tempDF$tadBinNum, tadMedian$bin)]
  } else {
    tempDF$tadBinNum <- NA
    tempDF$tadBinMedian <- NA
  }
  
  
  # Now, simulate using full pop model
  # write the adjusted mdata file first
  PMwriteMatrix(mdata, datafileName, override = T)
  
  set.seed(seed.start)
  argsSIM2 <- c(list(
    poppar = poppar, data = datafileName, model = modelfile, nsim = nsim,
    seed = runif(nsub, -100, 100), outname = "full"
  ), limits = limits, argsSIM)
  if (!is.na(includeID[1])) {
    argsSIM2$include <- includeID
  }
  if (!is.na(excludeID[1])) {
    argsSIM2$exclude <- excludeID
  }
  do.call("SIMrun", argsSIM2)
  # read and format the results of the simulation
  simFull <- SIMparse("full*", combine = T, quiet = T)
  # take out observations at time 0 from evid=4
  simFull$obs <- simFull$obs[simFull$obs$time > 0, ]
  # add TAD for plotting options
  if (tad) {
    simFull$obs$tad <- unlist(tapply(dataSub$tad[dataSub$evid == 0], dataSub$id[dataSub$evid == 0], function(x) rep(x, nsim)))
  }
  
  
  
  # pull in time bins from tempDF; only need median as tempDF contains median and mean,
  # but simulation is only from pop means
  
  simFull$obs$timeBinNum <- unlist(tapply(tempDF$timeBinNum[tempDF$icen == "median"], tempDF$id[tempDF$icen == "median"], function(x) rep(x, nsim)))
  # pull in tad bins from tempDF
  simFull$obs$tadBinNum <- unlist(tapply(tempDF$tadBinNum[tempDF$icen == "median"], tempDF$id[tempDF$icen == "median"], function(x) rep(x, nsim)))
  # make simulation number 1:nsim
  simFull$obs$simnum <- as.numeric(sapply(strsplit(simFull$obs$id, "\\."), function(x) x[1]))
  class(simFull) <- c("PMsim", "list")
  
  # NPDE --------------------------------------------------------------------
  
  
  # get npde from github
  checkRequiredPackages("npde", repos = "LAPKB/npde")
  
  # prepare data for npde
  obs <- tempDF[tempDF$icen == "mean", c("id", "time", "obs")]
  
  # remove missing obs
  obs <- obs[obs$obs != -99, ]
  names(obs)[3] <- "out"
  
  simobs <- simFull$obs
  # remove missing simulations
  simobs <- simobs[simobs$out != -99, ]
  simobs$id <- rep(obs$id, each = nsim)
  
  # get NPDE
  assign("thisobs", obs, pos = 1)
  assign("thissim", simobs, pos = 1)
  npdeRes <- tryCatch(npde::autonpde(namobs = thisobs, namsim = thissim, 1, 2, 3, verbose = T), error = function(e) {
    e
    return(NA)
  })
  
  
  
  
  
  
  # Clean Up ----------------------------------------------------------------
  
  valRes <- list(simdata = simFull, timeBinMedian = timeMedian, tadBinMedian = tadMedian, opDF = tempDF, npde = npdeRes)
  class(valRes) <- c("PMvalid", "list")
  
  # save it back to run so it can be loaded in the future
  NPAGout <- list(
    NPdata = getName("NPdata"),
    pop = getName("pop"),
    post = getName("post"),
    final = getName("final"),
    cycle = getName("cycle"),
    op = getName("op"),
    cov = getName("cov"),
    mdata = getName("data"),
    valid = valRes
  )
  save(NPAGout, file = "../outputs/NPAGout.Rdata")
  
  # #put sim in global environment
  # assign(paste("sim",as.character(run),sep="."),simFull,pos=1)
  
  setwd(currwd)
  return(valRes)
}