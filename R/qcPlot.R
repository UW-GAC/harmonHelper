#' Fit a model and plot the residuals
#' @param dat A dataframe
#' @param outcome name in dat for the quantitative trait
#' @param covarsF names in dat for covariates that are to be factors
#' @param covarsN names in dat for other covariates
#' @param byVars grouping variables of interest
#' @param type type of plot: should be "box" or "violin"
#' @param subj.include subject IDs for subjects to include; if NULL, all subjects in dat will be used
#' @param title brief text to prepend to automatic title
#' @export
#' @import ggplot2
#' @import dplyr
#' @import magrittr
qcPlot <- function(dat,
                   outcome,
                   covarsF,
                   covarsN,
                   byVars,
                   type,
                   subj.include = NULL,
                   title = NULL,
                   skewed){ 

# type = "box" yields box plots with sample sizes recorded below the box plots
# type = "violin" yields violin plots with variances recorded below the violin plots
# returns ggplot object

    covars <- c(covarsF, covarsN)
    if (!"data.frame" %in% class(dat)) stop("data must be data.frame")

    if (!all(c(outcome, covars, byVars) %in% names(dat))){
        stop("some designated variable is not in data")
    }

    if (!type %in% c("box", "violin")) stop("type must be box or violin")

    if (!is.element(class(dat[,outcome]), c("numeric", "integer"))){
        stop("outcome must be numeric")
    }

    vars <- c(covarsF, byVars)
    for(v in vars){
        dat[, v] <- as.factor(dat[, v])
    }
 
    if (is.null(subj.include)) subj.include <- dat$topmed_subject_id

    dat %<>% filter(topmed_subject_id %in% subj.include)

    model <- paste(outcome, "~", paste(covars,collapse = " + "))
    mod <- lm(model, dat)
    dat$resid <- mod$residuals

    rm <- "resid"
    mn <- min(dat[, rm], na.rm = T)
    mx <- max(dat[, rm], na.rm = T)
    if (skewed){
        q1 <- quantile(dat[, rm], 0.25, na.rm = TRUE)
        q3 <- quantile(dat[, rm], 0.75, na.rm = TRUE)
        iqr <- IQR(dat[, rm], na.rm = TRUE)
        mn %<>% max(q1 - 4 * iqr)
        mx %<>% min(q3 + 4 * iqr)
    } 
    yL <- mn - 0.15 * abs(mn)

    gtitle <- paste0("residuals by ", byVars, "\nmodel = ", model)

    if (!is.null(title)){
         gtitle <- paste(title, gtitle, sep = ": ")
    } 

cat(paste("plotting", gtitle, "\n\n"))

    g <- ggplot(dat, aes_string(x = byVars,y = rm)) + 
        ylim(yL,mx) 

    if (length(byVars) > 1){
        fw <- byVars[2:length(byVars)]
            g <- g + facet_wrap(fw, ncol = 1) 
   }


  if (type == "box"){
      gtitle <- paste0(gtitle, "\nlabel: n")
      g <- g + ggtitle(gtitle) +
          geom_text(aes(label = prettyNum(..count.., big.mark = ",")), 
                    y = yL, stat = "count", vjust = -1.2, 
                    color = "black", size = 4)
          if (skewed){
              g <- g + geom_boxplot(varwidth = TRUE, coef = 4, outlier.shape = NA)
          } else {
              g <- g + geom_boxplot(varwidth = TRUE)
          }
  } else {
    gtitle <- paste0(gtitle, "\nlabel: (n; variance)")
    .ggVar <- function(x){ 
        # As a work around to issues with NSE in ggplot2, this function is 
        # is defined here with the value for yL hardcoded in
        return(data.frame(y = yL, 
            label = paste0("(",
                           prettyNum(length(x), big.mark = ","),
                           "; ",
                           prettyNum(round(var(x, na.rm = T), 2), big.mark = ","),
                           ")")))
    }
      g <- g + geom_violin() + 
          ggtitle(gtitle) +
          stat_summary(fun.data = .ggVar, geom = "text")
   }
  return(g)
}
