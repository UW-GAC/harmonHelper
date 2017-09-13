#' @param dat A dataframe
#' @param outcome name in dat for the quantitative trait
#' @param covarsF names in dat for covariates that are to be factors
#' @param covarsN names in dat for other covariates
#' @param byVar grouping variable of interest
#' @param type type of plot: should be "box" or "violin"
#' @param subj.include subject IDs for subjects to include; if NULL, all subjects in dat will be used
#' @param fw Variable to facet by
#' @param title brief text to prepend to automatic title
#' @export
#' @import ggplot2
#' @import dplyr
#' @import magrittr
qc_plot <- function(dat,
                    outcome,
                    covarsF,
                    covarsN,
                    byVar,
                    type,
                    subj.include = NULL,
                    fw,
                    title = NULL){ 

# type = "box" yields box plots with sample sizes recorded below the box plots
# type = "violin" yields violin plots with variances recorded below the violin plots
# returns ggplot object

  covars <- c(covarsF,covarsN)
  if (!"data.frame" %in% class(dat)) stop("data must be data.frame")
  if (!all(c(outcome,covars,byVar) %in% names(dat))) stop("some designated variable is not in data")
  if (!type %in% c("box","violin")) stop("type must be box or violin")

  if (!is.element(class(dat[,outcome]),c("numeric","integer"))) stop("outcome must be numeric")

  vars <- c(covarsF,byVar)
  for(v in vars){
    dat[,v] <- as.factor(dat[,v])
  }
 
  if (is.null(subj.include)) subj.include <- dat$topmed_subject_id

  dat %<>% filter(topmed_subject_id %in% subj.include)

  model <- paste(outcome, "~",paste(covars,collapse = " + "))
  mod <- lm(model,dat)
  dat$resid <- mod$residuals

  rm <- "resid"
  mn <- min(dat[,rm], na.rm = T)
  mx <- max(dat[,rm],na.rm = T)
  yL <- mn-0.15*abs(mn)

  if (!is.null(title)){
     title  <- paste0(title," : residuals by ",byVar,"\nmodel = ",model)
  } else {
     title <- paste0("residuals by ",byVar,"\nmodel = ",model)
  }
cat(paste("plotting", title, "\n\n"))

  g <- ggplot(dat, aes_string(x = byVar,y = rm)) + 
         ggtitle(title)
         ylim(yL,mx) 

   if (hasArg(fw)){
     g <- g + facet_wrap(fw, ncol = 1)
   }

  if (type == "box"){
      g <- g + geom_boxplot(varwidth = TRUE) + 
        geom_text(aes(label = prettyNum(..count.., big.mark = ",")), 
                  y = yL, stat = "count", vjust = -1.2, color = "black",size = 4) 
  } else {
      g <- g + geom_violin()
      if (hasArg(fw)){
          g <- g + stat_summary(fun.data = .ggVar, geom = "text")

      } else {
          vr <- signif (tapply(dat[,rm],dat[,byVar],var),4)
          vr <- vr[!is.na(vr)]
          nvr <- length(vr)
          g <- g +
            annotate("text",x = 1:nvr, y = rep(yL,nvr), label = vr,vjust = -1.2, 
                     color = "black",size = 4) 
      }
   }
  return(g)
}
