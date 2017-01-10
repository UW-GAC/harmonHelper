#' Get encoded values by trait ID
#' @param data a dataframe
#' @param wbc The name of the column with WBC measurements
#' @param lymph The name of the column with lymphocyte measurements
#' @param mono The name of the column with monocyte measurements
#' @param neutro The name of the column with neutrophil measurements
#' @param eosin The name of the column with eosinophil measurements
#' @param baso The name of the column with basophil measurements
#' @export

wbcCheck<-function(data,wbc="wbc_ncnc_bld",lymph="lymphocyte_ncnc_bld",mono="monocyte_ncnc_bld",
             neutro="neutrophil_ncnc_bld",eosin="eosinophil_ncnc_bld",baso="basophil_ncnc_bld"){
  if(class(data) != "data.frame") stop("data needs to be a dataframe")
  nms<-c(wbc,lymph,mono,neutro,eosin,baso)
  print(nms)
  if(!all(is.element(nms,names(data)))) stop("missing required variables")
  nms2<-c("wbc","lymph","mono","neutro","eosin","baso")
  for(i in 1:6){
    names(data)[names(data)==nms[i]]<-nms2[i]
  }
  print(names(data))
  out<-vector("list",length=3)
  names(out)<-c("wbc.na","sum_withNA_bad","sum_noNA_bad")
  n<-is.na(data$wbc)
  n2<- !is.na(data$lymph) | !is.na(data$mono) | !is.na(data$neutro) | !is.na(data$eosin) | !is.na(data$baso)
  nq<-n2 & n
  out[[1]]<-sum(nq)

  sel<-!n &n2
  n3<- is.na(data$lymph) | is.na(data$mono) | is.na(data$neutro) | is.na(data$eosin) | is.na(data$baso)
  sel2<-sel & n3
  tmp<-data[sel2,]
  tmp3<-data[sel2,c("lymph","mono","neutro","eosin","baso")]
  tmp3$sum<-rowSums(tmp3,na.rm=TRUE)
  dif<-(tmp3$sum - tmp$wbc)/tmp$wbc
  ck<- dif > 0.05
  out[[2]]<-sum(ck)

  n4<- !is.na(data$lymph) & !is.na(data$mono) & !is.na(data$neutro) & !is.na(data$eosin) & !is.na(data$baso)
  sel3<-!n & n4
  tmp<-data[sel3,]
  tmp4<-tmp[,c("lymph","mono","neutro","eosin","baso")]
  tmp4$sum<-rowSums(tmp4,na.rm=TRUE)
  dif<-abs(tmp4$sum - tmp$wbc)/tmp$wbc
  ck<-dif > 0.05
  out[[3]]<-sum(ck)

  return(out)
}



