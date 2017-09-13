.ggVar <- function(x){
 return(data.frame(y = min(x) - 0.15 * abs(min(x)), 
                   label = round(var(x, na.rm = T), 2)))
}
