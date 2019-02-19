##  PROJECT:  rebooTZ
##  AUTHOR:   A.Chafetz | USAID
##  PURPOSE:  create fcn to convert 1,000 to 1 k
##  DATE:     2019-02-19
##  UPDATED:  


#thousand format to match % spacing on above graph
ks <- function (x) { 
  number_format(accuracy = 1,
                scale = 1/1000,
                suffix = "k",
                big.mark = ",")(x) 
}