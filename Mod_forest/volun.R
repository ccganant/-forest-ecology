library(tidyverse)
library(readxl)

data<- read_xlsx("volumen.xlsx")

smalian<- function(D, H){
  x<- c()
  for (i in 1:(NROW(D)-1)){
    val<- (pi/80000)*((D[i]^2+D[i+1]^2))*(H[i+1]-H[i])
    x[i]<- val
  }
  return(x)
}

data1<- data %>% mutate(No_Arbol= as.factor(No_Arbol)) %>% 
  group_by(No_Arbol) %>% 
  summarise(sma= sum(smalian(`Di_vble (cm)`, `Alt_vble (m)`)))

if(exists("myFirstFun", mode = "function"))
  source("MyUtils.R")


