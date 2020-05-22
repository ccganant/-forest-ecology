adjust<- function(Clas, Count,  distr, interval){
  Chicl<- NULL
  Chitab<-qchisq(0.95, NROW(Clas)-1, ncp = 0, lower.tail = T, log.p = FALSE)
  
  faesx<-function(x){ 
    n<- length(x)
    fres<- numeric(n)
    for (i in 1:n){
      fres[i]<- x[i+1]-x[i]
    }
    result <- c(x[1],fres)
    return(result)}
  
  if (distr == "Gamma"){
    Frect<- sum(Count)
    medp<- Clas%*%Count/Frect
    varp<-sum(Count*(Clas-medp)^2/(Frect-1))
    desp<-sqrt(varp)
    scale<-varp/medp
    alfa<-medp/scale
    
    probg<-pgamma(Clas+interval, alfa, scale= scale, lower.tail = TRUE, log.p = FALSE)
    Freqesp<-probg*Frect
    
    Fesp<- faesx(Freqesp)
    
    Fesp<- dplyr::filter(as.tibble(Fesp), value != "NA")
    
    Chicl<- sum((Count-Fesp)^2/Fesp)}
  
  if (distr == "Beta"){
    Frect<- sum(Count)
    medp<- Clas%*%Count/Frect
    varp<-sum(Count*(Clas-medp)^2/(Frect-1))
    
    a<- Clas[1]-1.5
    b<- Clas[length(Clas)]+1.5
    
    xpri<- (Clas-a)/(b-a)
    
    mur<- Count%*%xpri/Frect
    varr<- sum(Count%*%(xpri-mur)^2/(Frect-1))
    
    gar<- ((1-mur)/varr)*(mur*(1-mur)-varr)
    alr<- mur*gar/(1-mur)
    max<- (alr-1)/(alr+gar-2)
    
    probbeta<-pbeta(Clas+max, alr,gar, lower.tail = TRUE, log.p = FALSE)
    
    faes<-Frect*probbeta
    fresc<-faesx(faes)
    
    fresc<- dplyr::filter(as.tibble(fresc), value != "NA")
    vz<- (Count-fresc)^2/fresc
    
    vz<- dplyr::mutate(as.tibble(vz),
                       value= if_else(value == "Inf" | value == "NaN", 0, value))
    
    Chicl<- sum(vz)}
  
  if (distr == "Lognormal"){
    
    data100<- NULL
    for (i in 1:NROW(Clas)){
      data<- dplyr::tibble(d= rep(Clas[i]*100, Count[i]))
      
      data100<- rbind(data100, data)}
    
    data100<- dplyr::filter(data100, d != 0)
    ajlogn<- MASS::fitdistr(data100$d,"lognormal")
    mu<- ajlogn$estimate[1]
    sdl<- ajlogn$estimate[2]
    
    
    probln<- plnorm(log(Clas),mu,sdl, lower.tail = TRUE, log.p = F)
    
    faes<- probln*sum(Count)
    
    fres<- faesx(faes)
    fres<- dplyr::filter(as.tibble(fres), value != "NA" )
  
    
    vz<- (Count-fres)^2/fres
    
    vz<- dplyr::mutate(as.tibble(vz),
                       value= if_else(value == "Inf" | value == "NaN", 0, value))
    
    Chicl<- sum(vz,na.rm=T)}
  
  if (distr == "Weibull"){
    
    Clas<- Clas+1
    
    weibmv<- MASS::fitdistr(Clas,"weibull")
    
    a<- weibmv$estimate[1]
    b<- weibmv$estimate[2]
    
    prowei <- pweibull(Clas, shape = a, scale = b,
                      lower.tail = T, log.p=FALSE)
    
    faestwei <- sum(Count)*prowei
    
    fwei <- faesx(faestwei)
    fwei<- dplyr::filter(as.tibble(fwei), value != "NA")
    
    pchiwei<-(Count-fwei)^2/fwei
    Chicl<-sum(pchiwei,na.rm=T)}
  
  result<- matrix(c(Chitab, Chicl), nrow = 2, ncol = 1)
  
  row.names(result)<- c("Chitab", "Chicl")
  colnames(result)<- c(distr)
  
  return(result)
}

