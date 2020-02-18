library(readxl)
library(tidyverse)
library(ggplot2)


data<- read_xlsx("manual_est_2019_curso.xlsx")

data<- data %>% mutate(Region= as.factor(Region), 
                       ID_Comglomerado= as.factor(ID_Comglomerado),
                       N_SPF= as.factor(N_SPF)) %>% 
  mutate(altura= Dst_2*(tan(Vmas_2*pi/180) - tan(Vmenos_2*pi/180)))


modelo<- lm(altura ~ DAP_analisis, data)

anova<-anova(modelo)

broom::tidy(modelo)

broom::augment(modelo) %>% ggplot(., mapping = aes(x= .fitted, y= altura)) +
  geom_point() + geom_abline(slope = 0.34, intercept = 8.82)


data<- read.csv("Sin título 1.csv")


modelo<- lm(alt ~ dap, data)

data<- data %>% summarise(ss= ((sum(alt*dap)) - (sum(dap)*sum(alt))/10)^2,
                          ss2= sum(dap^2)- (sum(dap))^2/10,
                          sst= sum(alt^2)-(sum(alt))^2/10)
                                                              
anova(modelo)

e<- lm(log(altura) ~ log(DAP_analisis), data)

en<- nls(altura ~ a*DAP_analisis^b, data = data, start = list(a= exp(1.13), b= exp(0.524)))

cu<- lm(altura ~ DAP_analisis + I(DAP_analisis^2), data)

ggplot(data, mapping = aes(x= DAP_analisis, y= altura)) + 
  geom_point() + 
  geom_smooth(data= broom::augment(en), 
              mapping = aes(x= DAP_analisis, y= .fitted)) +
  geom_smooth(data= broom::augment(cu), 
              mapping = aes(x= DAP_analisis, y= .fitted), col= "red")
plot(en)







