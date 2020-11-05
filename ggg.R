#meds <- c(by(data$DAP_cm, data$Finca, median))

#ggplot(data, mapping = aes(x= Finca, y= DAP_cm, label= Grupos)) + geom_boxplot() + 
#geom_text(data=data.frame(), aes(x=names(meds), y=meds, label= c("a", "c", "d", "d", "b", "d")))

#corrplot(M, method = "color", type = "upper", addCoef.col = "black", 
#tl.col = "black", tl.srt = 25)

datos<- read_xlsx("parcial1.xlsx")
datos<- datos %>% select(Parcela, 
                         Finca, ...4) %>% 
  rename(DAP_cm= Parcela,
         Parcela= Finca,
         Finca= ...4) %>% 
  mutate(Parcela = as.factor(Parcela),
         Finca= as.factor(Finca))

model<- aov(DAP_cm ~ Finca, datos)

summary(model)

agricolae::HSD.test(model, "Finca", console = T)

pr<- prcomp(data2[, -1], scale. = TRUE)
biplot(pr, scale = 0, cex = 0.5, col = c("dodgerblue3", "deeppink3"))
data2<- spread(data2, Individuo, `%W`)

     