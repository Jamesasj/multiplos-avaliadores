library(ggplot2)
library(dplyr)
library(KappaGUI)

setwd("C:\\PROJETOS\\avaliacao_pares_professor")
lista_doc <- c("professor1.csv", "professor2.csv")

dados1 <- read.csv("professor1.csv", sep = ";")
dados2 <- read.csv("professor2.csv", sep = ";")

varl <- data_frame(x = dados2[,7])
var2 <- data_frame(x = dados1[,7])
scala <- data.frame(x =c(0:1000,100))
jpeg("01_GERAL.jpg")
plot <- ggplot()
plot + 
  stat_function(data = varl, aes(x = scala),fun = dnorm, args = list(mean(varl$x), sd(varl$x)), colour = "blue") +
  geom_vline(xintercept = mean(varl$x), colour = "blue",  linetype = "dashed") +
  stat_function(data = var2, aes(x = scala),fun = dnorm, args = list(mean(var2$x), sd(var2$x)), colour = "red")  +
  geom_vline(xintercept = mean(var2$x), colour = "red", linetype = "dashed") 
dev.off()

dadosC <- data.frame(professor1 = as.character(), professor2 =as.character(), nota = as.character(), kappa=as.numeric())

for (n in 2:7) {
  scores <- data.frame(
    Professor1_N = dados1[,n],
    Professor2_N = dados2[,n]
  )
  res <- kappaCohen(scores, weight="squared")
  registro <- data.frame(professor1 = 1, professor2 = 2, nota = n, kappa = res$Kappa)
  dadosC <- rbind(dadosC, registro)
}


