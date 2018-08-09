library(dplyr)


setwd("C:\\PROJETOS\\avaliacao_pares_professor")
dados <- read.csv("teste.CSV", sep = ";")

dados$professor <- factor(dados$professor)
dados$Aluno <- factor(dados$Aluno)

dadosF <- dados %>% 
  group_by(professor) %>%
  summarise(desvio = sd(R1))


library(KappaGUI)

# Here we create and display an artifical dataset,
# describing two traits coded by two raters:
scores <- data.frame(
  Professor1_A = c(1,4,5,2,3,5,1,2,3,4),
  Professor2_A = c(1,4,5,2,3,0,1,2,3,4),
  Professor3_A = c(1,4,5,2,3,5,1,2,3,4)
)
# Retrieve Cohen's kappa for Trait1 and Trait2,
# to evaluate inter-rater agreement between raters A and B:
res <- kappaCohen(scores, weight="unweighted")
kappaCohen(scores, weight="squared")