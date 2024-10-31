## carregar pacotes
library(dplyr)
library(ggplot2)
#importar conjunto de dados


proc <- read.csv2("proc.csv", header = T)

#inspecionar dados

head(proc)

#conhecer variaveis


str(proc)


# Visualizar a distribuiÃ§Ã£o
ggplot(data = proc, aes(x = quantidade, y = idade)) +
  geom_smooth(method = lm) +
  labs(x= "quantidade", y= "idade")


##Média, mediana e desvio padrão de idade
proc %>%
  group_by(idade)%>%
  summarise(meanidd = mean(quantidade),
            medianidd = median(quantidade),
            dpidd = sd(quantidade))

###Média, mediana e desvio padrão de peso
proc %>%
  group_by(peso)%>%
  summarise(meanps = mean(quantidade),
            medianps = median(quantidade),
            dpps = sd(quantidade))

##Média, mediana e desvio padrão de IG
proc %>%
  group_by(ig)%>%
  summarise(meanig = mean(quantidade),
            medianig = median(quantidade),
            dpig = sd(quantidade))
##Média, mediana e desvio padrão de processos fonológicos
proc %>%
  group_by(processos.fonologicos)%>%
  summarise(meanidd = mean(quantidade),
            medianidd = median(quantidade),
            dpidd = sd(quantidade))

##Média, mediana e desvio padrão de tipo de processos
proc %>%
  group_by(tipo)%>%
  summarise(meanTon = mean(quantidade),
            medianTon = median(quantidade),
            dpRT = sd(quantidade))


##Carregar pacotes
library(lme4)
library(lmerTest)

##Analise de regress?o multinivel tendo x=quantidade, y=demais vari?veis
modelo1 <- lmer(quantidade~idade+peso+ig+genero+(1|participantes)+(1|coletas), data=proc, REML = F)
summary(modelo1)
modelo2 <- lmer(quantidade~idade+peso+ig+(1|participantes)+(1|coletas), data=proc, REML = F)
anova (modelo1, modelo2)
summary(modelo2)
## Genero não é significativo, modelo 2 melhor
modelo3 <- lmer(quantidade~idade+peso+(1|participantes)+(1|coletas), data=proc, REML = F)
anova(modelo2, modelo3)
summary(modelo3)
##IG não é relevante, modelo 3 melhor
modelo4 <- lmer(quantidade~idade+(1|participantes)+(1|coletas), data=proc, REML = F)
anova(modelo4, modelo3)
summary(modelo4)
##Peso não é significativo, melhor 4 melhor. Apenas idade é significativo na comparação dos modelos 1 a 4.

modelo5 <- lmer(quantidade~idade+processo+processos.fonologicos+(1|participantes)+(1|coletas), data=proc, REML = F)
summary(modelo5)
modelo6 <- lmer(quantidade~idade+processos.fonologicos+(1|participantes)+(1|coletas), data=proc, REML= F)
anova(modelo5, modelo6)
##modelo 5 é mais significativo.

modelo7 <- lmer(quantidade~idade+processo+(1|participantes)+(1|coletas), data=proc, REML = F)
anova(modelo5, modelo7)

##modelo 5 é mais significativo.
modelo8 <- lmer(quantidade~processo+processos.fonologicos+(1|participantes)+(1|coletas), data=proc, REML = F)
anova(modelo5,modelo8)
###Melhor modelo 5
confint.merMod(modelo5)

fitted.values(m5)
residuals(m5)

##carregar pacote
library(effects)
##plotar efeito
efeitoprocesso <- effect("processo",modelo5)
plot(efeitoprocesso)
efeitoprocfono <- effect("processos.fonologicos", modelo5)
plot(efeitoprocfono)
efeitoidade <- effect("idade",modelo5)
plot(efeitoidade)
library(visreg)
visreg(modelo5, "idade",by= "processos.fonologicos", overlay= TRUE, scale = c ("response"))

efeitopeso <- effect ("peso", modelo2)
plot(efeitopeso)
efeitoig <- effect("ig",modelo2)
plot(efeitoig)
summary(modelo2)
