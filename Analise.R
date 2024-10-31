## carregar pacotes
library(dplyr)
library(ggplot2)
#importar conjunto de dados

bd1 <- read.csv2("bd1.csv", header=T)

#inspecionar dados

head(bd1)

#conhecer variaveis

str(bd1)


##Média, mediana e desvio padrão de dos segmentos
bd1 %>%
  group_by(tipo, tonicidade)%>%
  summarise(segts = n())%>% mutate(frequencia = segts/sum(segts)) %>%
  summarise (mean = mean(segts),
             median = median(segts),
             dp = sd(segts)) %>% write.table()

bd1 %>%
  group_by(idade,tipo, tonicidade)%>%
  summarise(segts = n())%>% mutate(frequencia = segts/sum(segts))%>% write.table()


##Carregar pacotes
library(lme4)
library(lmerTest)

regr1 <- lmer(idade~peso+ig+(1|participantes), data=bd1, REML = F)
summary(regr1)
regr2 <- lmer(idade~ig+(1|participantes), data=bd1, REML = F)
summary(regr2)
anova (regr1,regr2)
##REgr2 Ã© melhor que regr1, peso nÃ£o Ã© significativo.

regr3 <- lmer(idade~tonicidade+tipo+traço+(1|participantes), data=bd1, REML = F)
summary(regr3)

regr4 <- lmer(idade~tonicidade+tipo+(1|participantes), data=bd1, REML=F)
summary(regr4)
anova (regr3, regr4)
##REgr4 Ã© melhor que regr3.

regr5 <- lmer(idade~tipo+(1|participantes), data=bd1, REML = F)
summary(regr5)
anova(regr4, regr5)
## Regr4 é significativo tipo e traço 

regr6 <- lmer(idade~genero+ig+(1|participantes), data=bd1, REML= F)
summary(regr6)
anova(regr2, regr6)

#Regr2 é melhor que regr6. Sexo não é singificativo.

regr7 <- lmer(idade~tonicidade+(1|participantes), data=bd1, REML = F)
anova(regr4,regr7)

#Regr4 é significativo, tonicidade e tipo juntos.

regr8 <- lmer(idade~traço+(1|participantes), data=bd1, REML=F)
summary(regr8)

regr9  <- lmer(idade~genero+(1|participantes), data=bd1, REML = F)
anova(regr6, regr9)

##IG nÃo é significativo., regr9 mais significativo

regr10 <- lmer(idade~tonicidade+pl+ns+lq+fr+(1|participantes), data=bd1, REML = F)
summary(regr10)

regr11 <- lmer(idade~pl+ns+lq+fr+(1|participantes), data=bd1, REML = F)
anova(regr10, regr11)
summary(regr10)


##Regr10 é mais significativo 

confint.merMod(regr4)
confint.merMod(regr10)

fitted.values(m9)
residuals(m9)
# Biblioteca para plotar efeitos de interaÃ§Ã£o
library(effects) 
# Plotar o efeito da interaÃ§Ã£o do modelo de regressÃ£o multÃ�nivel
efeitoplosiva <- effect("pl",regr10)
plot(efeitoplosiva)
efeitonasal <- effect("ns", regr10)
plot(efeitonasal)
efeitofricativa <- effect("fr", regr10)
plot(efeitofricativa)
efeitoliquida <- effect("lq", regr10)
plot(efeitoliquida)






