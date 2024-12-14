rm(list=ls())
library(tidyverse)

#carichiamo i dati ricavati dalle simulazioni
data=read.table('dati_R.txt', header=TRUE)
str(data)
plot(data$p_in, data$phyto, xlab="Inorganic phosporous [µg/L]", ylab = "Peak phytoplankton concentration [µg/L]", pch=16)

#applichiamo una regressione lineare ai dati
fit=lm(data=data, phyto ~ p_in)
fit
summary(fit)

#applichiamo una regressione polinomiale
fit1=lm(data=data, phyto ~ poly(p_in, 2, raw=TRUE))
fit1
summary(fit1)

fit1$coefficients

#plot delle linee di tendenza
#level=0 toglie la rappresentazione grafica dell'intervallo di confidenza al 95%
library(ggplot2)
ggplot(data, aes(p_in, phyto)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = F, aes(colour="Linear")) +                 
  geom_smooth(method = "lm", formula = y ~ poly(x,2), level=0, aes(colour="Polynomial")) +
  labs(x="Inorganic phosporous [µg/L]", y="Peak phytoplankton concentration [µg/L]") +
  scale_colour_manual(name="Legend", values=c("blue", "red"))
