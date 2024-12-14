rm(list=ls())
library(tidyverse)

#stessa procedura del caso generale ma in questo script facciamo riferimento solo ai dati relativi all'epilimnio

data=read.table('epilimnio.txt', header=TRUE)
str(data)
plot(data$p_in, data$phyto, xlab="Inorganic phosporous [µg/L]", ylab = "Peak phytoplankton concentration [µg/L]", pch=16)

#regressione lineare
fit=lm(data=data, phyto ~ p_in)
fit
summary(fit)

#regressione polinomiale
fit1=lm(data=data, phyto ~ poly(p_in, 2, raw=TRUE))
fit1
summary(fit1)

fit1$coefficients

#plot delle linee di tendenza
#level=0 toglie la rappresentazione grafica dell'intervallo di confidenza al 95%
ggplot(data, aes(p_in, phyto)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = "lm", se = F, aes(colour="Linear")) +                 
  geom_smooth(method = "lm", formula = y ~ poly(x,2), level=0, aes(colour="Polynomial")) +
  labs(x="Inorganic phosporous [µg/L]", y="Peak phytoplankton concentration [µg/L]") +
  scale_colour_manual(name="Legend", values=c("blue", "red"))
