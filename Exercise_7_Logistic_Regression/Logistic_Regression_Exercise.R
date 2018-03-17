NH11 <- readRDS("dataSets/NatHealth2011.rds")
labs <- attributes(NH11)$labels

## Exercise:

str(NH11$everwrk)
levels(NH11$everwrk)
summary(NH11$everwrk)

str(NH11$age_p)
summary(NH11$age_p)

str(NH11$r_maritl)
levels(NH11$r_maritl)
summary(NH11$r_maritl)

hyp.out2 <- glm(everwrk~age_p+r_maritl, data=NH11, family="binomial")
summary(hyp.out2)

hyp.out.tab2 <- coef(summary(hyp.out2))
hyp.out.tab2[, "Estimate"] <- exp(coef(hyp.out2))
hyp.out.tab2[, "Estimate"]

plot(allEffects(hyp.out2))
