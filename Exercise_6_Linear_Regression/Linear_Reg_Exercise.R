states.data <- readRDS("dataSets/states.rds") 
states.info <- data.frame(attributes(states.data)[c("names", "var.labels")])

## Exercise 1:

names(states.data)

cor(na.omit(subset(states.data, select = c("energy", "metro"))))
plot(states.data$energy, states.data$metro)

energy_mod <- lm(energy ~ metro, data = states.data)
summary(energy_mod)

energy_mod2 <- lm(energy ~  metro + toxic + area + green, data = states.data)
summary(energy_mod2)

plot(energy_mod2)

# Adding the variables toxic, area and green to the model increases the R^2 
# and Adj R^2 value singnificantly.


## Exercise 2:
energy_mod3 <- lm(energy ~  metro + toxic*area*green, data = states.data)
summary(energy_mod3)

energy_mod4 <- lm(energy ~  metro + toxic*area*green + region, data = states.data)
summary(energy_mod4)

### Regions North East and Midwest are less significant compared to South.


