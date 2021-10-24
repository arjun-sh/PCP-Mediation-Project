library(mediation)
library(dplyr)
load("panel")

panel2 <- panel
panel2 <- panel2 %>% mutate_at(.vars = vars(contains("pc")),
                               .funs= ~ifelse(abs(.)>mean(.)+2*sd(.), NA, .))
panel2 <- panel2 %>% mutate_at(.vars = vars(contains("le")),
                               .funs= ~ifelse(abs(.)>mean(.)+2*sd(.), NA, .))
panel2 <- panel2 %>% mutate_at(.vars = vars(contains("urb")),
                               .funs= ~ifelse(abs(.)>mean(.)+2*sd(.), NA, .))
panel2 <- na.omit(panel2)



poll <- panel$poll
fem <- panel$fem
blk <- panel$blk
his <- panel$his
nam <- panel$nam
unemp <- panel$unemp
hobed <- panel$hobed
medct <- panel$medct
mcare <- panel$mcare
inc <- panel$inc
ed <- panel$ed
eld <- panel$eld
unins <- panel$unins


covariates <- cbind(poll,
                    fem,
                    blk,
                    his,
                    nam,
                    unemp,
                    hobed,
                    medct,
                    mcare,
                    inc,
                    ed,
                    eld,
                    unins)

fit.mediator=lm(pc~urb +
                  fem+
                  blk+
                  his+
                  nam+
                  unemp+
                  poll+
                  hobed+
                  medct+
                  mcare,
                data = panel)
summary(fit.mediator) 

# Effect of PCP density on life expectancy
fit.outcome=lm(le~urb + pc +
                 fem+
                 blk+
                 his+
                 nam+
                 unemp+
                 poll+
                 hobed+
                 medct+
                 mcare, data = panel)
summary(fit.outcome)
# proportion mediated is negative if the direct and indirect effects have opposite directions.

results = mediate(fit.mediator, 
                  fit.outcome, 
                  treat="urb", 
                  mediator="pc",
                  covariates = covariates,
                  boot = TRUE,
                  sims = 1)
summary(results)
write.csv(results, file = "results.csv")


fit.mediator=lm(pc~urb +
                  fem+
                  blk+
                  his+
                  nam+
                  unemp+
                  poll+
                  hobed+
                  medct+
                  mcare,
                data = panel2)
summary(fit.mediator) 

# Effect of PCP density on life expectancy
fit.outcome=lm(le~pc + urb +
                 fem+
                 blk+
                 his+
                 nam+
                 unemp+
                 poll+
                 hobed+
                 medct+
                 mcare, data = panel2)
summary(fit.outcome)
# proportion mediated is negative if the direct and indirect effects have opposite directions.

results = mediate(fit.mediator, 
                  fit.outcome, 
                  treat="urb", 
                  mediator="pc",
                  covariates = covariates,
                  boot = TRUE,
                  sims = 1)
summary(results)