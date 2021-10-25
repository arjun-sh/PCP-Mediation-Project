library(plyr)
library(dplyr)
library(tableone)
library(ggplot2)
library(ggpubr)
library(reshape2)
library(mediation)

View(panel)
load("panel")

table(panel['urbanity_status'])

panel$urb <- as.character(panel$urb)
panel <- panel %>%
  mutate(
    urbanity_status = case_when(
      endsWith(urb, "0") ~ "Urban",
      endsWith(urb, "1") ~ "Urban",
      endsWith(urb, "2") ~ "Urban",
      endsWith(urb, "3") ~ "Urban",
      endsWith(urb, "4") ~ "Urban",
      endsWith(urb, "5") ~ "Rural",
      endsWith(urb, "6") ~ "Rural"
    )
  )
panel$time <- as.character(panel$time)
panel <- panel %>%
  mutate(
    time_order = case_when(
      endsWith(time, "2010") ~ "1",
      endsWith(time, "2015") ~ "2",
      endsWith(time, "2017") ~ "3"
    )
  )

HRSA <- 100000/3500
SNRC <- 100000/1500
panel2010 <- subset(panel, time == "2010")
panel2015 <- subset(panel, time == "2015")
panel2017 <- subset(panel, time == "2017")
panelRural <- subset(panel, urbanity_status == "Rural")
panelUrban <- subset(panel, urbanity_status == "Urban")
panelRural2010 <- subset(panel, urbanity_status == "Rural" & time == "2010")
panelUrban2010 <- subset(panel, urbanity_status == "Urban" & time == "2010")
panelRural2015 <- subset(panel, urbanity_status == "Rural" & time == "2015")
panelUrban2015 <- subset(panel, urbanity_status == "Urban" & time == "2015")
panelRural2017 <- subset(panel, urbanity_status == "Rural" & time == "2017")
panelUrban2017 <- subset(panel, urbanity_status == "Urban" & time == "2017")
panelRuralShort <- panelRural %>% filter(panelRural$pc < HRSA)
panelRuralShort2010 <- panelRural2010 %>% filter(panelRural2010$pc < HRSA)
panelRuralShort2015 <- panelRural2015 %>% filter(panelRural2015$pc < HRSA)
panelRuralShort2017 <- panelRural2017 %>% filter(panelRural2017$pc < HRSA)


#### Plots and tables ####
g1 <- ggplot(panel, aes(pc, le, col = urbanity_status) ) +
  geom_point(alpha = 0.1, data = panelUrban, col = "#00BFC4") +
  geom_point(alpha = 0.1, data = panelRural, col = "#F8766D") +
  geom_point(alpha = 0, data = panel) +
  scale_color_manual(values=c("#F8766D","#00BFC4")) +
  #, color='#00aaff'
  geom_smooth(method = "lm", formula = y ~ x, color="#00BFC4", data = panelUrban) + 
  geom_smooth(method = "lm", formula = y ~ x, color="#F8766D", data = panelRural) +
  xlim(5, quantile(panel$pc,c(.975)))+
  ylim(72,83)+
  theme_minimal()+
  guides(colour = guide_legend(override.aes = list(alpha=1))) +
  labs(title="Rural (red) and urban (blue) primary care density\nversus life expectancy",
       x ="Primary care density (active physicians per 100,000)", y = "Age-adjusted life expectancy (years)", col = "County\nurbanity") +
  theme(text=element_text(size=15))
g1
# ggsave("figure1.jpg", width = 16, height = 9)


panelDelta <- as.data.frame(cbind(panel2010$pc - panel2017$pc, panel2010$le - panel2017$le))
urb <- panel2017$urbanity_status
panelDelta <- as.data.frame(cbind(panelDelta, urb))
panelDeltaUrban <- as.data.frame(subset(panelDelta, urb == "Urban"))
panelDeltaRural <- as.data.frame(subset(panelDelta, urb == "Rural"))

g2 <- ggplot(panelDelta, aes(x = V1, y = V2*365, col = urb)) +
  geom_point(alpha = 0.5) +
  labs(title="Urban and rural county changes in life expectancy and\nprimary care density from 2010 to 2017",
       x ="Change in primary care density (active physicians per 100,000)", 
       y = "Change in age-adjusted life expectancy (days)", col = "County\nurbanity") +
  theme_minimal() +
  theme(text=element_text(size=15)) +
  xlim(-80, quantile(panelDelta$v1,c(.975))) +
  ylim(-170, quantile(panelDelta$v2,c(.975)))
  # scale_x_continuous(breaks = seq(-80,20, by = 20)) +
  # scale_y_continuous(breaks = seq(-175,150, by = 50))
g2
# ggsave("figure2.jpg", width = 16, height = 9)

fig1 <- ggarrange(g1,g2)
fig1
ggsave("figure1.jpg", width = 16, height = 9)

panelRuralShortAndNon <- panelRural %>%
  mutate(
    short_status = case_when(
      panelRural$pc >= HRSA ~ "NonShort",
      panelRural$pc < HRSA ~ "Short"
    )
  )
vars <- c("pc", "le","poll", "fem", "blk","his","nam","unemp", "hobed", "medct","mcare","inc","ed","eld","unins","urb")
urb <- c("urb")
tab3 <- CreateTableOne(vars = urb, data = panelRuralShortAndNon, strata = "short_status",test = FALSE, addOverall = TRUE)
tab3 <- print(tab3, nonnormal = urb,exact = "stage", quote = FALSE, noSpaces = TRUE, normal = mean)
write.csv(tab3, file = "myTable4444.csv")

#### Stats/Prediction models ####

# HRSA: Increasing PCP density in the primary care shortage rural counties to the threshold of being non-shortage county 
panelRuralShort <- panelRural %>% filter(panelRural$pc < HRSA)
baseline <- lm(le ~ pc+
                 urb+
                 fem+
                 blk+
                 his+
                 nam+
                 unemp+
                 poll+
                 hobed+
                 medct+
                 mcare, data = panel)
panelRuralShortImp <- panelRuralShort %>% mutate(pc = HRSA)
rSub <- as.data.frame(predict(baseline, newdata = panelRuralShort, type = "response")) 
rImp <- as.data.frame(predict(baseline, newdata = panelRuralShortImp, type = "response")) 
comparison <- cbind(rSub,rImp)
names(comparison)[1] <- "before"
names(comparison)[2] <- "after"
comparison$delta <- (comparison$after - comparison$before)
delta <- as.vector(comparison$delta)
summary(delta*365)

# SNRC: Increasing PCP density to the higher standards recommended by a SNRC
panelRuralShort <- panelRural %>% filter(panelRural$pc < SNRC)
baseline <- lm(le ~ pc+
                 urb+
                 fem+
                 blk+
                 his+
                 nam+
                 unemp+
                 poll+
                 hobed+
                 medct+
                 mcare, data = panel)
panelRuralShortImp <- panelRuralShort %>% mutate(pc = SNRC)
rSub <- as.data.frame(predict(baseline, newdata = panelRuralShort, type = "response")) 
rImp <- as.data.frame(predict(baseline, newdata = panelRuralShortImp, type = "response")) 
comparison <- cbind(rSub,rImp)
names(comparison)[1] <- "before"
names(comparison)[2] <- "after"
comparison$delta <- (comparison$after - comparison$before)
delta <- as.vector(comparison$delta)
summary(delta*365)

# NEED TO FIGURE THIS OUT
# NEED TO FIGURE THIS OUT
# NEED TO FIGURE THIS OUT
# NEED TO FIGURE THIS OUT
# NEED TO FIGURE THIS OUT
# NEED TO FIGURE THIS OUT
# NEED TO FIGURE THIS OUT WHY IS IT 253.5
# Increasing PCP density from the 25th percentile to that of the 75th percentile in a rural county
panelRuralQ1 <- as.data.frame(quantile(panelRural$pc))[2,]
panelRuralQ3 <- as.data.frame(quantile(panelRural$pc))[4,]
panelRuralQ1 <- panelRural %>% filter(panelRural$pc == panelRuralQ1)
panelRuralQ3 <- panelRural %>% filter(panelRural$pc == panelRuralQ3)
q1 <- predict(baseline, newdata = panelRuralQ1, type = "response")
q3 <- predict(baseline, newdata = panelRuralQ3, type = "response")
diff <- (median(q3) - median(q1))*365
diff
q1f <- as.data.frame(quantile(q1))
q3f <- as.data.frame(quantile(q3))
iqr1 <- (q3f[2,] - q1f[2,])*365
iqr2 <- (q3f[4,] - q1f[4,])*365
median <- (median(q3) - median(q1))*365
summary(q1)
iqr1
iqr2
median


panelRuralQ1 <- panelRural %>% filter(panelRural$pc == 17)
panelRuralQ3 <- panelRural %>% filter(panelRural$pc == 48)
q1 <- predict(baseline, newdata = panelRuralQ1, type = "response")
q3 <- (predict(baseline, newdata = panelRuralQ3, type = "response"))
diff <- (mean(q3) - mean(q1))*365
diff

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
                  sims = 100)
summary(results)


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
                  sims = 100)
summary(results)
