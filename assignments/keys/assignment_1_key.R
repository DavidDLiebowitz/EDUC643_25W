##########################################################################################
# EDUC 643 Assignment 1 Key 
## David Liebowitz, Claire Zhang, Havisha Khurana
### First created: 1/10/23
### Last update: 2/5/24
### Inputs: nerds.csv
### Purpose: load in data, understand structure, conduct descriptive analysis
##########################################################################################

# Load necessary packages
library(tidyverse)
library(here)
library(modelsummary)

# Identify directory location of file
i_am("assignments/keys/assignment_1_key.R")


####################################
#  Read in the data 

nerds <- read.csv(here("data/nerds.csv"))

#########################################################
## 1. Descriptive statistics
#########################################################

# 1.1 Summary statistics
nerds_desc <- select(nerds, -c(schoolname, ncesid, ncesdistid_geo, distname, level, locale))

# Going to rename variables so that they are interpretable in table
      # could also do this by hand, but you should not just use the variable names
nerds_desc <- nerds_desc %>%
              rename(`Per-pupil expenditure ($)` = ppe,
                     `Enrollment` = enroll,
                     `SES index for all families` = sesavgall,
                     `Median family income ($)` = inc50avgall,
                     `BA+ rate` = baplusavgall,
                     `Unemployment rate` = unempavgall,
                     `SNAP receipt rate` = snapavgall,
                     `Free- and reduced-price lunch rate` = frpl)

datasummary_skim(nerds_desc,
                 fmt=2,
                 histogram = F,
                 notes = c("Source: National Education Resource Database on Schools (NERD$)"),
                 output = "assignments/keys/assignment_1_table1.docx")

# 1.2 Bivariate plot

ggplot(data=nerds, aes(x=frpl, y=ppe)) +
        geom_point(color = "cornflowerblue") +
        geom_smooth(method = 'lm', se = F, color = "deeppink") +
        labs(x = "Free- or reduced-price lunch rate",
        y = "Per-pupil expenditure ($)") +
        theme_minimal(base_size = 14)

ggsave("assignments/keys/assignment_1_bivariate.png")

########################################################
## 2. OLS regression
########################################################

# 2.3 OLS fit

fit <- lm(ppe ~ frpl, data=nerds)

# On-screen report
summary(fit)

# Full table
modelsummary(fit, 
             stars=T,
             fmt = 2,                                                                              # <- rounding to 2 digits
             gof_omit = "Adj.|AIC|BIC|Log",
             coef_rename = c("frpl" = "Free- or reduced-price lunch (0-1)"),
             notes = c("Notes: Cells represent coefficients and standard errors in parentheses."),
             output = "assignments/keys/assignment_1_table2.docx")

# 2.6 Can include confidence intervals directly in modelsummary output
modelsummary(fit, 
             stars=T,
             fmt = 2,                                                                               
             gof_omit = "Adj.|AIC|BIC|Log",
             statistic = 'conf.int',                                                                # <- here, by default set to 95%, but can change w/ conf_level = .99
             coef_rename = c("frpl" = "Free- or reduced-price lunch (0-1)"),
             notes = c("Notes: Cells report coefficients and standard errors in parentheses."))

# Can also produce via tidy command 
tidy(fit, conf.int = T)


##########################################################
## 3. Regression assumptions
##########################################################


# 3.2 Examine South Eugene High School
sehs <- filter(nerds, ncesid==410474000573)

summary(sehs$ppe)
summary(sehs$frpl)

all <- ggplot(nerds, aes(frpl, ppe)) + 
  geom_point(color='cornflowerblue', alpha=0.3) +
  geom_smooth(method='lm', color='black') +
  labs(x = "Free- or reduced-price lunch rate",
       y = "Per-pupil expenditure ($)") +
  theme_minimal(base_size=12)

all

all + 
  geom_point(data=sehs, aes(frpl, ppe), color="deeppink", size=2.5) +
  annotate('text', x=0.25, y=17000, label="S. Eugene HS", color='deeppink')

ggsave("assignments/keys/assignment_1_sehs.png")

# 3.3 Examine residuals

# Fit relationship
fit <- lm(ppe ~ frpl, data=nerds)

# Extract residuals from model fit
nerds$predict <- predict(fit)
nerds$raw_resid <- resid(fit)
nerds$std_resid <- rstandard(fit)
nerds$stu_resid <- rstudent(fit)

# Examine overall normality of residuals
## Going to use studentized residuals to avoid missing outlying values
## that are overly influential

## Histogram
hist(nerds$stu_resid,
     xlab="Studentized residuals",
     main="")

#### Export base R plot directly from code
# 1. Open png file
png("assignments/keys/assignment_1_hist.png")
# 2. Create the plot
hist(nerds$stu_resid,
     xlab="Studentized residuals",
     main="")
# 3. Close the file
dev.off()

## QQ plot
qq <- ggplot(nerds) + 
  stat_qq(aes(sample=stu_resid)) +
  geom_abline(color='cornflowerblue') +
  theme_minimal(base_size=12)

qq

ggsave("assignments/keys/assignment_1_qq.png")

## Fitted vs. residuals
ggplot(nerds, aes(x = predict, y = raw_resid)) + 
  geom_point() +
  geom_hline(yintercept = 0, color = "red", linetype="dashed") +
  ylab("Raw Residuals") + xlab("Fitted values") +
  theme_minimal(base_size = 16)

ggsave("assignments/keys/assignment_1_resid_fitted.png")
