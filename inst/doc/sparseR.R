## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, fig.height = 5, fig.width = 7)
library(recipes)
library(dplyr)
library(knitr)
library(kableExtra)
library(sparseR)


## -----------------------------------------------------------------------------
data(iris)
summary(iris)

## -----------------------------------------------------------------------------
srl <- sparseR(Sepal.Width ~ ., data = iris, k = 1, seed = 1)

## -----------------------------------------------------------------------------
srl

## -----------------------------------------------------------------------------
# At the lambda which minimizes CVE
summary(srl, at = "cvmin")

# At the lambda which is within 1 SE of the minimum CVE
summary(srl, at = "cv1se")

## -----------------------------------------------------------------------------
plot(srl, plot_type = "cv")

## -----------------------------------------------------------------------------
plot(srl, plot_type = "path")

## -----------------------------------------------------------------------------
X <- model.matrix(Sepal.Width ~ .*., data = iris)[,-1]

## -----------------------------------------------------------------------------
set.seed(1)
srl2 <- sparseR(pre_process = FALSE, model_matrix = X, y = iris$Sepal.Width)

## -----------------------------------------------------------------------------
set.seed(1)
srl2 <- sparseR(pre_process = FALSE, model_matrix = X, y = iris$Sepal.Width, max.iter = 1e6)

## -----------------------------------------------------------------------------
srl2
summary(srl2, at = "cv1se")

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  plot(srl2)

## ---- echo = FALSE, eval = TRUE, fig.height=8---------------------------------
old_par <- par(mfrow = c(2,1))
plot(srl2)
par(old_par)

## -----------------------------------------------------------------------------
set.seed(1) 
srl3 <- sparseR(Sepal.Width ~ ., data = iris, k = 1, 
                pre_proc_opts = c("none"), max.iter = 1e6)
srl3
summary(srl3, at = "cv1se")

## -----------------------------------------------------------------------------
cc <- iris %>%
    select(Sepal.Length, Petal.Length, Petal.Width) %>%
    apply(2, min, na.rm = TRUE)


p1 <- sparseR_prep(Sepal.Width ~ ., iris, k = 0, extra_opts = list(centers = cc))
(c2min <- bake(p1, iris))
summary(c2min)

## -----------------------------------------------------------------------------
srl_centered2min <- sparseR(Sepal.Width ~ ., iris, extra_opts = list(centers = cc), seed = 1)

## -----------------------------------------------------------------------------
p2 <- sparseR_prep(Sepal.Width ~ ., iris, k = 0, extra_opts = list(center_fn = min))
(c2min2 <- bake(p2, iris))
identical(c2min2, c2min)

## ----warning=FALSE------------------------------------------------------------
effect_plot(srl, "Petal.Width", by = "Species")

## ----warning=FALSE------------------------------------------------------------
effect_plot(srl_centered2min, "Petal.Width", by = "Species")

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  plot(srl3, plot_type = "cv", ylim = c(0,.2))
#  abline(h = min(srl3$fit$cve), col = "red")
#  plot(srl_centered2min, plot_type = "cv", ylim = c(0,.2))
#  abline(h = min(srl3$fit$cve), col = "red")
#  plot(srl, plot_type = "cv", ylim = c(0,.2))
#  abline(h = min(srl3$fit$cve), col = "red")
#  
#  effect_plot(srl3, "Petal.Width", by = "Species",
#              plot.args = list(ylim = c(1.5, 4.8)))
#  effect_plot(srl_centered2min, "Petal.Width", by = "Species",
#              plot.args = list(ylim = c(1.5, 4.8)))
#  effect_plot(srl, "Petal.Width", by = "Species",
#              plot.args = list(ylim = c(1.5, 4.8)))

## ---- echo = FALSE, warning=FALSE, fig.height=6, fig.width=8, out.width="100%"----
old_par <- par(mfrow = c(2,3), mar = c(4,4,5,2) + .1)
plot(srl3, plot_type = "cv", ylim = c(0,.2))
title("Centered to zero", line = 3)
abline(h = min(srl3$fit$cve), col = "red")
plot(srl_centered2min, plot_type = "cv", ylim = c(0,.2))
title("Centered to minimum values", line = 3)
abline(h = min(srl3$fit$cve), col = "red")
plot(srl, plot_type = "cv", ylim = c(0,.2))
title("Centered to mean", line = 3)
abline(h = min(srl3$fit$cve), col = "red")

par(mar = c(4,4,2,2) +.1)
effect_plot(srl3, "Petal.Width", by = "Species", 
            plot.args = list(ylim = c(1.5, 4.8)))
effect_plot(srl_centered2min, "Petal.Width", by = "Species", 
            plot.args = list(ylim = c(1.5, 4.8)))
effect_plot(srl, "Petal.Width", by = "Species", 
            plot.args = list(ylim = c(1.5, 4.8)))

par(old_par)

## -----------------------------------------------------------------------------
p1 <- predict(srl3, at = "cvmin")
p2 <- predict(srl_centered2min, at = "cvmin")
p3 <- predict(srl, at = "cvmin")

cor(cbind(p1, p2 ,p3))
pairs(cbind(p1, p2 ,p3))


## -----------------------------------------------------------------------------
# At CV1se
p4 <- predict(srl3, at = "cv1se")
p5 <- predict(srl_centered2min, at = "cv1se")
p6 <- predict(srl, at = "cv1se")

cor(cbind(p1, p2 ,p3, p4,p5,p6))
pairs(cbind(p1, p2 ,p3, p4,p5,p6))

## ---- echo = FALSE, eval = TRUE, warning=FALSE--------------------------------
old_par <- par(mfrow = c(1,3))
effect_plot(srl3, "Petal.Width", by = "Species", at = "cv1se", 
            plot.args = list(ylim = c(1.5, 5)))
effect_plot(srl_centered2min, "Petal.Width", by = "Species", at = "cv1se", 
            plot.args = list(ylim = c(1.5, 5)))
effect_plot(srl, "Petal.Width", by = "Species", at = "cv1se", 
            plot.args = list(ylim = c(1.5, 5)))
par(old_par)

## ---- echo = TRUE, eval = FALSE, warning = FALSE------------------------------
#  effect_plot(srl3, "Petal.Width", by = "Species", at = "cv1se")
#  effect_plot(srl_centered2min, "Petal.Width", by = "Species", at = "cv1se")
#  effect_plot(srl, "Petal.Width", by = "Species", at = "cv1se")

## ---- warning = FALSE---------------------------------------------------------
## Centered model
(rbic1 <- sparseRBIC_step(Sepal.Width ~ ., iris, pre_proc_opts = c("center", "scale")))

# Non-centered model
(rbic2 <- sparseRBIC_step(Sepal.Width ~ ., iris, pre_proc_opts = c("scale")))


## ---- echo = TRUE, eval=FALSE, warning = FALSE--------------------------------
#  effect_plot(rbic1, "Petal.Width", by = "Species", plot.args = list(ylim = c(1.5, 5)))
#  effect_plot(rbic2, "Petal.Width", by = "Species", plot.args = list(ylim = c(1.5, 5)))
#  effect_plot(rbic1, "Sepal.Length", by = "Species")
#  effect_plot(rbic2, "Sepal.Length", by = "Species")

## ---- echo = FALSE, eval=TRUE, warning = FALSE, fig.height=8, fig.width = 6----
old_par <- par(mfrow = c(2,2))
effect_plot(rbic1, "Petal.Width", by = "Species", plot.args = list(ylim = c(1.5, 5)))
effect_plot(rbic2, "Petal.Width", by = "Species", plot.args = list(ylim = c(1.5, 5)))
effect_plot(rbic1, "Sepal.Length", by = "Species")
effect_plot(rbic2, "Sepal.Length", by = "Species")
par(old_par)

## -----------------------------------------------------------------------------
summary(rbic1)
summary(rbic2)

## -----------------------------------------------------------------------------
s1 <- sparseRBIC_sampsplit(rbic1)

## -----------------------------------------------------------------------------
s1$results %>%
  kable(digits = 5) %>% 
  kable_styling(full_width = FALSE)

## -----------------------------------------------------------------------------
s2 <- sparseRBIC_sampsplit(rbic2)

## -----------------------------------------------------------------------------
s2$results %>%
  kable(digits = 5) %>% 
  kable_styling(full_width = FALSE)

## ---- message=FALSE-----------------------------------------------------------
set.seed(1)
## Centered model
b1 <- sparseRBIC_bootstrap(rbic1, B = 250)

## ---- echo = FALSE------------------------------------------------------------
b1$results %>%
  kable(digits = 5) %>% 
  kable_styling(full_width = FALSE)

## ---- message = FALSE---------------------------------------------------------
set.seed(1)
## Uncentered model
b2 <- sparseRBIC_bootstrap(rbic2, B = 250)

## ---- echo = FALSE------------------------------------------------------------
b2$results %>%
  kable(digits = 5) %>% 
  kable_styling(full_width = FALSE)

## -----------------------------------------------------------------------------
data("irlcs_radon_syn")
summary(irlcs_radon_syn)

## -----------------------------------------------------------------------------
irlcs_radon_syn <- select(irlcs_radon_syn, -ID)

## -----------------------------------------------------------------------------
set.seed(13)

N <- nrow(irlcs_radon_syn)
trainIDX <- sample(1:N, N * .75)
train <- irlcs_radon_syn[trainIDX,]
test <- irlcs_radon_syn[-trainIDX,]

## -----------------------------------------------------------------------------
prep_obj <- sparseR_prep(CASE ~ ., data = train, k = 0, poly = 1)
prep_obj

## -----------------------------------------------------------------------------
MASS::truehist(train$SMKYRS)

## -----------------------------------------------------------------------------
sparseR_prep(CASE ~ ., data = train, k = 0, poly = 1, filter = "zv")

## -----------------------------------------------------------------------------
sparseR_prep(CASE ~ ., data = train, k = 0, poly = 1, extra_opts = list(unique_cut = 5))

## -----------------------------------------------------------------------------
sparseR_prep(CASE ~ ., data = train, k = 1, poly = 1, extra_opts = list(unique_cut = 5))
sparseR_prep(CASE ~ ., data = train, k = 1, poly = 2, extra_opts = list(unique_cut = 5))

## -----------------------------------------------------------------------------
lso <- list(
  SRL  = sparseR(CASE ~ ., train, seed = 1),            ## SRL model
  APL  = sparseR(CASE ~ ., train, seed = 1, gamma = 0), ## APL model
  ME   = sparseR(CASE ~ ., train, seed = 1, k = 0),     ## Main effects model
  SRLp = sparseR(CASE ~ ., train, seed = 1, poly = 2)   ## SRL + polynomials
)

## -----------------------------------------------------------------------------
lso

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  n <- lapply(lso, plot, log.l = TRUE)

## ---- echo = FALSE, eval = TRUE, fig.height= 9, fig.width=7-------------------
old_par <- par(mfrow = c(4,2), mar = c(3, 4, 4, 2))
n <- lapply(lso, plot, log.l = TRUE)
par(old_par)

## -----------------------------------------------------------------------------
mcp <- list(
  SRM  = sparseR(CASE ~ ., train, seed = 1, penalty = "MCP"),            ## SRM model
  APM  = sparseR(CASE ~ ., train, seed = 1, gamma = 0, penalty = "MCP"), ## APM model
  MEM   = sparseR(CASE ~ ., train, seed = 1, k = 0, penalty = "MCP"),     ## Main effects MCP model
  SRMp = sparseR(CASE ~ ., train, seed = 1, poly = 2, penalty = "MCP")   ## SRM + polynomials
)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  n <- lapply(mcp, plot, log.l = TRUE)

## ---- echo = FALSE, eval = TRUE, fig.height= 9, fig.width=7-------------------
old_par <- par(mfrow = c(4,2), mar = c(3, 4, 4, 2))
n <- lapply(mcp, plot, log.l = TRUE)
par(old_par)

## -----------------------------------------------------------------------------
scad <- list(
  SRS  = sparseR(CASE ~ ., train, seed = 1, penalty = "SCAD"),            ## SRS model
  APS  = sparseR(CASE ~ ., train, seed = 1, gamma = 0, penalty = "SCAD"), ## APS model
  MES   = sparseR(CASE ~ ., train, seed = 1, k = 0, penalty = "SCAD"),    ## Main effects SCAD model
  SRSp = sparseR(CASE ~ ., train, seed = 1, poly = 2, penalty = "SCAD")   ## SRS + polynomials
)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  n <- lapply(scad, plot, log.l = TRUE)

## ---- echo = FALSE, eval = TRUE, fig.height= 9, fig.width=7-------------------
old_par <- par(mfrow = c(4,2), mar = c(3, 4, 4, 2))
n <- lapply(scad, plot, log.l = TRUE)
par(old_par)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  lapply(lso, function(x) bind_rows(x$results_summary, x$results1se_summary))
#  lapply(mcp, function(x) bind_rows(x$results_summary, x$results1se_summary))
#  lapply(scad, function(x) bind_rows(x$results_summary, x$results1se_summary))

## ---- echo = FALSE, message = FALSE-------------------------------------------

lso_sum <-
  lapply(lso, function(x)
    bind_cols(x$results_summary[, c(1, 5, 2:4)], x$results1se_summary[, -c(1:2, 5)], .name_repair = "universal")) %>%
  bind_rows()
mcp_sum <-
  lapply(mcp, function(x)
    bind_cols(x$results_summary[, c(1, 5, 2:4)], x$results1se_summary[, -c(1:2, 5)])) %>%
  bind_rows()
scad_sum <-
  lapply(scad, function(x)
    bind_cols(x$results_summary[, c(1, 5, 2:4)], x$results1se_summary[, -c(1:2, 5)])) %>%
  bind_rows()

names(mcp_sum)[6:7] <- names(scad_sum)[6:7] <- names(lso_sum)[6:7] <- names(lso_sum)[4:5] <- names(mcp_sum)[4:5] <- names(scad_sum)[4:5] <-
  c("Selected", "Saturation")
lso_sum %>% 
  kable(digits = 3) %>% 
  kable_styling("striped", full_width = FALSE) %>%
  add_header_above(header = c(" " = 3, "Min CV" = 2, "CV1se" = 2)) %>% 
  group_rows(index = c("SRL" = 2, "APL" = 2, "MEL" = 1, "SRLp" = 3)) 
  
mcp_sum %>% 
  kable(digits = 3) %>% 
  kable_styling("striped", full_width = FALSE) %>%
  add_header_above(header = c(" " = 3, "Min CV" = 2, "CV1se" = 2)) %>% 
  group_rows(index = c("SRM" = 2, "APM" = 2, "MEM" = 1, "SRMp" = 3)) 
  
scad_sum %>% 
  kable(digits = 3) %>% 
  kable_styling("striped", full_width = FALSE) %>%
  add_header_above(header = c(" " = 3, "Min CV" = 2, "CV1se" = 2)) %>% 
  group_rows(index = c("SRS" = 2, "APS" = 2, "MES" = 1, "SRSp" = 3)) 
    

## ---- fig.height=8, fig.width=7-----------------------------------------------

## Load Data set, correctly code factors + outcome
data("Detrano")
cleveland$thal <- factor(cleveland$thal)
cleveland$case <- 1*(cleveland$num > 0)

# Convert variables into factor variables if necessary!
summary(cleveland)
sapply(cleveland, function(x) length(unique(x)))
cleveland$sex <- factor(cleveland$sex)
cleveland$fbs <- factor(cleveland$fbs)
cleveland$exang <- factor(cleveland$exang)

# Set seed for reproducibility
set.seed(167)

# Split data into test and train
N <- nrow(cleveland)
trainIDX <- sample(1:N, N*.5)
trainDF <- cleveland[trainIDX,] %>%
  select(-num)
testDF <- cleveland[-trainIDX,] %>%
  select(-num)

# Simulate missing data
trainDF$thal[2] <- trainDF$thalach[1] <- NA

lso <- list(
  SRL  = sparseR(case ~ ., trainDF, seed = 1), ## SRL model
  APL  = sparseR(case ~ ., trainDF, seed = 1, gamma = 0), ## APL model
  ME   = sparseR(case ~ ., trainDF, seed = 1, k = 0), ## Main effects model
  SRLp = sparseR(case ~ ., trainDF, seed = 1, poly = 2) ## SRL + polynomials
)

lso
plot(lso$SRL)

## ---- echo = TRUE, eval = FALSE-----------------------------------------------
#  lapply(lso, plot, log.l = TRUE)

## ---- eval = TRUE, echo=FALSE, fig.height=8, fig.width=7----------------------
old_par <- par(mfrow = c(4,2))
lapply(lso, plot, log.l = TRUE)
par(old_par)

