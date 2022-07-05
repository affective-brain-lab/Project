
######################### SUBJECTIVE CARD GAME
#Figure 2
BIC_scores <- data.frame(18150,
                         18296,
                         18876,
                         18506,
                         18735,
                         19244,
                         18707,
                         19333,
                         18574,#
                         18847,#
                         19157,#
                         18326,
                         18529,
                         18167,
                         18760,
                         18506,
                         18322,
                         19012,
                         19009,
                         19314,
                         19327,
                         19768,
                         19311,
                         19375)

# AIC Bastien-style plots
# Create data frame with AIC scores
# Create a matrix to represent the models
model_n <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 , 20, 21, 22, 23, 24)

m_has_AntiAffect <-   c(1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0)
m_has_SubUncertain <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0)
m_has_IUSubagency <-  c(1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0)

m_has_EV <-           c(0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0)
m_has_SD <-           c(0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0)
m_has_IUobjAg <-      c(0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0)

m_has_Entropy <-      c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

m_matrix <- data.frame(model_n, m_has_AntiAffect, m_has_SubUncertain, m_has_IUSubagency, m_has_EV, m_has_SD, m_has_IUobjAg, m_has_Entropy)

library(tidyr)
library(ggplot2)
m_matrix_long <- gather(m_matrix, variable, bool, m_has_AntiAffect:m_has_Entropy, factor_key=TRUE)
m_matrix_plot <- ggplot(m_matrix_long , aes(variable, model_n)) + geom_tile(aes(fill = bool),
                                                                            color="black") + theme_classic() +
  theme(legend.position="none") +
  scale_y_continuous(breaks=seq(1, 24, 1), labels=model_n) +
  scale_x_discrete(labels=c("AntiAffect", "SubUncertain", "IUSubagency", "EV", "SD", "IUobjAg", "Entropy")) +
  scale_fill_gradient(low="white", high="grey") +
  labs(x = "Fixed variable", y = "Model number")
m_matrix_plot

# Define argmin function for the plots
argmin <- function(x) {
  min <- min(x)
  for (i in 1:length(x)) {
    if (x[i] == min) {
      return (i)
    }
  }
}


m_BIC_dat <- data.frame(model_n, as.numeric(BIC_scores))
colnames(m_BIC_dat)<-c('model_n', 'BIC')

#m_AIC_dat_4_5$highlight <- ifelse(m_AIC_dat_4_5$m_AIC_4_5 == min(m_AIC_4_5), "#AAAAAA", "#444444")
#m_AIC_dat_4_5$size <- ifelse(m_AIC_dat_4_5$m_AIC_4_5 == min(m_AIC_4_5), 5, 3)

m_bIC_dat_plot <- ggplot(m_BIC_dat, aes(x = BIC, y = model_n)) +
  geom_vline(xintercept = min(m_BIC_dat$BIC), linetype="dashed") +
  theme_classic() +
  scale_y_continuous(breaks=seq(1, 24, 1),  limits=c(1, 24)) +
  scale_x_continuous(breaks=seq(18000, 19400, 250), limits=c(round(min(m_BIC_dat$BIC)), round(max(m_BIC_dat$BIC)))) +
  labs(x = "BIC score", y = "Model number")
for (n in model_n) {
  m_bIC_dat_plot <- m_bIC_dat_plot + geom_segment(x=18000, y=n, xend=m_BIC_dat$BIC[n], yend=n)
}
m_bIC_dat_plot <- m_bIC_dat_plot + geom_point(size=2, color='black')
m_bIC_dat_plot
library(gridExtra)
grid.arrange(m_matrix_plot, m_bIC_dat_plot, ncol=2)

#Figure 4
BIC_scores <- data.frame(45203,
                         45626,
                         47127,
                         45935,
                         46697,
                         47788,
                         46706,
                         46294,
                         46186,#
                         46708,#
                         47537,#
                         45844,
                         45909,
                         45208,
                         46712,
                         45890,
                         45853,
                         47129,
                         47478,
                         48165,
                         48059,
                         49075,
                         48156,
                         47950)

# AIC Bastien-style plots
# Create data frame with AIC scores
# Create a matrix to represent the models
model_n <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19 , 20, 21, 22, 23, 24)

m_has_AntiAffect <-   c(1, 1, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0)
m_has_SubUncertain <- c(1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0)
m_has_IUSubagency <-  c(1, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0)

m_has_EV <-           c(0, 0, 0, 0, 1, 1, 1, 0, 1, 1, 0, 1, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 0, 0)
m_has_SD <-           c(0, 0, 0, 0, 1, 1, 0, 1, 0, 0, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0, 0)
m_has_IUobjAg <-      c(0, 0, 0, 0, 1, 0, 1, 1, 1, 0, 1, 0, 0, 1, 0, 1, 1, 0, 0, 0, 0, 0, 1, 0)

m_has_Entropy <-      c(0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)

m_matrix <- data.frame(model_n, m_has_AntiAffect, m_has_SubUncertain, m_has_IUSubagency, m_has_EV, m_has_SD, m_has_IUobjAg, m_has_Entropy)

library(tidyr)
library(ggplot2)
m_matrix_long <- gather(m_matrix, variable, bool, m_has_AntiAffect:m_has_Entropy, factor_key=TRUE)
m_matrix_plot <- ggplot(m_matrix_long , aes(variable, model_n)) + geom_tile(aes(fill = bool),
                                                                            color="black") + theme_classic() +
  theme(legend.position="none") +
  scale_y_continuous(breaks=seq(1, 24, 1), labels=model_n) +
  scale_x_discrete(labels=c("AntiAffect", "SubUncertain", "IUSubagency", "EV", "SD", "IUobjAg", "Entropy")) +
  scale_fill_gradient(low="white", high="grey") +
  labs(x = "Fixed variable", y = "Model number")
m_matrix_plot

# Define argmin function for the plots
argmin <- function(x) {
  min <- min(x)
  for (i in 1:length(x)) {
    if (x[i] == min) {
      return (i)
    }
  }
}


m_BIC_dat <- data.frame(model_n, as.numeric(BIC_scores))
colnames(m_BIC_dat)<-c('model_n', 'BIC')

#m_AIC_dat_4_5$highlight <- ifelse(m_AIC_dat_4_5$m_AIC_4_5 == min(m_AIC_4_5), "#AAAAAA", "#444444")
#m_AIC_dat_4_5$size <- ifelse(m_AIC_dat_4_5$m_AIC_4_5 == min(m_AIC_4_5), 5, 3)

m_bIC_dat_plot <- ggplot(m_BIC_dat, aes(x = BIC, y = model_n)) +
  geom_vline(xintercept = min(m_BIC_dat$BIC), linetype="dashed") +
  theme_classic() +
  scale_y_continuous(breaks=seq(1, 24, 1),  limits=c(1, 24)) +
  scale_x_continuous(breaks=seq(45200, 48200, 600), limits=c(round(min(m_BIC_dat$BIC)), round(max(m_BIC_dat$BIC)))) +
  labs(x = "BIC score", y = "Model number")
for (n in model_n) {
  m_bIC_dat_plot <- m_bIC_dat_plot + geom_segment(x=18000, y=n, xend=m_BIC_dat$BIC[n], yend=n)
}
m_bIC_dat_plot <- m_bIC_dat_plot + geom_point(size=2, color='black')
m_bIC_dat_plot
library(gridExtra)
grid.arrange(m_matrix_plot, m_bIC_dat_plot, ncol=2)

######################### OBJECTIVE CARD GAME

#Figure 5 (top)
BIC_scores <- data.frame(41740, ## Objective Model
                         43228, ## EVandSD
                         41747, ## EVandIU
                         43113, #SD&objAgency
                         43492, #EV
                         44391, #SD
                         43125, #IU
                         41022,
                         42285,
                         41708,
                         42827) 
                        

# AIC Bastien-style plots
# Create data frame with AIC scores
# Create a matrix to represent the models
model_n <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
m_has_EV <-           c(1,1,1,0,1,0,0, 1, 0, 1, 0)
m_has_SD <-           c(1,1,0,1,0,1,0, 0, 0, 0, 0)
m_has_IUobjAg <-      c(1,0,1,1,0,0,1, 1, 1, 0, 0)
m_has_Entropy <-      c(0,0,0,0,0,0,1, 1, 1, 1, 1)
m_matrix <- data.frame(model_n, m_has_EV, m_has_SD, m_has_IUobjAg, m_has_Entropy)

library(tidyr)
library(ggplot2)
m_matrix_long <- gather(m_matrix, variable, bool, m_has_EV:m_has_Entropy, factor_key=TRUE)
m_matrix_plot <- ggplot(m_matrix_long , aes(variable, model_n)) + geom_tile(aes(fill = bool),
                                                                            color="black") + theme_classic() +
  theme(legend.position="none") +
  scale_y_continuous(breaks=seq(1, 11, 1), labels=model_n) +
  scale_x_discrete(labels=c("EV", "SD", "IUobjAg", "Entropy")) +
  scale_fill_gradient(low="white", high="grey") +
  labs(x = "Fixed variable", y = "Model number")
m_matrix_plot

# Define argmin function for the plots
argmin <- function(x) {
  min <- min(x)
  for (i in 1:length(x)) {
    if (x[i] == min) {
      return (i)
    }
  }
}


m_BIC_dat <- data.frame(model_n, as.numeric(BIC_scores))
colnames(m_BIC_dat)<-c('model_n', 'BIC')

m_bIC_dat_plot <- ggplot(m_BIC_dat, aes(x = BIC, y = model_n)) +
  geom_vline(xintercept = min(m_BIC_dat$BIC), linetype="dashed") +
  theme_classic() +
  scale_y_continuous(breaks=seq(1, 11, 1),  limits=c(1, 11)) +
  scale_x_continuous(breaks=seq(41700, 45950, 600), limits=c(round(min(m_BIC_dat$BIC)), round(max(m_BIC_dat$BIC)))) +
  labs(x = "BIC score", y = "Model number")
for (n in model_n) {
  m_bIC_dat_plot <- m_bIC_dat_plot + geom_segment(x=18000, y=n, xend=m_BIC_dat$BIC[n], yend=n)
}
m_bIC_dat_plot <- m_bIC_dat_plot + geom_point(size=2, color='black')
m_bIC_dat_plot
library(gridExtra)
grid.arrange(m_matrix_plot, m_bIC_dat_plot, ncol=2)


#Figure 5 (bottom)
BIC_scores <- data.frame(70721,
                         72601,
                         70810,
                         73346,
                         72991,
                         74933,
                         73417,
                         70165,
                         71101,
                         72489,
                         73174)

# AIC Bastien-style plots
# Create data frame with AIC scores
# Create a matrix to represent the models
model_n <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
m_has_EV <-           c(1,1,1,0,1,0,0, 1, 0, 1, 0)
m_has_SD <-           c(1,1,0,1,0,1,0, 0, 0, 0, 0)
m_has_IUobjAg <-      c(1,0,1,1,0,0,1, 1, 1, 0, 0)
m_has_Entropy <-      c(0,0,0,0,0,0,1, 1, 1, 1, 1)
m_matrix <- data.frame(model_n, m_has_EV, m_has_SD, m_has_IUobjAg, m_has_Entropy)

library(tidyr)
library(ggplot2)
m_matrix_long <- gather(m_matrix, variable, bool, m_has_EV:m_has_Entropy, factor_key=TRUE)
m_matrix_plot <- ggplot(m_matrix_long , aes(variable, model_n)) + geom_tile(aes(fill = bool),
                                                                            color="black") + theme_classic() +
  theme(legend.position="none") +
  scale_y_continuous(breaks=seq(1, 11, 1), labels=model_n) +
  scale_x_discrete(labels=c("EV", "SD", "IUobjAg")) +
  scale_fill_gradient(low="white", high="grey") +
  labs(x = "Fixed variable", y = "Model number")
m_matrix_plot

# Define argmin function for the plots
argmin <- function(x) {
  min <- min(x)
  for (i in 1:length(x)) {
    if (x[i] == min) {
      return (i)
    }
  }
}


m_BIC_dat <- data.frame(model_n, as.numeric(BIC_scores))
colnames(m_BIC_dat)<-c('model_n', 'BIC')

m_bIC_dat_plot <- ggplot(m_BIC_dat, aes(x = BIC, y = model_n)) +
  geom_vline(xintercept = min(m_BIC_dat$BIC), linetype="dashed") +
  theme_classic() +
  scale_y_continuous(breaks=seq(1, 11, 1),  limits=c(1, 11)) +
  scale_x_continuous(breaks=seq(70721, 76200, 500), limits=c(round(min(m_BIC_dat$BIC)), round(max(m_BIC_dat$BIC)))) +
  labs(x = "BIC score", y = "Model number")
for (n in model_n) {
  m_bIC_dat_plot <- m_bIC_dat_plot + geom_segment(x=18000, y=n, xend=m_BIC_dat$BIC[n], yend=n)
}
m_bIC_dat_plot <- m_bIC_dat_plot + geom_point(size=2, color='black')
m_bIC_dat_plot
library(gridExtra)
grid.arrange(m_matrix_plot, m_bIC_dat_plot, ncol=2)


# Experiment 5
# subjective
61486
62246.57
62983.37
62786.27

#objective
62868.99
64661.6
62873.08
64433.02

#mix
61634.75
62760.57
61486
62908.01
62742.71
61639.25

# subjective IU
64502
# objective IU
64471

