library(tidyverse)
library(corrplot)
library(ggplot2)
library(glmnet)
library(car)

#load data
diffs <- read.csv("~adv_stat_diff_a10.csv")
bball <- read.csv("~adv_stat_a10.csv")

#gmu difference in turnover percentage - winning games
gmu_W_dto.p <- stats %>%
  filter(WTeamID == 1206) %>%
  select(WTeamID, LTeamID, WTO.P, LTO.P) %>%
  mutate(diff = WTO.P - LTO.P) %>%
  pull()

#gmu difference in turnover percentage - losing games
gmu_L_dto.p <- stats %>%
  filter(LTeamID == 1206) %>%
  select(WTeamID, LTeamID, WTO.P, LTO.P) %>%
  mutate(diff =LTO.P - WTO.P) %>%
  pull()

#gmu difference in turnovers - winning games
gmu_W_dto <- stats %>%
  filter(WTeamID == 1206) %>%
  select(WTeamID, LTeamID, WTO, LTO) %>%
  mutate(diff = WTO - LTO) %>%
  pull()

#gmu difference in turnovers - losing games
gmu_L_dto <- stats %>%
  filter(LTeamID == 1206) %>%
  select(WTeamID, LTeamID, WTO, LTO) %>%
  mutate(diff = LTO - WTO) %>%
  pull()

#ranked winning teams turnover percentage
ranked_WTO.P <- bball %>%
  select(WTeamID, WTO.P) %>%
  pivot_longer(cols = c(WTeamID), names_to = "ID") %>%
  pivot_longer(cols = c(WTO.P), values_to = "TO.P") %>%
  group_by(value) %>%
  summarise(GMU_TO.P = mean(TO.P)) %>%
  arrange(-GMU_TO.P)

#ranked losing teams turnover percentage
ranked_LTO.P <- bball %>%
  select(LTeamID, LTO.P) %>%
  pivot_longer(cols = c(LTeamID), names_to = "ID") %>%
  pivot_longer(cols = c(LTO.P), values_to = "TO.P") %>%
  group_by(value) %>%
  summarise(GMU_TO.P = mean(TO.P)) %>%
  arrange(GMU_TO.P)


#ranked winning teams turnovers
ranked_WTO <- bball %>%
  select(WTeamID, WTO) %>%
  pivot_longer(cols = c(WTeamID), names_to = "ID") %>%
  pivot_longer(cols = c(WTO), values_to = "TO") %>%
  group_by(value) %>%
  summarise(GMU_TO = mean(TO)) %>%
  arrange(-GMU_TO)

#ranked losing teams turnovers
ranked_LTO <- bball %>%
  select(LTeamID, LTO) %>%
  pivot_longer(cols = c(LTeamID), names_to = "ID") %>%
  pivot_longer(cols = c(LTO), values_to = "TO") %>%
  group_by(value) %>%
  summarise(GMU_TO = mean(TO)) %>%
  arrange(GMU_TO)

#average of gmu difference in turnover percentage - winning games
gmu_diff_TO.P <- bball %>%
  filter(WTeamID == 1206) %>%
  select(WTeamID, LTeamID, WTO.P, LTO.P) %>%
  mutate(diff = WTO.P - LTO.P) %>%
  summarise(GMU_W_DIFF_DTO.P = mean(diff)) %>%
  pull()

#average of gmu difference in turnover percentage - losing games
gmu_diff_TO.P_losing <- bball %>%
  filter(LTeamID == 1206) %>%
  select(WTeamID, LTeamID, WTO.P, LTO.P) %>%
  mutate(diff =LTO.P - WTO.P) %>%
  summarise(GMU_W_DIFF_DTO.P = mean(diff)) %>%
  pull()

#average of gmu difference in turnovers - winning games
gmu_diff_TO <- bball %>%
  filter(WTeamID == 1206) %>%
  select(WTeamID, LTeamID, WTO, LTO) %>%
  mutate(diff = WTO - LTO) %>%
  summarise(GMU_W_DIFF_DTO = mean(diff)) %>%
  pull()

#average of gmu difference in turnovers - losing games
gmu_diff_TO_losing <- bball %>%
  filter(LTeamID == 1206) %>%
  select(WTeamID, LTeamID, WTO, LTO) %>%
  mutate(diff =LTO - WTO) %>%
  summarise(GMU_W_DIFF_DTO = mean(diff)) %>%
  pull()

#Scatterplot of Difference in Team Turnover Percentage
library(ggplot2)
library(tidyverse)

gmu_color <-  "#009E73"
a10colors <- "#0072B2"
bars_color <- "grey50"
gmu_losing_color <- "#D55E00"

#scatter plot of difference in turnvoer percentage
png("~/STAT 634/rc_basketball/Analysis/Figures/Turnovers_2.png")
diffs %>% 
  ggplot(aes(x = DTO.P)) +
  geom_histogram(fill = bars_color, col = "black") +
  geom_vline(aes(xintercept = mean(DTO.P)), col = a10colors, size = 2) +
  geom_vline(aes(xintercept = mean(gmu_W_dto.p)), col = gmu_color, size = 2) +
  geom_vline(aes(xintercept = mean(gmu_L_dto.p)), col = gmu_losing_color, size = 2) +
  xlab("Difference in Team Turnover Percentage (%)") +
  ylab("Number of Games") +
  ggtitle("Turnovers 2: Difference in Team Turnover Percentage") +
  scale_x_continuous(labels = scales::percent) +
  annotate("text", x = .105, y = 67.5, label = paste("GMU Losing Mean = ",round(mean(gmu_L_dto.p)*100,1),"%"), col = gmu_losing_color) +
  annotate("text", x = -.175, y = 67.5, label = paste("GMU Winning Mean = ",round(mean(gmu_W_dto.p)*100,1),"%"), col= gmu_color) +
  annotate("text", x = .105, y = 62.5, label = paste("GMU Losing SD = ",round(sd(gmu_L_dto.p)*100,1),"%"), col = gmu_losing_color) +
  annotate("text", x = -.175, y = 62.5, label = paste("GMU Winning SD = ",round(sd(gmu_W_dto.p)*100,1),"%"), col= gmu_color) +
  annotate("text", x = -.175, y = 51, label = paste("Mean = ",round(mean(diffs$DTO.P)*100,1),"%"), col = a10colors) +
  annotate("text", x = -.17, y = 47.5, label = paste("Median = ",round(median(diffs$DTO.P)*100,1),"%"), col = a10colors) +
  annotate("text", x = -.18, y = 43.5, label = paste("SD = ",round(sd(diffs$DTO.P)*100,1),"%"), col= a10colors) +
  theme_bw()
dev.off()

png("~/STAT 634/rc_basketball/Analysis/Figures/Turnovers_3.png")
diffs %>% 
  ggplot(aes(x = DTO)) +
  geom_histogram(fill = bars_color, col = "black") +
  geom_vline(aes(xintercept = mean(DTO)), col = a10colors, size = 2) +
  geom_vline(aes(xintercept = mean(gmu_W_dto)), col = gmu_color, size = 2) +
  geom_vline(aes(xintercept = mean(gmu_L_dto)), col = gmu_losing_color, size = 2) +
  xlab("Difference in Team Turnovers") +
  ylab("Number of Games") +
  ggtitle("Turnovers 3: Difference in Team Turnovers") +
  annotate("text", x = 8, y = 67.5, label = paste("GMU Losing Mean = ",round(mean(gmu_L_dto),1)), col = gmu_losing_color) +
  annotate("text", x = -9.2, y = 67.5, label = paste("GMU Winning Mean = ",round(mean(gmu_W_dto),1)), col= gmu_color) +
  annotate("text", x = 7.5, y = 62.5, label = paste("GMU Losing SD = ",round(sd(gmu_L_dto),1)), col = gmu_losing_color) +
  annotate("text", x = -9.5, y = 62.5, label = paste("GMU Winning SD = ",round(sd(gmu_W_dto),1)), col= gmu_color) +
  annotate("text", x = -11, y = 51, label = paste("Mean = ",round(mean(diffs$DTO),1)), col = a10colors) +
  annotate("text", x = -11, y = 47.5, label = paste("Median = ",round(median(diffs$DTO),1)), col = a10colors) +
  annotate("text", x = -11.3, y = 43.5, label = paste("SD = ",round(sd(diffs$DTO),1)), col= a10colors) +
  theme_bw()
dev.off()

#Scatterplot of differences in score and differences in turnover percentage
lm1 <- lm(DTO.P~DScore, data = diffs)
library(olsrr)
#ols_plot_cooksd_bar(lm1)
#ols_plot_dfbetas(lm1)
#ols_plot_dffits(lm1)
#ols_plot_resid_stud(lm1)
#ols_plot_resid_stand(lm1)
#ols_plot_resid_lev(lm1)
#ols_plot_resid_stud_fit(lm1)
lm_rsq <- summary(lm1)$adj.r.squared
png("~/STAT 634/rc_basketball/Analysis/Figures/Turnovers_1.png")
diffs %>%
  ggplot(aes(x = DTO.P, y = DScore)) +
  geom_point() +
  geom_smooth(method = lm, se = F,col="blue") +
  scale_x_continuous(labels = scales::percent) +
  geom_vline(aes(xintercept = mean(gmu_W_dto.p)), col = gmu_color, size = 2) +
  geom_vline(aes(xintercept = mean(gmu_L_dto.p)), col = gmu_losing_color, size = 2) +
  geom_vline(aes(xintercept = mean(DTO.P)), col = a10colors, size = 2) +
  annotate("text", x = .1048, y = 47.5, label = paste("GMU Losing Mean = ",round(mean(gmu_L_dto.p)*100,1),"%"), col = gmu_losing_color) +
  annotate("text", x = -.096, y = 47.5, label = paste("GMU Winning Mean = ",round(mean(gmu_W_dto.p)*100,1),"%"), col= gmu_color) +
  annotate("text", x = .1, y = 42.5, label = paste("GMU Losing SD = ",round(sd(gmu_L_dto.p)*100,1),"%"), col = gmu_losing_color) +
  annotate("text", x = -.1, y = 42.5, label = paste("GMU Winning SD = ",round(sd(gmu_W_dto.p)*100,1),"%"), col= gmu_color) +
  annotate("text", x = -.205, y = 47.5, label = paste("A10 Mean = ",round(mean(diffs$DTO.P)*100,1),"%"), col = a10colors) +
  annotate("text", x = -.21, y = 44, label = paste("A10 SD = ",round(sd(diffs$DTO.P)*100,1),"%"), col= a10colors) +
  annotate("text", x = -.21, y = 40.5, label = paste("Adj R-sq = ", round(lm_rsq,3)),col="blue") +
  xlab("Difference in Turnover Percentage (%)") +
  ylab("Difference in Final Score (Points)") +
  ggtitle("Turnovers 1: Differences in Score and Differences in Turnover Percentage") +
  theme_bw()
dev.off()

#linear regression of big four
#bigfour_to.p <- lm(DTO.P~DDR+DOR+DEFG.P+DFT.R, data=diffs)
#summary(bigfour_to.p)
#bigfour_to <- lm(DTO~DDR+DOR+DEFG.P+DFT.R, data=diffs)
#summary(bigfour_to)

#effect of turnover percentage on other big four
#dr_to.p <- lm(DDR~DTO.P, data=diffs)
#summary(dr_to.p)
#or_to.p <- lm(DOR~DTO.P, data=diffs)
#summary(or_to.p)
#efgp_to.p <- lm(DEFG.P~DTO.P, data=diffs)
#summary(efgp_to.p)
#ftr_to.p <- lm(DFT.R~DTO.P, data=diffs)
#summary(ftr_to.p)

#effect of turnovers on other big four
#dr_to <- lm(DDR~DTO, data=diffs)
#summary(dr_to)
#or_to <- lm(DOR~DTO, data=diffs)
#summary(or_to)
#efgp_to <- lm(DEFG.P~DTO, data=diffs)
#summary(efgp_to)
#ftr_to <- lm(DFT.R~DTO, data=diffs)
#summary(ftr_to)

#comparing turnovers to oppsing field goal percentage
#to_fgp <- lm(LFG.P~WTO,data=bball)
#summary(to_fgp)

#correlation matrix
cor_diffs <- diffs[,c(24,4:5,8,12,14,15,17:19,29,23,26:27)]
cor.bball <- cor(cor_diffs)
png("~/STAT 634/rc_basketball/Analysis/Figures/Turnovers_4.png")
corrplot(cor.bball, method = "circle",type="lower")
dev.off()


#linear regression based on correlation plot
lm_corr_dto.p <- lm(DTO.P ~ DFGA+DDR+DStl,data=diffs)
summary(lm_corr_dto.p)

#linear regression including game location
lm_loc_dto.p <- lm(DTO.P ~ WLoc +DFGA+DDR+DStl,data=diffs)
summary(lm_loc_dto.p)

#F-test of importance of location - both models
linearHypothesis(lm_loc_dto.p, c("WLocH=0","WLocN=0"),test="F")
#highly significant significant
linearHypothesis(lm_loc_dto, c("WLocH=0","WLocN=0"),test="F")
#highly significant significant

#####
#Final Model based on Correlation Plot
corr_best <- lm(DTO.P ~ WLoc +DFGA+DDR+DStl,data=diffs)
summary(corr_best)
#plot(corr_best)
#####

#lasso regression turnover percentage - no constraint
lasso <- function (x,y){
  b<-0
  lasso <- cv.glmnet(x=x,y=y, family="gaussian")
  lasso1 <- glmnet(x=x,y=y, lambda = lasso$lambda.min, family="gaussian")
  b <- as.matrix(lasso1[["beta"]])
  return(b)
}
parms_nogroup <- diffs[,c(3,5:15,17:23,25:27,29:ncol(diffs))]
lasso_noint <- lasso(as.matrix(parms_nogroup),as.matrix(diffs$DTO.P))
length(lasso_noint[lasso_noint!=0])

#linear regression using lasso coefficients
lm_lasso <- lm(DTO.P~NumOT+DFGA+DFGA3+DFTA+DOR
               +DDR+DAst+DStl+DBlk+DPF+DFG3.P
               +DFT.P+DOR.P+DDRtg+DTS.P, data=diffs)
summary(lm_lasso)

#F-test on lasso models - one term at a time
linearHypothesis(lm_lasso, c("NumOT=0"),test="F")
linearHypothesis(lm_lasso, c("DFGA=0"),test="F")
linearHypothesis(lm_lasso, c("DFGA3=0"),test="F")
linearHypothesis(lm_lasso, c("DFTA=0"),test="F")
linearHypothesis(lm_lasso, c("DOR=0"),test="F")
linearHypothesis(lm_lasso, c("DDR=0"),test="F")
linearHypothesis(lm_lasso, c("DAst=0"),test="F")
linearHypothesis(lm_lasso, c("DStl=0"),test="F")
linearHypothesis(lm_lasso, c("DBlk=0"),test="F")
linearHypothesis(lm_lasso, c("DPF=0"),test="F")
linearHypothesis(lm_lasso, c("DFG3.P=0"),test="F")
linearHypothesis(lm_lasso, c("DFT.P=0"),test="F")
linearHypothesis(lm_lasso, c("DOR.P=0"),test="F")
linearHypothesis(lm_lasso, c("DDRtg=0"),test="F")
linearHypothesis(lm_lasso, c("DTS.P=0"),test="F")
#DFGA, DFTA, DOR, DDR, DStl, DDRtg, DPF, DTS.P significant at the 10% level
#Joint F test
linearHypothesis(lm_lasso, c("DFGA=0","DFTA=0","DOR=0","DDR=0","DStl=0","DDRtg=0","DPF=0","DTS.P=0"),test="F")
#are significant all together
#Joint F test for excluded variables
linearHypothesis(lm_lasso, c("NumOT=0","DFGA3=0",
                             "DAst=0","DBlk=0","DFG3.P=0",
                             "DFT.P=0","DOR.P=0"),test="F")
#not significant - remove all above varibles

#lasso model linear regression with location
lm_lasso_location <- lm(DTO.P~WLoc+NumOT+DFGA+DFGA3+DFTA+DOR
                        +DDR+DAst+DStl+DBlk+DPF+DFG3.P
                        +DFT.P+DOR.P+DDRtg+DTS.P, data=diffs)
summary(lm_lasso_location)

#F tests for linear model with locations
linearHypothesis(lm_lasso_location, c("NumOT=0"),test="F")
linearHypothesis(lm_lasso_location, c("DFGA=0"),test="F")
linearHypothesis(lm_lasso_location, c("DFGA3=0"),test="F")
linearHypothesis(lm_lasso_location, c("DFTA=0"),test="F")
linearHypothesis(lm_lasso_location, c("DOR=0"),test="F")
linearHypothesis(lm_lasso_location, c("DDR=0"),test="F")
linearHypothesis(lm_lasso_location, c("DAst=0"),test="F")
linearHypothesis(lm_lasso_location, c("DStl=0"),test="F")
linearHypothesis(lm_lasso_location, c("DBlk=0"),test="F")
linearHypothesis(lm_lasso_location, c("DPF=0"),test="F")
linearHypothesis(lm_lasso_location, c("DFG3.P=0"),test="F")
linearHypothesis(lm_lasso_location, c("DFT.P=0"),test="F")
linearHypothesis(lm_lasso_location, c("DOR.P=0"),test="F")
linearHypothesis(lm_lasso_location, c("DDRtg=0"),test="F")
linearHypothesis(lm_lasso_location, c("DTS.P=0"),test="F")
#DFGA, DFTA, DOR, DDR, DStl, DDRtg, DPF, DTS.P significant at the 10% level
#Joint F test
linearHypothesis(lm_lasso_location, c("DFGA=0","DFTA=0","DOR=0","DDR=0","DStl=0","DDRtg=0","DPF=0","DTS.P=0"),test="F")
#are significant all together
#Joint F test for excluded variables
linearHypothesis(lm_lasso_location, c("NumOT=0","DFGA3=0",
                             "DAst=0","DBlk=0","DFG3.P=0",
                             "DFT.P=0","DOR.P=0"),test="F")
#not significant - remove all above varibles
#not significant
#compare model with location to model without
linearHypothesis(lm_lasso_location, c("WLocH=0","WLocN=0"),test="F")
#highly significant

#######
#Final LASSO Model
lasso_best <- lm(DTO.P~WLoc+DFGA+DFTA+DOR+DDR+DStl
                 +DPF+DDRtg+DTS.P,data=diffs)
summary(lasso_best)
#plot(lasso_best)
#######

#compare to model from correlation
summary(lm_loc_dto.p)

#lasso for turnovers - no constraint
parms_nogroup <- diffs[,c(3,5:15,17:23,26:27,29:ncol(diffs))]
lasso_noint_to <- lasso(as.matrix(parms_nogroup),as.matrix(diffs$DTO))
length(lasso_noint_to[lasso_noint_to!=0])
#linear regression of lasso variables
lm_lasso_to <- lm(DTO~NumOT+Pace+DFGA+DFGA3+DOR+DDR+DAst
             +DStl+DBlk+DPF+DFG3.P+DFT.P+DDRtg,data=diffs)
summary(lm_lasso_to)
#with location
lm_lasso_to_loc <- lm(DTO~WLoc+NumOT+Pace+DFGA+DFGA3+DOR+DDR+DAst
                  +DStl+DBlk+DPF+DFG3.P+DFT.P+DDRtg,data=diffs)
summary(lm_lasso_to_loc)

#F test on location
linearHypothesis(lm_lasso_to_loc, c("WLocH=0","WLocN=0"),test="F")
#highly significant

#F test for model with location
linearHypothesis(lm_lasso_to_loc, c("NumOT=0"),test="F")
linearHypothesis(lm_lasso_to_loc, c("Pace=0"),test="F")
linearHypothesis(lm_lasso_to_loc, c("DFGA=0"),test="F")
linearHypothesis(lm_lasso_to_loc, c("DFGA3=0"),test="F")
linearHypothesis(lm_lasso_to_loc, c("DOR=0"),test="F")
linearHypothesis(lm_lasso_to_loc, c("DDR=0"),test="F")
linearHypothesis(lm_lasso_to_loc, c("DAst=0"),test="F")
linearHypothesis(lm_lasso_to_loc, c("DStl=0"),test="F")
linearHypothesis(lm_lasso_to_loc, c("DBlk=0"),test="F")
linearHypothesis(lm_lasso_to_loc, c("DPF=0"),test="F")
linearHypothesis(lm_lasso_to_loc, c("DFG3.P=0"),test="F")
linearHypothesis(lm_lasso_to_loc, c("DFT.P"),test="F")
linearHypothesis(lm_lasso_to_loc, c("DDRtg=0"),test="F")
#Pace, DFGA, DFGA3, DOR, DDR, DStl, DPF, DFG3.P, DDRtg significant

#F test all not significant
linearHypothesis(lm_lasso_to_loc, c("NumOT=0","DAst=0",
                                "DBlk=0","DFT.P=0"),test="F")
#not significant

#####
#Final LASSO Model - Turnovers
lasso_best_to <- lm(DTO~WLoc+Pace+DFGA+DFGA3+DOR
                    +DDR+DStl+DPF+DFG3.P+DDRtg,data=diffs)
summary(lasso_best_to)
#plot(lasso_best_to)
#####

#compare to correlation model
summary(lm_loc_dto)

#model on other big four
lm_big_four <- lm(DTO.P~DOR+DDR+DEFG.P+DFT.R,data=diffs)
summary(lm_big_four)

#attempt to fit a model not using other big four or components
#turnover percentage
nocross <- diffs[,c(3,5,15,17,18,19,29)]
lasso_nocross <- lasso(as.matrix(nocross),as.matrix(diffs$DTO.P))
length(lasso_nocross[lasso_nocross!=0])+1

#linear model
lm_nocross <- lm(DTO.P~NumOT+DAst+DStl+DBlk+DPF+DDRtg,data=diffs)
summary(lm_nocross)

#with location
lm_nocross_loc <- lm(DTO.P~WLoc+NumOT+DAst+DStl+DBlk+DPF+DDRtg,data=diffs)
summary(lm_nocross_loc)

#F-test for significance of location
linearHypothesis(lm_nocross_loc, c("WLocH=0","WLocN=0"),test="F")
#highly significant

#F test of variables in location model
linearHypothesis(lm_nocross_loc, c("NumOT=0"),test="F")
linearHypothesis(lm_nocross_loc, c("DAst=0"),test="F")
linearHypothesis(lm_nocross_loc, c("DStl=0"),test="F")
linearHypothesis(lm_nocross_loc, c("DBlk=0"),test="F")
linearHypothesis(lm_nocross_loc, c("DPF=0"),test="F")
linearHypothesis(lm_nocross_loc, c("DDRtg=0"),test="F")
#DStl, DBlk, DDrtg are significant
#Ftest on all
linearHypothesis(lm_nocross_loc, c("NumOT=0","DAst=0","DPF=0"),test="F")
#not significant

#####
#Final Model LASSO no crossover terms
nocross_best <- lm(DTO.P~WLoc+DStl+DBlk+DDRtg,data=diffs)
summary(nocross_best)
plot(nocross_best)
