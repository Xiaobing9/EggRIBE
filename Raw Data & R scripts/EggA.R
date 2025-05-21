setwd("C:/Users/mrthy/Desktop/Uni boiiiiis/PhD/Papers to Submit/Egg Bystander/Analysis")

##### Libraries #####
library(tidyverse)
library(Matrix)
library(lme4)
library(emmeans)
library(effects)
library(car)
library(ggpubr)
library(cowplot)
library(sjPlot) #for plotting lmer and glmer mods
library(sjmisc) 
library(effects)
library(sjstats) #use for r2 functions


#### data ####
Eggdata <- read.csv("EggData.csv")
FEggdata <- read.csv("FeEggData.csv")
MEggdata <- read.csv("MaEggData.csv")

######population level#####

####controlling for replication effects####
MatTime <- lmer(MatTime ~  Treatment + Sex + (1 | Rep), data = Eggdata)
MatMass <- lmer(MatMass ~  Treatment + Sex + (1 | Rep), data = Eggdata)
MatRate <- lmer(GrowthRate ~  Treatment + Sex + (1 | Rep), data = Eggdata)

####confidence intervals####
confint.merMod(MatTime)
confint.merMod(MatMass)
confint.merMod(MatRate)


####estimate means####
emmeans(MatTime, specs = "Treatment") |> pwpm()
emmeans(MatMass, specs = "Treatment") |> pwpm()
emmeans(MatRate, specs = "Treatment") |> pwpm()

####plots####
time <- sjPlot::plot_model(MatTime, show.values = T, show.p = T, title = "Time to Maturation (day) Compared to Sham",
                           vline.color = "#061423",
                           axis.labels = 
                             c("Males compared to Females", "Gauze", "Bystander"
                             )) +
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Maturation Time (Days)") 
#

mass <- sjPlot::plot_model(MatMass, show.values = T, show.p = T, title = "Mass at Maturation (mg) Compared to Sham",
                           vline.color = "#061423",
                           axis.labels = 
                             c("Males compared to Females", "Gauze", "Bystander"
                             )) +
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Mass at Maturation (mg)") 
#

rate <- sjPlot::plot_model(MatRate, show.values = T, show.p = T, title = "Average Growth Rate (mg/day) Compared to Sham",
                           vline.color = "#061423",
                           axis.labels = 
                             c("Males compared to Females", "Gauze", "Bystander"
                             ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Average Growth Rate (mg/day)") 

ggarrange(time, mass, rate, ncol = 1,labels = "auto")

#####sex specfic#####

####replication control####
FMatTime <- lmer(MatTime ~  Treatment + (1 | Rep), data = FEggdata)
FMatMass <- lmer(MatMass ~  Treatment +  (1 | Rep), data = FEggdata)
FMatRate <- lmer(GrowthRate ~  Treatment +  (1 | Rep), data = FEggdata)

MMatTime <- lmer(MatTime ~  Treatment + (1 | Rep), data = MEggdata)
MMatMass <- lmer(MatMass ~  Treatment +  (1 | Rep), data = MEggdata)
MMatRate <- lmer(GrowthRate ~  Treatment +  (1 | Rep), data = MEggdata)

#### confidence intervals ####

confint.merMod(FMatTime)
confint.merMod(FMatMass)
confint.merMod(FMatRate)

confint.merMod(MMatTime)
confint.merMod(MMatMass)
confint.merMod(MMatRate)

#### plots ####
Ftime <- sjPlot::plot_model(FMatTime, show.values = T, show.p = T, title = "Female Time to Maturation (day) Compared to Sham",
                            vline.color = "#061423",
                            axis.labels = 
                              c("Gauze", "Bystander"
                              ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Maturation Time (Days)") 
#

Fmass <- sjPlot::plot_model(FMatMass, show.values = T, show.p = T, title = "Female Mass at Maturation (mg) Compared to Sham",
                            vline.color = "#061423",
                            axis.labels = 
                              c("Gauze", "Bystander"
                              ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Mass at Maturation (mg)") 
#

Frate <- sjPlot::plot_model(FMatRate, show.values = T, show.p = T, title = "Female Average Growth Rate (mg/day) Compared to Sham",
                            vline.color = "#061423",
                            axis.labels = 
                              c("Gauze", "Bystander"
                              ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Average Growth Rate (mg/day)") 

ggarrange(Ftime, Fmass, Frate, ncol = 1,labels = "auto")

Mtime <- sjPlot::plot_model(MMatTime, show.values = T, show.p = T, title = "Male Time to Maturation (day) Compared to Sham",
                            vline.color = "#061423",
                            axis.labels = 
                              c("Gauze", "Bystander"
                              ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Maturation Time (Days)") 
#

Mmass <- sjPlot::plot_model(MMatMass, show.values = T, show.p = T, title = "Male Mass at Maturation (mg) Compared to Sham",
                            vline.color = "#061423",
                            axis.labels = 
                              c("Gauze", "Bystander"
                              ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Mass at Maturation (mg)") 
#

Mrate <- sjPlot::plot_model(MMatRate, show.values = T, show.p = T, title = "Male Average Growth Rate (mg/day) Compared to Sham",
                            vline.color = "#061423",
                            axis.labels = 
                              c("Gauze", "Bystander"
                              ))+
  theme_classic()+
  scale_colour_manual(values = c("black", "black")) +
  ylab("Average Growth Rate (mg/day)") 

ggarrange(Mtime, Mmass, Mrate, ncol = 1,labels = "auto")


ggarrange(time, Ftime, Mtime, ncol = 1, labels = "auto", align = c("hv"))
ggarrange(mass, Fmass, Mmass, ncol = 1, labels = "auto", align = c("hv"))
ggarrange(rate, Frate, Mrate, ncol = 1, labels = "auto", align = c("hv"))


#### emmeans ####
emmeans(FMatTime, specs = "Treatment") |> pwpm()
emmeans(FMatMass, specs = "Treatment") |> pwpm()
emmeans(FMatRate, specs = "Treatment") |> pwpm()

emmeans(MMatTime, specs = "Treatment") |> pwpm()
emmeans(MMatMass, specs = "Treatment") |> pwpm()
emmeans(MMatRate, specs = "Treatment") |> pwpm()
