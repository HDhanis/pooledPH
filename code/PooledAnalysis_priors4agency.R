#deletes everything in the global environment
rm(list=ls()) 
#eliminates from memory
gc()

dataDir = "YOUR_DATA_DIRECTORY"
setwd(dataDir)

library(tidyverse)    # data manipulation
library(readxl)       # read excel
library(ggplot2)      # plot
library(brms)         # analysis
library(tidybayes)    # data manipulation
library(RColorBrewer) # needed for some extra colours in one of the graphs

study_labels = c("Pilot 1",
                 "Pilot 2", 
                 "Pilot 3", 
                 "Bernasconi (2021) Exp-1",
                 "Bernasconi (2021) Exp-2", 
                 "Pilot 4", 
                 "Orepic 2021 Exp-1", 
                 "Orepic_2021 Exp-2", 
                 "Pilot 5", 
                 "in-prep", 
                 "Serino 2021 Exp-1", 
                 "Serino 2021 Exp-2", 
                 "Blanke 2014", 
                 "Pilot 6", 
                 "Salomon (2020)", 
                 "Serino (2021) Exp-3", 
                 "Orepic (2024) Exp-1", 
                 "Faivre (2020) Exp-1",
                 "Faivre (2020) Exp-2",
                 "Faivre (2020) Exp-3",
                 "Orepic (2024) Exp-2", 
                 "Pilot 7",
                 "Dhanis (2024) Ses-1",
                 "Pilot 8",
                 "Albert 2024 Exp-1",
                 "Albert 2024 Exp-2")

filename = "PooledData.xlsx"

data_orig <- read_excel(filename, sheet = "Data")
temp_async = filter(data_orig, Synchrony == 0) # used later
temp_sync  = filter(data_orig, Synchrony == 1) # used later

noSubjects = data_orig$Subject_No
noSubjects = noSubjects[length(noSubjects)]

data_orig$Experiment_ID     = factor(data_orig$Experiment_ID, labels = study_labels)

data_orig$Subject_No        = factor(data_orig$Subject_No)
data_orig$Gender_IsMale     = factor(data_orig$Gender_IsMale, labels = c("Female", "Male"))
data_orig$Location          = factor(data_orig$Location, labels = c("Back", "Hand"))
data_orig$Position          = factor(data_orig$Position, labels = c("Standing", "Laying"))
data_orig$Order             = factor(data_orig$Order)
data_orig$Previous_Exposure = factor(data_orig$Previous_Exposure, labels = c("None", "Robot manipulation"))
data_orig$Cognitive_Load    = factor(data_orig$Cognitive_Load, labels = c("No", "Yes"))
data_orig$Force_field       = factor(data_orig$Force_field, labels = c("No", "Yes"))
data_orig$Condition         = factor(data_orig$Condition, labels = c("Sync", "Async"))
data_orig$Synchrony         = factor(data_orig$Synchrony, labels = c("Async", "Sync"))
data_orig$Duration_sec      = scale(data_orig$Duration_sec)
data_orig$`Question_ID_1`   = factor(round(data_orig$`Question_ID_1`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_2`   = factor(round(data_orig$`Question_ID_2`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_3`   = factor(round(data_orig$`Question_ID_3`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_4`   = factor(round(data_orig$`Question_ID_4`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_5`   = factor(round(data_orig$`Question_ID_5`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_6`   = factor(round(data_orig$`Question_ID_6`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_7`   = factor(round(data_orig$`Question_ID_7`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_8`   = factor(round(data_orig$`Question_ID_8`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_9`   = factor(round(data_orig$`Question_ID_9`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_10`  = factor(round(data_orig$`Question_ID_10`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_11`  = factor(round(data_orig$`Question_ID_11`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_12`  = factor(round(data_orig$`Question_ID_12`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_13`  = factor(round(data_orig$`Question_ID_13`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_14`  = factor(round(data_orig$`Question_ID_14`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_15`  = factor(round(data_orig$`Question_ID_15`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_16`  = factor(round(data_orig$`Question_ID_16`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_17`  = factor(round(data_orig$`Question_ID_17`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_18`  = factor(round(data_orig$`Question_ID_18`, digits = 0), ordered = TRUE)
#data_orig$`Question_ID_19`  = factor(round(data_orig$`Question_ID_19`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_20`  = factor(round(data_orig$`Question_ID_20`, digits = 0), ordered = TRUE)
data_orig$`Question_ID_21`  = factor(round(data_orig$`Question_ID_21`, digits = 0), ordered = TRUE)

questionToColumn = c(17:(17+20));

questions   = c(1:21)
noQuestions = max(questions)
questionsDescription = c("I felt as if I had no body", # control
                         "I felt as if I was touching my body", 
                         "I felt as if I was touching someone else' boby", 
                         "I felt I was behind my body", 
                         "I felt I had more than one body", # control
                         "I felt as if someone else was touching my body", 
                         "I felt as if someone was standing behing my body",
                         "I felt as if someone was standing in front of my body", 
                         "I felt as if I had two right hands",
                         "I felt as if the was a kind/gentle presence behind me",
                         "I felt as if there was a weird/unpleasend presence next to me",
                         "I felt as if I could hear someone else's voice in my mind",
                         "I felt as if I could share my thoughts with someone else",
                         "I had the impression that I could not control my own thoughts",
                         "I felt as if someone could read my mind or hear my thoughts",
                         "I felt as if I was not controlling my movements or actions",
                         "I felt anxious or stressed",
                         "I felt as if I was in front of my body",
                         "Presence position (0-no answer, 1-left, 2-middle, 3-right, 4-other)",
                         "I felt as if I was touched by a robot",
                         "I felt as if someone else was controlling my movements or actions")


colors_paper = c("#d4bbff", "#4589ff")
colors_ppt = c("#007380", "#00a79f")

fontsize_paper = 7
fontsize_ppt = 24


data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  if(ymax > 6){
    ymax = 6
  }
  if(ymin < 0){
    ymin = 0
  }
  return(c(y=m,ymin=ymin,ymax=ymax))
}
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# 1  no body (322) | 2 self-touch (554) | 5 more than one body (437) | 6 PE (580) | 7 PH (580) | 16 agency (189)
iquest = 16
sl <- RColorBrewer::brewer.pal(9,'Set1')
print(paste0(":::::::::: Starting analysis for question ", iquest, ": ", questionsDescription[iquest], " ::::::::::"))

# colours
EPFL_canard = "#007480"
EPFL_leman  = "#00A79F"

# Main full model -----------------------------------------------------------------

# there's like 4 participants without age 
selectedData = filter(data_orig, !is.na(Age))
selectedData$Age = (selectedData$Age-mean(selectedData$Age))/sd(selectedData$Age)
formula_full_dem =
  as.formula(paste(colnames(data_orig)[questionToColumn[iquest]],
                   "~ Condition +
                   Position + Condition:Position +
                   Location + Condition:Location +
                   Previous_Exposure + Condition:Previous_Exposure +
                   Duration_sec + Condition:Duration_sec +
                   Age + Age:Condition +
                   Gender_IsMale + (1+Condition|Experiment_ID)"))

# # use this for SoA
# formula_full_dem = 
#   as.formula(paste(colnames(data_orig)[questionToColumn[iquest]],
#                    "~ Condition + 
#                    Position + Condition:Position + 
#                    Previous_Exposure + Condition:Previous_Exposure +
#                    Duration_sec + Condition:Duration_sec +
#                    Age + Age:Condition +
#                    Gender_IsMale + (1+Condition|Experiment_ID)"))


print(formula_full_dem)
print(paste0("Selected question ", iquest, ": ", questionsDescription[iquest]))

model_fullExperimentalParameters_dem = 
  brm(formula_full_dem, 
      data = selectedData,
      family = cumulative("probit", threshold = "flexible"),
      prior = c(set_prior("normal(0,5)", class = "b"),
                set_prior("student_t(3, 0, 2)", class = "sd")),
      warmup = 2000, iter = 8000,
      cores = 4, chains = 4, 
      control = list(max_treedepth = 13), init = 0)

# For the forest plot:
library(tidyverse)
library(tidybayes)

colors_paper = c("#d4bbff", "#4589ff")
colors_ppt = c("#007380", "#00a79f")

A = spread_draws(model_fullExperimentalParameters_dem, 
                 b_ConditionAsync, r_Experiment_ID[Experiment_ID,])

# add the grand mean to the group-specific deviations
B = mutate(A, mu = b_ConditionAsync + r_Experiment_ID)

C = ungroup(B)

D = mutate(C, outcome = str_replace_all(Experiment_ID, "[.]", " "))

ggplot(D, aes(x = mu, y = reorder(outcome, mu))) +
  geom_vline(xintercept = 0, linetype = "dotdash", color = "black") +
  geom_vline(xintercept = -0.5, linetype = "dotdash", color = "grey") +
  geom_vline(xintercept = 0.5, linetype = "dotdash", color = "grey") +
  geom_vline(xintercept = -1.0, linetype = "dotdash", color = "grey") +
  geom_vline(xintercept = 1.0, linetype = "dotdash", color = "grey") +
  stat_halfeye(.width = .89, size = 2/3, fill = "#d4bbff", color = "#4589ff") +
  scale_x_continuous(limits = c(-1.25,1.3)) +
  labs(x = expression(italic("as Cohen's d")),
       y = NULL) +
  theme_minimal() +
  theme(panel.grid   = element_blank(),
        axis.ticks.y = element_blank(),
        axis.text.y  = element_text(hjust = 0.95),
        text = element_text(size = 7, family = "Arial"))


library(svglite)

figureName = "ForestPlot.svg"
ggsave(figureName, units="mm", width = 90, height = 130) # cut axis


# Model with order ------------------------------------------------------------------------------------------------
selectedData = filter(data_orig, !is.na(Age))
selectedData$Age = (selectedData$Age-mean(selectedData$Age))/sd(selectedData$Age)
formula_full_dem_order = 
  as.formula(paste(colnames(data_orig)[questionToColumn[iquest]],
                   "~ Condition +
                   Position + Condition:Position + 
                   Location + Condition:Location + 
                   Previous_Exposure + Condition:Previous_Exposure +
                   Duration_sec + Duration_sec:Condition +
                   Age + Age:Condition +
                   Gender_IsMale + 
                   Order + Order:Condition + 
                   (1+Condition|Experiment_ID)"))

model_fullExperimentalParameters_dem_order = 
  brm(formula_full_dem_order, 
      data = selectedData,
      family = cumulative("probit", threshold = "flexible"),
      prior = c(set_prior("normal(0.4534, 0.4271)", class = "Intercept", coef = "1"),
                set_prior("normal(1.0222, 0.4294)", class = "Intercept", coef = "2"),
                set_prior("normal(1.4106, 0.4310)", class = "Intercept", coef = "3"),
                set_prior("normal(1.9057, 0.4333)", class = "Intercept", coef = "4"),
                set_prior("normal(2.4085, 0.4378)", class = "Intercept", coef = "5"),
                set_prior("normal(2.9432, 0.4511)", class = "Intercept", coef = "6"),
                set_prior("normal(0.8941, 0.2484)", coef = "ConditionAsync"),
                set_prior("normal(0.3479, 0.6182)", coef = "PositionLaying"),
                set_prior("normal(0.5656, 0.3467)", coef = "LocationHand"),
                set_prior("normal(0.0868, 0.3149)", coef = "Previous_ExposureRobotmanipulation"),
                set_prior("normal(-0.4565, 0.6029)", coef = "Duration_sec"),
                set_prior("normal(-0.1789, 0.0963)", coef = "Age"),
                set_prior("normal(-0.1990, 0.1165)", coef = "Gender_IsMaleMale"),
                set_prior("normal(-0.4995, 0.3022)", coef = "ConditionAsync:PositionLaying"),
                set_prior("normal(-0.6853, 0.4150)", coef = "ConditionAsync:LocationHand"),
                set_prior("normal(0.2116, 0.3776)", coef = "ConditionAsync:Previous_ExposureRobotmanipulation"),
                set_prior("normal(0.0242, 0.3237)", coef = "ConditionAsync:Duration_sec"),
                set_prior("normal(0.1105, 0.1197)", coef = "ConditionAsync:Age")),
      warmup = 2000, iter = 8000,
      cores = 4, chains = 4, 
      control = list(max_treedepth = 13), init=0)


# Model: EHI ------------------------------------------------------------------------------------------------------
formula_full_dem_EHI = 
  as.formula(paste(colnames(data_orig)[questionToColumn[iquest]],
                   "~ Condition + 
                   Position + Condition:Position + 
                   Location + Condition:Location +
                   Previous_Exposure + Condition:Previous_Exposure +
                   Duration_sec + Duration_sec:Condition +
                   Age + Age:Condition +
                   Gender_IsMale +
                   EHI + EHI:Condition + 
                   (1+Condition|Experiment_ID)"))

selectedData = filter(data_orig, !is.na(Age), !is.na(EHI))
selectedData$EHI = (selectedData$EHI-mean(selectedData$EHI))/sd(selectedData$EHI)
selectedData$Age = (selectedData$Age-mean(selectedData$Age))/sd(selectedData$Age)
model_fullExperimentalParameters_dem_EHI = 
  brm(formula_full_dem_EHI, 
      data = selectedData,
      family = cumulative("probit", threshold = "flexible"),
      prior = c(set_prior("normal(0.4534, 0.4271)", class = "Intercept", coef = "1"),
                set_prior("normal(1.0222, 0.4294)", class = "Intercept", coef = "2"),
                set_prior("normal(1.4106, 0.4310)", class = "Intercept", coef = "3"),
                set_prior("normal(1.9057, 0.4333)", class = "Intercept", coef = "4"),
                set_prior("normal(2.4085, 0.4378)", class = "Intercept", coef = "5"),
                set_prior("normal(2.9432, 0.4511)", class = "Intercept", coef = "6"),
                set_prior("normal(0.8941, 0.2484)", coef = "ConditionAsync"),
                set_prior("normal(0.3479, 0.6182)", coef = "PositionLaying"),
                set_prior("normal(0.5656, 0.3467)", coef = "LocationHand"),
                set_prior("normal(0.0868, 0.3149)", coef = "Previous_ExposureRobotmanipulation"),
                set_prior("normal(-0.4565, 0.6029)", coef = "Duration_sec"),
                set_prior("normal(-0.1789, 0.0963)", coef = "Age"),
                set_prior("normal(-0.1990, 0.1165)", coef = "Gender_IsMaleMale"),
                set_prior("normal(-0.4995, 0.3022)", coef = "ConditionAsync:PositionLaying"),
                set_prior("normal(-0.6853, 0.4150)", coef = "ConditionAsync:LocationHand"),
                set_prior("normal(0.2116, 0.3776)", coef = "ConditionAsync:Previous_ExposureRobotmanipulation"),
                set_prior("normal(0.0242, 0.3237)", coef = "ConditionAsync:Duration_sec"),
                set_prior("normal(0.1105, 0.1197)", coef = "ConditionAsync:Age")),
      warmup = 2000, iter = 8000,
      cores = 4, chains = 4, 
      control = list(max_treedepth = 13))


# Model: PDI ------------------------------------------------------------------------------------------------------
formula_full_dem_PDI = 
  as.formula(paste(colnames(data_orig)[questionToColumn[iquest]],
                   "~ Condition + 
                   Position + Condition:Position + 
                   Location + Condition:Location + 
                   Previous_Exposure + Condition:Previous_Exposure +
                   Duration_sec + Duration_sec:Condition +
                   Age + Age:Condition +
                   Gender_IsMale +
                   PDI + PDI:Condition + 
                   (1+Condition|Experiment_ID)"))

selectedData = filter(data_orig, !is.na(Age), !is.na(PDI))
selectedData$PDI = (selectedData$PDI-mean(selectedData$PDI))/sd(selectedData$PDI)
selectedData$Age = (selectedData$Age-mean(selectedData$Age))/sd(selectedData$Age)
model_fullExperimentalParameters_dem_PDI = 
  brm(formula_full_dem_PDI, 
      data = selectedData,
      family = cumulative("probit", threshold = "flexible"),
      prior = c(set_prior("normal(0.4534, 0.4271)", class = "Intercept", coef = "1"),
                set_prior("normal(1.0222, 0.4294)", class = "Intercept", coef = "2"),
                set_prior("normal(1.4106, 0.4310)", class = "Intercept", coef = "3"),
                set_prior("normal(1.9057, 0.4333)", class = "Intercept", coef = "4"),
                set_prior("normal(2.4085, 0.4378)", class = "Intercept", coef = "5"),
                set_prior("normal(2.9432, 0.4511)", class = "Intercept", coef = "6"),
                set_prior("normal(0.8941, 0.2484)", coef = "ConditionAsync"),
                set_prior("normal(0.3479, 0.6182)", coef = "PositionLaying"),
                set_prior("normal(0.5656, 0.3467)", coef = "LocationHand"),
                set_prior("normal(0.0868, 0.3149)", coef = "Previous_ExposureRobotmanipulation"),
                set_prior("normal(-0.4565, 0.6029)", coef = "Duration_sec"),
                set_prior("normal(-0.1789, 0.0963)", coef = "Age"),
                set_prior("normal(-0.1990, 0.1165)", coef = "Gender_IsMaleMale"),
                set_prior("normal(-0.4995, 0.3022)", coef = "ConditionAsync:PositionLaying"),
                set_prior("normal(-0.6853, 0.4150)", coef = "ConditionAsync:LocationHand"),
                set_prior("normal(0.2116, 0.3776)", coef = "ConditionAsync:Previous_ExposureRobotmanipulation"),
                set_prior("normal(0.0242, 0.3237)", coef = "ConditionAsync:Duration_sec"),
                set_prior("normal(0.1105, 0.1197)", coef = "ConditionAsync:Age")),
      warmup = 2000, iter = 8000,
      cores = 4, chains = 4, 
      control = list(max_treedepth = 13), init = 0)

# Assess convergency (catterpillar plots) ---------------------------------
modelADPT_full = ggs(model_fullExperimentalParameters_dem)
ggplot(filter(modelADPT_full, Parameter %in% 
                betas[c(4,5,6)]),
       aes(x   = Iteration,
           y   = value, 
           col = as.factor(Chain)))+
  geom_line() +
  scale_y_continuous(limits = c(-3, 3)) + 
  geom_vline(xintercept = 1000)+
  facet_grid(Parameter ~ . ,
             scale  = 'free_y',
             switch = 'y')+
  labs(title = "Caterpillar Plots", 
       col   = "Chains")


# Intercept only predictions -----------------------

# check rating histogram
ggplot(data = data_orig, aes(x = Question_ID_7)) +
  geom_bar() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = "PH rating - (nal data)", y = "Counts") + 
  theme_minimal()

pp_check(model_fullExperimentalParameters)

# Real PH rating histogram
histo = ggplot(data_orig, aes(x = Question_ID_7)) +
  geom_bar() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  labs(x = "PH rating (observed data)", y = "Counts") + 
  theme_minimal()
histo


# Mostly testing, the appropriate thresholds are computed in fuller models
# gives posterior samples, selects only the intercepts, and adds the iteration column
post_predictions = posterior_samples(model_fullExperimentalParameters_dem) %>%
  select(`b_Intercept[1]`:`b_Intercept[6]`) %>%
  mutate(iter = 1:n())

# check if counts have similar histogram
set.seed(23)
post_predictions %>% 
  mutate(z = rnorm(n(), mean = 0, sd = 1)) %>% 
  mutate(Question_ID_7 = case_when(
    z < `b_Intercept[1]` ~ 0,
    z < `b_Intercept[2]` ~ 1,
    z < `b_Intercept[3]` ~ 2,
    z < `b_Intercept[4]` ~ 3,
    z < `b_Intercept[5]` ~ 4,
    z < `b_Intercept[6]` ~ 5,
    z >= `b_Intercept[6]` ~ 6
  ) %>% as.factor(.)) %>% 
  
  ggplot(aes(x = Question_ID_7)) +
  geom_bar() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) + 
  labs(x = "PH rating (simulated data: using intercepts only)", y = "Counts") + 
  theme_minimal()

# magrittr: https://cran.r-project.org/web/packages/magrittr/vignettes/magrittr.html
# meaning of transmute and mutate
# https://www.datanovia.com/en/lessons/compute-and-add-new-variables-to-a-data-frame-in-r/
# basically:
# post = select(post, -iter)
# post = mutate_all(post, .funs) 
# post = transmute(post, .funs)
# 

colors_paper = c("#d4bbff", "#4589ff")
fontsize_paper = 7

post_predictions %>% 
  select(-iter) %>% 
  mutate_all(.funs = ~pnorm(. , 0, 1)) %>% 
  transmute(`p[Q7==0]` = `b_Intercept[1]`,
            `p[Q7==1]` = `b_Intercept[2]` - `b_Intercept[1]`,
            `p[Q7==2]` = `b_Intercept[3]` - `b_Intercept[2]`,
            `p[Q7==3]` = `b_Intercept[4]` - `b_Intercept[3]`,
            `p[Q7==4]` = `b_Intercept[5]` - `b_Intercept[4]`,
            `p[Q7==5]` = `b_Intercept[6]` - `b_Intercept[5]`,
            `p[Q7==6]` = 1 - `b_Intercept[6]`) %>% 
  set_names(0:6) %>% 
  pivot_longer(everything(), names_to = "Q7") %>% 
  
  ggplot(aes(x = value, y = Q7)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89,
               fill = colors_paper[1], color = colors_paper[2], size = 1/2) +
  scale_x_continuous(expression(italic(p)*"("*italic(rating)*")"), 
                     expand = c(0, 0), limits = c(0, 0.65)) + 
  labs(y = "Rating") +
  theme_minimal() + 
  theme(legend.position = "none",
        text = element_text(size = fontsize_paper, family = "Arial"))
ggsave("PH_Intercepts.svg", units="mm", width = 70, height = 70) # for all


tibble(x = seq(from = -3.5, to = 3.5, by = .01)) %>%
  mutate(d = dnorm(x)) %>% 
  
  ggplot(aes(x = x, ymin = 0, ymax = d)) +
  geom_ribbon(fill = "black") +
  geom_vline(xintercept = fixef(model_fullExperimentalParameters_dem)[1:6, 1], 
                                color = colors_paper[1], linetype = 2, size = 0.75) +
  scale_x_continuous("Posterior modes for the rating scale intercepts", 
                     breaks = fixef(model_fullExperimentalParameters_dem)[1:6, 1],
                     labels = parse(text = str_c("theta[", 1:6, "]"))) +
  scale_y_continuous(NULL, breaks = NULL, expand = expansion(mult = c(0, 0.05))) +
  # ggtitle("Standard normal distribution underlying the ordinal Y data:",
  #         subtitle = "The dashed vertical lines mark the posterior means for the thresholds.") +
  coord_cartesian(xlim = c(-3, 5)) + 
  theme_minimal() +
  theme(
        # title = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial"))
ggsave("PH_Intercepts_norm.svg", units="mm", width = 70, height = 70) # for all




# Coefficients for different effects --------------------------------------

# model_fullExperimentalParameters_dem_EHI
post = posterior_samples(model_fullExperimentalParameters_dem) %>%
  mutate(iter = 1:n())

figure_name = c("PH_Async.svg",
                "PH_BodyPosition.svg",
                "PH_BodyLocation.svg",
                "PH_Previousexposure.svg",
                "PH_CognitiveLoad.svg",
                "PH_Duration.svg",
                "PH_Age.svg",
                "PH_Async_BodyPosition.svg",
                "PH_Async_BodyLocation.svg",
                "PH_Async_PreviousExposure.svg",
                "PH_Async_CognitiveLoad.svg",
                "PH_Async_Duration.svg",
                "PH_Async_Age.svg",
                "PH_SexAtBirth.svg")

betas = c("b_ConditionAsync", 
          "b_PositionLaying", 
          "b_LocationHand",
          "b_Previous_ExposureRobotmanipulation", 
          "b_Cognitive_LoadYes", 
          "b_Duration_sec", 
          "b_Age",
          "b_ConditionAsync:PositionSupine",
          "b_ConditionAsync:LocationHand",
          "b_ConditionAsync:Previous_ExposureRobotmanipulation", 
          "b_ConditionAsync:Cognitive_Load_Yes",
          "b_ConditionAsync:Duration_sec",
          "b_ConditionAsync:Age",
          "b_Gender_IsMaleMale")

xtitles = c("Asynchrony\n(async)", 
            "Body Position\n(supine)",
            "Body Location\n(hand)",
            "Previous Exposure\n(+)",
            "Cognitive Load\n(+)", 
            "Duration\n(increasing)",
            "Age\n(increasing in age)",
            "Asynchrony : Body Position\n(Async, supine)",
            "Asynchrony : Body Location\n(Async, hand)",
            "Asynchrony : Prev. Exp.\n(Async, +)", 
            "Asynchrony : Cog. Load\n(Async, +)",
            "Asynchrony : Duration\n(Async, Increasing)",
            "Asynchrony : Age\n(Async, Increasing)",
            "Sex at birth\n(Male)"
            )

effects_condE = c("Condition", "Position", "Previous_Exposure", "Cognitive_Load",  "Duration_sec")

# figures
library(extrafont)
# font_import()
# loadfonts(device = "win")

colors_paper = c("#d4bbff", "#4589ff")
colors_ppt = c("#007380", "#00a79f")

fontsize_paper = 7
fontsize_ppt = 24


# estimate of beta 
for(i in c(2:7)){ 
    p = ggplot(post, aes_string(x = betas[i], y = 0, fill = paste0("abs(stat(x)) < 0.1"))) +
    stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
    # scale_x_continuous(limits = c(-1.0,1.3)) + # for async only
    scale_fill_manual(values = c(colors_paper[1], "gray80")) +
    geom_vline(xintercept = 0.1, linetype = "dashed") +
    geom_vline(xintercept = -0.1, linetype = "dashed") +
    # geom_vline(xintercept = 0, linetype = "dotdash", color = "black") +  # for async only
    # geom_vline(xintercept = 0.5, linetype = "dotdash", color = "grey") +  # for async only
    # geom_vline(xintercept = -0.5, linetype = "dotdash", color = "grey") + # for async only
    # geom_vline(xintercept = 1.0, linetype = "dotdash", color = "grey") +  # for async only
    # geom_vline(xintercept = -1.0, linetype = "dotdash", color = "grey") + # for async only
    theme_minimal() + 
    theme(panel.grid.major.y = element_blank(),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.ticks.y = element_blank(),
          legend.position = "none",
          axis.title.y = element_blank(),
          axis.text.y = element_blank(),
          text = element_text(size = fontsize_paper, family = "Arial")) +
    xlab(xtitles[i])

    print(p)
    
    if(i==1){
      ggsave(figure_name[i], units="mm", width = 61.35, height = 40) # for all
    } else if (i==7){ # age
      ggsave(figure_name[i], units="mm", width = 42, height = 48) # for all
    }else if(i>1){
      ggsave(figure_name[i], units="mm", width = 31, height = 40) # for all
    }
}

# estimate of the effect over the different ordered answers
ce_sync = conditional_effects(model_fullExperimentalParameters_dem, categorical = T, effects = "Age")

ce_sync_plot <- plot(ce_sync, plot = TRUE)[[1]] 
ce_sync_plot + xlab("Age") +
  theme_minimal() +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) 

ggsave("PH_ConditionalEffects_Age.svg", units="mm", width = 70, height = 60) 
  

# include below for duration
ce_sync_plot + 
  scale_y_continuous(limits = c(0, 0.80)) +
  # scale_x_discrete(labels=c("Standing" = "Original Robot", "Laying" = "MR-compatible robot"))+
  # scale_x_discrete(labels=c("None" = "None", "Robot manipulation" = "Yes"))+
  scale_colour_discrete(name = "PH rating", labels = c("0", "1", "2", "3", "4", "5", "6")) +
  scale_fill_discrete(name = "PH rating", labels = c("0", "1", "2", "3", "4", "5", "6")) + 
  facet_wrap("cats__")

figureName = "PH_ConditionalEffects_PreviousExposure.tiff"
ggsave(figureName, units="mm", width = 135, height = 135, dpi=500) # then divide by 3

# below for age, EHI and PDI
figureName = "PH_ConditionalEffects_Age.tiff"
ggsave(figureName, units="mm", width = 65, height = 60, dpi=500) 

i = 8
post %>% 
  ggplot(aes(x = `b_ConditionAsync:PositionLaying`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) +
  xlab(xtitles[i])

figureName = "PH_Interaction_Condition_Position.svg"
ggsave(figureName, units="mm", width = 35, height = 40)

i = 9
post %>% 
  ggplot(aes(x = `b_ConditionAsync:LocationHand`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) +
  xlab(xtitles[i])

figureName = "PH_Interaction_Location_Hand.svg"
ggsave(figureName, units="mm", width = 35, height = 40)

i = 10
post %>% 
  ggplot(aes(x = `b_ConditionAsync:Previous_ExposureRobotmanipulation`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.10, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) +
  xlab(xtitles[i])

figureName = "PH_Interaction_Condition_PreviousExp.svg"
ggsave(figureName, units="mm", width = 35, height = 40)


i = 11
post %>% 
  ggplot(aes(x = `b_ConditionAsync:Cognitive_LoadYes`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) +
  xlab(xtitles[i])

figureName = "PH_Interaction_Condition_CogLoad.svg"
ggsave(figureName, units="mm", width = 35, height = 40)


i = 12
post %>% 
  ggplot(aes(x = `b_ConditionAsync:Duration_sec`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) +
  xlab(xtitles[i])

figureName = "PH_Interaction_Condition_Duration.svg"
ggsave(figureName, units="mm", width = 35, height = 40)


i = 13
post %>% 
  ggplot(aes(x = `b_ConditionAsync:Age`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Arial")) +
  xlab(xtitles[i])

figureName = "PH_Interaction_Condition_Age.svg"
ggsave(figureName, units="mm", width = 42, height = 48)


# Plot: order -----------------------------------------------------------------------------------------------------

post_order = posterior_samples(model_fullExperimentalParameters_dem_order) %>%
  mutate(iter = 1:n())

post_order %>% 
  ggplot(aes(x = b_Order2, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = 7, family = "Times New Roman")) +
  xlab("Order\n(second)")
figureName = "PH_Order.svg"
ggsave(figureName, units="mm", width = 35, height = 40)

post_order %>% 
  ggplot(aes(x = `b_ConditionAsync:Order2`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = 7, family = "Times New Roman")) +
  xlab("Synchrony : Order\n(Async, Second)")
figureName = "PH_Interaction_Condition_Order.svg"
ggsave(figureName, units="mm", width = 35, height = 40)


# Plot EHI --------------------------------------------------------------------------------------------------------

post_EHI = posterior_samples(model_fullExperimentalParameters_dem_EHI) %>%
  mutate(iter = 1:n())

post_EHI %>% 
  ggplot(aes(x = `b_EHI`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = 7, family = "Times New Roman")) +
  xlab("EHI\n(right-handedness)")

figureName = "PH_EHI.svg"
ggsave(figureName, units="mm", width = 42, height = 48, dpi=500)

post_EHI %>% 
  ggplot(aes(x = `b_ConditionAsync:EHI`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  # geom_vline(xintercept = -0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = 7, family = "Times New Roman")) +
  xlab("Synchrony : EHI\n(Async, right-handedness)")

figureName = "PH_Interaction_EHI.svg"
ggsave(figureName, units="mm", width = 42, height = 48, dpi=500)



# Plot: PDI -------------------------------------------------------------------------------------------------------

post_PDI = posterior_samples(model_fullExperimentalParameters_dem_PDI) %>%
  mutate(iter = 1:n())

post_PDI %>% 
  ggplot(aes(x = `b_PDI`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  geom_vline(xintercept = 0.1, linetype = "dashed") +
  # geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Times New Roman")) +
  xlab("PDI\n(higher scores)")

figureName = "PH_PDI.svg"
ggsave(figureName, units="mm", width = 42, height = 48, dpi=500)

post_PDI %>% 
  ggplot(aes(x = `b_ConditionAsync:PDI`, y = 0, fill = abs(stat(x)) > 0.1)) +
  stat_halfeye(point_interval = mode_hdi, .width = .89, color = colors_paper[2]) +
  # scale_y_continuous(NULL, breaks = NULL) + 
  scale_fill_manual(values = c("gray80", colors_paper[1])) +
  geom_vline(xintercept = 0.1, linetype = "dashed") +
  geom_vline(xintercept = c(-0.1, 0.1), linetype = "dashed") +
  theme_minimal() + 
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        text = element_text(size = fontsize_paper, family = "Times New Roman")) +
  xlab("Synchrony : PDI\n(Async, higher scores)")

figureName = "PH_Interaction_PDI.svg"
ggsave(figureName, units="mm", width = 42, height = 48, dpi=500)


ce_sync = conditional_effects(model_fullExperimentalParameters_dem_PDI, categorical = T, effects = "PDI")

ce_sync_plot <- plot(ce_sync, plot = TRUE)[[1]]
ce_sync_plot + theme_minimal() +
  ylab("Rating probability") +
  scale_y_continuous(limits = c(0, 1)) +
  scale_colour_discrete(name = "PH rating", labels = c("0", "1", "2", "3", "4", "5", "6")) +
  scale_fill_discrete(name = "PH rating", labels = c("0", "1", "2", "3", "4", "5", "6")) +
  theme(panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        text = element_text(size = fontsize_paper, family = "Times New Roman")) 

figureName = "PH_ConditionalEffects_PDI.svg"
ggsave(figureName, units="mm", width = 90, height = 75, dpi=500)



# Get overlaps HDI ROPE ---------------------------------------------------

library("bayestestR")
rope(model_fullExperimentalParameters_dem, range = c(-0.1, 0.1), ci = 1)
rope(model_fullExperimentalParameters_dem_order, range = c(-0.1, 0.1), ci = 1)
rope(model_fullExperimentalParameters_dem_EHI, range = c(-0.1, 0.1), ci = 1)
rope(model_fullExperimentalParameters_dem_PDI, range = c(-0.1, 0.1), ci = 1)



