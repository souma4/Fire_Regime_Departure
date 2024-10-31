library(tidyverse)
library(foreach)
library(doParallel)
library(PresenceAbsence)
library(ggrepel)
#library(predicts)
create_class <- function(mortality){
  if(is.na(mortality)){
    NA
  } else if (mortality >=0 & mortality < 25){
    "low"
  } else if (mortality >=25 & mortality < 75){
    "mixed"
  } else {
    "high"
  }
}
logit_bin_classes <- function(class, low = T){
  if(low == T){
    if(class == "low"){
      0
    } else{
      1
    }
  }else {
    if(class == "high"){
      1
    } else{
      0
    } 
  }
}

sigmoid <- function(z){
  1/(1+exp(-z))
}

Loss <- function(w, X, Y){
  z = w[1] +w[2]*X
  s = sigmoid(z)
  L = Y*log(s) + (1-Y)*log(1-s)
  return(sum(L))
}

L_grad <- function(w, X, Y) {
  Phi = array(data = 1, dim = c(length(X), 2))
  Phi[,2] = X
  grad_mat <- c((Y-sigmoid(Phi %*% w))) * Phi
  grad = -(colSums(grad_mat))
  return(grad)
}

descent <- function(X, w, label, eta, n_sims){
  N = 2
  params = array(data = 0, dim = c(n_sims, N))
  params[1,] <- w
  for (i in 1:(n_sims-1)){
    gradient = L_grad( params[i,], X, label)
    next_params = params[i,] - (eta*gradient)
    params[i+1,] = next_params
  }
  return(params)
}

curve_classifier <- function(y, thresh){
  if (y < thresh){
    0
  } else{
    1
  }
}

pct_sev_classifier <- function(pct_mort){
  ifelse(pct_mort <25, "low",
         ifelse(pct_mort <75, "mixed",
                ifelse(pct_mort <= 100, "high", NA)))
}
union_dataframes <- function(df_list) {
  if(length(df_list) == 1) {
    return(df_list[[1]])
  } else {
    return(Reduce(union_all, df_list))
  }
}


df_1 <- read_csv("C:/Users/JChandler/Desktop/Work/Masters/Fire_Regime_departure/data/thresholds/CBI_classification/Hammer_Creek_Fire_CBI.csv")
df_2 <- read_csv("C:/Users/JChandler/Desktop/Work/Masters/Fire_Regime_departure/data/thresholds/CBI_classification/GILA_Miller_Fire_CBI.csv")
df <- union_all(df_1,df_2)

df_mutate <- df %>%
  mutate(total_cover = pre_fire_cover_big+pre_fire_cover_int,
         pct_mortality = pre_fire_cover_int/total_cover*pct_mortality_int +
           pre_fire_cover_big/total_cover*pct_mortality_big,
         mortality_class = pct_sev_classifier(pct_mortality)
         ) 
# t <- df_mutate %>%
#   select(pct_mortality, CBI_total, mortality_class)
# cbi_mort_plot <- ggplot(t, aes(x = CBI_total, y = pct_mortality, color = as.factor(mortality_class)))+
#   geom_point()
#pct_mortality = (pct_mortality_int+pct_mortality_big)/2 simple average
cbi_mort_plot <- ggplot(df_mutate, aes(x = CBI_total, y = pct_mortality))+
  geom_point() +
  theme_bw()
cbi_mort_plot


df_cut <- df_mutate %>%
  dplyr::select(pct_mortality, CBI_total)

cores <- detectCores()/2
registerDoParallel(cores)
class <- foreach(i = 1:nrow(df_cut),
        .combine = c("c"),
        .inorder = F,
        .export = c("df_cut")) %dopar% {
          class <- create_class(df_cut[i,1])
          return(class)
        }
stopImplicitCluster()
df_class <- cbind(df_cut, class) %>%
  drop_na()
df_class$class <- factor(x = df_class$class,
                         levels = c("low","mixed","high"))


class_plot <- ggplot(data = df_class, aes(x=class, y = CBI_total))+
  geom_boxplot()
class_plot


# lm logit ----
#
#
#
#

class_lm <- foreach(i = 1:nrow(df_class),
                 .combine = c("c"),
                 .export = c("df_class")) %do% {
                   class_lm <- logit_bin_classes(df_class$class[i])
                   return(class_lm)
                 }


df_logit_lm <- cbind(df_class, class_lm)
mu <- mean(df_logit_lm$CBI_total)
sd <- sd(df_logit_lm$CBI_total)
df_logit_lm <- df_logit_lm %>%
  mutate(normalized = (CBI_total-mu)/sd,
         class_lm = ifelse(class_lm == 0,1, 0))


#glm bootstrap threshold calculation
n_length <- dim(df_logit_lm)[1]
rows <- 1:n_length
n.samples <- 10000
models_lm <- list(length = n.samples)
model_accuracy <- list(length = n.samples)
set.seed(939)
boot_threshs <- foreach(i = 1:n.samples,.combine = c,
        .export= c("df_logit_lm", "rows", "n_length"))%do%{
subsample <- sample(rows, floor(1*n_length), replace = T)
          
df_logit_lm_sub <- df_logit_lm[subsample,]
modlm.LR <- glm(as.factor(class_lm) ~ normalized, data = df_logit_lm_sub, family = "binomial")
modlm.fit <- 100*(1-modlm.LR$deviance/modlm.LR$null.deviance)
mod1.lm.pred <- modlm.LR$fitted.values
mod1 <- "modlm.LR"
dat2.lm <- data.frame(mod1 = mod1,observed = as.numeric(df_logit_lm_sub[,4]), prob = as.numeric(mod1.lm.pred))
mod.cut.lm.GLM <- optimal.thresholds(dat2.lm, opt.methods = c("MaxKappa"))
#calculate threshold probability

threshlm <- as.numeric(mod.cut.lm.GLM[2])

#store model

models_lm[[i]] <- modlm.LR

#evaluate model accuracy
classification <- factor(as.numeric(ifelse(dat2.lm$prob >= threshlm,1, 0)))
mod2.cfmat <- table(dat2.lm$observed, classification)
mod2.acc <- presence.absence.accuracy(dat2.lm, threshold = mod.cut.lm.GLM[[2]])
tss <- mod2.acc$sensitivity + mod2.acc$specificity - 1
mod2.acc <- cbind(mod2.acc[2:7], tss)
model_accuracy[[i]] <- mod2.acc
#return threshold
return(as.numeric(threshlm))
        }

threshold_lm_med <- quantile(boot_threshs, 0.5, names = F)
threshold_lm_low <- quantile(boot_threshs, 0.025, names = F)
threshold_lm_high <- quantile(boot_threshs, 0.975, names = F)
median_model_lm <- data.frame(CBI = seq(0, 3, 0.01)) %>%
  mutate(normalized = (CBI-mu)/sd)
median_model_lm$prediction <- predict(models_lm[[which(threshold_lm_med == boot_threshs)[1]]],median_model_lm, type = "response")
median_model_lm <- median_model_lm %>%
  mutate(low_thresh = ifelse(prediction < threshold_lm_low, 1, 0),
         med_thresh = ifelse(prediction < threshold_lm_med, 1, 0),
         high_thresh = ifelse(prediction < threshold_lm_high, 1, 0))

##### These are the values used in the paper. I match the classification to a CBI value
threshold_actual_lm_maxkap <- median_model_lm$CBI[which(median_model_lm$med_thresh == 1)[1]] #1.56
threshold_actual_lm_maxkap_low <- median_model_lm$CBI[which(median_model_lm$low_thresh == 1)[1]] #1.72
threshold_actual_lm_maxkap_high <- median_model_lm$CBI[which(median_model_lm$high_thresh == 1)[1]] #1.33
#####
# model accuracy
model_accuracy_lm <- union_dataframes(model_accuracy)
model_accuracy_lm_means <- colMeans(model_accuracy_lm[,2:7])


#'
#'
#'
#'
#'
class_mh <- foreach(i = 1:nrow(df_class),
                    .combine = c("c"),
                    .export = c("df_class")) %do% {
                      class_mh <- logit_bin_classes(df_class$class[i], low = F)
                      return(class_mh)
                    }
df_logit_mh <-cbind(df_class, class_mh)

mu <- mean(df_logit_mh$CBI_total)
sd <- sd(df_logit_mh$CBI_total)
df_logit_mh <- df_logit_mh %>%
  mutate(normalized = (CBI_total-mu)/sd)

#glm bootstrap for MH, follows same protocal but using mh dataset
n_length <- dim(df_logit_lm)[1]
rows <- 1:n_length
n.samples <- 10000
models_mh <- list(length = n.samples)
model_accuracy_mh <- list(length = n.samples)
boot_threshs <- foreach(i = 1:n.samples,.combine = c,
                        .export= c("df_logit_mh", "rows", "n_length"))%do%{
                          subsample <- sample(rows, floor(1*n_length), replace = T)
                          
                          df_logit_mh_sub <- df_logit_mh[subsample,]
                          modmh.LR <- glm(as.factor(class_mh) ~ normalized, data = df_logit_mh_sub, family = "binomial")
                          modmh.fit <- 100*(1-modmh.LR$deviance/modmh.LR$null.deviance)
                          mod1.mh.pred <- predict(modmh.LR, type = "response")
                          mod1 <- "modmh.LR"
                          dat2.mh <- data.frame(mod1 = mod1,observed = as.numeric(df_logit_mh_sub[,4]), prob = as.numeric(mod1.mh.pred))
                          mod.cut.mh.GLM <- optimal.thresholds(dat2.mh, opt.methods = c("MaxKappa"))
                          
                          
                    
                          #store model
                          
                          models_mh[[i]] <- modmh.LR
                          
                          #evaluate model accuracy
                          threshmh <- as.numeric(mod.cut.mh.GLM[2])
                          classification <- factor(as.numeric(ifelse(dat2.mh$prob >= threshmh,1, 0)))
                          mod2.cfmat <- table(dat2.mh$observed, classification)
                          mod2.acc <- presence.absence.accuracy(dat2.mh, threshold = mod.cut.mh.GLM[[2]])
                          tss <- mod2.acc$sensitivity + mod2.acc$specificity - 1
                          mod2.acc <- cbind(mod2.acc[2:7], tss)
                          model_accuracy_mh[[i]] <- mod2.acc
                          return(as.numeric(threshmh))
                        }
threshold_mh_med <- quantile(boot_threshs, 0.5, names = F)
threshold_mh_low <- quantile(boot_threshs, 0.025, names = F)
threshold_mh_high <- quantile(boot_threshs, 0.975, names = F)

median_model_mh <- data.frame(CBI = seq(0, 3, 0.01)) %>%
  mutate(normalized = (CBI-mu)/sd)

median_model_mh$prediction <- predict(models_mh[[which(threshold_mh_med == boot_threshs)[1]]],median_model_mh, type = "response")
median_model_mh <- median_model_mh %>%
  mutate(low_thresh = ifelse(prediction > threshold_mh_low, 1, 0),
         med_thresh = ifelse(prediction > threshold_mh_med, 1, 0),
         high_thresh = ifelse(prediction > threshold_mh_high, 1, 0))


##### These are the values used in the paper
threshold_actual_mh_maxkap <- median_model_mh$CBI[which(median_model_mh$med_thresh == 1)[1]] #2.01
threshold_actual_mh_maxkap_low <- median_model_mh$CBI[which(median_model_mh$low_thresh == 1)[1]] #1.92
threshold_actual_mh_maxkap_high <- median_model_mh$CBI[which(median_model_mh$high_thresh == 1)[1]] #2.17
##### 
# model accuracy
model_accuracy_mh <- union_dataframes(model_accuracy_mh)
model_accuracy_mh_means <- colMeans(model_accuracy_mh[,2:7])





##graphs



median_model_lm$des <- as.factor("lm")
median_model_mh$des <- as.factor("mh")
curve <- rbind(median_model_lm, median_model_mh) 

x <- c(threshold_actual_lm_maxkap, threshold_actual_mh_maxkap)
#y <- c(mod.cut.lm.GLM[[2]]*100, mod.cut.mh.GLM[[2]]*100)
des <- as.factor(c("lm","mh"))
thresh <- "Threshold"
thresholds <- tibble(x = x, des = des, thresh = thresh)
#df_mutate
category_text <- c("Low", "Mixed", "High")
pos.x = c(.6,1.77,2.6)
#cbPalette <- c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#gradientPalette <- RColorBrewer::brewer.pal(n = 3,  name =  "BrBG")

gradientPalette <- c("#5D9543", "#A1885D", "#557D9F")

save(x, des, thresholds, gradientPalette, curve, pos.x, df_mutate, category_text, file = "Figures/Final_figures/main/thresholds.RData")


threshold_plot <- ggplot()+
  geom_ribbon(aes(x = c(0,x[1]),  ymin = 0, ymax = 100), thresholds, fill = gradientPalette[1], alpha = .4)+
  geom_ribbon(aes(x = c(x[1],x[2]),  ymin = 0, ymax = 100), thresholds, fill = gradientPalette[2], alpha = .4)+
  geom_ribbon(aes(x = c(x[2],3),  ymin = 0, ymax = 100), thresholds, fill = gradientPalette[3], alpha = .4)+
  
  #geom_line(aes(x=x, y =y*100, color = des), curve,size = 1.2)+
  geom_vline(aes(xintercept = x, color = des), thresholds, linewidth =1.2, linetype = "dashed", show.legend = F)+
  #geom_point(aes(x = x, y = y, color = des), thresholds, size = 2.3, show.legend = F) +
  #geom_segment(x = x[1], xend = x[1], y = 0, yend = y[1],  data = thresholds, size = .75, color =cbPalette[1], linetype = "dashed") +
  #geom_segment(x = x[2], xend = x[2], y = 0, yend = y[2], data = thresholds, size = .75, color = cbPalette[2], linetype = "dotdash") +
  #geom_segment(x = 0, xend = x[1], y = y[1], yend = y[1],  data = thresholds, size = .75, color =cbPalette[1], linetype = "dashed") +
  #geom_segment(x = 0, xend = x[2], y = y[2], yend = y[2], data = thresholds, size = .75, color = cbPalette[2], linetype = "dotdash") +
  geom_line(aes(x=CBI, y = prediction*100, color = des), curve, size = 1.2)+
  #geom_line(aes(x=x, y = Mixed*100), multinomial_curves, size = 1.2, color = gradientPalette[2])+
#geom_line(aes(x=x, y = High*100), multinomial_curves, size = 1.2, color = gradientPalette[3])+
  
  geom_point(aes(x=CBI_total, y = pct_mortality), df_mutate, show.legend = F) +
  geom_text_repel(aes(x=c(1.5,2.2),y=c(50, 50), 
                      label = paste0("Threshold: ", round(x,2))), force = 70,
                  color = c("#015C01",  "#1565C0" ),
                  direction = "x", show.legend = F, seed = 1, size = 6)+
  
  annotate("text", x = pos.x[1],  y = 105, label = category_text[1], size = 6)+
  annotate("text", x = pos.x[2],  y = 105, label = category_text[2], size = 6)+
  annotate("text", x = pos.x[3],  y = 105, label = category_text[3], size = 6)+

  
  scale_y_continuous(
    name = "Percent Tree Mortality",
    breaks = c(0,25,50,75,100),
    #sec.axis = sec_axis( trans=~.*1, name="Probability of Class")
  ) +
  labs(x = "CBI") +
  guides(color = guide_legend(
    override.aes=list(shape = 0)))+
  scale_color_manual(values = gradientPalette[c(1,3,3,1)],
                     name = "Fitted Regression",
                     labels = c(expression('T'[lm]),
                                expression('T'[mh]))) +
  
  
  
  theme_classic() +
  theme(legend.position = "topleft",
        legend.title = element_text(size = 30),
        legend.text = element_text(size = 25),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 30))

threshold_plot
#save
ggsave("Figures/Final_figures/main/thresholds.jpg", threshold_plot, width = 12, height = 8, units = "in", dpi = 300)

#'
#'
#'
#'
#'
#' test to saberi et al.
#' 

df_sab <- read_csv("data/test_data/CBI_classification/fieldmeasures_IPNW.csv")

df_cut_sab <- df_sab %>%
  select(prop_killedSTEM, TotalPlotCBI_mean) %>%
  rename("pct_mortality" = "prop_killedSTEM", "CBI_total" = "TotalPlotCBI_mean" ) %>%
  mutate(pct_mortality = pct_mortality*100) %>%
  mutate_at(c('CBI_total'), ~na_if(.,0)) %>%
  drop_na()
df_cut_sab <- rbind(df_cut_sab, df_cut)
cores <- detectCores()/2
registerDoParallel(cores)
class <- foreach(i = 1:nrow(df_cut_sab),
                 .combine = c("c"),
                 .inorder = F,
                 .export = c("df_cut_sab")) %dopar% {
                   class <- create_class(df_cut_sab[i,1])
                   return(class)
                 }
stopImplicitCluster()
df_class <- cbind(df_cut_sab, class) %>%
  drop_na()
df_class$class <- factor(x = df_class$class,
                         levels = c("low","mixed","high"))

cbi_mort_plot <- ggplot(df_class, aes(x = CBI_total, y = pct_mortality))+
  geom_point()
cbi_mort_plot

class_plot <- ggplot(data = df_class, aes(x=class, y = CBI_total))+
  geom_boxplot()
class_plot


class_lm <- foreach(i = 1:nrow(df_class),
                    .combine = c("c"),
                    .export = c("df_class")) %do% {
                      class_lm <- logit_bin_classes(df_class$class[i])
                      return(class_lm)
                    }


df_logit_lm <- cbind(df_class, class_lm)
mu <- mean(df_logit_lm$CBI_total)
sd <- sd(df_logit_lm$CBI_total)
df_logit_lm <- df_logit_lm %>%
  mutate(normalized = (CBI_total-mu)/sd,
         class_lm = ifelse(class_lm == 0,1, 0))


#glm bootstrap threshold calculation
n_length <- dim(df_logit_lm)[1]
rows <- 1:n_length
n.samples <- 10000
models_lm <- list(length = n.samples)
set.seed(939)
boot_threshs <- foreach(i = 1:n.samples,.combine = c,
        .export= c("df_logit_lm", "rows", "n_length"))%do%{
subsample <- sample(rows, floor(1*n_length), replace = T)
          
df_logit_lm_sub <- df_logit_lm[subsample,]
modlm.LR <- glm(as.factor(class_lm) ~ normalized, data = df_logit_lm_sub, family = "binomial")
modlm.fit <- 100*(1-modlm.LR$deviance/modlm.LR$null.deviance)
mod1.lm.pred <- predict(modlm.LR, type = "response")
mod1 <- "modlm.LR"
dat2.lm <- data.frame(mod1 = mod1,observed = as.numeric(df_logit_lm_sub[,4]), prob = as.numeric(mod1.lm.pred))
mod.cut.lm.GLM <- optimal.thresholds(dat2.lm, opt.methods = c("MaxKappa"))

threshlm <- mod.cut.lm.GLM[2]
models_lm[[i]] <- modlm.LR
return(as.numeric(threshlm))
        }


threshold_lm_med <- quantile(boot_threshs, 0.5, names = F)
threshold_lm_low <- quantile(boot_threshs, 0.025, names = F)
threshold_lm_high <- quantile(boot_threshs, 0.975, names = F)
median_model_lm <- data.frame(CBI = seq(0, 3, 0.01)) %>%
  mutate(normalized = (CBI-mu)/sd)
median_model_lm$prediction <- predict(models_lm[[which(threshold_lm_med == boot_threshs)[1]]],median_model_lm, type = "response")
median_model_lm <- median_model_lm %>%
  mutate(low_thresh = ifelse(prediction < threshold_lm_low, 1, 0),
         med_thresh = ifelse(prediction < threshold_lm_med, 1, 0),
         high_thresh = ifelse(prediction < threshold_lm_high, 1, 0))

##### These are the values used in the paper. I match the classification to a CBI value
threshold_actual_lm_maxkap <- median_model_lm$CBI[which(median_model_lm$med_thresh == 1)[1]] #1.32
threshold_actual_lm_maxkap_low <- median_model_lm$CBI[which(median_model_lm$low_thresh == 1)[1]] #1.54
threshold_actual_lm_maxkap_high <- median_model_lm$CBI[which(median_model_lm$high_thresh == 1)[1]] #1.99
#####



#'
#'
#'
#'
#'
class_mh <- foreach(i = 1:nrow(df_class),
                    .combine = c("c"),
                    .export = c("df_class")) %do% {
                      class_mh <- logit_bin_classes(df_class$class[i], low = F)
                      return(class_mh)
                    }
df_logit_mh <-cbind(df_class, class_mh)

mu <- mean(df_logit_mh$CBI_total)
sd <- sd(df_logit_mh$CBI_total)
df_logit_mh <- df_logit_mh %>%
  mutate(normalized = (CBI_total-mu)/sd)

#glm bootstrap for MH, follows same protocal but using mh dataset
n_length <- dim(df_logit_lm)[1]
rows <- 1:n_length
n.samples <- 10000
models_mh <- list(length = n.samples)
boot_threshs <- foreach(i = 1:n.samples,.combine = c,
                        .export= c("df_logit_mh", "rows", "n_length"))%do%{
                          subsample <- sample(rows, floor(1*n_length), replace = T)
                          
                          df_logit_mh_sub <- df_logit_mh[subsample,]
                          modmh.LR <- glm(as.factor(class_mh) ~ normalized, data = df_logit_mh_sub, family = "binomial")
                          modmh.fit <- 100*(1-modmh.LR$deviance/modmh.LR$null.deviance)
                          mod1.mh.pred <- predict(modmh.LR, type = "response")
                          mod1 <- "modmh.LR"
                          dat2.mh <- data.frame(mod1 = mod1,observed = as.numeric(df_logit_mh_sub[,4]), prob = as.numeric(mod1.mh.pred))
                          mod.cut.mh.GLM <- optimal.thresholds(dat2.mh, opt.methods = c("MaxKappa"))
                          
                          threshlm <- mod.cut.mh.GLM[2]
                          models_mh[[i]] <- modmh.LR
                          return(as.numeric(threshlm))
                        }
threshold_mh_med <- quantile(boot_threshs, 0.5, names = F)
threshold_mh_low <- quantile(boot_threshs, 0.025, names = F)
threshold_mh_high <- quantile(boot_threshs, 0.975, names = F)

median_model_mh <- data.frame(CBI = seq(0, 3, 0.01)) %>%
  mutate(normalized = (CBI-mu)/sd)

median_model_mh$prediction <- predict(models_mh[[which(threshold_mh_med == boot_threshs)[1]]],median_model_mh, type = "response")
median_model_mh <- median_model_mh %>%
  mutate(low_thresh = ifelse(prediction > threshold_mh_low, 1, 0),
         med_thresh = ifelse(prediction > threshold_mh_med, 1, 0),
         high_thresh = ifelse(prediction > threshold_mh_high, 1, 0))


##### These are the values used in the paper
threshold_actual_mh_maxkap <- median_model_mh$CBI[which(median_model_mh$med_thresh == 1)[1]] #1.97
threshold_actual_mh_maxkap_low <- median_model_mh$CBI[which(median_model_mh$low_thresh == 1)[1]] #1.86
threshold_actual_mh_maxkap_high <- median_model_mh$CBI[which(median_model_mh$high_thresh == 1)[1]] #2.05
##### 





##graphs



median_model_lm$des <- as.factor("lm")
median_model_mh$des <- as.factor("mh")
curve <- rbind(median_model_lm, median_model_mh) 

x <- c(threshold_actual_lm_maxkap, threshold_actual_mh_maxkap)
#y <- c(mod.cut.lm.GLM[[2]]*100, mod.cut.mh.GLM[[2]]*100)
des <- as.factor(c("lm","mh"))
thresh <- "Threshold"
thresholds <- tibble(x = x, des = des, thresh = thresh)
#df_mutate
category_text <- c("Low", "Mixed", "High")
pos.x = c(.6,1.77,2.6)
#cbPalette <- c( "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#gradientPalette <- RColorBrewer::brewer.pal(n = 3,  name =  "BrBG")

gradientPalette <- c("#5D9543", "#A1885D", "#557D9F")


ggplot()+
  geom_ribbon(aes(x = c(0,x[1]),  ymin = 0, ymax = 100), thresholds, fill = gradientPalette[1], alpha = .4)+
  geom_ribbon(aes(x = c(x[1],x[2]),  ymin = 0, ymax = 100), thresholds, fill = gradientPalette[2], alpha = .4)+
  geom_ribbon(aes(x = c(x[2],3),  ymin = 0, ymax = 100), thresholds, fill = gradientPalette[3], alpha = .4)+
  
  #geom_line(aes(x=x, y =y*100, color = des), curve,size = 1.2)+
  geom_vline(aes(xintercept = x, color = des), thresholds, linewidth =1.2, linetype = "dashed", show.legend = F)+
  #geom_point(aes(x = x, y = y, color = des), thresholds, size = 2.3, show.legend = F) +
  #geom_segment(x = x[1], xend = x[1], y = 0, yend = y[1],  data = thresholds, size = .75, color =cbPalette[1], linetype = "dashed") +
  #geom_segment(x = x[2], xend = x[2], y = 0, yend = y[2], data = thresholds, size = .75, color = cbPalette[2], linetype = "dotdash") +
  #geom_segment(x = 0, xend = x[1], y = y[1], yend = y[1],  data = thresholds, size = .75, color =cbPalette[1], linetype = "dashed") +
  #geom_segment(x = 0, xend = x[2], y = y[2], yend = y[2], data = thresholds, size = .75, color = cbPalette[2], linetype = "dotdash") +
  geom_line(aes(x=CBI, y = prediction*100, color = des), curve, size = 1.2)+
  #geom_line(aes(x=x, y = Mixed*100), multinomial_curves, size = 1.2, color = gradientPalette[2])+
#geom_line(aes(x=x, y = High*100), multinomial_curves, size = 1.2, color = gradientPalette[3])+
  
  geom_point(aes(x=CBI_total, y = pct_mortality), df_mutate, show.legend = F) +
  geom_text_repel(aes(x=c(1.5,2.2),y=c(50, 50), color = des, label = paste0("Threshold: ", round(x,2))), force = 70,direction = "x", show.legend = F, seed = 1, size = 4)+
  
  annotate("text", x = pos.x[1],  y = 105, label = category_text[1])+
  annotate("text", x = pos.x[2],  y = 105, label = category_text[2])+
  annotate("text", x = pos.x[3],  y = 105, label = category_text[3])+

  
  scale_y_continuous(
    name = "Percent Tree Mortality",
    breaks = c(0,25,50,75,100),
    #sec.axis = sec_axis( trans=~.*1, name="Probability of Class")
  ) +
  labs(x = "CBI") +
  guides(color = guide_legend(
    override.aes=list(shape = 0)))+
  scale_color_manual(values = gradientPalette[c(1,3)],
                     name = "Fitted Regression",
                     labels = c(expression('T'[lm]),
                                expression('T'[mh]))) +
  
  
  
  theme(legend.position = "topleft",
        legend.title = element_text(size=30),
        legend.text = element_text(size = 25),
        axis.text = element_text(size = 25),
        axis.title = element_text(size = 30))+
  theme_classic()

##################################################
#################################################
################GARBAGE###########################
##################################################
#################################################
#'
#'
#'# lm <- ggplot(data = df_logit_lm, aes(x = normalized)) +
#   geom_histogram(bins = 7, 
#                  mapping = aes(fill = as.factor(class_lm)), 
#                  alpha = .6, position = "identity")
# lm
# n_sims <- 50
# 
# desc <- descent(df_logit_lm$normalized,c(1,1),df_logit_lm$class_lm, .1, n_sims)
# 
# df_logit_lm$curve <- sigmoid(desc[n_sims,1] + desc[n_sims,2]*df_logit_lm$normalized)
# 
# ggplot(data = df_logit_lm, aes(x = normalized, y = curve))+
#   geom_line()
# 
# x <- seq(-2, 3, by = .01)
# y <- sigmoid(desc[n_sims,1] + desc[n_sims,2]*x)
# x<- x*sd+mu
# lm_curve = tibble(x = x, y = y)
# 
# 
# curve_class <- foreach(i =1:length(y),
#                        .export = c("y"),
#                        .combine = c("c")) %do%{
#                          class <- curve_classifier(y[i],.5 )
#                        }
# #threshold_norm <- x[match( unique(curve_class),curve_class)][2]
# threshold_norm <- -desc[n_sims,1]/desc[n_sims,2]
# threshold_actual_lm <- threshold_norm*sd +mu
# 
# #10 fold cross validation
# folds = 10
# folds_lm <- cut(seq(1,nrow(df_logit_lm)), breaks = folds, labels = FALSE)
# result_lm <- list()
# for(i in 1:10){
#   testindexes <- which(folds_lm ==i, arr.ind = TRUE)
#   testData <- df_logit_lm[testindexes,]
#   trainData <- df_logit_lm[-testindexes,]
#   model <- descent(trainData$normalized,c(1,1),trainData$class_lm, .1, n_sims)[n_sims,]
#   preds <- sigmoid(model[1] + model[2]*testData$normalized)
#   curve_class <- foreach(j =1:length(preds),
#                          .combine = c("c")) %do%{
#                            class <- curve_classifier(preds[j] ,threshlm)
#                            return(class)
#                          }
#   result_lm[i] <- sum(curve_class == testData$class_lm)/length(curve_class)
# }
# mean(unlist(result_lm))
# sd(unlist(result_lm))


# doing it via glm
# modlm.LR <- glm(as.factor(class_lm) ~ normalized, data = df_logit_lm, family = "binomial")
# modlm.fit <- 100*(1-modlm.LR$deviance/modlm.LR$null.deviance)
# mod1.lm.pred <- predict(modlm.LR, type = "response")
# mod1 <- "modlm.LR"
# dat2.lm <- data.frame(mod1 = mod1,observed = as.numeric(df_logit_lm[,4]), prob = as.numeric(mod1.lm.pred))
# mod.cut.lm.GLM <- optimal.thresholds(dat2.lm, opt.methods = c("MaxKappa"))
# modlm.cfmat <- table(dat2.lm[[2]], factor(as.numeric(dat2.lm$prob >= mod.cut.lm.GLM$prob)))
# modlm.acc <- presence.absence.accuracy(dat2.lm, threshold = mod.cut.lm.GLM$prob, st.dev = F)
# tss <- modlm.acc$sensitivity +modlm.acc$specificity -1
# 
# modlm.acc <- cbind(modlm.acc[1:7],tss)
# modlm.acc[c(1,4:5,7:8)]
# auc.roc.plot(dat2.lm, color = T)
# 
# threshlm <- mod.cut.lm.GLM[2]
# curve_class <- foreach(i =1:length(y),
#                        .export = c("y"),
#                        .combine = c("c")) %do%{
#                          class <- curve_classifier(y[i],threshlm )
#                        }
#'
#'
#'# mh <- ggplot(data = df_logit_mh, aes(x = normalized)) +
#   geom_histogram(bins = 7, 
#                  mapping = aes(fill = as.factor(class_mh)), 
#                  alpha = .6, position = "identity")
# mh
# n_sims <- 50
# desc <- descent(df_logit_mh$normalized,c(1,1),df_logit_mh$class_mh, .1, n_sims)
# 
# df_logit_mh$curve <- sigmoid(desc[n_sims,1] + desc[n_sims,2]*df_logit_mh$normalized)
# 
# ggplot(data = df_logit_mh, aes(x = normalized, y = curve))+
#   geom_line()
# 
# x <- seq(-2, 3, by = .01)
# y <- sigmoid(desc[n_sims,1] + desc[n_sims,2]*x)
# x<- x*sd+mu
# mh_curve = tibble(x = x, y = y)
# 
# curve_class <- foreach(i =1:length(y),
#                        .export = c("y"),
#                        .combine = c("c")) %do%{
#                          class <- curve_classifier(y[i], .5)
#                        }
# #threshold_norm <- x[match( unique(curve_class),curve_class)][2]
# threshold_norm <- -desc[n_sims,1]/desc[n_sims,2]
# threshold_actual_mh <- threshold_norm*sd +mu
# 
#Cross validation

# folds_mh <- cut(seq(1,nrow(df_logit_mh)), breaks = 10, labels = FALSE)
# result_mh <- list()
# for(i in 1:10){
#   testindexes <- which(folds_mh ==i, arr.ind = TRUE)
#   testData <- df_logit_mh[testindexes,]
#   trainData <- df_logit_mh[-testindexes,]
#   model <- descent(trainData$normalized,c(1,1),trainData$class_mh, .1, n_sims)[n_sims,]
#   preds <- sigmoid(model[1] + model[2]*testData$normalized)
#   curve_class <- foreach(j =1:length(preds),
#                          .combine = c("c")) %do%{
#                            class <- curve_classifier(preds[j], threshmh)
#                            return(class)
#                          }
#   result_mh[i] <- sum(curve_class == testData$class_mh)/length(curve_class)
# }
# mean(unlist(result_mh))
# sd(unlist(result_mh))
# 

# 
# 
# 
# # doing it via glm
# modmh.LR <- glm(as.factor(class_mh) ~ normalized, data = df_logit_mh, family = "binomial")
# modmh.fit <- 100*(1-modmh.LR$deviance/modmh.LR$null.deviance)
# mod1.mh.pred <- predict(modmh.LR, type = "response")
# mod1 <- "modmh.LR"
# dat2.mh <- data.frame(mod1 = mod1,observed = as.numeric(df_logit_mh[,4]), prob = as.numeric(mod1.mh.pred))
# mod.cut.mh.GLM <- optimal.thresholds(dat2.mh, opt.methods = c("MaxKappa"))
# modmh.cfmat <- table(dat2.mh[[2]], factor(as.numeric(dat2.mh$prob >= mod.cut.mh.GLM$prob)))
# modmh.acc <- presence.absence.accuracy(dat2.mh, threshold = mod.cut.mh.GLM$prob, st.dev = F)
# tss <- modmh.acc$sensitivity +modmh.acc$specificity -1
# 
# modmh.acc <- cbind(modmh.acc[1:7],tss)
# modmh.acc[c(1,4:5,7:8)]
# auc.roc.plot(dat2.mh, color = T)
# 
# threshmh <- mod.cut.mh.GLM[2]
# curve_class <- foreach(i =1:length(y),
#                        .export = c("y"),
#                        .combine = c("c")) %do%{
#                          class <- curve_classifier(y[i],threshmh )
#                        }
# 
# threshold_actual_mh_maxkap <- x[match( unique(curve_class),curve_class)][2]

#'
#'
#'
#'
#'
softmax <- function(a){
numerator = exp(a)
denominator = sum(numerator)
return(numerator/denominator)
}
one_hot <- function(y){
  N = length(unique(y))
  m = length(y)
  z = matrix(data = 0, nrow = m, ncol = N)
  for (i in 1:m){
    z[i,y[i]] = 1
  }
  return(z)
}

class_oh <- one_hot(df_class$class)

dt <- sort(sample(nrow(df_class), nrow(df_class)*.7))
train_x <- df_class[dt,2]
train_z <- class_oh[dt,]
test_x <- df_class[-dt,2]
test_z <- class_oh[-dt,]

phi_train = cbind(base::rep(1,length(train_x)), train_x)

N = length(unique(df_class))
n = dim(phi_train)[2]
w = array(runif(n*N), c(n,N))/1000

a = phi_train %*% w

Loss <- function(phi, w, label){
  a = phi %*% w
  train_sum = sum(label *log(softmax(a)))
  return(-1/(dim(label)[2]*length(phi))*train_sum)
}
Loss(phi_train,w, train_z)

L_grad <- function(phi, w, label){
  N = dim(label)[2]
  n = dim(phi)[2]
  m = dim(phi)[1]
  a = phi %*% w
  max = softmax(a)
  
  train_obs = matrix(data = 0, nrow = n, ncol = N)
  train_obs <- t(label-max)
  train_loop <- train_obs %*% phi
  
  train_loop <- t(train_loop)
  return ((-1/(dim(label)[2]*dim(phi)[1])) * train_loop)
}
L_grad(phi_train, w, train_z)

descent <- function(phi, w, label, eta, n_sims){
  N = dim(label)[2]
  n = dim(phi)[2]
  params = array(data = 0, dim = c(n_sims, n, N))
  params[1,,] <- w
  for (i in 1:(n_sims-1)){
    gradient = L_grad(phi, params[i,,], label)
    next_params = params[i,,] - (eta*gradient)
    params[i+1,,] = next_params
  }
  return(params)
}
n_sims = 100
eta = .1
desc <- descent(phi_train, w, train_z, eta, n_sims)

phi_test <- cbind(base::rep(1,length(test_x)), test_x)
loss_data = array(data = 0, dim = c(n_sims))
for (i in 1:n_sims){
  loss_data[i] = Loss(phi_test, desc[i,,], test_z)
}
x <- seq(1,n_sims)
plot(x, loss_data)

#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'library(tidyverse)
library(terra)
library(randomForest)
library(ROCR)


sev <- read_csv("C:\\Users\\JChandler\\Desktop\\Work\\Masters\\data\\test_data\\SelwayBitteroot\\outputs\\contemporary_severity.csv") %>%
  select(sev, x, y)
clim <- terra::rast('E:\\dwr_distribmodels-2020\\data\\preds\\terrestrial\\usfws\\km_1\\usfws_allvars_clim_wna.tif')

df <- as.data.frame(extract(clim,vect(sev, geom = c("x", "y")), bind = T))
df$sev <- as.factor(df$sev)

mod.form <- function(dat, r.col, p.col) {
  n.col <- ncol(dat)
  resp <- colnames(dat[r.col])
  resp <- paste("as.factor(", colnames(dat[r.col]), ")", sep = "")
  pred <- colnames(dat[c(p.col:n.col)])
  mod.formula <- as.formula(paste(resp, "~", paste(pred, collapse = "+")))
}

mtry <- tuneRF(df[-1], df$sev, ntreeTry = 500,
               stepFactor = 1.5,
               improve = 0.01,
               trace = TRUE,
               plot = TRUE)
best.m <-mtry[mtry[,2] == min(mtry[,2]), 1]
print(mtry)
print(best.m)
mod1.RF <- randomForest(mod.form(df, 1, 2), 
                        importance = T,
                        keep.forest = T, 
                        data = df,
                        ntrees = 10000)
print(mod1.RF)
importance(mod1.RF)
varImpPlot(mod1.RF)

mod1.pred <- predict(mod1.RF, type = "prob")
head(mod1.pred)

perf = prediction(mod1.pred[,1], df$sev)

auc = performance(perf, "auc")

auc

pred3 <- performance(perf, "tpr","fpr")
plot(pred3, main = "ROC Curve for Random Forest", col = 2, lwd = 2)
abline(a = 0, b= 1, lwd = 2, lty = 2, col = "gray")
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
#'
library(tidyverse)
library(terra)
library(randomForest)
library(ROCR)


sev <- read_csv("C:\\Users\\JChandler\\Desktop\\Work\\Masters\\data\\test_data\\SelwayBitteroot\\outputs\\contemporary_severity.csv") %>%
  select(sev, x, y)
clim <- terra::rast('E:\\dwr_distribmodels-2020\\data\\preds\\terrestrial\\usfws\\km_1\\usfws_allvars_clim_wna.tif')

df <- as.data.frame(extract(clim,vect(sev, geom = c("x", "y")), bind = T))
df$sev <- as.factor(df$sev)

mod.form <- function(dat, r.col, p.col) {
  n.col <- ncol(dat)
  resp <- colnames(dat[r.col])
  resp <- paste("as.factor(", colnames(dat[r.col]), ")", sep = "")
  pred <- colnames(dat[c(p.col:n.col)])
  mod.formula <- as.formula(paste(resp, "~", paste(pred, collapse = "+")))
}

mtry <- tuneRF(df[-1], df$sev, ntreeTry = 500,
               stepFactor = 1.5,
               improve = 0.01,
               trace = TRUE,
               plot = TRUE)
best.m <-mtry[mtry[,2] == min(mtry[,2]), 1]
print(mtry)
print(best.m)
mod1.RF <- randomForest(mod.form(df, 1, 2), 
                        importance = T,
                        keep.forest = T, 
                        data = df,
                        ntrees = 10000)
print(mod1.RF)
importance(mod1.RF)
varImpPlot(mod1.RF)

mod1.pred <- predict(mod1.RF, type = "prob")
head(mod1.pred)

perf = prediction(mod1.pred[,1], df$sev)

auc = performance(perf, "auc")

auc

pred3 <- performance(perf, "tpr","fpr")
plot(pred3, main = "ROC Curve for Random Forest", col = 2, lwd = 2)
abline(a 0, b= 1, lwd = 2, lty = 2, col = "gray")

#Ordinal regression?
library(ordinal)
od_log <- clm(class ~ CBI_total, data = df_class)
od_loggam <- clm(class ~CBI_total, data = df_class, link = "log-gamma")
t <- seq(0,3,.01)
t <- data.frame(CBI_total = t)
log_pred <- predict(od_log, t)
log_pred <- cbind(log_pred, t)
loggam_pred <- predict(od_loggam, t)
loggam_pred <- cbind(loggam_pred,t)
log_pred %>%
  pivot_longer(c(fit.low,fit.mixed,fit.high),names_to = "cat", values_to = "y") %>%
  ggplot()+
  geom_line(aes(x=CBI_total, y = y, color = cat))+
  labs(y = "Probability")+
  scale_color_discrete(name = "Fitted Regression",
                       labels = c("P(low)",
                                  "P(mixed)",
                                  "P(high)"))+
  theme_bw()

loggam_pred %>%
  pivot_longer(c(fit.low,fit.mixed,fit.high),names_to = "cat", values_to = "y") %>%
  ggplot()+
  geom_line(aes(x=CBI_total, y = y, color = cat))+
  labs(y = "Probability")+
  scale_color_discrete(name = "Fitted Regression",
                       labels = c("P(low)",
                                  "P(mixed)",
                                  "P(high)"))+
  theme_bw()

