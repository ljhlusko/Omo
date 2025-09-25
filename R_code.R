# Required libraries
library(MASS)
library(tidyverse)
library(knitr)
library(ggplot2)
library(ggrepel)
library(tibble)
library(dplyr)
library(readxl)
library(tidyr)
library(scales)
library(ggtext)
library(kableExtra)
library(purrr)
library(gmodels)
library(cvms)
library(caret)
library(ggpubr)
library(stringr)
library(factoextra)
library(nomclust)

# LDA ANALYSIS ----

## Loading Omo specimens data ----
omo_numeric_mdbl2 <- read_excel("omo_numeric_mdbl2.xlsx")

## Loading comparative data ----
diameter_comparative <- read_excel("lda_comparative_dataset.xlsx")

### Sample size of the comparative dataset ----
sample_size_sp <- as.data.frame(table(diameter_comparative$Species, 
                                      diameter_comparative$Tooth,
                                      diameter_comparative$Position)) %>%
  setNames(c("Species", "Tooth", "Position", "Frequency")) %>%
  pivot_wider(names_from = Tooth, values_from = Frequency)

sample_size_ge <- as.data.frame(table(diameter_comparative$Genera, 
                                      diameter_comparative$Tooth,
                                      diameter_comparative$Position)) %>%
  setNames(c("Genus", "Tooth", "Position", "Frequency")) %>%
  pivot_wider(names_from = Tooth, values_from = Frequency)


print(sample_size_sp)        # printing the table by Species, Position and tooth type
print(sample_size_ge)        # printing the table by Species, Position and tooth type
sum(sample_size_sp[,c(3:7)]) # total number of teeth in the dataset (Species)
sum(sample_size_ge[,c(3:7)]) # total number of teeth in the dataset (Genera)

### Splitting the comparative dataset per tooth type ----
M1lower <- diameter_comparative[diameter_comparative$Tooth == "M1" & diameter_comparative$Position == "L",]
M2lower <- diameter_comparative[diameter_comparative$Tooth == "M2" & diameter_comparative$Position == "L",]
M3lower <- diameter_comparative[diameter_comparative$Tooth == "M3" & diameter_comparative$Position == "L",] 
M1upper <- diameter_comparative[diameter_comparative$Tooth == "M1" & diameter_comparative$Position == "U",]
M2upper <- diameter_comparative[diameter_comparative$Tooth == "M2" & diameter_comparative$Position == "U",]
M3upper <- diameter_comparative[diameter_comparative$Tooth == "M3" & diameter_comparative$Position == "U",]
P3lower <- diameter_comparative[diameter_comparative$Tooth == "P3" & diameter_comparative$Position == "L",]
P4lower <- diameter_comparative[diameter_comparative$Tooth == "P4" & diameter_comparative$Position == "L",]
P3upper <- diameter_comparative[diameter_comparative$Tooth == "P3" & diameter_comparative$Position == "U",]
P4upper <- diameter_comparative[diameter_comparative$Tooth == "P4" & diameter_comparative$Position == "U",]

## LDA (Species) ----
lda_omo_sp <- function(data,
                    tooth_type = c("m1", "m2", "m3", "M1", "M2",
                                   "M3", "P3", "P4", "p3", "p4"), 
                    position = c("U", "L")) {
  
  # LDA
  model <- lda(Species ~ scale(MD) + scale(BL) + scale(Size) + scale(Shape), data = data) 
  tooth <- omo_numeric_mdbl2
  tooth <- tooth[tooth$Tooth == tooth_type & tooth$Position == position, ]
  tooth <- tooth[complete.cases(tooth[, c("MD", "BL")]), ]
  
  # PREDICTION
  predictions <- model %>%
    predict(tooth)
  pred <- as.data.frame(predictions$posterior)*100
  pred$Tooth_ID <- tooth$Inventaire
  pred$MD <- tooth$MD
  pred$BL <- tooth$BL
  pred$Prediction <- as.character(predictions$class)
  xpred <- as.data.frame(predictions$x) # table with LD1 and LD2 values of Omo
  pred$LD1 <- xpred$LD1
  pred$LD2 <- xpred$LD2

  # COLOR CODE SPECIES  
  group.colors <- c(`Australopithecus LP` = "#08306B", 
                    `Au. anamensis`       = "#08306B", 
                    `Au. deyiremeda`      = "#08306B", 
                    `Au. garhi`           = "#08306B", 
                    `H. erectus`          = "#006D2C",
                    `Homo EEA`            = "#006D2C",
                    `P. boisei`           = "#A63603",
                    `P. aethiopicus`      = "#A63603",
                    `Ar. ramidus`         = "red",
                    `Ar. kadabba`         = "red")
  
  group.shapes <- c(`Australopithecus LP` = 0, 
                    `Au. anamensis`       = 7, 
                    `Au. deyiremeda`      = 12, 
                    `Au. garhi`           = 14, 
                    `H. erectus`          = 2,
                    `Homo EEA`            = 6,
                    `P. boisei`           = 1,
                    `P. aethiopicus`      = 10,
                    `Ar. ramidus`         = 3,
                    `Ar. kadabba`         = 4)
  # PLOT
  lda.data <- cbind(data, predict(model)$x)
  hull_data <-
    lda.data %>%
    group_by(Species) %>%
    slice(chull(LD1, LD2))
  ggplot_omo <- ggplot(lda.data, aes(LD1, LD2)) +
    geom_hline(yintercept = 0, lty = 2, lwd = 0.5, alpha = 0.15) + 
    geom_vline(xintercept = 0, lty = 2, lwd = 0.5, alpha = 0.15) +
    geom_polygon(data = hull_data, aes(fill = Species, colour = Species), 
                 alpha = 0.3, show.legend = FALSE, linetype = "blank") +
    geom_point(aes(fill = Species, colour = Species, shape = Species), size = 2, alpha = 0.4) +
    scale_fill_manual (values = group.colors) +
    scale_shape_manual(values = group.shapes) +
    scale_color_manual(values = group.colors) +
    scale_y_continuous(breaks=seq(-40,40,1)) +
    scale_x_continuous(breaks=seq(-40,40,1)) +
    geom_point(data=pred, aes(x=LD1, y=LD2), 
               fill = "black", colour = "black", shape = 20, size = 2) +
    geom_text_repel(data=pred,
                    aes(x=LD1, y=LD2, label = Tooth_ID), colour = "black",
                    size = 2.5) +
    theme_minimal() +
    theme(legend.text = element_text(face = "italic", size = 8),
          legend.position="bottom",
          legend.key.size = unit(0.4, "cm"),
          legend.title=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "gray90")) +
    guides(fill=guide_legend(nrow=3, byrow=TRUE))
  
  
  # OUTPUT LIST
  list(Tooth_DB = tooth,
       LDA = model, 
       Prediction = pred, 
       GGPLOT = ggplot_omo)
}

lda_omo_M1lower_sp <- lda_omo_sp(M1lower, "m1", "L")
lda_omo_M2lower_sp <- lda_omo_sp(M2lower, "m2", "L")
lda_omo_M3lower_sp <- lda_omo_sp(M3lower, "m3", "L")
lda_omo_M1upper_sp <- lda_omo_sp(M1upper, "M1", "U")
lda_omo_M2upper_sp <- lda_omo_sp(M2upper, "M2", "U")
lda_omo_M3upper_sp <- lda_omo_sp(M3upper, "M3", "U")
lda_omo_P3lower_sp <- lda_omo_sp(P3lower, "p3", "L")
lda_omo_P4lower_sp <- lda_omo_sp(P4lower, "p4", "L")
lda_omo_P3upper_sp <- lda_omo_sp(P3upper, "P3", "U")
lda_omo_P4upper_sp <- lda_omo_sp(P4upper, "P4", "U")

lda_omo_teeth <- list(P3_lower = lda_omo_P3lower_sp, P3_upper = lda_omo_P3upper_sp,
                      P4_lower = lda_omo_P4lower_sp, P4_upper = lda_omo_P4upper_sp,
                      M1_lower = lda_omo_M1lower_sp, M1_upper = lda_omo_M1upper_sp,
                      M2_lower = lda_omo_M2lower_sp, M2_upper = lda_omo_M2upper_sp,
                      M3_lower = lda_omo_M3lower_sp, M3_upper = lda_omo_M3upper_sp)
print(lda_omo_teeth)

### LD functions ----
lda_omo_coef <- function(data) {
  data$LDA$scaling
}

ldfunctions_sp <- lapply(lda_omo_teeth, lda_omo_coef)
print(ldfunctions_sp)

### LD proportion of variance ----
lda_omo_summary <- function(data) {
  Prop_Trace <- prop.table(data$LDA$svd^2)
  Prop_Trace <- Prop_Trace[1:2]
  list(`Proportion of Trace` = Prop_Trace)
}

la <- lapply(lda_omo_teeth, lda_omo_summary)
pr_tr <- data.frame(matrix(unlist(la), nrow=length(la), byrow=TRUE), stringsAsFactors=FALSE)*100
pr_tr$Accum. <- pr_tr$X1 + pr_tr$X2
pr_tr$Tooth_type <- names(la)
colnames(pr_tr) <- c("LD1", "LD2", "LD1+LD2", "Tooth type")
print(pr_tr)

### LD accuracy predictions ----
confusion_matrix <- function(data){
  lda_model <- lda(Species ~ scale(MD) + scale(BL) + scale(Size) + scale(Shape), data = data) 
  lda_prediction <- predict(lda_model)
  conf <- table(predicted=lda_prediction$class, observed=data$Species)
  cfm <- as_tibble(conf)
  conf2 <- CrossTable(lda_prediction$class, data$Species)
  plot_cm <- plot_confusion_matrix(cfm, 
                                   target_col = "observed", 
                                   prediction_col = "predicted",
                                   counts_col = "n",
                                   add_normalized = F) +
    ggtitle(deparse(substitute(data))) +
    theme(axis.text.x = element_text(angle = 15),
          axis.text.y = element_text(angle = 15))
  
  # how to get Accuracy of the predictions
  # https://www.digitalocean.com/community/tutorials/confusion-matrix-in-r
  acc <- confusionMatrix(data=factor(lda_prediction$class), reference = factor(data$Species))
  
  # OUTPUT LIST
  list(Conf_Accuracy = acc,
       Confusion_Matrix_Simple = conf, 
       Confusion_Matrix_Complete = conf2, 
       Plot_Conf_Matrix = plot_cm)
}

lda_comparative_teeth <- list(M1_lower = M1lower, M2_lower = M2lower, M3_lower = M3lower,
                      M1_upper = M1upper, M2_upper = M2upper, M3_upper = M3upper,
                      P3_lower = P3lower, P4_lower = P4lower,
                      P3_upper = P3upper, P4_upper = P4upper)

lapply(lda_comparative_teeth, confusion_matrix)

### Omo prediction ----
pred_omo <- function(data){
  data$Prediction
}

lapply(lda_omo_teeth, pred_omo)

## LDA (Genera) ----
lda_omo_ge <- function(data,
                       tooth_type = c("m1", "m2", "m3", "M1", "M2",
                                      "M3", "P3", "P4", "p3", "p4"), 
                       position = c("U", "L")) {
  
  # LDA 
  model <- lda(Genera ~ scale(MD) + scale(BL) + scale(Size) + scale(Shape), data = data) 
  tooth <- omo_numeric_mdbl2
  tooth <- tooth[tooth$Tooth == tooth_type & tooth$Position == position, ]
  tooth <- tooth[complete.cases(tooth[, c("MD", "BL")]), ]
  
  # PREDICTION
  predictions <- model %>%
    predict(tooth)
  pred <- as.data.frame(predictions$posterior)*100
  pred$Tooth_ID <- tooth$Inventaire
  pred$MD <- tooth$MD
  pred$BL <- tooth$BL
  pred$Prediction <- as.character(predictions$class)
  xpred <- as.data.frame(predictions$x) # table with LD1 and LD2 values of Omo
  pred$LD1 <- xpred$LD1
  pred$LD2 <- xpred$LD2
  
  # COLOR CODE GENERA
  group.colors <- c(`Australopithecus` = "#08306B", 
                    `Homo`             = "#006D2C",
                    `Paranthropus`     = "#A63603",
                    `Ardipithecus`     = "red")
  
  group.shapes <- c(`Australopithecus` = 15, 
                    `Homo`             = 17,
                    `Paranthropus`     = 16,
                    `Ardipithecus`     = 2)
  
  # PLOT
  lda.data <- cbind(data, predict(model)$x)
  hull_data <-
    lda.data %>%
    group_by(Genera) %>%
    slice(chull(LD1, LD2))
  ggplot_omo <- ggplot(lda.data, aes(LD1, LD2)) +
    geom_hline(yintercept = 0, lty = 2, lwd = 0.5, alpha = 0.15) + 
    geom_vline(xintercept = 0, lty = 2, lwd = 0.5, alpha = 0.15) +
    geom_polygon(data = hull_data, aes(fill = Genera, colour = Genera), 
                 alpha = 0.3, show.legend = FALSE, linetype = "blank") +
    geom_point(aes(fill = Genera, colour = Genera, shape = Genera), size = 2, alpha = 0.4) +
    scale_y_continuous(breaks=seq(-40,40,1)) +
    scale_x_continuous(breaks=seq(-40,40,1)) +
    geom_point(data=pred, aes(x=LD1, y=LD2), 
               fill = "black", colour = "black", shape = 20, size = 2) +
    geom_text_repel(data=pred,
                    aes(x=LD1, y=LD2, label = Tooth_ID), colour = "black",
                    size = 2.5) +
    scale_fill_manual(values=group.colors) + 
    scale_color_manual(values=group.colors) +
    scale_shape_manual(values = group.shapes) +
    theme_minimal() +
    theme(legend.text = element_text(face = "italic", size = 8),
          legend.position="bottom",
          legend.key.size = unit(0.4, "cm"),
          legend.title=element_blank(),
          panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          axis.line = element_line(colour = "gray90")) +
    guides(fill=guide_legend(nrow=3, byrow=TRUE))
  
  # OUTPUT LIST
  list(LDA = model, 
       Prediction = pred, 
       GGPLOT = ggplot_omo)
}

lda_omo_M1lower_ge <- lda_omo_ge(M1lower, "m1", "L")
lda_omo_M2lower_ge <- lda_omo_ge(M2lower, "m2", "L")
lda_omo_M3lower_ge <- lda_omo_ge(M3lower, "m3", "L")
lda_omo_M1upper_ge <- lda_omo_ge(M1upper, "M1", "U")
lda_omo_M2upper_ge <- lda_omo_ge(M2upper, "M2", "U")
lda_omo_M3upper_ge <- lda_omo_ge(M3upper, "M3", "U")
lda_omo_P3lower_ge <- lda_omo_ge(P3lower, "p3", "L")
lda_omo_P4lower_ge <- lda_omo_ge(P4lower, "p4", "L")
lda_omo_P3upper_ge <- lda_omo_ge(P3upper, "P3", "U")
lda_omo_P4upper_ge <- lda_omo_ge(P4upper, "P4", "U")

lda_omo_teeth_ge <- list(P3_lower = lda_omo_P3lower_ge, P3_upper = lda_omo_P3upper_ge,
                      P4_lower = lda_omo_P4lower_ge, P4_upper = lda_omo_P4upper_ge,
                      M1_lower = lda_omo_M1lower_ge, M1_upper = lda_omo_M1upper_ge,
                      M2_lower = lda_omo_M2lower_ge, M2_upper = lda_omo_M2upper_ge,
                      M3_lower = lda_omo_M3lower_ge, M3_upper = lda_omo_M3upper_ge)

### LD functions ----
ldfunctions_ge <- lapply(lda_omo_teeth_ge, lda_omo_coef)
print(ldfunctions_ge)

### LD proportion of variance ----
la_ge <- lapply(lda_omo_teeth_ge, lda_omo_summary)
pr_tr <- data.frame(matrix(unlist(la_ge), nrow=length(la_ge), byrow=TRUE), stringsAsFactors=FALSE)*100
pr_tr$Accum. <- pr_tr$X1 + pr_tr$X2
pr_tr$Tooth_type <- names(la_ge)
colnames(pr_tr) <- c("LD1", "LD2", "LD1+LD2", "Tooth type")
print(pr_tr)

### LD accuracy predictions ----
confusion_matrix_ge <- function(data){
  lda_model <- lda(Genera ~ scale(MD) + scale(BL) + scale(Size) + scale(Shape), data = data) 
  lda_prediction <- predict(lda_model)
  conf <- table(predicted=lda_prediction$class, observed=data$Genera)
  cfm <- as_tibble(conf)
  conf2 <- CrossTable(lda_prediction$class, data$Genera)
  plot_cm <- plot_confusion_matrix(cfm, 
                                   target_col = "observed", 
                                   prediction_col = "predicted",
                                   counts_col = "n",
                                   add_normalized = F) +
    ggtitle(deparse(substitute(data))) +
    theme(axis.text.x = element_text(angle = 15),
          axis.text.y = element_text(angle = 15))
  
  # Accuracy of the predictions
  acc <- confusionMatrix(data=factor(lda_prediction$class), reference = factor(data$Genera))
  
  # OUTPUT LIST
  list(Conf_Accuracy = acc,
       Confusion_Matrix_Simple = conf, 
       Confusion_Matrix_Complete = conf2, 
       Plot_Conf_Matrix = plot_cm)
}

lda_comparative_teeth_ge <- list(M1_lower = M1lower, M2_lower = M2lower, M3_lower = M3lower,
                              M1_upper = M1upper, M2_upper = M2upper, M3_upper = M3upper,
                              P3_lower = P3lower, P4_lower = P4lower,
                              P3_upper = P3upper, P4_upper = P4upper)

lapply(lda_comparative_teeth_ge, confusion_matrix_ge)

### Omo prediction ----
lapply(lda_omo_teeth_ge, pred_omo)

# BOXPLOTS COMPARATIVE DATA ----

group.colors <- c(`Australopithecus LP` = "#08306B", 
                  `Au. anamensis`       = "#08306B", 
                  `Au. deyiremeda`      = "#08306B", 
                  `Au. garhi`           = "#08306B", 
                  `H. erectus`          = "#006D2C",
                  `Homo EEA`            = "#006D2C",
                  `P. boisei`           = "#A63603",
                  `P. aethiopicus`      = "#A63603",
                  `Ar. ramidus`         = "red",
                  `Ar. kadabba`         = "red")

group.shapes <- c(`Australopithecus LP` = 0, 
                  `Au. anamensis`       = 7, 
                  `Au. deyiremeda`      = 12, 
                  `Au. garhi`           = 14, 
                  `H. erectus`          = 2,
                  `Homo EEA`            = 6,
                  `P. boisei`           = 1,
                  `P. aethiopicus`      = 10,
                  `Ar. ramidus`         = 3,
                  `Ar. kadabba`         = 4)

group.colors.gen <- c(`Australopithecus` = "#08306B", 
                      `Homo`             = "#006D2C",
                      `Paranthropus`     = "#A63603",
                      `Ardipithecus`     = "red")

group.shapes.gen <- c(`Australopithecus` = 15, 
                      `Homo`             = 17,
                      `Paranthropus`     = 16,
                      `Ardipithecus`     = 2)

### Genera ----
ggplot(diameter_comparative, aes(x = Genera, y = MD, fill =  Genera)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual (values = group.colors.gen) +
  scale_shape_manual(values = group.shapes.gen) +
  scale_color_manual(values = group.colors.gen) +
  geom_jitter(aes(colour =  Genera), alpha = 0.5) +
  facet_wrap(~Tooth + Position, ncol = 2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face = "italic"),
        legend.position = "none") +
  ylab("Mesiodistal diameter (mm.)")

ggplot(diameter_comparative, aes(x = Genera, y = BL, fill =  Genera)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual (values = group.colors.gen) +
  scale_shape_manual(values = group.shapes.gen) +
  scale_color_manual(values = group.colors.gen) +
  geom_jitter(aes(colour =  Genera), alpha = 0.5) +
  facet_wrap(~Tooth + Position, ncol = 2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face = "italic"),
        legend.position = "none") +
  ylab("Buccolingual diameter (mm.)")

ggplot(diameter_comparative, aes(x = Genera, y = MD*BL, fill =  Genera)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual (values = group.colors.gen) +
  scale_shape_manual(values = group.shapes.gen) +
  scale_color_manual(values = group.colors.gen) +
  geom_jitter(aes(colour =  Genera), alpha = 0.5) +
  facet_wrap(~Tooth + Position, ncol = 2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face = "italic"),
        legend.position = "none") +
  ylab("Crown surface - MDxBL (mm2)")

ggplot(diameter_comparative, aes(x = Genera, y = MD/BL, fill =  Genera)) +
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual (values = group.colors.gen) +
  scale_shape_manual(values = group.shapes.gen) +
  scale_color_manual(values = group.colors.gen) +
  geom_jitter(aes(colour =  Genera), alpha = 0.5) +
  facet_wrap(~Tooth + Position, ncol = 2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face = "italic"),
        legend.position = "none") +
  ylab("Crown index - MD/BL")

## Species ----
ggplot(diameter_comparative, aes(x = Species, y = MD, fill =  Species)) + 
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual (values = group.colors) +
  scale_shape_manual(values = group.shapes) +
  scale_color_manual(values = group.colors) +
  geom_jitter(aes(colour =  Species), alpha = 0.5) +
  facet_wrap(~Tooth + Position, ncol = 2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face = "italic"),
        legend.position = "none") +
  ylab("Mesiodistal diameter (mm.)")

ggplot(diameter_comparative, aes(x = Species, y = BL, fill =  Species)) + 
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual (values = group.colors) +
  scale_shape_manual(values = group.shapes) +
  scale_color_manual(values = group.colors) +
  geom_jitter(aes(colour =  Species), alpha = 0.5) +
  facet_wrap(~Tooth + Position, ncol = 2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face = "italic"),
        legend.position = "none") +
  ylab("Buccolingual diameter (mm.)")

ggplot(diameter_comparative, aes(x = Species, y = MD*BL, fill =  Species)) + 
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual (values = group.colors) +
  scale_shape_manual(values = group.shapes) +
  scale_color_manual(values = group.colors) +
  geom_jitter(aes(colour =  Species), alpha = 0.5) +
  facet_wrap(~Tooth + Position, ncol = 2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face = "italic"),
        legend.position = "none") +
  ylab("Crown surface - MDxBL (mm2)")

ggplot(diameter_comparative, aes(x = Species, y = MD/BL, fill =  Species)) + 
  geom_boxplot(alpha = 0.5) +
  scale_fill_manual (values = group.colors) +
  scale_shape_manual(values = group.shapes) +
  scale_color_manual(values = group.colors) +
  geom_jitter(aes(colour =  Species), alpha = 0.5) +
  facet_wrap(~Tooth + Position, ncol = 2) +
  theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1, face = "italic"),
        legend.position = "none") +
  ylab("Crown index - MD/BL")

# CUSP AREA (MOLARS) ----

areas_allcusps_lwr <- read_excel("cusp areas.xlsx", sheet = "All cusp data LOWER")
areas_allcusps_upr <- read_excel("cusp areas.xlsx", sheet = "All cusp data UPPER")
areas_4cusps_lwr   <- read_excel("cusp areas.xlsx", sheet = "4 cusp data LOWER")
areas_4cusps_upr   <- read_excel("cusp areas.xlsx", sheet = "4 cusp data UPPER")

# Total area
areas_allcusps_lwr$Total_area <- areas_allcusps_lwr$Protoconid + areas_allcusps_lwr$Metaconid + 
  areas_allcusps_lwr$Hypoconid + areas_allcusps_lwr$Entoconid + areas_allcusps_lwr$Hypoconulid + 
  areas_allcusps_lwr$`Cusp 6 (Entoconulid)` + areas_allcusps_lwr$`Cusp 7 (Metaconulid)`

areas_allcusps_upr$Total_area <- areas_allcusps_upr$Paracone + areas_allcusps_upr$Protocone + 
  areas_allcusps_upr$Metacone + areas_allcusps_upr$Hypocone + areas_allcusps_upr$Metaconule

areas_4cusps_lwr$Total_area <- areas_4cusps_lwr$Protoconid + areas_4cusps_lwr$Metaconid + 
  areas_4cusps_lwr$Hypoconid + areas_4cusps_lwr$Entoconid

areas_4cusps_upr$Total_area <- areas_4cusps_upr$Paracone + areas_4cusps_upr$Protocone + 
  areas_4cusps_upr$Metacone + areas_4cusps_upr$Hypocone

# Proportions
areas_allcusps_lwr$Ratio_Protoconid  <-  (areas_allcusps_lwr$Protoconid * 100) / areas_allcusps_lwr$Total_area
areas_allcusps_lwr$Ratio_Metaconid   <-  (areas_allcusps_lwr$Metaconid * 100) / areas_allcusps_lwr$Total_area
areas_allcusps_lwr$Ratio_Hypoconid   <-  (areas_allcusps_lwr$Hypoconid * 100) / areas_allcusps_lwr$Total_area
areas_allcusps_lwr$Ratio_Entoconid   <-  (areas_allcusps_lwr$Entoconid * 100) / areas_allcusps_lwr$Total_area
areas_allcusps_lwr$Ratio_Hypoconulid <-  (areas_allcusps_lwr$Hypoconulid * 100) / areas_allcusps_lwr$Total_area
areas_allcusps_lwr$Ratio_Entoconulid <-  (areas_allcusps_lwr$`Cusp 6 (Entoconulid)` * 100) / areas_allcusps_lwr$Total_area
areas_allcusps_lwr$Ratio_Metaconulid <-  (areas_allcusps_lwr$`Cusp 7 (Metaconulid)` * 100) / areas_allcusps_lwr$Total_area

areas_4cusps_lwr$Ratio_Protoconid <-  (areas_4cusps_lwr$Protoconid * 100) / areas_4cusps_lwr$Total_area
areas_4cusps_lwr$Ratio_Metaconid  <-  (areas_4cusps_lwr$Metaconid * 100) / areas_4cusps_lwr$Total_area
areas_4cusps_lwr$Ratio_Hypoconid  <-  (areas_4cusps_lwr$Hypoconid * 100) / areas_4cusps_lwr$Total_area
areas_4cusps_lwr$Ratio_Entoconid  <-  (areas_4cusps_lwr$Entoconid * 100) / areas_4cusps_lwr$Total_area

areas_allcusps_upr$Ratio_Paracone   <-  (areas_allcusps_upr$Paracone * 100) / areas_allcusps_upr$Total_area
areas_allcusps_upr$Ratio_Protocone  <-  (areas_allcusps_upr$Protocone * 100) / areas_allcusps_upr$Total_area
areas_allcusps_upr$Ratio_Metacone   <-  (areas_allcusps_upr$Metacone * 100) / areas_allcusps_upr$Total_area
areas_allcusps_upr$Ratio_Hypocone   <-  (areas_allcusps_upr$Hypocone * 100) / areas_allcusps_upr$Total_area
areas_allcusps_upr$Ratio_Metaconule <-  (areas_allcusps_upr$Metaconule * 100) / areas_allcusps_upr$Total_area

areas_4cusps_upr$Ratio_Paracone  <-  (areas_4cusps_upr$Paracone * 100) / areas_4cusps_upr$Total_area
areas_4cusps_upr$Ratio_Protocone <-  (areas_4cusps_upr$Protocone * 100) / areas_4cusps_upr$Total_area
areas_4cusps_upr$Ratio_Metacone  <-  (areas_4cusps_upr$Metacone * 100) / areas_4cusps_upr$Total_area
areas_4cusps_upr$Ratio_Hypocone  <-  (areas_4cusps_upr$Hypocone * 100) / areas_4cusps_upr$Total_area

# convert to dataframe
areas_allcusps_lwr_df <- data.frame(areas_allcusps_lwr)
areas_allcusps_upr_df <- data.frame(areas_allcusps_upr)
areas_4cusps_lwr_df   <- data.frame(areas_4cusps_lwr)
areas_4cusps_upr_df   <- data.frame(areas_4cusps_upr)

# ROWNAMES
rownames(areas_allcusps_lwr_df) <- areas_allcusps_lwr_df$Lower.molars
rownames(areas_4cusps_lwr_df)   <- areas_4cusps_lwr_df$Lower.molars
rownames(areas_4cusps_upr_df)   <- areas_4cusps_upr_df$Upper.molars
rownames(areas_allcusps_upr_df) <- areas_allcusps_upr_df$Upper.Molars

# Remove first column
areas_allcusps_lwr_df_final <- subset(areas_allcusps_lwr_df, select = -c(Lower.molars))
areas_allcusps_upr_df_final <- subset(areas_allcusps_upr_df, select = -c(Upper.Molars))
areas_4cusps_upr_df_final   <- subset(areas_4cusps_upr_df, select = -c(Upper.molars))
areas_4cusps_lwr_df_final   <- subset(areas_4cusps_lwr_df, select = -c(Lower.molars))

# Function to run HCA
# scaling data
areas_allcusps_lwr_df_final_scale <- scale(areas_allcusps_lwr_df_final)
areas_allcusps_upr_df_final_scale <- scale(areas_allcusps_upr_df_final)
areas_4cusps_lwr_df_final_scale   <- scale(areas_4cusps_lwr_df_final)
areas_4cusps_upr_df_final_scale   <- scale(areas_4cusps_upr_df_final)

hca_areas <- function(data, title, n_clusters) {
  # Dissimilarity matrix
  d <- dist(data, method = "euclidean")
  # HCA
  hca <- hclust(d, method = "ward.D2" )
  # Number of grups
  k_groups <- n_clusters
  # Colors of the groups
  # Wether the plot should be saved as SVG file or not
  if(n_clusters == 2) {
    k_colors_grups <- c("#9E9AC8", "#3F007D")
  }
  if(n_clusters == 3) {
    k_colors_grups <- c("#9E9AC8", "#6A51A3", "#3F007D")
  } 
  if(n_clusters == 4) {
    k_colors_grups <- c("#DADAEB", "#9E9AC8", "#6A51A3", "#3F007D")
  } 
  if(n_clusters == 5) {
    k_colors_grups <- c("#DADAEB", "#9E9AC8", "#6A51A3", "#3F007D", "#2A0053")
  } 
  # Cut tree into 3 groups
  sub_grp <- cutree(hca, k = k_groups)
  # ggplot 1 - PC
  fvizplot  <- fviz_cluster(list(data = data, cluster = sub_grp),
                            repel = F,
                            labelsize = 0) +
    ggtitle(title) + 
    theme_minimal() + 
    scale_fill_manual(values = k_colors_grups) + 
    scale_color_manual(values = k_colors_grups) +
    scale_x_reverse() +
    scale_y_reverse()
  
  # ggplot 2 - DENDROGRAM
  fviz_dendplot <- fviz_dend(hca, k = k_groups, # Cut in groups
                             cex = 0.4, # label size
                             k_colors = k_colors_grups,
                             color_labels_by_k = TRUE, # color labels by groups
                             type = "rectangle",
                             rect = TRUE,
                             horiz = TRUE) + # Add rectangle around groups
    # ggtitle(title) + 
    theme_minimal()
  
  # Create list
  return(list(HCA = hca, 
              fvizplot = fvizplot, 
              fviz_dendplot = fviz_dendplot))
}

all_lwr_3  <- hca_areas(areas_allcusps_lwr_df_final_scale, "Lower - All cusps", n_clusters = 3)
all_upr_3  <- hca_areas(areas_allcusps_upr_df_final_scale, "Upper - All cusps", n_clusters = 3)
four_lwr_3 <- hca_areas(areas_4cusps_lwr_df_final_scale, "Lower - 4 cusps",    n_clusters = 3)
four_upr_3 <- hca_areas(areas_4cusps_upr_df_final_scale, "Upper - 4 cusps",    n_clusters = 3)

all_lwr_2  <- hca_areas(areas_allcusps_lwr_df_final_scale, "Lower - All cusps", n_clusters = 2)
all_upr_2  <- hca_areas(areas_allcusps_upr_df_final_scale, "Upper - All cusps", n_clusters = 2)
four_lwr_2 <- hca_areas(areas_4cusps_lwr_df_final_scale, "Lower - 4 cusps",    n_clusters = 2)
four_upr_2 <- hca_areas(areas_4cusps_upr_df_final_scale, "Upper - 4 cusps",    n_clusters = 2)

all_lwr_4  <- hca_areas(areas_allcusps_lwr_df_final_scale, "Lower - All cusps", n_clusters = 4)
all_upr_4  <- hca_areas(areas_allcusps_upr_df_final_scale, "Upper - All cusps", n_clusters = 4)
four_lwr_4 <- hca_areas(areas_4cusps_lwr_df_final_scale, "Lower - 4 cusps",    n_clusters = 4)
four_upr_4 <- hca_areas(areas_4cusps_upr_df_final_scale, "Upper - 4 cusps",    n_clusters = 4)

four_upr_5 <- hca_areas(areas_4cusps_upr_df_final_scale, "Upper - 4 cusps",    n_clusters = 5)

# OPTIMAL NUMBER OF CLUSTERS AND CLUSTERS
ggarrange(fviz_nbclust(areas_4cusps_upr_df_final_scale,   kmeans, method = "silhouette", k.max = 8),
          four_upr_2$fviz_dendplot, ncol = 1)

ggarrange(fviz_nbclust(areas_4cusps_lwr_df_final_scale,   kmeans, method = "silhouette", k.max = 8),
          four_lwr_3$fviz_dendplot, ncol = 1)

ggarrange(fviz_nbclust(areas_allcusps_lwr_df_final_scale,   kmeans, method = "silhouette", k.max = 8),
          all_lwr_2$fviz_dendplot, ncol = 1)

ggarrange(fviz_nbclust(areas_allcusps_upr_df_final_scale,   kmeans, method = "silhouette", k.max = 8),
          all_upr_4$fviz_dendplot, ncol = 1)

# MORPHOLOGITAL TRAITS (MOLARS) ----

# Load data
UI_scores <- read_excel("morphological traits.xlsx", sheet = "all teeth scored", range = "A2:I9",    na = "NA")
UP_scores <- read_excel("morphological traits.xlsx", sheet = "all teeth scored", range = "A18:O35",  na = "NA")
LP_scores <- read_excel("morphological traits.xlsx", sheet = "all teeth scored", range = "A38:O65",  na = "NA")
UM_scores <- read_excel("morphological traits.xlsx", sheet = "all teeth scored", range = "A68:P99",  na = "NA")
LM_scores <- read_excel("morphological traits.xlsx", sheet = "all teeth scored", range = "A102:Q159", na = "NA")

UI_scores <- as.data.frame(UI_scores)
UP_scores <- as.data.frame(UP_scores)
LP_scores <- as.data.frame(LP_scores)
UM_scores <- as.data.frame(UM_scores)
LM_scores <- as.data.frame(LM_scores)

# Renaming rows with ID of teeth
rownames(UI_scores) <- UI_scores$`Inventaire spécimen`
rownames(UP_scores) <- UP_scores$`Inventaire spécimen`
rownames(LP_scores) <- LP_scores$`Inventaire spécimen`
rownames(UM_scores) <- UM_scores$`Inventaire spécimen`
rownames(LM_scores) <- LM_scores$`Inventaire spécimen`

# Keeping only morphological variables
UI_scores_df <- UI_scores[,-c(1:3, ncol(UI_scores), ncol(UI_scores)-1, ncol(UI_scores)-2)]
UP_scores_df <- UP_scores[,-c(1:3, ncol(UP_scores), ncol(UP_scores)-1, ncol(UP_scores)-2)]
LP_scores_df <- LP_scores[,-c(1:3, ncol(LP_scores), ncol(LP_scores)-1, ncol(LP_scores)-2)]
UM_scores_df <- UM_scores[,-c(1:3, ncol(UM_scores), ncol(UM_scores)-1, ncol(UM_scores)-2)]
LM_scores_df <- LM_scores[,-c(1:3, ncol(LM_scores), ncol(LM_scores)-1, ncol(LM_scores)-2)]

# Convert to factors all columns
UI_scores_df[sapply(UI_scores_df, is.character)] <- lapply(UI_scores_df[sapply(UI_scores_df, is.character)], as.factor)
UP_scores_df[sapply(UP_scores_df, is.character)] <- lapply(UP_scores_df[sapply(UP_scores_df, is.character)], as.factor)
LP_scores_df[sapply(LP_scores_df, is.character)] <- lapply(LP_scores_df[sapply(LP_scores_df, is.character)], as.factor)
UM_scores_df[sapply(UM_scores_df, is.character)] <- lapply(UM_scores_df[sapply(UM_scores_df, is.character)], as.factor)
LM_scores_df[sapply(LM_scores_df, is.character)] <- lapply(LM_scores_df[sapply(LM_scores_df, is.character)], as.factor)

# Function to run HCA
nomclust_omo <- function(data, titulo){
  # Run the nomclust function
  hca.omo <- nomclust(na.omit(data), measure = "lin")
  # this creates df with summary and AIC values (to know number clusters)
  # this identifies the lowest AIC and determine which is the row it belongs to   
  # https://stackoverflow.com/questions/20782218/how-to-find-row-number-of-a-value-in-r-code
  df_sum <- as.data.frame(hca.omo$eval) 
  number_clusters <- which(df_sum$AIC == min(df_sum$AIC))
  # plotting either AIC and dendrogram
  # par(mfrow = c(1,2))
  nf <- layout( matrix(c(1,2), nrow=2, byrow=TRUE),
                heights = c(1.5,2.5))
  eval.plot(hca.omo, criteria = "AIC")
  dend.plot(hca.omo, clusters = number_clusters, style = "dark", colorful = TRUE, clu.col = c("#DADAEB", "#9E9AC8", "#6A51A3", "#3F007D"))
  mtext(titulo, side = 3, line = -1.5, outer = TRUE, cex = 2)
  # Creation of dataframe with the assignation of groups based on number of clusters
  if(number_clusters == 4){
    cl1 <- data.frame(Tooth = row.names(na.omit(data))[hca.omo$mem$clu_4 == 1])
    cl1$Cluster <- "Group 1"
    cl2 <- data.frame(Tooth = row.names(na.omit(data))[hca.omo$mem$clu_4 == 2])
    cl2$Cluster <- "Group 2"
    cl3 <- data.frame(Tooth = row.names(na.omit(data))[hca.omo$mem$clu_4 == 3])
    cl3$Cluster <- "Group 3"
    cl4 <- data.frame(Tooth = row.names(na.omit(data))[hca.omo$mem$clu_4 == 4])
    cl4$Cluster <- "Group 4"
    cluster_df <- rbind(cl1, cl2, cl3, cl4)
    row.names(cluster_df) <- cluster_df$Tooth
    cluster_df <- cluster_df[2]
  }
  if(number_clusters == 3) {
    cl1 <- data.frame(Tooth = row.names(na.omit(data))[hca.omo$mem$clu_3 == 1])
    cl1$Cluster <- "Group 1"
    cl2 <- data.frame(Tooth = row.names(na.omit(data))[hca.omo$mem$clu_3 == 2])
    cl2$Cluster <- "Group 2"
    cl3 <- data.frame(Tooth = row.names(na.omit(data))[hca.omo$mem$clu_3 == 3])
    cl3$Cluster <- "Group 3"
    cluster_df <- rbind(cl1, cl2, cl3)    
    row.names(cluster_df) <- cluster_df$Tooth
    cluster_df <- cluster_df[2]
  }
  if(number_clusters == 2) {
    cl1 <- data.frame(Tooth = row.names(na.omit(data))[hca.omo$mem$clu_2 == 1])
    cl1$Cluster <- "Group 1"
    cl2 <- data.frame(Tooth = row.names(na.omit(data))[hca.omo$mem$clu_2 == 2])
    cl2$Cluster <- "Group 2"
    cluster_df <- rbind(cl1, cl2)    
    row.names(cluster_df) <- cluster_df$Tooth
    cluster_df <- cluster_df[2]
  }
  if(number_clusters == 5){
    cl1 <- data.frame(Tooth = row.names(na.omit(data))[hca.omo$mem$clu_5 == 1])
    cl1$Cluster <- "Group 1"
    cl2 <- data.frame(Tooth = row.names(na.omit(data))[hca.omo$mem$clu_5 == 2])
    cl2$Cluster <- "Group 2"
    cl3 <- data.frame(Tooth = row.names(na.omit(data))[hca.omo$mem$clu_5 == 3])
    cl3$Cluster <- "Group 3"
    cl4 <- data.frame(Tooth = row.names(na.omit(data))[hca.omo$mem$clu_5 == 4])
    cl4$Cluster <- "Group 4"
    cl5 <- data.frame(Tooth = row.names(na.omit(data))[hca.omo$mem$clu_5 == 5])
    cl5$Cluster <- "Group 5"
    cluster_df <- rbind(cl1, cl2, cl3, cl4, cl5)
    row.names(cluster_df) <- cluster_df$Tooth
    cluster_df <- cluster_df[2]
  }
  # We create a list with the different elements
  list(hca.omo = hca.omo,
       number_clusters = number_clusters,
       Cluster_Assignment = cluster_df)
}

nomclust_omo_UP <- nomclust_omo(data = UP_scores_df, titulo = "UPPER PREMOLARS")
nomclust_omo_LP <- nomclust_omo(data = LP_scores_df, titulo = "LOWER PREMOLARS")
nomclust_omo_UM <- nomclust_omo(data = UM_scores_df, titulo = "UPPER MOLARS")
nomclust_omo_LM <- nomclust_omo(data = LM_scores_df, titulo = "LOWER MOLARS")
