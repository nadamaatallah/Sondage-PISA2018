# Projet 7 
# Application aux données PISA 2018 - France

rm(list = ls())

library(haven)
library(dplyr)

# 1) Chargement et préparation des données

data <- read_sav("C:/Users/nadam/Desktop/Projet_Sondage/CY07_MSU_STU_QQQ.sav")

dataset <- data %>%
  select(CNT, CNTSCHID, ST004D01T, ESCS) %>%
  filter(!is.na(CNT), !is.na(CNTSCHID), !is.na(ST004D01T), !is.na(ESCS)) %>%
  filter(CNT == "FRA") %>%
  rename(
    country = CNT,
    school_id = CNTSCHID,
    gender = ST004D01T,
    escs = ESCS
  ) %>%
  mutate(
    country = as.character(as_factor(country)),
    gender = as.character(as_factor(gender)),
    escs = as.numeric(escs)
  )

cat("===== POPULATION ETUDIEE =====\n")
cat("Pays :", unique(dataset$country), "\n")
cat("Nombre d'élèves :", nrow(dataset), "\n")
cat("Nombre d'écoles :", length(unique(dataset$school_id)), "\n")
cat("Répartition par sexe :\n")
print(table(dataset$gender))
cat("\nRésumé de ESCS :\n")
print(summary(dataset$escs))

mu_pop <- mean(dataset$escs)
cat("\nMoyenne réelle de la population (ESCS) :", round(mu_pop, 6), "\n\n")


# 2) Visualisation des figures

# Figure 1 : répartition selon le sexe
effectifs_sexe <- table(
  factor(dataset$gender,
         levels = c("Female", "Male"),
         labels = c("Fille", "Garçon"))
)

barplot(
  effectifs_sexe,
  main = "Répartition des élèves selon le sexe",
  xlab = "Sexe",
  ylab = "Effectif",
  col = c("#E8A0BF", "#7FA7D9"),
  border = "white"
)

# Figure 2 : histogramme de ESCS
hist(
  dataset$escs,
  main = "Distribution de l'indice socio-économique",
  xlab = "Indice socio-économique (ESCS)",
  ylab = "Effectif",
  col = "#A8D5BA",
  border = "white"
)

# Figure 3 : boxplot ESCS selon sexe
sexe_fr <- factor(dataset$gender,
                  levels = c("Female", "Male"),
                  labels = c("Fille", "Garçon"))

boxplot(
  dataset$escs ~ sexe_fr,
  main = "Indice socio-économique selon le sexe",
  xlab = "Sexe",
  ylab = "Indice socio-économique (ESCS)",
  col = c("#E8A0BF", "#7FA7D9"),
  border = "#444444"
)

# Figure 4 : distribution du nombre d'élèves par école
taille_ecoles <- table(dataset$school_id)

hist(
  taille_ecoles,
  main = "Distribution du nombre d'élèves par école",
  xlab = "Nombre d'élèves par école",
  ylab = "Nombre d'écoles",
  col = "#F6C177",
  border = "white"
)

# Aperçu du dataset
head(dataset)

# 3) Paramètres de l'étude

B <- 500
m_clusters <- 10

cat("===== PARAMETRES =====\n")
cat("Nombre de répétitions B =", B, "\n")
cat("Nombre d'écoles tirées par strate =", m_clusters, "\n\n")

# 4) Fonctions d'échantillonnage

sample_srs <- function(df, n) {
  df[sample.int(nrow(df), size = n, replace = FALSE), , drop = FALSE]
}

sample_stratified <- function(df, n_total) {
  Nh <- table(df$gender)
  n_h <- floor(n_total * Nh / sum(Nh))
  
  reste <- n_total - sum(n_h)
  if (reste > 0) {
    frac <- (n_total * Nh / sum(Nh)) - n_h
    idx <- order(frac, decreasing = TRUE)[1:reste]
    n_h[idx] <- n_h[idx] + 1
  }
  
  bind_rows(
    df %>% filter(gender == names(n_h)[1]) %>% sample_n(n_h[1]),
    df %>% filter(gender == names(n_h)[2]) %>% sample_n(n_h[2])
  )
}

sample_combined <- function(df, m_clusters_per_stratum) {
  res <- data.frame()
  
  for (g in unique(df$gender)) {
    df_g <- df %>% filter(gender == g)
    schools <- unique(df_g$school_id)
    selected_schools <- sample(schools, size = m_clusters_per_stratum, replace = FALSE)
    temp <- df_g %>% filter(school_id %in% selected_schools)
    res <- bind_rows(res, temp)
  }
  
  res
}

# 5) Contrôle sur un tirage


ech_comb <- sample_combined(dataset, m_clusters)
n_comb <- nrow(ech_comb)

cat("===== CONTROLE : 1 TIRAGE =====\n")
cat("Taille de l'échantillon combiné :", n_comb, "\n")
cat("Répartition par sexe dans l'échantillon combiné :\n")
print(table(ech_comb$gender))

mean_comb_1 <- mean(ech_comb$escs)
mean_srs_1  <- mean(sample_srs(dataset, n_comb)$escs)
mean_strat_1 <- mean(sample_stratified(dataset, n_comb)$escs)

cat("\nMoyennes estimées sur un tirage :\n")
cat("Combiné   :", round(mean_comb_1, 6), "\n")
cat("PESR      :", round(mean_srs_1, 6), "\n")
cat("PEAR      :", round(mean_strat_1, 6), "\n\n")

# 6) Simulation Monte-Carlo

set.seed(2026)

res_comb  <- numeric(B)
res_srs   <- numeric(B)
res_strat <- numeric(B)

for (b in 1:B) {
  res_comb[b]  <- mean(sample_combined(dataset, m_clusters)$escs)
  res_srs[b]   <- mean(sample_srs(dataset, n_comb)$escs)
  res_strat[b] <- mean(sample_stratified(dataset, n_comb)$escs)
}

# 7) Résultats


var_emp_comb  <- var(res_comb)
var_emp_srs   <- var(res_srs)
var_emp_strat <- var(res_strat)

sd_emp_comb  <- sd(res_comb)
sd_emp_srs   <- sd(res_srs)
sd_emp_strat <- sd(res_strat)

cat("===== VARIANCES EMPIRIQUES =====\n")
cat("Var(mean) Combiné =", round(var_emp_comb, 6), "\n")
cat("Var(mean) PESR    =", round(var_emp_srs, 6), "\n")
cat("Var(mean) PEAR    =", round(var_emp_strat, 6), "\n\n")

cat("===== ECARTS-TYPES EMPIRIQUES =====\n")
cat("SD(mean) Combiné =", round(sd_emp_comb, 6), "\n")
cat("SD(mean) PESR    =", round(sd_emp_srs, 6), "\n")
cat("SD(mean) PEAR    =", round(sd_emp_strat, 6), "\n\n")

# 8) Boxplot comparatif

boxplot(
  res_comb, res_srs, res_strat,
  names = c("Combiné", "PESR", "PEAR"),
  main = "Distribution des moyennes estimées selon le plan",
  ylab = "Moyenne estimée de ESCS",
  col = c("#B7C9F2", "#F2C6C2", "#BFE3C0"),
  border = "#444444"
)

abline(h = mu_pop, col = "red", lwd = 2, lty = 2)

legend("topright",
       legend = c("Moyenne réelle de la population"),
       col = "red",
       lty = 2,
       lwd = 2,
       bty = "n")

# 9) Tableau final

results <- data.frame(
  Plan = c("Combiné (strates+grappes)", "PESR (simple)", "PEAR (stratifié)"),
  n = c(n_comb, n_comb, n_comb),
  Mean_of_estimates = c(mean(res_comb), mean(res_srs), mean(res_strat)),
  Var_of_estimates = c(var_emp_comb, var_emp_srs, var_emp_strat),
  SD_of_estimates = c(sd_emp_comb, sd_emp_srs, sd_emp_strat)
)

results$Mean_of_estimates <- round(results$Mean_of_estimates, 6)
results$Var_of_estimates  <- round(results$Var_of_estimates, 6)
results$SD_of_estimates   <- round(results$SD_of_estimates, 6)

cat("===== TABLEAU FINAL =====\n")
print(results)
cat("\n")

