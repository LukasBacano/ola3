# OPG 4
#
# Opgave 4 – Stabilitet i jeres forbrugertillidsindikator
# Opgave 4.1 – Test af model fra opgave 1
# Undersøg stabiliteten af jeres fundne indikator fra opgave 1. Giv en grundig forklaring på
# opsætning til at undersøge stabiliteten.
#
#
# Opgave 1 – Den bedste forbrugertillidsindikator

########################################################################################################
# Opgave 1.1 – Kombinationsalgoritme i R
# Lav alle kombinationer af de 12 spørgsmål i forbrugerundersøgelsen af DST. I skal bruge data fra 1.
# kvartal 2000 til og med 4. kvartal 2024.
########################################################################################################
library(dkstat)
library(tidyverse)

# Hent data
FORV1 <- dst_meta(table = "FORV1", lang = "da")

FORV1_filter <- list(
  INDIKATOR = "*",
  Tid = "*"
)

FORV1Data <- dst_get_data(table = "FORV1", query = FORV1_filter, lang = "da")

FORV1Data <- as.data.frame(FORV1Data)
unikke_indikatorer <- unique(FORV1Data$INDIKATOR)
indikator_lister <- list()
for (indikator in unikke_indikatorer) {
  indikator_lister[[indikator]] <- FORV1Data[FORV1Data$INDIKATOR == indikator, ]
}

samlet_liste <- as.data.frame(indikator_lister)

dst_ftiMonths <- samlet_liste[304:nrow(samlet_liste), c(2,3,6,9,12,15,18,21,24,27,30,33,36,39)]

dst_ftiMonths$Arbejdsløsheden.om.et.år..sammenlignet.med.i.dag.value <- 
  dst_ftiMonths$Arbejdsløsheden.om.et.år..sammenlignet.med.i.dag.value * (-1)

dst_ftiMonths$Priser.i.dag..sammenlignet.med.for.et.år.siden.value <- 
  dst_ftiMonths$Priser.i.dag..sammenlignet.med.for.et.år.siden.value * (-1)

# Opret kvartalsdata
DST_FTI <- data.frame(
  Tid = dst_ftiMonths$Forbrugertillidsindikatoren.TID[seq(1, nrow(dst_ftiMonths), by = 3)]
)

for (col in 2:ncol(dst_ftiMonths)) {
  DST_FTI[[colnames(dst_ftiMonths)[col]]] <- sapply(seq(1, nrow(dst_ftiMonths), by = 3), function(i) {
    mean(dst_ftiMonths[i:(i+2), col], na.rm = TRUE)
  })
}

DST_FTI98 <- DST_FTI[1:(nrow(DST_FTI)-2),]

Forbrugv <- dst_meta(table = "NKHC021")
Forbrugvdf <- list(
  FORMAAAL="*",
  PRISENHED="*",
  SÆSON="*", 
  Tid="*"
)

Forbrugdata <- dst_get_data(table = "NKHC021", query = Forbrugvdf, lang="da")
Forbrugdata <- pivot_wider(Forbrugdata, names_from = FORMAAAL, values_from = value)

# Justér indeks efter datatilgængelighed (tilpas efter dit datasæt)
Forbrugdata <- Forbrugdata[277:nrow(Forbrugdata),]
Forbrugdata <- Forbrugdata[139:nrow(Forbrugdata),]
Forbrugdata <- Forbrugdata[37:nrow(Forbrugdata), 3:4]
Forbrugdata <- as.data.frame(Forbrugdata)

Forbrugdata[, 2] <- as.numeric(Forbrugdata[, 2])
Forbrugdata$Årlig_vækst <- c(rep(NA, 4), diff(Forbrugdata[, 2], lag = 4) / Forbrugdata[-(1:4), 2] * 100)
Forbrugdata <- na.omit(Forbrugdata)

DST_FTISPØRG <- DST_FTI[,2:ncol(DST_FTI)]

# Liste over kolonnenavne (de 12 spørgsmål)
kolonnenavne <- names(DST_FTI)[3:ncol(DST_FTI)]

# Generer alle kombinationer
alle_kombinationer <- list()
for (kol in 1:length(kolonnenavne)) {
  kombi <- combn(kolonnenavne, kol, simplify = FALSE)
  alle_kombinationer <- c(alle_kombinationer, kombi)
}

Forbrugdata <- Forbrugdata[-c(99),]

Forbrug <- Forbrugdata[-c(99:101), c(1,3)]
Forbrug <- na.omit(Forbrug)

# Regression på alle kombinationer
regression_resultater <- list()
for (i in 1:length(alle_kombinationer)) {
  kombi <- alle_kombinationer[[i]]
  
  if (all(kombi %in% names(DST_FTI98))) {
    FTI <- rowMeans(DST_FTI98[, kombi, drop = FALSE], na.rm = TRUE)
    FTI <- data.frame(FTI = FTI)
    
    if (nrow(FTI) == nrow(Forbrug)) {
      model_data <- data.frame(FTI = FTI$FTI, forbrug = Forbrug$Årlig_vækst)
      regression_resultater[[i]] <- tryCatch({
        mod <- lm(forbrug ~ FTI, data = model_data)
        summary(mod)
      }, error = function(e) NULL)
    } else {
      regression_resultater[[i]] <- NULL
    }
  } else {
    regression_resultater[[i]] <- NULL
  }
}

# Udtræk R² værdier
Ri2 <- numeric(length(regression_resultater))
for (i in 1:length(regression_resultater)) {
  if (!is.null(regression_resultater[[i]])) {
    Ri2[i] <- regression_resultater[[i]]$r.squared
  } else {
    Ri2[i] <- NA
  }
}

# Sortér kombinationer efter R²
gyldige_indeks <- which(!is.na(Ri2))
Ri2_gyldig <- Ri2[gyldige_indeks]
top_all_index <- gyldige_indeks[order(Ri2_gyldig, decreasing = TRUE)]

# Simpel funktion til at teste stabilitet
beregn_r_squared_stabilitet <- function(model_data, kombi) {
  r_squared_value <- c()
  
  for (i in 1:nrow(model_data)) {
    reduceret_data <- model_data[-i, ]
    model <- lm(forbrug ~ FTI, data = reduceret_data)
    # Da vi altid får et model objekt, behøver vi ikke tjek for NULL
    r_squared_value <- c(r_squared_value, summary(model)$r.squared)
  }
  
  r_squared_mean <- mean(r_squared_value)
  r_squared_sd <- sd(r_squared_value)
  
  return(list(kombination = paste(kombi, collapse = " + "), mean = r_squared_mean, sd = r_squared_sd))
}

r_squared_stabilitet <- list()

# Test stabilitet for top-kombinationer
for (i in top_all_index) {
  kombi <- alle_kombinationer[[i]]
  if (all(kombi %in% names(DST_FTI98))) {
    FTI <- rowMeans(DST_FTI98[, kombi, drop = FALSE], na.rm = TRUE)
    model_data <- data.frame(FTI = FTI, forbrug = Forbrug$Årlig_vækst)
    
    if (nrow(model_data) == nrow(Forbrug) && nrow(model_data) > 1) {
      stabilitet <- beregn_r_squared_stabilitet(model_data, kombi)
      r_squared_stabilitet[[i]] <- stabilitet
    } else {
      r_squared_stabilitet[[i]] <- NULL
    }
  } else {
    r_squared_stabilitet[[i]] <- NULL
  }
}

# Fjern NULLs
r_squared_stabilitet <- r_squared_stabilitet[!sapply(r_squared_stabilitet, is.null)]

# Konverter til data.frame
r_squared_stabilitet_df <- do.call(rbind, lapply(r_squared_stabilitet, function(x) {
  as.data.frame(x, stringsAsFactors = FALSE)
}))

if (!is.null(r_squared_stabilitet_df) && nrow(r_squared_stabilitet_df) > 0) {
  r_squared_stabilitet_df$mean <- as.numeric(r_squared_stabilitet_df$mean)
  r_squared_stabilitet_df$sd <- as.numeric(r_squared_stabilitet_df$sd)
  
  # Tilføj en stabilitetsmåling
  r_squared_stabilitet_df$stabilitet <- r_squared_stabilitet_df$mean / r_squared_stabilitet_df$sd
  
  # Find mest stabile kombination
  mest_stabile <- r_squared_stabilitet_df[order(r_squared_stabilitet_df$sd, na.last = NA), ][1, ]
  
  cat("Den mest stabile kombination:", mest_stabile$kombination, "\n")
  cat("Gennemsnitlig R-squared:", mest_stabile$mean, "\n")
  cat("Standardafvigelse af R-squared:", mest_stabile$sd, "\n")
} else {
  cat("Ingen stabile kombinationer fundet\n")
}

########################    VORES FTI FRA OPG 1  ########################
# Her kan du stadig sammenligne med en specifik kombination, hvis du vil.

spørgsmål <- c("Danmarks.økonomiske.situation.i.dag..sammenlignet.med.for.et.år.siden.value",
               "Priser.om.et.år..sammenlignet.med.i.dag.value",
               "Anskaffelse.af.større.forbrugsgoder..inden.for.de.næste.12.mdr..value",
               "Regner.med.at.kunne.spare.op.i.de.kommende.12.måneder.value")

if (!is.null(r_squared_stabilitet_df) && nrow(r_squared_stabilitet_df) > 0) {
  resultat <- r_squared_stabilitet_df[sapply(r_squared_stabilitet_df$kombination, function(x) {
    vars_in_kombi <- unlist(strsplit(x, " \\+ "))
    all(spørgsmål %in% vars_in_kombi) && length(vars_in_kombi) == length(spørgsmål)
  }), ]
  
  print(resultat)
} else {
  cat("Ingen kombinationer at sammenligne\n")
}