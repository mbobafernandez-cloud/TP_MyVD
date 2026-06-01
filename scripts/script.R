# =============================================================================
# TP ENGLOBADOR- Yazmin Dellacasa, Martina Boba Fernandez, Naiara Bustos Diaz. 

# Preguntas de investigación:
#   1. ¿Qué relación hay entre capacidad económica y desempeño académico?
#   2. Controlando por cap. económica: ¿tener libros en casa mejora el desempeño en Lengua?
#   3. Controlando por cap. económica: ¿usar redes sociales afecta el desempeño académico?
#   4. ¿La diferencia entre clases sociales afecta en el bullying/discriminación?
# =============================================================================

#cargamos librerias: 

library(haven)
library(dplyr)
library(ggplot2)
library(here)   
library(tidyr)  

dir.create(here("outputs"), showWarnings = FALSE) # Crea carpeta de salida si no existe

# Carga de la base desde la carpeta /data

#!!! ACLARACION: 
#LA BASE NO ESTA SUBIDA PORQUE ES MUY PESADA PARA GITHUB. 

path_base <- here("data", "aprender_2023.dta")
base <- read_dta(path_base)

# =============================================================================
# SECCIÓN 1: CONSTRUCCIÓN DE LA BASE DE INTERÉS
# =============================================================================

#seleccionamos las variables de interes y creamos una nueva base 

nueva_base <- base %>%
  select(
    # Sociodemográficas
    edad         = ap01,
    sexo         = ap03,
    ed_madre     = nivel_ed_madre,
    ed_padre     = nivel_ed_padre,
    region       = region,
    
    # Variables para construir proxy NSE
    agua         = ap09a,    # Agua potable (1=sí, 2=no)
    habitaciones = ap14,     # Cantidad de habitaciones
    internet     = ap09d,    # Conexión a internet (1=sí, 2=no)
    compu        = ap09i,    # Computadora (1=sí, 2=no)
    streaming    = ap09h,    # Netflix/Disney+ (1=sí, 2=no)
    auto         = ap09g,    # Auto o moto (1=sí, 2=no)
    
    # Variable clave P2: libros en el hogar
    libros       = ap10,     # 1=sin libros ... 6=más de 100 libros
    
    # Variable clave P3: redes sociales
    redes        = ap05a,    # Usa redes en tiempo libre (1=sí, 2=no)
    
    # Variables clave P4: bullying y discriminación
    discriminacion  = apa37,  # Te discriminan en la escuela (1=sí, 2=no)
    disc_nse        = apa38j, # Discriminado por sit. socioeconómica (0=no, 1=sí)
    bullying_fisico = apb38a, # Agresiones físicas/verbales (1=siempre...4=nunca)
    bullying_virtual= apb38b, # Agresiones en redes sociales
    robo            = apb38c, # Te quitaron o rompieron cosas
    
    # Variables de desempeño
    ldesemp  = ldesemp, #desempeño en la prueba de lengua 
    mdesemp  = mdesemp,
    lpuntaje = lpuntaje,
    mpuntaje = mpuntaje
  )


# =============================================================================
# SECCIÓN 2: TRANSFORMACIÓN Y LIMPIEZA DE VARIABLES
# =============================================================================

# Con el mutate() transformamos las variables a binarias y eliminamos algunos valores negativos

nueva_base <- nueva_base %>%
  mutate(
    
    # --- Sociodemográficas ---
    sexo     = ifelse(sexo == 1, 1, ifelse(sexo == 2, 0, NA)),
    edad     = ifelse(as.numeric(edad) < 0, NA, as.numeric(edad)),
    ed_madre = ifelse(ed_madre < 0, NA, ed_madre),
    ed_padre = ifelse(ed_padre < 0, NA, ed_padre),
    
    # --- Componentes del proxy NSE (1=tiene, 0=no tiene) ---
    agua         = case_when(agua == 1 ~ 1, agua == 2 ~ 0, TRUE ~ NA_real_),
    streaming    = case_when(streaming == 1 ~ 1, streaming == 2 ~ 0, TRUE ~ NA_real_),
    internet     = case_when(internet == 1 ~ 1, internet == 2 ~ 0, TRUE ~ NA_real_),
    auto         = case_when(auto == 1 ~ 1, auto == 2 ~ 0, TRUE ~ NA_real_),
    compu        = case_when(compu == 1 ~ 1, compu == 2 ~ 0, TRUE ~ NA_real_),
    habitaciones = ifelse(habitaciones < 0, NA, habitaciones),
    
    # --- Libros (P2): 1=sin libros, 2=1-5, 3=6-20, 4=21-50, 5=51-100, 6=+100 ---
    libros = ifelse(libros < 0, NA, libros),
    
    # --- Redes sociales (P3) ---
    redes = case_when(redes == 1 ~ 1, redes == 2 ~ 0, TRUE ~ NA_real_),
    
    # --- Discriminación y bullying (P4) ---
    discriminacion = case_when(discriminacion == 1 ~ 1, discriminacion == 2 ~ 0, TRUE ~ NA_real_),
    disc_nse       = ifelse(disc_nse < 0, NA, disc_nse),
    
    bullying_fisico_frec  = case_when(
      bullying_fisico  %in% c(1, 2) ~ 1,
      bullying_fisico  %in% c(3, 4) ~ 0,
      TRUE ~ NA_real_),
    bullying_virtual_frec = case_when(
      bullying_virtual %in% c(1, 2) ~ 1,
      bullying_virtual %in% c(3, 4) ~ 0,
      TRUE ~ NA_real_),
    robo_frec = case_when(
      robo %in% c(1, 2) ~ 1,
      robo %in% c(3, 4) ~ 0,
      TRUE ~ NA_real_),
    
    # --- Dummies de desempeño: hacemos q el desempeño de lengua y matematica sean bianrias ---
    ldesemp_avanzado = ifelse(ldesemp == 4, 1, ifelse(ldesemp > 0, 0, NA)),
    mdesemp_avanzado = ifelse(mdesemp == 4, 1, ifelse(mdesemp > 0, 0, NA)),
    ldesemp_bajo     = ifelse(ldesemp == 1, 1, ifelse(ldesemp > 0, 0, NA)),
    mdesemp_bajo     = ifelse(mdesemp == 1, 1, ifelse(mdesemp > 0, 0, NA))
  )


# =============================================================================
# SECCIÓN 3: CONSTRUCCIÓN DEL PROXY DE NSE
# =============================================================================
# NSE_puntaje viene vacío en el dataset (osea son todos NA), entonces construimos nuestro propio índice.
# Estandarizamos cada variable (media 0, desvío 1) para que todas pesen igual,
# y promediamos. Mayor valor = NSE más alto.

nueva_base <- nueva_base %>%
  mutate(
    nse_proxy = rowMeans(
      scale(cbind(streaming, internet, compu, auto, agua, habitaciones)),
      na.rm = TRUE
    )
  )

# Versión categórica: 4 cuartiles para tablas y gráficos
nueva_base <- nueva_base %>%
  mutate(
    nse_cuartil = ntile(nse_proxy, 4),
    nse_cuartil = factor(nse_cuartil,
                         labels = c("NSE Bajo", "NSE Medio-Bajo",
                                    "NSE Medio-Alto", "NSE Alto"))
  )


# =============================================================================
# SECCIÓN 4: ANÁLISIS EXPLORATORIO DE DATOS (EDA)
# =============================================================================

cat("============================================================\n")
cat("           ANÁLISIS EXPLORATORIO - APRENDER 2023           \n")
cat("============================================================\n")
cat("Observaciones totales:", nrow(nueva_base), "\n\n")


# ------------------------------------------------------------
# 1) FRECUENCIAS DE VARIABLES CLAVE
# ------------------------------------------------------------

cat("----- Desempeño en Lengua (1=Bajo, 2=Básico, 3=Satisfactorio, 4=Avanzado) -----\n")
print(table(nueva_base$ldesemp, useNA = "ifany"))

cat("\n----- Desempeño en Matemática -----\n")
print(table(nueva_base$mdesemp, useNA = "ifany"))

cat("\n----- Libros en el hogar (1=Ninguno ... 6=Más de 100) -----\n")
print(table(nueva_base$libros, useNA = "ifany"))

cat("\n----- Usa redes sociales (0=No, 1=Sí) -----\n")
print(table(nueva_base$redes, useNA = "ifany"))

cat("\n----- Discriminación en la escuela (0=No, 1=Sí) -----\n")
print(table(nueva_base$discriminacion, useNA = "ifany"))

cat("\n----- Bullying físico frecuente (0=No, 1=Sí) -----\n")
print(table(nueva_base$bullying_fisico_frec, useNA = "ifany"))

cat("\n----- NSE por cuartil -----\n")
print(table(nueva_base$nse_cuartil, useNA = "ifany"))


# ------------------------------------------------------------
# 2) ESTADÍSTICAS DESCRIPTIVAS DE  VARIABLES CONTINUAS
# ------------------------------------------------------------

cat("\n----- Resumen puntaje Lengua -----\n")
print(summary(nueva_base$lpuntaje))

cat("\n----- Resumen puntaje Matemática -----\n")
print(summary(nueva_base$mpuntaje))

cat("\n----- Resumen proxy NSE -----\n")
print(summary(nueva_base$nse_proxy))


# ------------------------------------------------------------
# 3) HISTOGRAMAS (copiados en el informe)
# ------------------------------------------------------------

hist(nueva_base$lpuntaje,
     main = "Distribución del puntaje de Lengua",
     xlab = "Puntaje", col = "steelblue", border = "white")

hist(nueva_base$mpuntaje,
     main = "Distribución del puntaje de Matemática",
     xlab = "Puntaje", col = "coral", border = "white")

hist(nueva_base$nse_proxy,
     main = "Distribución del proxy de NSE",
     xlab = "NSE (estandarizado)", col = "seagreen", border = "white")

png(here("outputs", "hist_lengua.png"), width = 800, height = 600)
hist(nueva_base$lpuntaje, main = "Distribución del puntaje de Lengua", xlab = "Puntaje", col = "steelblue", border = "white")
dev.off()

png(here("outputs", "hist_matematica.png"), width = 800, height = 600)
hist(nueva_base$mpuntaje, main = "Distribución del puntaje de Matemática", xlab = "Puntaje", col = "coral", border = "white")
dev.off()

png(here("outputs", "hist_nse.png"), width = 800, height = 600)
hist(nueva_base$nse_proxy, main = "Distribución del proxy de NSE", xlab = "NSE (estandarizado)", col = "seagreen", border = "white")
dev.off()
# ------------------------------------------------------------
# 4) CORRELACIONES
# ------------------------------------------------------------


cat("\n----- Correlaciones: NSE proxy, puntajes, libros y redes -----\n")
cor_vars <- nueva_base %>%
  select(nse_proxy, lpuntaje, mpuntaje, libros, redes) %>%
  cor(use = "complete.obs")
print(round(cor_vars, 2))
# ------------------------------------------------------------
# 5) TABLA: DESEMPEÑO SEGÚN NSE  (P1)
# ------------------------------------------------------------

cat("\n----- Puntaje promedio de Lengua y Matemática por cuartil NSE -----\n")
tab_nse <- nueva_base %>%
  filter(!is.na(nse_cuartil)) %>%
  group_by(nse_cuartil) %>%
  summarise(
    lengua = round(mean(lpuntaje, na.rm = TRUE), 1),
    mat    = round(mean(mpuntaje, na.rm = TRUE), 1),
    n      = n()
  )
print(tab_nse)


# ------------------------------------------------------------
# 6) TABLA: DESEMPEÑO EN LENGUA SEGÚN LIBROS  (P2)
# ------------------------------------------------------------

cat("\n----- Puntaje promedio de Lengua según libros en el hogar -----\n")
tab_libros <- nueva_base %>%
  filter(!is.na(libros)) %>%
  mutate(libros = factor(libros,
                         labels = c("Sin libros", "1-5", "6-20", "21-50", "51-100", "+100"))) %>%
  group_by(libros) %>%
  summarise(
    lengua = round(mean(lpuntaje, na.rm = TRUE), 1),
    n      = n()
  )
print(tab_libros)


# ------------------------------------------------------------
# 7) TABLA: DESEMPEÑO SEGÚN REDES SOCIALES  (P3)
# ------------------------------------------------------------

cat("\n----- Puntaje promedio de Lengua y Matemática según uso de redes -----\n")
tab_redes <- nueva_base %>%
  filter(!is.na(redes)) %>%
  mutate(redes = ifelse(redes == 1, "Usa redes", "No usa redes")) %>%
  group_by(redes) %>%
  summarise(
    lengua = round(mean(lpuntaje, na.rm = TRUE), 1),
    mat    = round(mean(mpuntaje, na.rm = TRUE), 1),
    n      = n()
  )
print(tab_redes)


# ------------------------------------------------------------
# 8) TABLA: BULLYING Y DISCRIMINACIÓN SEGÚN NSE  (P4)
# ------------------------------------------------------------

cat("\n----- Discriminación y bullying según cuartil NSE -----\n")
tab_bullying <- nueva_base %>%
  filter(!is.na(nse_cuartil)) %>%
  group_by(nse_cuartil) %>%
  summarise(
    pct_discriminacion  = round(mean(discriminacion, na.rm = TRUE) * 100, 1),
    pct_bullying_fisico = round(mean(bullying_fisico_frec, na.rm = TRUE) * 100, 1),
    n                   = n()
  )
print(tab_bullying)

# ------------------------------------------------------------
# SECCION 5: GRÁFICOS (copiados en el informe)
# ------------------------------------------------------------

# P1: Boxplot puntaje Lengua por NSE
graf_nse_l <- nueva_base %>%
  filter(!is.na(nse_cuartil), !is.na(lpuntaje)) %>%
  ggplot(aes(x = nse_cuartil, y = lpuntaje, fill = nse_cuartil)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Puntaje de Lengua según nivel socioeconómico",
       x = "", y = "Puntaje Lengua") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
graf_nse_l
ggsave(here("outputs", "boxplot_lengua_nse.png"), plot = graf_nse_l, width = 8, height = 6)

# P1: Boxplot puntaje Matemática por NSE
graf_nse_m <- nueva_base %>%
  filter(!is.na(nse_cuartil), !is.na(mpuntaje)) %>%
  ggplot(aes(x = nse_cuartil, y = mpuntaje, fill = nse_cuartil)) +
  geom_boxplot(alpha = 0.7) +
  labs(title = "Puntaje de Matemática según nivel socioeconómico",
       x = "", y = "Puntaje Matemática") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
graf_nse_m
ggsave(here("outputs", "boxplot_matematica_nse.png"), plot = graf_nse_m, width = 8, height = 6)

# P2: Barras % nivel avanzado en Lengua por cantidad de libros
graf_libros <- nueva_base %>%
  filter(!is.na(libros)) %>%
  mutate(libros_label = factor(libros,
                               labels = c("Sin libros", "1-5", "6-20", "21-50", "51-100", "+100"))) %>%
  group_by(libros_label) %>%
  summarise(pct_avanzado = mean(ldesemp_avanzado, na.rm = TRUE) * 100) %>%
  ggplot(aes(x = libros_label, y = pct_avanzado, fill = libros_label)) +
  geom_col(alpha = 0.85) +
  labs(title = "% nivel avanzado en Lengua según libros en el hogar",
       x = "Libros en el hogar", y = "% Nivel Avanzado") +
  theme_minimal() +
  theme(legend.position = "none",
        plot.title = element_text(face = "bold", hjust = 0.5))
graf_libros
ggsave(here("outputs", "barras_libros_desempeño.png"), plot = graf_libros, width = 10, height = 6)

# P4: Barras % discriminación y bullying por NSE
graf_bullying <- tab_bullying %>%
  tidyr::pivot_longer(
    cols = c(pct_discriminacion, pct_bullying_fisico),
    names_to = "tipo", values_to = "porcentaje") %>%
  mutate(tipo = recode(tipo,
                       pct_discriminacion  = "Discriminación general",
                       pct_bullying_fisico = "Bullying físico/verbal")) %>%
  ggplot(aes(x = nse_cuartil, y = porcentaje, fill = tipo)) +
  geom_col(position = "dodge", alpha = 0.85) +
  labs(title = "Discriminación y bullying según nivel socioeconómico",
       x = "", y = "% de estudiantes afectados", fill = "") +
  theme_minimal() +
  theme(legend.position = "bottom",
        plot.title = element_text(face = "bold", hjust = 0.5))
graf_bullying
ggsave(here("outputs", "graf_bullying.png"), plot = graf_bullying, width = 10, height = 6)

# -----------------------------------------------------------------------------

# SECCIÓN 6: MODELOS DE REGRESIÓN
# =============================================================================

library(stargazer)

# -----------------------------------------------------------------------------
# LENGUA: modelos acumulativos OLS y Logit
# Modelo 1: solo NSE
# Modelo 2: NSE + libros
# Modelo 3: NSE + libros + redes
# -----------------------------------------------------------------------------

# Logit — variable dependiente: alcanzó nivel avanzado en Lengua (0/1)
logit_l1 <- glm(ldesemp_avanzado ~ nse_proxy + sexo + ed_madre + ed_padre,
                data = nueva_base, family = binomial("logit"))

logit_l2 <- glm(ldesemp_avanzado ~ nse_proxy + libros + sexo + ed_madre + ed_padre,
                data = nueva_base, family = binomial("logit"))

logit_l3 <- glm(ldesemp_avanzado ~ nse_proxy + libros + redes + sexo + ed_madre + ed_padre,
                data = nueva_base, family = binomial("logit"))

stargazer(logit_l1, logit_l2, logit_l3,
          type = "text",
          title = "Logit — Nivel avanzado en Lengua",
          column.labels = c("Solo NSE", "NSE + Libros", "NSE + Libros + Redes"),
          covariate.labels = c("NSE proxy", "Libros en el hogar", "Usa redes sociales",
                               "Sexo", "Ed. madre", "Ed. padre"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = TRUE)


# -----------------------------------------------------------------------------
# MATEMÁTICA: misma estructura
# -----------------------------------------------------------------------------

ols_m1 <- lm(mpuntaje ~ nse_proxy + sexo + ed_madre + ed_padre,
             data = nueva_base)

ols_m2 <- lm(mpuntaje ~ nse_proxy + libros + sexo + ed_madre + ed_padre,
             data = nueva_base)

ols_m3 <- lm(mpuntaje ~ nse_proxy + libros + redes + sexo + ed_madre + ed_padre,
             data = nueva_base)

stargazer(ols_m1, ols_m2, ols_m3,
          type = "text",
          title = "OLS — Puntaje de Matemática",
          column.labels = c("Solo NSE", "NSE + Libros", "NSE + Libros + Redes"),
          covariate.labels = c("NSE proxy", "Libros en el hogar", "Usa redes sociales",
                               "Sexo", "Ed. madre", "Ed. padre"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = TRUE)


logit_m1 <- glm(mdesemp_avanzado ~ nse_proxy + sexo + ed_madre + ed_padre,
                data = nueva_base, family = binomial("logit"))

logit_m2 <- glm(mdesemp_avanzado ~ nse_proxy + libros + sexo + ed_madre + ed_padre,
                data = nueva_base, family = binomial("logit"))

logit_m3 <- glm(mdesemp_avanzado ~ nse_proxy + libros + redes + sexo + ed_madre + ed_padre,
                data = nueva_base, family = binomial("logit"))

stargazer(logit_m1, logit_m2, logit_m3,
          type = "text",
          title = "Logit — Nivel avanzado en Matemática",
          column.labels = c("Solo NSE", "NSE + Libros", "NSE + Libros + Redes"),
          covariate.labels = c("NSE proxy", "Libros en el hogar", "Usa redes sociales",
                               "Sexo", "Ed. madre", "Ed. padre"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = TRUE)


# -----------------------------------------------------------------------------
# P4: NSE y violencia escolar (solo logit, dependientes son binarias)
# -----------------------------------------------------------------------------

logit_p4_disc  <- glm(discriminacion       ~ nse_proxy + sexo + ed_madre + ed_padre,
                      data = nueva_base, family = binomial("logit"))

logit_p4_bully <- glm(bullying_fisico_frec ~ nse_proxy + sexo + ed_madre + ed_padre,
                      data = nueva_base, family = binomial("logit"))

stargazer(logit_p4_disc, logit_p4_bully,
          type = "text",
          title = "Logit — Discriminación y bullying según NSE",
          column.labels = c("Discriminación", "Bullying físico frecuente"),
          covariate.labels = c("NSE proxy", "Sexo", "Ed. madre", "Ed. padre"),
          star.cutoffs = c(0.05, 0.01, 0.001),
          no.space = TRUE)




# =============================================================================
# PREDICCIONES — Logit nivel avanzado en Lengua (Modelo 3)
# =============================================================================

# Valores de referencia: media o moda de los controles
media_nse    <- mean(nueva_base$nse_proxy, na.rm = TRUE)   # cercano a 0
media_madre  <- median(nueva_base$ed_madre, na.rm = TRUE)
media_padre  <- median(nueva_base$ed_padre, na.rm = TRUE)

# -----------------------------------------------------------------------------
# PREDICCIÓN 1: efecto del NSE sobre prob. de nivel avanzado
# (libros y redes fijos en su mediana)
# -----------------------------------------------------------------------------

pred_nse <- data.frame(
  nse_proxy = seq(
    quantile(nueva_base$nse_proxy, 0.05, na.rm = TRUE),
    quantile(nueva_base$nse_proxy, 0.95, na.rm = TRUE),
    length.out = 100
  ),
  libros    = median(nueva_base$libros, na.rm = TRUE),
  redes     = 1,   # asumimos que usa redes (moda)
  sexo      = 0,   # mujer
  ed_madre  = media_madre,
  ed_padre  = media_padre
)

pred_nse$prob <- predict(logit_l3, newdata = pred_nse, type = "response")

ggplot(pred_nse, aes(x = nse_proxy, y = prob)) +
  geom_line(linewidth = 1, color = "steelblue") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title    = "Probabilidad de nivel avanzado en Lengua según NSE",
    subtitle = "Libros y redes fijos en su mediana, sexo femenino",
    x        = "NSE proxy (estandarizado)",
    y        = "Probabilidad de nivel avanzado"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


# -----------------------------------------------------------------------------
# PREDICCIÓN 2: efecto de los libros sobre prob. de nivel avanzado
# (NSE fijo en su media)
# -----------------------------------------------------------------------------

pred_libros <- data.frame(
  libros    = 1:6,
  nse_proxy = media_nse,
  redes     = 1,
  sexo      = 0,
  ed_madre  = media_madre,
  ed_padre  = media_padre
)

pred_libros$prob <- predict(logit_l3, newdata = pred_libros, type = "response")

pred_libros$libros_label <- factor(pred_libros$libros,
                                   labels = c("Sin libros", "1-5", "6-20", "21-50", "51-100", "+100"))

ggplot(pred_libros, aes(x = libros_label, y = prob)) +
  geom_col(fill = "steelblue", alpha = 0.85) +
  geom_text(aes(label = paste0(round(prob * 100, 1), "%")),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.5)) +
  labs(
    title    = "Probabilidad de nivel avanzado en Lengua según libros en el hogar",
    subtitle = "NSE fijo en su media, sexo femenino",
    x        = "Libros en el hogar",
    y        = "Probabilidad de nivel avanzado"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


# -----------------------------------------------------------------------------
# PREDICCIÓN 3: efecto de las redes según NSE
# Compara "usa redes" vs "no usa redes" a lo largo del rango de NSE
# -----------------------------------------------------------------------------

pred_redes <- expand.grid(
  nse_proxy = seq(
    quantile(nueva_base$nse_proxy, 0.05, na.rm = TRUE),
    quantile(nueva_base$nse_proxy, 0.95, na.rm = TRUE),
    length.out = 100
  ),
  redes    = c(0, 1),
  libros   = median(nueva_base$libros, na.rm = TRUE),
  sexo     = 0,
  ed_madre = media_madre,
  ed_padre = media_padre
)

pred_redes$prob       <- predict(logit_l3, newdata = pred_redes, type = "response")
pred_redes$redes_label <- ifelse(pred_redes$redes == 1, "Usa redes", "No usa redes")

ggplot(pred_redes, aes(x = nse_proxy, y = prob, color = redes_label)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_color_manual(values = c("Usa redes" = "steelblue", "No usa redes" = "coral")) +
  labs(
    title    = "Probabilidad de nivel avanzado en Lengua: redes vs. no redes",
    subtitle = "Libros fijos en su mediana, sexo femenino",
    x        = "NSE proxy (estandarizado)",
    y        = "Probabilidad de nivel avanzado",
    color    = ""
  ) +
  theme_minimal() +
  theme(plot.title    = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom")



# =============================================================================
# PREDICCIONES — Logit nivel avanzado en Matemática (Modelo 3)
# =============================================================================

media_nse   <- mean(nueva_base$nse_proxy, na.rm = TRUE)
media_madre <- median(nueva_base$ed_madre, na.rm = TRUE)
media_padre <- median(nueva_base$ed_padre, na.rm = TRUE)

# -----------------------------------------------------------------------------
# PREDICCIÓN 1: efecto del NSE sobre prob. de nivel avanzado en Matemática
# -----------------------------------------------------------------------------

pred_nse_m <- data.frame(
  nse_proxy = seq(
    quantile(nueva_base$nse_proxy, 0.05, na.rm = TRUE),
    quantile(nueva_base$nse_proxy, 0.95, na.rm = TRUE),
    length.out = 100
  ),
  libros   = median(nueva_base$libros, na.rm = TRUE),
  redes    = 1,
  sexo     = 1,   # varón porque en Mat. los varones tienen mejor desempeño
  ed_madre = media_madre,
  ed_padre = media_padre
)

pred_nse_m$prob <- predict(logit_m3, newdata = pred_nse_m, type = "response")

ggplot(pred_nse_m, aes(x = nse_proxy, y = prob)) +
  geom_line(linewidth = 1, color = "coral") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  labs(
    title    = "Probabilidad de nivel avanzado en Matemática según NSE",
    subtitle = "Libros y redes fijos en su mediana, sexo masculino",
    x        = "NSE proxy (estandarizado)",
    y        = "Probabilidad de nivel avanzado"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


# -----------------------------------------------------------------------------
# PREDICCIÓN 2: efecto de los libros en Matemática (controlando NSE)
# -----------------------------------------------------------------------------

pred_libros_m <- data.frame(
  libros    = 1:6,
  nse_proxy = media_nse,
  redes     = 1,
  sexo      = 1,
  ed_madre  = media_madre,
  ed_padre  = media_padre
)

pred_libros_m$prob <- predict(logit_m3, newdata = pred_libros_m, type = "response")

pred_libros_m$libros_label <- factor(pred_libros_m$libros,
                                     labels = c("Sin libros", "1-5", "6-20", "21-50", "51-100", "+100"))

ggplot(pred_libros_m, aes(x = libros_label, y = prob)) +
  geom_col(fill = "coral", alpha = 0.85) +
  geom_text(aes(label = paste0(round(prob * 100, 1), "%")),
            vjust = -0.5, size = 3.5) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 0.5)) +
  labs(
    title    = "Probabilidad de nivel avanzado en Matemática según libros en el hogar",
    subtitle = "NSE fijo en su media, sexo masculino",
    x        = "Libros en el hogar",
    y        = "Probabilidad de nivel avanzado"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold", hjust = 0.5))


# -----------------------------------------------------------------------------
# PREDICCIÓN 3: redes vs no redes en Matemática a lo largo del NSE
# (spoiler: las líneas van a estar muy juntas porque redes no es significativo)
# -----------------------------------------------------------------------------

pred_redes_m <- expand.grid(
  nse_proxy = seq(
    quantile(nueva_base$nse_proxy, 0.05, na.rm = TRUE),
    quantile(nueva_base$nse_proxy, 0.95, na.rm = TRUE),
    length.out = 100
  ),
  redes    = c(0, 1),
  libros   = median(nueva_base$libros, na.rm = TRUE),
  sexo     = 1,
  ed_madre = media_madre,
  ed_padre = media_padre
)

pred_redes_m$prob        <- predict(logit_m3, newdata = pred_redes_m, type = "response")
pred_redes_m$redes_label <- ifelse(pred_redes_m$redes == 1, "Usa redes", "No usa redes")

ggplot(pred_redes_m, aes(x = nse_proxy, y = prob, color = redes_label)) +
  geom_line(linewidth = 1) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0, 1)) +
  scale_color_manual(values = c("Usa redes" = "coral", "No usa redes" = "steelblue")) +
  labs(
    title    = "Probabilidad de nivel avanzado en Matemática: redes vs. no redes",
    subtitle = "Libros fijos en su mediana, sexo masculino",
    x        = "NSE proxy (estandarizado)",
    y        = "Probabilidad de nivel avanzado",
    color    = ""
  ) +
  theme_minimal() +
  theme(plot.title      = element_text(face = "bold", hjust = 0.5),
        legend.position = "bottom")
