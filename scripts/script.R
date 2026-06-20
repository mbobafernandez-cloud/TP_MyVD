# =============================================================================
# TP ENGLOBADOR- Yazmin Dellacasa, Martina Boba Fernandez, Naiara Bustos Diaz. 

# Preguntas de investigación:
#   1. ¿Qué relación hay entre capacidad económica y desempeño académico?
#   2. Controlando por cap. económica: ¿tener libros en casa mejora el desempeño en Lengua?
#   3. Controlando por cap. económica: ¿usar redes sociales afecta el desempeño académico?
#   4. ¿La diferencia entre clases sociales afecta en el bullying/discriminación?
# =============================================================================

#cargamos librerias: 
install.packages("tidyr")

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
    # --- Sociodemográficas ---
    edad         = ap01,
    sexo         = ap03,
    ed_madre     = nivel_ed_madre, 
    ed_padre     = nivel_ed_padre, 
    region       = region,
    
    # --- Bienes de distinción y confort (Para Proxy NSE) ---
    lavarropas   = ap09c,
    tv_cable     = ap09e,
    auto         = ap09g,
    streaming    = ap09h,
    compu        = ap09i,
    consola      = ap09j,
    tablet       = ap09k,
    
    # --- Variables de Hacinamiento ---
    habitaciones = ap14,
    personas     = ap15,
    
    # --- Variables Clave del Estudio ---
    libros           = ap10,
    redes            = ap05a,
    discriminacion   = apa37,  
    disc_nse         = apa38j, 
    bullying_fisico  = apb38a, 
    bullying_virtual = apb38b, 
    robo             = apb38c, 
    
    # --- Variables de Desempeño Académico ---
    ldesemp  = ldesemp, 
    mdesemp  = mdesemp,
    lpuntaje = lpuntaje,
    mpuntaje = mpuntaje
  )

# =============================================================================
# SECCIÓN 2: TRANSFORMACIÓN Y LIMPIEZA DE DATOS (Manejo de valores negativos)
# =============================================================================

nueva_base1 <- nueva_base %>%
  mutate(
    # --- Sociodemográficas ---
    # Limpiamos los -9 (Blanco), -8 (No disp), -6 (Multimarca)
    sexo     = ifelse(sexo == 1, 1, ifelse(sexo == 2, 0, NA)),
    edad     = ifelse(as.numeric(edad) < 0, NA, as.numeric(edad)),
    ed_madre = ifelse(ed_madre < 0, NA, ed_madre),
    ed_padre = ifelse(ed_padre < 0, NA, ed_padre),
    
    # --- Componentes del proxy NSE (1 = Tiene, 0 = No tiene) ---
    lavarropas = case_when(lavarropas == 1 ~ 1, lavarropas == 2 ~ 0, TRUE ~ NA_real_),
    tv_cable   = case_when(tv_cable == 1 ~ 1, tv_cable == 2 ~ 0, TRUE ~ NA_real_),
    auto       = case_when(auto == 1 ~ 1, auto == 2 ~ 0, TRUE ~ NA_real_),
    streaming  = case_when(streaming == 1 ~ 1, streaming == 2 ~ 0, TRUE ~ NA_real_),
    compu      = case_when(compu == 1 ~ 1, compu == 2 ~ 0, TRUE ~ NA_real_),
    consola    = case_when(consola == 1 ~ 1, consola == 2 ~ 0, TRUE ~ NA_real_),
    tablet     = case_when(tablet == 1 ~ 1, tablet == 2 ~ 0, TRUE ~ NA_real_),
    
    habitaciones = ifelse(habitaciones < 0, NA, habitaciones),
    personas     = ifelse(personas < 0, NA, personas),
    
    # --- Indicador de Hacinamiento (Castigo para el índice) ---
    # Más de 2 personas por cuarto = 1 (Hacinamiento), de lo contrario 0
    hacinamiento = ifelse((personas / habitaciones) > 2, 1, 0),
    
    # --- Variables Clave ---
    libros = ifelse(libros < 0, NA, libros),
    redes  = case_when(redes == 1 ~ 1, redes == 2 ~ 0, TRUE ~ NA_real_),
    
    discriminacion = case_when(discriminacion == 1 ~ 1, discriminacion == 2 ~ 0, TRUE ~ NA_real_),
    disc_nse       = case_when(disc_nse == 1 ~ 1, disc_nse == 0 ~ 0, TRUE ~ NA_real_),
    
    bullying_fisico_frec  = case_when(bullying_fisico  %in% c(1, 2) ~ 1, bullying_fisico  %in% c(3, 4) ~ 0, TRUE ~ NA_real_),
    bullying_virtual_frec = case_when(bullying_virtual %in% c(1, 2) ~ 1, bullying_virtual %in% c(3, 4) ~ 0, TRUE ~ NA_real_),
    robo_frec             = case_when(robo %in% c(1, 2) ~ 1, robo %in% c(3, 4) ~ 0, TRUE ~ NA_real_),
    
    # --- Desempeño Avanzado (Dummies para anexos) ---
    ldesemp_avanzado = ifelse(ldesemp == 4, 1, ifelse(ldesemp > 0, 0, NA)),
    mdesemp_avanzado = ifelse(mdesemp == 4, 1, ifelse(mdesemp > 0, 0, NA))
  )

# =============================================================================
# SECCIÓN 3: CONSTRUCCIÓN DEL PROXY NSE 
# =============================================================================

nueva_base2 <- nueva_base1 %>%
  mutate(
    respuestas_bienes = rowSums(!is.na(cbind(compu, streaming, auto, consola, tablet))),
    suma_bienes = rowSums(cbind(compu, streaming, auto, consola, tablet), na.rm = TRUE),
    
    hacinamiento_limpio = ifelse(is.na(hacinamiento), 0, hacinamiento),
    
    # 2. Nueva fórmula: Bienes de lujo relativo MENOS (Hacinamiento * 2)
    # Al multiplicar el hacinamiento por 2, castigamos más fuerte a los sectores vulnerables
    # y compensamos la sobre-declaración de bienes.
    puntaje_bienes = ifelse(respuestas_bienes >= 3, suma_bienes - (hacinamiento_limpio * 2), NA_real_),
    
    nse_proxy_ajustado = as.numeric(scale(puntaje_bienes)),
    
    nse_cuartil = ntile(nse_proxy_ajustado, 4),
    nse_cuartil = factor(nse_cuartil,
                         labels = c("NSE Bajo", "NSE Medio-Bajo",
                                    "NSE Medio-Alto", "NSE Alto"))
  )

# =============================================================================
# VALIDACIÓN DEL PROXY NSE 
# =============================================================================

# --- A) Datos Duros: Tabla de Validación Interna ---
#Muestra cómo los bienes se  distribuyen lógicamente a través de las clases sociales estimadas.

tabla_validacion_nse <- nueva_base2 %>%
  filter(!is.na(nse_cuartil)) %>%
  group_by(nse_cuartil) %>%
  summarise(
    `% Compu`      = mean(compu, na.rm = TRUE) * 100,
    `% Streaming`  = mean(streaming, na.rm = TRUE) * 100,
    `% Consola`    = mean(consola, na.rm = TRUE) * 100,
    `% Auto`       = mean(auto, na.rm = TRUE) * 100,
    `% Hacinados`  = mean(hacinamiento, na.rm = TRUE) * 100,
    n_estudiantes  = n()
  ) %>%
  mutate_if(is.numeric, round, 1)

cat("\n--- Validación Interna del Índice por Cuartil ---\n")
print(tabla_validacion_nse)

# --- B) Gráfico: Distribución de Varianza ---
# Muestra si logramos capturar la desigualdad sin cuellos de botella

graf_dist_nse <- ggplot(nueva_base2 %>% filter(!is.na(nse_proxy_ajustado)), 
                        aes(x = nse_proxy_ajustado)) +
  geom_histogram(aes(y = after_stat(density)), bins = 20, 
                 fill = "steelblue", color = "white", alpha = 0.7) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red", linewidth = 0.8) +
  theme_minimal() +
  labs(
    title = "Distribución del Índice Socioeconómico (Proxy Ajustado)",
    subtitle = "Construido mediante bienes de distinción y control de hacinamiento",
    x = "Índice NSE (Estandarizado)",
    y = "Densidad",
    caption = "Nota: La línea roja punteada indica la media."
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold")
  ) +
  theme(
    panel.grid = element_blank()
  )

print(graf_dist_nse)
ggsave(here("outputs", "distribucion_nse_ajustado.png"), plot = graf_dist_nse, width = 8, height = 6)



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
print(table(nueva_base2$ldesemp, useNA = "ifany"))

cat("\n----- Desempeño en Matemática -----\n")
print(table(nueva_base2$mdesemp, useNA = "ifany"))

cat("\n----- Libros en el hogar (1=Ninguno ... 6=Más de 100) -----\n")
print(table(nueva_base2$libros, useNA = "ifany"))

cat("\n----- Usa redes sociales (0=No, 1=Sí) -----\n")
print(table(nueva_base2$redes, useNA = "ifany"))

cat("\n----- Discriminación en la escuela (0=No, 1=Sí) -----\n")
print(table(nueva_base2$discriminacion, useNA = "ifany"))

cat("\n----- Bullying físico frecuente (0=No, 1=Sí) -----\n")
print(table(nueva_base2$bullying_fisico_frec, useNA = "ifany"))

cat("\n----- NSE por cuartil -----\n")
print(table(nueva_base2$nse_cuartil, useNA = "ifany"))


# ------------------------------------------------------------
# 2) ESTADÍSTICAS DESCRIPTIVAS DE  VARIABLES CONTINUAS
# ------------------------------------------------------------

cat("\n----- Resumen puntaje Lengua -----\n")
print(summary(nueva_base2$lpuntaje))

cat("\n----- Resumen puntaje Matemática -----\n")
print(summary(nueva_base2$mpuntaje))

cat("\n----- Resumen proxy NSE -----\n")
print(summary(nueva_base2$nse_proxy))


# ------------------------------------------------------------
# 3) HISTOGRAMAS 
# ------------------------------------------------------------

hist(nueva_base2$lpuntaje,
     main = "Distribución del puntaje de Lengua",
     xlab = "Puntaje", col = "steelblue", border = "white")

hist(nueva_base2$mpuntaje,
     main = "Distribución del puntaje de Matemática",
     xlab = "Puntaje", col = "coral", border = "white")

# ------------------------------------------------------------
# 4) CORRELACIONES
# ------------------------------------------------------------

cat("\n----- Correlaciones: NSE proxy, puntajes, libros y redes -----\n")
cor_vars <- nueva_base2 %>%
  select(nse_proxy_ajustado, lpuntaje, mpuntaje, libros, redes) %>%
  cor(use = "complete.obs")
print(round(cor_vars, 2))
# ------------------------------------------------------------
# 5) TABLA: DESEMPEÑO SEGÚN NSE  (P1)
# ------------------------------------------------------------

cat("\n----- Puntaje promedio de Lengua y Matemática por cuartil NSE -----\n")
tab_nse <- nueva_base2 %>%
  filter(!is.na(nse_cuartil)) %>%
  group_by(nse_cuartil) %>%
  summarise(
    lengua = round(mean(lpuntaje, na.rm = TRUE), 1),
    mat    = round(mean(mpuntaje, na.rm = TRUE), 1),
    n      = n()
  )
print(tab_nse)


# Gráfico
tab_nse_long <- tab_nse %>%
  pivot_longer(
    cols = c(lengua, mat),
    names_to = "materia",
    values_to = "puntaje"
  )
ggplot(tab_nse_long,
       aes(x = nse_cuartil,
           y = puntaje,
           group = materia,
           color = materia)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  labs(
    title = "Rendimiento promedio según NSE",
    x = "Cuartil NSE",
    y = "Puntaje promedio",
    color = "Materia"
  ) +
  theme_minimal()+
  theme(
    panel.grid = element_blank()
  )

# ------------------------------------------------------------
# 6) TABLA: DESEMPEÑO EN LENGUA SEGÚN LIBROS  (P2)
# ------------------------------------------------------------

cat("\n----- Puntaje promedio de Lengua según libros en el hogar -----\n")
tab_libros <- nueva_base2 %>%
  filter(!is.na(libros)) %>%
  mutate(libros = factor(libros,
                         labels = c("Sin libros", "1-5", "6-20", "21-50", "51-100", "+100"))) %>%
  group_by(libros) %>%
  summarise(
    lengua = round(mean(lpuntaje, na.rm = TRUE), 1),
    n      = n()
  )
print(tab_libros)

# Gráfico
ggplot(tab_libros,
       aes(x = libros,
           y = lengua,
           group = 1)) +
  geom_line(linewidth = 1.2,  color = "purple") +
  geom_point(size = 3, color = "purple") +
  labs(
    title = "Puntaje promedio de Lengua según cantidad de libros en el hogar",
    x = "Cantidad de libros",
    y = "Puntaje promedio de Lengua"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line()
  )

# ------------------------------------------------------------
# 7) TABLA: DESEMPEÑO SEGÚN REDES SOCIALES  (P3)
# ------------------------------------------------------------

cat("\n----- Puntaje promedio de Lengua y Matemática según uso de redes -----\n")
tab_redes <- nueva_base2 %>%
  filter(!is.na(redes)) %>%
  mutate(redes = ifelse(redes == 1, "Usa redes", "No usa redes")) %>%
  group_by(redes) %>%
  summarise(
    lengua = round(mean(lpuntaje, na.rm = TRUE), 1),
    mat    = round(mean(mpuntaje, na.rm = TRUE), 1),
    n      = n()
  )
print(tab_redes)

# Gráfico
tab_redes_long <- tab_redes %>%
  pivot_longer(
    cols = c(lengua, mat),
    names_to = "materia",
    values_to = "puntaje"
  )
ggplot(tab_redes_long,
       aes(x = redes,
           y = puntaje,
           fill = materia)) +
  geom_col(position = "dodge") +
  labs(
    title = "Puntaje promedio según uso de redes sociales",
    x = "",
    y = "Puntaje promedio",
    fill = "Materia"
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line()
  )

# ------------------------------------------------------------
# 8) TABLA: BULLYING Y DISCRIMINACIÓN SEGÚN NSE  (P4)
# ------------------------------------------------------------

cat("\n----- Discriminación y bullying según cuartil NSE -----\n")
tab_bullying <- nueva_base2 %>%
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
graf_nse_l <- nueva_base2 %>%
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
graf_nse_m <- nueva_base2 %>%
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
graf_libros <- nueva_base2 %>%
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
graf_bullying <- nueva_base2 %>%
  filter(!is.na(nse_cuartil)) %>%
  group_by(nse_cuartil) %>%
  summarise(
    `Discriminación general` = mean(discriminacion, na.rm = TRUE) * 100,
    `Bullying físico/verbal` = mean(bullying_fisico_frec, na.rm = TRUE) * 100
  ) %>%
  pivot_longer(cols = -nse_cuartil, names_to = "tipo", values_to = "porcentaje") %>%
  ggplot(aes(x = nse_cuartil, y = porcentaje, fill = tipo)) +
  geom_col(position = "dodge", alpha = 0.85, width = 0.6) +
  scale_fill_manual(values = c("Discriminación general" = "#C2185B", "Bullying físico/verbal" = "forestgreen")) +
  geom_text(aes(label = sprintf("%.1f%%", porcentaje)), 
            position = position_dodge(width = 0.6), vjust = -0.5, size = 3.5) +
  labs(title = "Incidencia de Violencia Escolar según Estrato Socioeconómico",
       x = "Cuartil Socioeconómico", y = "% de estudiantes afectados", fill = "Tipo de violencia") +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom") +
  theme(
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.ticks = element_line()
  )

  
print(graf_bullying)

# -----------------------------------------------------------------------------
# =============================================================================
# SECCIÓN 6: MODELOS DE REGRESIÓN Y RESULTADOS
# =============================================================================
install.packages("stargazer")
library(stargazer)

# -----------------------------------------------------------------------------
# PREGUNTA 1, 2 Y 3 (LENGUA): Modelos OLS Acumulativos
# Usamos OLS (lpuntaje) para no perder la varianza de la variable continua.
# -----------------------------------------------------------------------------

ols_l1 <- lm(lpuntaje ~ nse_proxy_ajustado + sexo + ed_madre + ed_padre, data = nueva_base2)
ols_l2 <- lm(lpuntaje ~ nse_proxy_ajustado + libros + sexo + ed_madre + ed_padre, data = nueva_base2)
ols_l3 <- lm(lpuntaje ~ nse_proxy_ajustado + libros + redes + sexo + ed_madre + ed_padre, data = nueva_base2)

stargazer(ols_l1, ols_l2, ols_l3,
          type = "text", out = here("outputs", "tabla_lengua_ols.html"),
          title = "Modelos OLS — Desempeño en Lengua",
          column.labels = c("Solo NSE", "NSE + Libros", "NSE + Libros + Redes"),
          covariate.labels = c("Índice NSE", "Libros en el hogar", "Usa redes sociales",
                               "Sexo (M=1)", "Ed. Madre", "Ed. Padre"),
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE)

# -----------------------------------------------------------------------------
# PREGUNTA 1, 2 Y 3 (MATEMÁTICA)
# -----------------------------------------------------------------------------

ols_m1 <- lm(mpuntaje ~ nse_proxy_ajustado + sexo + ed_madre + ed_padre, data = nueva_base2)
ols_m2 <- lm(mpuntaje ~ nse_proxy_ajustado + libros + sexo + ed_madre + ed_padre, data = nueva_base2)
ols_m3 <- lm(mpuntaje ~ nse_proxy_ajustado + libros + redes + sexo + ed_madre + ed_padre, data = nueva_base2)

stargazer(ols_m1, ols_m2, ols_m3,
          type = "text", out = here("outputs", "tabla_matematica_ols.html"),
          title = "Modelos OLS — Desempeño en Matemática",
          column.labels = c("Solo NSE", "NSE + Libros", "NSE + Libros + Redes"),
          covariate.labels = c("Índice NSE", "Libros en el hogar", "Usa redes sociales",
                               "Sexo (M=1)", "Ed. Madre", "Ed. Padre"),
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE)

# -----------------------------------------------------------------------------
# PREGUNTA 4: NSE y Violencia Escolar (Modelos Logit Binarios)
# -----------------------------------------------------------------------------

logit_p4_disc  <- glm(discriminacion ~ nse_proxy_ajustado + sexo + ed_madre + ed_padre,
                      data = nueva_base2, family = binomial("logit"))

logit_p4_bully <- glm(bullying_fisico_frec ~ nse_proxy_ajustado + sexo + ed_madre + ed_padre,
                      data = nueva_base2, family = binomial("logit"))

stargazer(logit_p4_disc, logit_p4_bully,
          type = "text", out = here("outputs", "tabla_violencia.html"),
          title = "Logit — Discriminación y Bullying según NSE",
          column.labels = c("Discriminación General", "Bullying Físico/Verbal"),
          covariate.labels = c("Índice NSE", "Sexo (M=1)", "Ed. Madre", "Ed. Padre"),
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE)

# -----------------------------------------------------------------------------
# ANEXO PARA EL PROFESOR: Modelo Desagregado 
# -----------------------------------------------------------------------------
ols_l_anexo <- lm(lpuntaje ~ compu + streaming + consola + auto + tablet + hacinamiento + 
                    libros + redes + sexo + ed_madre + ed_padre, data = nueva_base2)

ols_l_anexom <- lm(mpuntaje ~ compu + streaming + consola + auto + tablet + hacinamiento + 
                    libros + redes + sexo + ed_madre + ed_padre, data = nueva_base2)
stargazer(ols_l_anexo, ols_l_anexom, type = "text", out = here("outputs", "tabla_anexo_profesor.html"),
          title = "Anexo: Modelo OLS con bienes desagregados",
          star.cutoffs = c(0.05, 0.01, 0.001), no.space = TRUE)


# =============================================================================
# SECCIÓN 7: PREDICCIONES Y EFECTOS MARGINALES VISUALES
# =============================================================================

# Calculamos medias y medianas para fijar los controles
media_nse   <- mean(nueva_base2$nse_proxy_ajustado, na.rm = TRUE)
media_madre <- median(nueva_base2$ed_madre, na.rm = TRUE)
media_padre <- median(nueva_base2$ed_padre, na.rm = TRUE)
mediana_libros <- median(nueva_base2$libros, na.rm = TRUE)

# -----------------------------------------------------------------------------
# GRÁFICO 1 MEJORADO: Efecto del NSE con Intervalos de Confianza (95%)
# -----------------------------------------------------------------------------

# 1. Predecimos pidiendo el Error Estándar (se.fit = TRUE)
pred_lengua <- predict(ols_l3, newdata = pred_nse, se.fit = TRUE)
pred_mate   <- predict(ols_m3, newdata = pred_nse, se.fit = TRUE)

# 2. Guardamos la predicción y calculamos los límites (1.96 * error estándar para el 95% CI)
pred_nse$Lengua_fit <- pred_lengua$fit
pred_nse$Lengua_inf <- pred_lengua$fit - (1.96 * pred_lengua$se.fit)
pred_nse$Lengua_sup <- pred_lengua$fit + (1.96 * pred_lengua$se.fit)

pred_nse$Matemática_fit <- pred_mate$fit
pred_nse$Matemática_inf <- pred_mate$fit - (1.96 * pred_mate$se.fit)
pred_nse$Matemática_sup <- pred_mate$fit + (1.96 * pred_mate$se.fit)

# 3. Formato largo adaptado para incluir los intervalos
library(tidyr)
pred_nse_largo <- pred_nse %>%
  pivot_longer(
    cols = c("Lengua_fit", "Matemática_fit"),
    names_to = "Materia",
    values_to = "puntaje_esperado"
  ) %>%
  mutate(
    # Limpiamos el nombre de la materia
    Materia = gsub("_fit", "", Materia),
    # Asignamos el límite inferior y superior correcto a cada fila
    lim_inf = ifelse(Materia == "Lengua", Lengua_inf, Matemática_inf),
    lim_sup = ifelse(Materia == "Lengua", Lengua_sup, Matemática_sup)
  )

# 4. El gráfico con geom_ribbon (la sombra del intervalo de confianza)
graf_pred_nse <- ggplot(pred_nse_largo, aes(x = nse_proxy_ajustado, y = puntaje_esperado, color = Materia, fill = Materia)) +
  geom_ribbon(aes(ymin = lim_inf, ymax = lim_sup), alpha = 0.2, color = NA) + # SOMBRA DE CONFIANZA
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("Lengua" = "steelblue", "Matemática" = "coral")) +
  scale_fill_manual(values = c("Lengua" = "steelblue", "Matemática" = "coral")) +
  labs(
    title    = "Impacto de la Capacidad Económica en el Desempeño Académico",
    subtitle = "Puntaje esperado e Intervalos de Confianza (95%) controlando por controles sociodemográficos",
    x        = "Índice NSE (Proxy Estandarizado)",
    y        = "Puntaje Esperado en Aprender",
    color    = "Materia", fill = "Materia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(graf_pred_nse)
ggsave(here("outputs", "prediccion_nse_ambas_materias_CI.png"), plot = graf_pred_nse, width = 8, height = 6)
# -----------------------------------------------------------------------------
# GRÁFICO 2: Efecto de tener libros sobre Lengua (Controlando por NSE medio)
# -----------------------------------------------------------------------------

pred_libros <- data.frame(
  libros             = 1:6,
  nse_proxy_ajustado = media_nse, # Fijamos la clase social en la media
  redes              = 1,
  sexo               = 0,
  ed_madre           = media_madre,
  ed_padre           = media_padre
)

pred_libros$puntaje_esperado <- predict(ols_l3, newdata = pred_libros)
pred_libros$libros_label <- factor(pred_libros$libros, labels = c("0", "1-5", "6-20", "21-50", "51-100", "+100"))

graf_pred_libros <- ggplot(pred_libros, aes(x = libros_label, y = puntaje_esperado)) +
  geom_col(fill = "steelblue", alpha = 0.85, width = 0.6) +
  geom_text(aes(label = round(puntaje_esperado, 1)), vjust = -0.5, size = 4) +
  coord_cartesian(ylim = c(min(pred_libros$puntaje_esperado)-10, max(pred_libros$puntaje_esperado)+10)) +
  labs(
    title    = "El valor de los libros controlando por nivel económico",
    subtitle = "Puntaje esperado para un estudiante de NSE Medio",
    x        = "Cantidad de libros en el hogar",
    y        = "Puntaje Esperado (Lengua)"
  ) +
  theme_minimal() +
  theme(plot.title = element_text(face = "bold"))
print(graf_pred_libros)
ggsave(here("outputs", "prediccion_libros_lengua.png"), plot = graf_pred_libros, width = 8, height = 6)


# -----------------------------------------------------------------------------
# GRÁFICO 3  Redes vs No Redes con Intervalos de Confianza (95%)
# -----------------------------------------------------------------------------

pred_redes <- expand.grid(
  nse_proxy_ajustado = seq(
    quantile(nueva_base2$nse_proxy_ajustado, 0.05, na.rm = TRUE),
    quantile(nueva_base2$nse_proxy_ajustado, 0.95, na.rm = TRUE),
    length.out = 100
  ),
  redes    = c(0, 1),
  libros   = mediana_libros,
  sexo     = mean(nueva_base2$sexo, na.rm = TRUE), 
  ed_madre = media_madre,
  ed_padre = media_padre
)

# 1. Predecimos pidiendo el Error Estándar
pred_l <- predict(ols_l3, newdata = pred_redes, se.fit = TRUE)
pred_m <- predict(ols_m3, newdata = pred_redes, se.fit = TRUE)

# 2. Calculamos estimaciones e intervalos de confianza
pred_redes$Lengua_fit <- pred_l$fit
pred_redes$Lengua_inf <- pred_l$fit - (1.96 * pred_l$se.fit)
pred_redes$Lengua_sup <- pred_l$fit + (1.96 * pred_l$se.fit)

pred_redes$Matemática_fit <- pred_m$fit
pred_redes$Matemática_inf <- pred_m$fit - (1.96 * pred_m$se.fit)
pred_redes$Matemática_sup <- pred_m$fit + (1.96 * pred_m$se.fit)

pred_redes$redes_label <- ifelse(pred_redes$redes == 1, "Usa Redes", "No usa Redes")

# 3. Transformamos a formato largo adaptado para intervalos
library(tidyr)
pred_redes_largo <- pred_redes %>%
  pivot_longer(
    cols = c("Lengua_fit", "Matemática_fit"),
    names_to = "Materia",
    values_to = "puntaje_esperado"
  ) %>%
  mutate(
    Materia = gsub("_fit", "", Materia),
    lim_inf = ifelse(Materia == "Lengua", Lengua_inf, Matemática_inf),
    lim_sup = ifelse(Materia == "Lengua", Lengua_sup, Matemática_sup)
  )

# 4. Armamos el gráfico con la sombra de confianza (geom_ribbon)
graf_pred_redes <- ggplot(pred_redes_largo, aes(x = nse_proxy_ajustado, y = puntaje_esperado, color = redes_label, fill = redes_label)) +
  geom_ribbon(aes(ymin = lim_inf, ymax = lim_sup), alpha = 0.2, color = NA) +
  geom_line(linewidth = 1.2) +
  scale_color_manual(values = c("Usa Redes" = "#E69F00", "No usa Redes" = "#56B4E9")) +
  scale_fill_manual(values = c("Usa Redes" = "#E69F00", "No usa Redes" = "#56B4E9")) +
  facet_wrap(~ Materia) +
  labs(
    title    = "Impacto del Uso de Redes Sociales en el Desempeño",
    subtitle = "Puntaje esperado e IC (95%) controlando por capacidad económica, libros y educación",
    x        = "Índice NSE (Proxy Estandarizado)",
    y        = "Puntaje Esperado en Aprender",
    color    = "Uso en tiempo libre", fill = "Uso en tiempo libre"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    strip.text = element_text(face = "bold", size = 12),
    panel.spacing = unit(1, "lines"),
    panel.border = element_rect(color = "grey80", fill = NA)
  )

print(graf_pred_redes)
ggsave(here("outputs", "prediccion_redes_ambas_materias_CI.png"), plot = graf_pred_redes, width = 10, height = 6)
# -----------------------------------------------------------------------------
# GRÁFICO 4: Riesgo de Violencia con Intervalos de Confianza Logit
# -----------------------------------------------------------------------------

pred_violencia <- data.frame(
  nse_proxy_ajustado = seq(
    quantile(nueva_base2$nse_proxy_ajustado, 0.05, na.rm = TRUE),
    quantile(nueva_base2$nse_proxy_ajustado, 0.95, na.rm = TRUE),
    length.out = 100
  ),
  sexo     = mean(nueva_base2$sexo, na.rm = TRUE), 
  ed_madre = media_madre,
  ed_padre = media_padre
)

# 1. Predecimos en la escala logit original (type = "link") para tener un error estándar matemático puro
pred_disc <- predict(logit_p4_disc, newdata = pred_violencia, type = "link", se.fit = TRUE)
pred_bull <- predict(logit_p4_bully, newdata = pred_violencia, type = "link", se.fit = TRUE)

# 2. Transformamos el ajuste y los intervalos a probabilidades exactas con plogis()
pred_violencia$Disc_fit <- plogis(pred_disc$fit)
pred_violencia$Disc_inf <- plogis(pred_disc$fit - (1.96 * pred_disc$se.fit))
pred_violencia$Disc_sup <- plogis(pred_disc$fit + (1.96 * pred_disc$se.fit))

pred_violencia$Bull_fit <- plogis(pred_bull$fit)
pred_violencia$Bull_inf <- plogis(pred_bull$fit - (1.96 * pred_bull$se.fit))
pred_violencia$Bull_sup <- plogis(pred_bull$fit + (1.96 * pred_bull$se.fit))

# 3. Transformamos a formato largo
library(tidyr)
pred_violencia_largo <- pred_violencia %>%
  pivot_longer(
    cols = c("Disc_fit", "Bull_fit"),
    names_to = "Tipo",
    values_to = "probabilidad"
  ) %>%
  mutate(
    Tipo = ifelse(Tipo == "Disc_fit", "Discriminación General", "Bullying Físico/Verbal"),
    lim_inf = ifelse(Tipo == "Discriminación General", Disc_inf, Bull_inf),
    lim_sup = ifelse(Tipo == "Discriminación General", Disc_sup, Bull_sup)
  )

# 4. Armamos el gráfico final
graf_pred_violencia <- ggplot(pred_violencia_largo, aes(x = nse_proxy_ajustado, y = probabilidad, color = Tipo, fill = Tipo)) +
  geom_ribbon(aes(ymin = lim_inf, ymax = lim_sup), alpha = 0.2, color = NA) + 
  geom_line(linewidth = 1.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) + 
  scale_color_manual(values = c("Discriminación General" = "#999999", "Bullying Físico/Verbal" = "#D55E00")) +
  scale_fill_manual(values = c("Discriminación General" = "#999999", "Bullying Físico/Verbal" = "#D55E00")) +
  labs(
    title    = "Riesgo de Violencia Escolar según Nivel Socioeconómico",
    subtitle = "Probabilidad esperada e IC (95%) controlando por sexo y educación familiar",
    x        = "Índice NSE (Proxy Estandarizado)",
    y        = "Probabilidad de ser afectado",
    color    = "Tipo de violencia", fill = "Tipo de violencia"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 13),
    legend.position = "bottom",
    legend.title = element_text(face = "bold")
  )

print(graf_pred_violencia)
ggsave(here("outputs", "prediccion_violencia_nse_CI.png"), plot = graf_pred_violencia, width = 8, height = 6)
