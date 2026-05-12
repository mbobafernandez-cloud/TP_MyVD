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