library(tidyverse)
set.seed(1307)

# =============================================================================
# EX 1: Análisis de datos con dataset libre (Reservas hoteleras)
# =============================================================================
# -----------------------------------------------------------------------------
# 0) Carga y limpieza de datos
# -----------------------------------------------------------------------------

ruta_datos_hoteles <- file.path("data", "hotel_bookings.csv")

# Limpiar datos

datos_hoteles <- read_csv(ruta_datos_hoteles) %>%
  filter(!is.na(arrival_date_week_number),
         !is.na(is_canceled),
         !is.na(children)) %>%
  filter(adr > 0)

# -----------------------------------------------------------------------------
# Pregunta 1: ¿Cómo se comportan las reservas por semana
#             de los 5 países con más reservas?
# -----------------------------------------------------------------------------

# 1) Top 5 países por total de reservas
top5_paises <- datos_hoteles %>%
  count(country, name = "total_reservas") %>%
  arrange(desc(total_reservas)) %>%
  head(5)

# 2) Subconjunto: solo top 5 países
datos_top5 <- datos_hoteles %>%
  semi_join(top5_paises, by = "country")

# 3) Conteo por semana y país (solo top 5)
reservas_semana_paises <- datos_top5 %>%
  filter(!is.na(arrival_date_week_number)) %>%
  group_by(arrival_date_week_number, country) %>%
  summarise(reservas = n(), .groups = "drop")

# 4) Gráfico
ggplot(reservas_semana_paises,
       aes(x = arrival_date_week_number, y = reservas, fill = country)) +
  geom_col() +
  labs(title = "Reservas por semana (Top 5 países)",
       x = "Semana de llegada",
       y = "Cantidad de reservas",
       fill = "País") +
  theme_minimal()

# 5) Tabla de las 5 semanas con más reservas (sumando los 5 países)
top5_semanas <- reservas_semana_paises %>%
  group_by(arrival_date_week_number) %>%
  summarise(reservas_totales = sum(reservas), .groups = "drop") %>%
  arrange(desc(reservas_totales)) %>%
  head(n = 5)


# -----------------------------------------------------------------------------
# Pregunta 2: ¿Cómo varía el ADR por semana para los 5 países con más reservas?
# (promedio global entre esos cinco países)
# -----------------------------------------------------------------------------

# 1) Calcular ADR promedio
adr_por_semana <- datos_top5 %>%
  group_by(arrival_date_week_number) %>%
  summarise(adr_promedio = mean(adr, na.rm = TRUE), .groups = "drop") %>%
  arrange(arrival_date_week_number)

# 2) Gráfico
ggplot(adr_por_semana,
       aes(x = arrival_date_week_number, y = adr_promedio, group = 1)) +
  geom_line() +
  geom_point(size = 1.8) +
  labs(title = "Evolución semanal del precio promedio (ADR) - Top 5 países", x = "Semana de llegada", y = "ADR promedio (USD)") +
  theme_minimal()

# -----------------------------------------------------------------------------
# Pregunta 3: ¿Cómo se comportan las cancelaciones según la composición del grupo?
# -----------------------------------------------------------------------------

# A) Distribución semanal de cancelaciones: Con/Sin niños (discreto, no densidad)

# 1) Filtrar canceladas y clasificar por presencia de niños
cancelaciones <- datos_top5 %>%
  filter(is_canceled == 1) %>%
  mutate(tiene_ninios = if_else(children > 0, "Con niños", "Sin niños"))

# 2) Calcular proporción dentro de cada grupo
cancel_semana_comp <- cancelaciones %>%
  count(arrival_date_week_number, tiene_ninios, name = "n") %>%
  group_by(tiene_ninios) %>%
  mutate(prop_grupo = n / sum(n)) %>%
  ungroup()

# 3) Gráfico
ggplot(
  cancel_semana_comp,
  aes(x = arrival_date_week_number, y = prop_grupo, fill = tiene_ninios)
) +
  geom_col() +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Distribución semanal de cancelaciones según reservas con o sin niños",
       x = "Semana del año",
       y = "Proporción dentro de cada grupo",
       fill = "Tipo de reserva") +
  theme_minimal()

#  B) Tasa semanal de cancelación (canceladas/total) por grupo

# 1) Etiquetar grupos y agregar por semana
tasa_cancelacion_semanal <- datos_top5 %>%
  mutate(tiene_ninios = if_else(children > 0, "Con niños", "Sin niños")) %>%
  group_by(arrival_date_week_number, tiene_ninios) %>%
  summarise(
    total      = n(),
    canceladas = sum(is_canceled == 1),
    .groups    = "drop"
  ) %>%
  mutate(tasa_cancelacion = canceladas / total)

# 2) Gráfico
ggplot(
  tasa_cancelacion_semanal,
  aes(x = arrival_date_week_number, y = tasa_cancelacion, color = tiene_ninios)
) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 1.6) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Tasa semanal de cancelación",
    x = "Semana del año",
    y = "Canceladas / Total",
    color = "Tipo de reserva"
  ) +
  theme_minimal()

# C) Tasa semanal de reservas

# 1) Contar reservas por semana y grupo
reservas_semanal <- datos_top5 %>%
  mutate(tiene_ninios = if_else(children > 0, "Con niños", "Sin niños")) %>%
  count(arrival_date_week_number, tiene_ninios, name = "reservas") %>%
  group_by(tiene_ninios) %>%
  mutate(prop_anual = reservas / sum(reservas)) %>%
  ungroup()

# 3) Gráfico
ggplot(
  reservas_semanal,
  aes(x = arrival_date_week_number, y = prop_anual, color = tiene_ninios)
) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 1.8) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Composición semanal de reservas (cada grupo suma 1 en el año)",
    x = "Semana del año",
    y = "Proporción del total anual del grupo",
    color = "Tipo de reserva"
  ) +
  theme_minimal()

# =============================================================================
# EX 2: Análisis econométrico con datos de gapminder
# =============================================================================
# -----------------------------------------------------------------------------
# Parte 1
# -----------------------------------------------------------------------------
# Ex 2.1 ----------------------------------------------------------------------

ruta_gapminder     <- file.path("data", "gapminder.csv")
datos_gapminder  <- readr::read_csv(ruta_gapminder)

datos_argentina  <- datos_gapminder %>% filter(country == "Argentina")


#Gráfico ingreso per cápita - año
ggplot(datos_argentina, aes(x = year, y = income_per_person)) +
  geom_line() +
  labs(title = "Evolución del ingreso per cápita en Argentina", x = "Año", y = "Ingreso por persona (USD constantes)") +
  theme_minimal()

# Ex 2.2 ----------------------------------------------------------------------

#Separar conjunto train - test

datos_argentina_train <- datos_argentina %>% filter(year <= max(year) - 10)
datos_argentina_test  <- datos_argentina %>% filter(year >  max(year) - 10)

# Modelos
modelo_grado_1  <- lm(income_per_person ~ poly(year, 1, raw = TRUE), data = datos_argentina_train)
modelo_grado_2  <- lm(income_per_person ~ poly(year, 2, raw = TRUE), data = datos_argentina_train)
modelo_grado_10 <- lm(income_per_person ~ poly(year, 10, raw = TRUE), data = datos_argentina_train)

# Grilla de predicciones

grid_base <- tibble(year = seq(
  min(datos_argentina$year, na.rm = TRUE),
  max(datos_argentina$year, na.rm = TRUE)
))

# Agregar predicciones
grid_predicciones <- grid_base %>%
  mutate(
    y_1  = predict(modelo_grado_1, newdata = grid_base),
    y_2  = predict(modelo_grado_2, newdata = grid_base),
    y_10 = predict(modelo_grado_10, newdata = grid_base)
  )

# Gráfico 3 modelos
ggplot(datos_argentina, aes(x = year, y = income_per_person)) +
  geom_line() +
  geom_line(data = grid_predicciones,
            aes(y = y_1, color = "Grado 1"),
            linewidth = 1) +
  geom_line(data = grid_predicciones,
            aes(y = y_2, color = "Grado 2"),
            linewidth = 1) +
  geom_line(data = grid_predicciones,
            aes(y = y_10, color = "Grado 10"),
            linewidth = 1) +
  coord_cartesian(ylim = c(9000, 20000)) +  # cotas eje Y para mejorar visualización
  geom_vline(xintercept = max(datos_argentina_train$year)) +
  scale_color_discrete(name = "Modelo") +
  labs(title  = "Evolución del ingreso per cápita en Argentina", x      = "Año", y      = "Ingreso por persona (USD constantes)") +
  theme_minimal()

# función RMSE
rmse <- function(obs, pred)
  sqrt(mean((obs - pred)^2))

# RMSE in-sample
rmse_train <- c(
  "Grado 1"  = rmse(
    datos_argentina_train$income_per_person,
    fitted(modelo_grado_1)
  ),
  "Grado 2"  = rmse(
    datos_argentina_train$income_per_person,
    fitted(modelo_grado_2)
  ),
  "Grado 10" = rmse(
    datos_argentina_train$income_per_person,
    fitted(modelo_grado_10)
  )
)


# RMSE out-of-sample
rmse_test <- c(
  "Grado 1"  = rmse(
    datos_argentina_test$income_per_person,
    predict(modelo_grado_1, newdata = datos_argentina_test)
  ),
  "Grado 2"  = rmse(
    datos_argentina_test$income_per_person,
    predict(modelo_grado_2, newdata = datos_argentina_test)
  ),
  "Grado 10" = rmse(
    datos_argentina_test$income_per_person,
    predict(modelo_grado_10, newdata = datos_argentina_test)
  )
)

#Tablas

rmse_train_tbl <- tibble(modelo = names(rmse_train),
                         rmse_train = as.numeric(rmse_train)) %>%
  arrange(rmse_train)

rmse_test_tbl <- tibble(modelo = names(rmse_test),
                        rmse_test = as.numeric(rmse_test)) %>%
  arrange(rmse_test)


# Ex 2.3A ----------------------------------------------------------------------

# Filtrar paises

datos_paises <- datos_gapminder %>%
  filter(country %in% c("Brazil", "Chile", "Peru", "Uruguay")) %>%
  select(country, year, income_per_person)

# Matriz de correlaciones de niveles

niveles_wide <- datos_paises %>%
  pivot_wider(names_from = country, values_from = income_per_person)

niveles_cor <- niveles_wide %>%
  select(-year) %>%
  cor()

print(niveles_cor)

# Ex 2.3B ----------------------------------------------------------------------

# Calcular tasa

crec_paises <- datos_paises %>%
  group_by(country) %>%
  arrange(year, .by_group = TRUE) %>%
  mutate(crec = (income_per_person / lag(income_per_person)) - 1) %>%
  ungroup() %>%
  select(country, year, crec)

# Matriz de correlaciones de tasa

crec_wide <- crec_paises %>%
  pivot_wider(names_from = country, values_from = crec)

crec_cor <- crec_wide %>%
  select(-year) %>%
  cor()

print (crec_cor)


# -----------------------------------------------------------------------------
# Parte 2
# -----------------------------------------------------------------------------
# Ex 2.5 ----------------------------------------------------------------------

datos_2010 <- datos_gapminder %>% filter(year == 2010)

# Limpiar datos
datos_2010_filtrados <- datos_2010 %>%
  filter(!is.na(life_expectancy)) %>%
  filter(life_expectancy_female != "-") %>%
  # La esperanza de Haití total no da entre la masculina y la femenina
  filter(!(country == "Haiti" & year == 2010)) %>%
  mutate(
    life_expectancy_female = as.numeric(life_expectancy_female),
    life_expectancy        = as.numeric(life_expectancy)
  )

# Gráfico

ggplot(datos_2010_filtrados,
       aes(x = life_expectancy_female, y = life_expectancy)) +
  geom_point() +
  labs(title = "Relación entre esperanza de vida total y femenina (2010)", x = "Esperanza de vida femenina (años)", y = "Esperanza de vida total (años)") +
  theme_minimal()

# Ex 2.6 ----------------------------------------------------------------------

# Modelo lineal

modelo_lineal <- lm(life_expectancy ~ life_expectancy_female, data = datos_2010_filtrados)
resumen_modelo_lineal <- summary(modelo_lineal)

print(resumen_modelo_lineal)

# Ex 2.7 ----------------------------------------------------------------------

# Extraer coeficiente y error estándar

beta1_hat <- coef(modelo_lineal)["life_expectancy_female"]
se_beta1  <- coef(resumen_modelo_lineal)[2, "Std. Error"]

# Test unilateral

t_stat <- (beta1_hat - 1) / se_beta1
gl     <- nrow(datos_2010_filtrados) - 2
conf_95 <- beta1_hat + qt(c(0.025, 0.975), gl) * se_beta1

# Tabla de resultados

print(
  tibble(
    beta1_hat = round(beta1_hat, 4),
    se_beta1  = round(se_beta1, 4),
    t_stat    = round(t_stat, 3),
    gl        = gl,
    IC_95_inf = round(conf_95[1], 4),
    IC_95_sup = round(conf_95[2], 4)
  )
)

# Ex 2.8 ----------------------------------------------------------------------

# Modelo con income

modelo_multiple <- lm(life_expectancy ~ life_expectancy_female + income_per_person,
                      data = datos_2010_filtrados)

summary(modelo_multiple)

# Ex 2.9 ----------------------------------------------------------------------

# Modelo income_per_person y child_|mortality

modelo_2var <- lm(life_expectancy ~ income_per_person + child_mortality, data = datos_2010_filtrados)

summary(modelo_2var)

# =============================================================================
# EX 3: Simulación — Envido en el Truco
# =============================================================================

# Ex 3.1 ----------------------------------------------------------------------

# Crear mazo

palos   <- c("espada", "basto", "oro", "copa")
valores <- c(1:7, 10:12)

mazo <- tibble(valor = rep(valores, times = 4),
               palo  = rep(palos, each  = 10))


# Ex 3.2 ----------------------------------------------------------------------

# Función sacar al azar

sacar_al_azar <- function(mazo) {
  mano <- mazo[sample(1:nrow(mazo), 3, replace = FALSE), ]
  mazo_restante <- anti_join(mazo, mano, by = c("valor", "palo"))
  list(mano = mano, mazo_restante = mazo_restante)
}

# Hacer n calculos de la media

repeticiones <- 10000
medias <- numeric(repeticiones)

for (i in seq_len(repeticiones)) {
  sorteo_bucle <- sacar_al_azar(mazo)
  medias[i] <- mean(sorteo_bucle$mano$valor)
}

# Gráfico

hist(
  medias,
  breaks = seq(1, 12, by = 1 / 3),
  main   = paste(
    "Histograma del valor medio de la mano con",
    repeticiones,
    "repeticiones"
  ),
  xlab   = "Valor medio (saltos de 1/3)",
  xaxt   = "n"
)
axis(1,
     at = seq(1, 12, by = 1 / 3),
     labels = seq(1, 12, by = 1 / 3))


# Ex 3.3 ----------------------------------------------------------------------

# Función contar tantos

contar_tantos <- function(mano) {
  mano_valores_envido <- mano
  mano_valores_envido$valor[mano_valores_envido$valor >= 10] <- 0
  
  cartas_por_palo <- mano_valores_envido %>%
    count(palo, name = "n") %>%
    arrange(desc(n))
  
  #Separar según cantidad de un mismo palo en la mano
  
  if (cartas_por_palo$n[1] >= 2) {
    palo_top <- cartas_por_palo$palo[1]
    mano_en_palo <- mano_valores_envido %>%
      filter(palo == palo_top) %>%
      arrange(desc(valor))
    sum(mano_en_palo$valor[1:2]) + 20
  } else {
    max(mano_valores_envido$valor)
  }
}

# Muestra inicial

sorteo <- sacar_al_azar(mazo)
mano <- sorteo$mano
mazo_restante <- sorteo$mazo_restante
mis_tantos <- contar_tantos(mano)


# Ex 3.4 ----------------------------------------------------------------------

# Función probabilidad envido

probabilidad_oponente_ganar_envido <- function(mano, mazo_restante) {
  mis_tantos <- contar_tantos(mano)
  cant_cartas_mazo_restante <- nrow(mazo_restante)
  
  # Crear contadores
  
  total_manos   <- 0
  gana_oponente <- 0
  empate        <- 0
  
  # Calcular los envidos de todas las manos rsetantes
  
  for (i in 1:(cant_cartas_mazo_restante - 2)) {
    for (j in (i + 1):(cant_cartas_mazo_restante - 1)) {
      for (k in (j + 1):cant_cartas_mazo_restante) {
        mano_oponente   <- mazo_restante[c(i, j, k), ]
        tantos_oponente <- contar_tantos(mano_oponente)
        
        total_manos <- total_manos + 1
        if (tantos_oponente > mis_tantos) {
          gana_oponente <- gana_oponente + 1
        } else if (tantos_oponente == mis_tantos) {
          empate <- empate + 1
        }
      }
    }
  }
  
  p_oponente_mano    <- (gana_oponente + empate) / total_manos   # si el oponente es mano
  p_oponente_no_mano <- gana_oponente / total_manos              # si el oponente NO es mano
  
  list(
    mano             = mano,
    oponente_mano    = p_oponente_mano,
    oponente_no_mano = p_oponente_no_mano
  )
}


# Ex 3.5 ----------------------------------------------------------------------

mis_tantos <- contar_tantos(mano)

# Crear contadores

n_aceptadas_obj <- 100
aceptadas <- 0
victorias <- 0
empates   <- 0
generadas <- 0

# Simular hasta n_aceptadas_obj

while (aceptadas < n_aceptadas_obj) {
  generadas <- generadas + 1
  
  # Muestreo de 15 cartas
  muestra15 <- mazo_restante[sample(1:nrow(mazo_restante), 15, replace = FALSE), ] %>%
    mutate(jugador = rep(c("c1", "c2", "r1", "r2", "r3"), each = 3))
  
  # Manos por jugador
  c1 <- muestra15 %>% filter(jugador == "c1") %>% select(valor, palo)
  c2 <- muestra15 %>% filter(jugador == "c2") %>% select(valor, palo)
  r1 <- muestra15 %>% filter(jugador == "r1") %>% select(valor, palo)
  r2 <- muestra15 %>% filter(jugador == "r2") %>% select(valor, palo)
  r3 <- muestra15 %>% filter(jugador == "r3") %>% select(valor, palo)
  
  # Condición compañeros con envido < 27
  tantos_c1 <- contar_tantos(c1)
  tantos_c2 <- contar_tantos(c2)
  
  if (tantos_c1 < 27 && tantos_c2 < 27) {
    aceptadas <- aceptadas + 1
    
    tantos_r1 <- contar_tantos(r1)
    tantos_r2 <- contar_tantos(r2)
    tantos_r3 <- contar_tantos(r3)
    
    tantos_equipo <- max(mis_tantos, tantos_c1, tantos_c2)
    tantos_equipo_rival <- max(tantos_r1, tantos_r2, tantos_r3)
    
    if (tantos_equipo > tantos_equipo_rival) {
      victorias <- victorias + 1
    } else if (tantos_equipo == tantos_equipo_rival) {
      empates <- empates + 1
    }
  }
}

# Estimaciones
p_sin_empates   <- victorias / aceptadas
p_con_empates   <- (victorias + empates) / aceptadas
tasa_acept      <- aceptadas / generadas

resumen_mc <- tibble(
  generadas                 = generadas,
  aceptadas                 = aceptadas,
  tasa_aceptacion           = round(tasa_acept, 4),
  empates_mano              = empates,
  p_gana_equipo             = round(p_sin_empates, 6),
  p_gana_equipo_siendo_mano = round(p_con_empates, 6),
)
print(resumen_mc)



# Ex 3.6 ----------------------------------------------------------------------

n_aceptadas_obj <- 1000
aceptadas <- 0
generadas <- 0

# Vector de tantos máximos
tantos_rival <- numeric(n_aceptadas_obj)

while (aceptadas < n_aceptadas_obj) {
  generadas <- generadas + 1
  
  
  muestra15 <- mazo_restante[sample(1:nrow(mazo_restante), 15, replace = FALSE), ] %>%
    mutate(jugador = rep(c("c1", "c2", "r1", "r2", "r3"), each = 3))
  
  c1 <- muestra15 %>% filter(jugador == "c1") %>% select(valor, palo)
  c2 <- muestra15 %>% filter(jugador == "c2") %>% select(valor, palo)
  r1 <- muestra15 %>% filter(jugador == "r1") %>% select(valor, palo)
  r2 <- muestra15 %>% filter(jugador == "r2") %>% select(valor, palo)
  r3 <- muestra15 %>% filter(jugador == "r3") %>% select(valor, palo)
  
  
  tantos_c1 <- contar_tantos(c1)
  tantos_c2 <- contar_tantos(c2)
  
  if (tantos_c1 < 27 && tantos_c2 < 27) {
    aceptadas <- aceptadas + 1
    
    
    tantos_r1 <- contar_tantos(r1)
    tantos_r2 <- contar_tantos(r2)
    tantos_r3 <- contar_tantos(r3)
    # Guardar cada tanto maximo rival
    tantos_rival[aceptadas] <- max(tantos_r1, tantos_r2, tantos_r3)
  }
}

# Histograma del máximo envido rival


hist(
  tantos_rival,
  breaks = seq(0, 33, by = 1),
  main   = paste(
    "Máximo envido del equipo rival (3 jugadores) —",
    n_aceptadas_obj,
    "muestras válidas"
  ),
  xlab   = "Máximo envido rival",
  xaxt   = "n"
)
axis(1, at = seq(0, 33, by = 1), labels = seq(0, 33, by = 1))
