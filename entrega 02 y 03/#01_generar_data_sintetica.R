#01_generar_data_sintetica
library(tidyverse)

# 1. Configuración de parámetros y directorio
set.seed(2026) # Semilla para reproducibilidad
n_clientes <- 850
fecha_extraccion <- Sys.Date() # Fecha actual (2026-09-27)

# Crear directorio /data si no existe
if (!dir.exists("data")) {
  dir.create("data")
}

# 2. Funciones auxiliares
# Generador de RUT congruente con la edad
generar_rut <- function(edad) {
  # A menor edad, número de RUT más alto
  base_min <- round(22000000 - (edad - 20) * 300000)
  base_max <- base_min + 200000
  numero <- sample(base_min:base_max, 1)
  
  # Cálculo de dígito verificador (Módulo 11)
  s <- 0
  m <- 2
  temp <- numero
  while (temp > 0) {
    s <- s + (temp %% 10) * m
    m <- if (m == 6) 2 else m + 1
    temp <- floor(temp / 10)
  }
  dv_num <- 11 - (s %% 11)
  dv <- if (dv_num == 11) "0" else if (dv_num == 10) "K" else as.character(dv_num)
  
  paste0(numero, "-", dv)
}

# 3. Asignación de Estados de Cartera (Perfil del Cliente)
# 45% Al día, 35% VP 1 mes, 15% VP $0 sin deuda, 5% VP $0 Caída Default
perfiles <- c(
  rep("AL_DIA", round(n_clientes * 0.45)),
  rep("VP_1_MES", round(n_clientes * 0.35)),
  rep("VP_ZERO_SIN_DEUDA", round(n_clientes * 0.15)),
  rep("VP_ZERO_DEFAULT", n_clientes - round(n_clientes * 0.45) - round(n_clientes * 0.35) - round(n_clientes * 0.15))
) %>% sample()

# 4. Generación de Base Principal de Clientes
df_maestro <- tibble(
  perfil = perfiles,
  edad = sample(20:60, n_clientes, replace = TRUE),
  sueldo = round(runif(n_clientes, min = 800000, max = 5000000), -4)
) %>%
  rowwise() %>%
  mutate(
    rut = generar_rut(edad),
    # Prima pactada entre 90k y 350k, topada al 15% del sueldo
    prima_pactada = round(min(runif(1, 90000, 350000), sueldo * 0.15), -3)
  ) %>%
  ungroup()

# Desglose teórico de la prima: 50% Ahorro, 30% Costos, 20% Gastos
df_maestro <- df_maestro %>%
  mutate(
    monto_costo_mensual = prima_pactada * 0.30,
    monto_gasto_mensual = prima_pactada * 0.20
  )

# Lógica de deudas y primas pendientes según perfil de riesgo
df_maestro <- df_maestro %>%
  rowwise() %>%
  mutate(
    primas_pendientes = case_when(
      perfil == "AL_DIA" ~ 0,
      perfil == "VP_1_MES" ~ sample(1:2, 1),
      perfil == "VP_ZERO_SIN_DEUDA" ~ sample(1:3, 1),
      perfil == "VP_ZERO_DEFAULT" ~ 3
    ),
    # En default la deuda equivale a 1.5 primas (costos + gastos)
    costo_adeudado = case_when(
      perfil == "AL_DIA" ~ 0,
      perfil == "VP_ZERO_DEFAULT" ~ round(monto_costo_mensual * 1.5, -2),
      TRUE ~ round(monto_costo_mensual * (primas_pendientes * runif(1, 0.5, 1)), -2)
    ),
    # El gasto adeudado es congruente proporcionalmente con el costo adeudado
    gastos_adeudados = case_when(
      perfil == "AL_DIA" ~ 0,
      perfil == "VP_ZERO_DEFAULT" ~ round(monto_gasto_mensual * 1.5, -2),
      costo_adeudado > 0 ~ round(costo_adeudado * (0.20 / 0.30), -2),
      TRUE ~ 0
    ),
    via_pago = sample(
      c("PAC", "PAT", "PREVIRED", "DIRECTO"), 
      1, 
      prob = c(0.35, 0.40, 0.15, 0.10)
    )
  ) %>%
  ungroup()

# 5. División en los 3 Archivos Requeridos

# Archivo 1: clientes_info.csv
archivo_1 <- df_maestro %>%
  select(rut, sueldo, prima_pactada) %>%
  mutate(fecha_archivo = fecha_extraccion)

# Archivo 2: polizas_deuda.csv
archivo_2 <- df_maestro %>%
  select(rut, primas_pendientes, edad, gastos_adeudados) %>%
  mutate(fecha_archivo = fecha_extraccion)

# Archivo 3: vias_pago.csv
archivo_3 <- df_maestro %>%
  select(rut, via_pago, costo_adeudado) %>%
  mutate(fecha_archivo = fecha_extraccion)

# 6. Exportación de CSVs
write_csv(archivo_1, "data/clientes_info.csv")
write_csv(archivo_2, "data/polizas_deuda.csv")
write_csv(archivo_3, "data/vias_pago.csv")

cat("✅ ¡Proceso completado! Se generaron exitosamente los 3 archivos en la carpeta '/data':\n")
cat(" - data/clientes_info.csv (", nrow(archivo_1), " registros)\n")
cat(" - data/polizas_deuda.csv (", nrow(archivo_2), " registros)\n")
cat(" - data/vias_pago.csv (", nrow(archivo_3), " registros)\n")