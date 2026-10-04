#02_procesador_mvp
library(tidyverse)
library(openxlsx)

# ==============================================================================
# 1. CONTROL DE HISTORIAL Y SELECCIÓN DE ARCHIVOS MÁS RECIENTES
# ==============================================================================

# Directorio de datos
dir_data <- "data"

# Obtener lista de archivos CSV en la carpeta
archivos_disponibles <- list.files(path = dir_data, pattern = "\\.csv$", full.names = TRUE)

# Archivo de registro de archivos ya procesados
archivo_historial <- file.path(dir_data, "historial_archivos_procesados.txt")
archivos_procesados <- if (file.exists(archivo_historial)) readLines(archivo_historial) else character(0)

# Filtrar archivos no procesados previamente
archivos_nuevos <- archivos_disponibles[!basename(archivos_disponibles) %in% archivos_procesados]

if (length(archivos_nuevos) < 3) {
  stop("⚠️ No se encontraron 3 archivos nuevos/sin procesar en la carpeta '/data'.")
}

# Identificar los 3 archivos requeridos
f_info <- archivos_nuevos[str_detect(archivos_nuevos, "clientes_info")]
f_deuda <- archivos_nuevos[str_detect(archivos_nuevos, "polizas_deuda")]
f_pago <- archivos_nuevos[str_detect(archivos_nuevos, "vias_pago")]

if (length(f_info) == 0 | length(f_deuda) == 0 | length(f_pago) == 0) {
  stop("⚠️ Faltan tipos de archivos requeridos (clientes_info, polizas_deuda o vias_pago).")
}

cat("📥 Procesando los siguientes archivos más recientes:\n")
cat(" - ", basename(f_info[1]), "\n")
cat(" - ", basename(f_deuda[1]), "\n")
cat(" - ", basename(f_pago[1]), "\n")

# ==============================================================================
# 2. INGESTA Y CONSOLIDACIÓN DE DATOS (JOIN POR RUT)
# ==============================================================================

df_info  <- read_csv(f_info[1], show_col_types = FALSE)
df_deuda <- read_csv(f_deuda[1], show_col_types = FALSE)
df_pago  <- read_csv(f_pago[1], show_col_types = FALSE)

# Unión de tablas por RUT
df_consolidado <- df_info %>%
  inner_join(df_deuda, by = c("rut", "fecha_archivo")) %>%
  inner_join(df_pago, by = c("rut", "fecha_archivo"))

# ==============================================================================
# 3. LÓGICA DE NEGOCIO, DEUDA TOTAL Y SEMÁFORO DE RIESGO
# ==============================================================================

df_procesado <- df_consolidado %>%
  mutate(
    # Deuda total sumando costo y gasto
    deuda_total_acumulada = costo_adeudado + gastos_adeudados,
    
    # Proyección de meses para caer en default (Límite: 3 meses)
    meses_para_default = case_when(
      primas_pendientes == 0 ~ NA_real_, # No aplica a clientes al día
      TRUE ~ pmax(0, 3 - primas_pendientes)
    ),
    
    # Regla del Semáforo de Riesgo
    semaforo_riesgo = case_when(
      primas_pendientes == 0 ~ "🟢 Saludable",
      primas_pendientes >= 1 & primas_pendientes <= 2 ~ "🟡 Advertencia (Próximo a Caducar)",
      primas_pendientes >= 3 ~ "🔴 Crítico (Caduco - Default)"
    )
  )

# ==============================================================================
# 4. RESUMEN EJECUTIVO E IMPACTO MONETARIO (TABLA RESUMEN)
# ==============================================================================

resumen_ejecutivo <- df_procesado %>%
  group_by(semaforo_riesgo) %>%
  summarise(
    cantidad_clientes = n(),
    porcentaje_cartera = round((n() / nrow(df_procesado)) * 100, 2),
    monto_total_prima_pactada = sum(prima_pactada, na.rm = TRUE),
    monto_total_deuda_costos = sum(costo_adeudado, na.rm = TRUE),
    monto_total_deuda_gastos = sum(gastos_adeudados, na.rm = TRUE),
    monto_total_deuda_riesgo = sum(deuda_total_acumulada, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(factor(semaforo_riesgo, levels = c("🟢 Saludable", "🟡 Advertencia (Próximo a Caducar)", "🔴 Crítico (Caduco - Default)")))

# Total general de la cartera
fila_total <- tibble(
  semaforo_riesgo = "TOTAL CARTERA",
  cantidad_clientes = sum(resumen_ejecutivo$cantidad_clientes),
  porcentaje_cartera = 100.0,
  monto_total_prima_pactada = sum(resumen_ejecutivo$monto_total_prima_pactada),
  monto_total_deuda_costos = sum(resumen_ejecutivo$monto_total_deuda_costos),
  monto_total_deuda_gastos = sum(resumen_ejecutivo$monto_total_deuda_gastos),
  monto_total_deuda_riesgo = sum(resumen_ejecutivo$monto_total_deuda_riesgo)
)

resumen_ejecutivo_completo <- bind_rows(resumen_ejecutivo, fila_total)

# ==============================================================================
# 5. PERSISTENCIA DE ARCHIVOS Y EXPORTACIÓN EN EXCEL MULTI-HOJA
# ==============================================================================

fecha_corte <- unique(df_procesado$fecha_archivo)[1]

# Guardar dataset procesado en CSV e RDS (Persistencia del historial)
write_csv(df_procesado, file.path(dir_data, paste0("cartera_procesada_", fecha_corte, ".csv")))
saveRDS(df_procesado, file.path(dir_data, paste0("cartera_procesada_", fecha_corte, ".rds")))

# Crear Libro Excel con openxlsx
wb <- createWorkbook()

# Hoja 1: Resumen Ejecutivo
addWorksheet(wb, "Resumen Ejecutivo")
writeData(wb, "Resumen Ejecutivo", resumen_ejecutivo_completo)

# Hoja 2: Detalle Por Cliente
addWorksheet(wb, "Detalle Cartera")
writeData(wb, "Detalle Cartera", df_procesado)

# Guardar Excel
ruta_excel <- file.path(dir_data, paste0("Reporte_Monitoreo_VP_", fecha_corte, ".xlsx"))
saveWorkbook(wb, ruta_excel, overwrite = TRUE)

# Registrar archivos como procesados en el archivo de texto
cat(c(basename(f_info[1]), basename(f_deuda[1]), basename(f_pago[1])), 
    file = archivo_historial, sep = "\n", append = TRUE)

cat("\n✅ ¡MVP Ejecutado con éxito!\n")
cat(" 📊 Reporte Excel generado en: ", ruta_excel, "\n")
cat(" 💾 Dataset histórico guardado en CSV e RDS para fecha: ", as.character(fecha_corte), "\n")