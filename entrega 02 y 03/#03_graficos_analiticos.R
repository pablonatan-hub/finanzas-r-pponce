library(tidyverse)
library(scales)

# ==============================================================================
# 1. BÚSQUEDA DE TODOS LOS ARCHIVOS RDS PROCESADOS
# ==============================================================================

dir_data <- "."
f_rds_lista <- list.files(path = dir_data, pattern = "cartera_procesada_.*\\.rds$", full.names = TRUE)

if (length(f_rds_lista) == 0) {
  stop("⚠️ No se encontraron archivos RDS procesados.")
}

if (!dir.exists("graficos")) {
  dir.create("graficos")
}

# ==============================================================================
# 2. FUNCIÓN GENERADORA CORREGIDA
# ==============================================================================

generar_graficos_por_archivo <- function(ruta_rds) {
  df_cartera <- readRDS(ruta_rds)
  
  # Extraer fecha de corte directamente del dataset o del nombre de archivo
  fecha_corte <- unique(df_cartera$fecha_archivo)[1]
  if (is.na(fecha_corte)) {
    fecha_corte <- str_extract(basename(ruta_rds), "\\d{4}-\\d{2}-\\d{2}")
  }
  
  # Normalizar la columna semaforo_riesgo evitando que se transforme en NA
  df_cartera <- df_cartera %>%
    mutate(
      semaforo_limpio = case_when(
        str_detect(semaforo_riesgo, "Saludable|Verde|🟢") ~ "Verde\n(Saludable)",
        str_detect(semaforo_riesgo, "Advertencia|Amarillo|🟡") ~ "Amarillo\n(Advertencia)",
        str_detect(semaforo_riesgo, "Critico|Default|Rojo|🔴") ~ "Rojo\n(Default)",
        TRUE ~ "Verde\n(Saludable)"
      ),
      etiqueta_eje = factor(semaforo_limpio, levels = c("Verde\n(Saludable)", "Amarillo\n(Advertencia)", "Rojo\n(Default)"))
    )
  
  colores_semaforo <- c(
    "Verde\n(Saludable)" = "#2ecc71",
    "Amarillo\n(Advertencia)" = "#f1c40f",
    "Rojo\n(Default)" = "#e74c3c"
  )
  
  # --- GRÁFICO 1: DISTRIBUCIÓN DE CLIENTES (CORREGIDO) ---
  df_g1 <- df_cartera %>%
    count(etiqueta_eje, .drop = FALSE)
  
  total_clientes <- sum(df_g1$n)
  df_g1 <- df_g1 %>%
    mutate(porcentaje = if (total_clientes > 0) n / total_clientes else 0)

  g1 <- ggplot(df_g1, aes(x = etiqueta_eje, y = n, fill = etiqueta_eje)) +
    geom_col(width = 0.55, show.legend = FALSE) +
    geom_text(
      aes(label = paste0(comma(n, big.mark = "."), "\n(", percent(porcentaje, accuracy = 0.1), ")")),
      vjust = -0.2, fontface = "bold", size = 3.8
    ) +
    scale_fill_manual(values = colores_semaforo) +
    scale_y_continuous(labels = comma_format(big.mark = "."), limits = c(0, max(df_g1$n, 10) * 1.25)) +
    labs(
      title = paste0("Distribucion de Clientes por Categoria de Riesgo (Corte: ", fecha_corte, ")"),
      subtitle = "Evaluacion de cartera segun comportamiento de pago y ahorro (VP)",
      x = "Estado de Póliza", y = "Cantidad de Clientes", caption = "Fuente: Sistema de Monitoreo VP"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 13), axis.text.x = element_text(face = "bold", size = 10))

  ruta_g1 <- file.path("graficos", paste0("grafico_1_distribucion_", fecha_corte, ".png"))
  ggsave(ruta_g1, g1, width = 8, height = 5.5, dpi = 300)

  # --- GRÁFICO 2: IMPACTO FINANCIERO ---
  g2 <- df_cartera %>%
    group_by(etiqueta_eje, .drop = FALSE) %>%
    summarise(monto_deuda = sum(deuda_total_acumulada, na.rm = TRUE), .groups = "drop") %>%
    ggplot(aes(x = etiqueta_eje, y = monto_deuda, fill = etiqueta_eje)) +
    geom_col(width = 0.55, show.legend = FALSE) +
    geom_text(
      aes(label = paste0("$ ", comma(monto_deuda, big.mark = "."))),
      vjust = -0.4, fontface = "bold", size = 3.8
    ) +
    scale_fill_manual(values = colores_semaforo) +
    scale_y_continuous(labels = dollar_format(prefix = "$ ", big.mark = "."), limits = c(0, max(sum(df_cartera$deuda_total_acumulada, na.rm = TRUE), 100) * 0.8)) +
    labs(
      title = paste0("Impacto Financiero: Monto Total en Riesgo (Corte: ", fecha_corte, ")"),
      subtitle = "Deuda acumulada por costos y gastos administrativos (CLP)",
      x = "Estado de Póliza", y = "Monto Adeudado Total ($)", caption = "Fuente: Sistema de Monitoreo VP"
    ) +
    theme_minimal(base_size = 12) +
    theme(plot.title = element_text(face = "bold", size = 13), axis.text.x = element_text(face = "bold", size = 10))

  ruta_g2 <- file.path("graficos", paste0("grafico_2_impacto_financiero_", fecha_corte, ".png"))
  ggsave(ruta_g2, g2, width = 8, height = 5.5, dpi = 300)

  cat(" ✅ Procesado corte:", as.character(fecha_corte), "-> Imágenes en /graficos\n")
}

# ==============================================================================
# 3. EJECUCIÓN
# ==============================================================================

cat("\n🔄 Procesando todos los archivos de cartera histórica...\n")
walk(f_rds_lista, generar_graficos_por_archivo)
cat("\n✨ ¡Proceso finalizado con éxito! Revisa la carpeta /graficos.\n")