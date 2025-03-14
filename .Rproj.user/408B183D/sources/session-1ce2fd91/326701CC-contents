# server.R
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(tidyr)
library(scales)

server <- function(input, output, session) {
  
  # Reactivo que filtra dfPoblacion según la selección del usuario
  filteredPopulation <- reactive({
    dfPoblacion %>%
      filter(Entidad %in% input$entidad,
             Grupo %in% input$grupo)
  })
  
  # Gráfico: Evolución de la población (Año en x, Población en y)
  output$populationPlot <- renderPlot({
    ggplot(filteredPopulation(), aes(x = Ano, y = Poblacion, color = Grupo, linetype = Entidad, group = interaction(Grupo, Entidad))) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(x = "Año", y = "Población", title = "Evolución de la Población") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  # Tabla: Datos filtrados
  output$populationTable <- DT::renderDT({
    wide_data <- filteredPopulation() %>%
      tidyr::pivot_wider(
        names_from = Ano,       # Los valores de "Ano" se convertirán en nombres de columnas
        values_from = Poblacion # Los valores en "Poblacion" se asignarán a esas columnas
      )
    DT::datatable(wide_data, options = list(pageLength = 10)) %>%
      DT::formatRound(
        columns = setdiff(names(wide_data), c("Entidad", "Grupo")),
        digits = 0,
        mark = ","
      )
  })
  
  # Descarga: Exporta los datos filtrados como CSV
  output$downloadPopulation <- downloadHandler(
    filename = function() { paste0("Poblacion_", Sys.Date(), ".csv") },
    content = function(file) {
      wide_data <- filteredPopulation() %>%
        tidyr::pivot_wider(
          names_from = Ano,
          values_from = Poblacion
        )
      write.csv(wide_data, file, row.names = FALSE)
    }
  )

  
  
  
  
  
  # Reactivo: Filtra dfInmigracionInt según la selección del usuario
  filteredInmigracionInt <- reactive({
    dfInmigracionInt %>%
      filter(Entidad %in% input$entidadInm)
  })
  
  # Gráfico: Inmigración Internacional (Año vs. Población)
  output$inmigracionIntPlot <- renderPlot({
    data <- filteredInmigracionInt()
    ggplot(data, aes(x = Ano, y = Poblacion, color = Entidad, group = Entidad)) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(x = "Año", y = "Población", title = "Inmigración Internacional") +
      scale_y_continuous(labels = comma) +
      theme_minimal()
  })
  
  # Tabla: Transforma a formato ancho (pivot_wider) y muestra los datos
  output$inmigracionIntTable <- DT::renderDT({
    wide_data <- filteredInmigracionInt() %>%
      pivot_wider(
        names_from = Ano,
        values_from = Poblacion
      )
    DT::datatable(wide_data, options = list(pageLength = 10)) %>%
      DT::formatRound(
        columns = setdiff(names(wide_data), "Entidad"),
        digits = 0,
        mark = ","
      )
  })
  
  # Descarga: Exporta los datos transformados a CSV
  output$downloadInmigracionInt <- downloadHandler(
    filename = function() { paste0("InmigracionInt_", Sys.Date(), ".csv") },
    content = function(file) {
      wide_data <- filteredInmigracionInt() %>%
        pivot_wider(
          names_from = Ano,
          values_from = Poblacion
        )
      write.csv(wide_data, file, row.names = FALSE)
    }
  )


  
  
  
  
  # Reactivo: Filtra dfMigracionNac según Entidad y Tipo seleccionados
  filteredMigracionNac <- reactive({
    # Convertimos "Ano" a numérico para graficar, ya que viene como carácter
    dfMigracionNac %>%
      filter(Entidad %in% input$entidad_mn,
             Tipo %in% input$tipo_mn) %>%
      mutate(Ano = as.numeric(Ano))
  })
  
  # Gráfico: Migración Nacional
  output$migracionNacPlot <- renderPlot({
    ggplot(filteredMigracionNac(), aes(x = Ano, y = Poblacion, color = Tipo, 
                                       linetype = Entidad, group = interaction(Entidad, Tipo))) +
      geom_line(size = 1) +
      geom_point(size = 2) +
      labs(x = "Año", y = "Población", title = "Migración Nacional") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  # Tabla: Convertir a formato ancho usando pivot_wider
  output$migracionNacTable <- DT::renderDT({
    wide_data <- filteredMigracionNac() %>%
      tidyr::pivot_wider(
        names_from = Ano,
        values_from = Poblacion
      )
    DT::datatable(wide_data, options = list(pageLength = 10)) %>%
      DT::formatRound(
        columns = setdiff(names(wide_data), c("Entidad", "Tipo")),
        digits = 0,
        mark = ","
      )
  })
  
  # Descarga: Exportar datos en formato ancho a CSV
  output$downloadMigracionNac <- downloadHandler(
    filename = function() { paste0("MigracionNac_", Sys.Date(), ".csv") },
    content = function(file) {
      wide_data <- filteredMigracionNac() %>%
        tidyr::pivot_wider(
          names_from = Ano,
          values_from = Poblacion
        )
      write.csv(wide_data, file, row.names = FALSE)
    }
  )
  
  
  
  
  # Módulo de Inflación (dentro de server.R)
  output$inflacion <- renderPlot({
    # Convertir Periodo a factor con orden de aparición
    df_plot <- dfInflacion
    df_plot$Periodo <- factor(df_plot$Periodo, levels = unique(df_plot$Periodo))
    
    ggplot(df_plot, aes(x = Periodo, y = Inflacion, group = 1)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(x = "Periodo", y = "Inflación (%)", title = "Inflación últimos 10 años") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$inflacionTable <- DT::renderDT({
    DT::datatable(dfInflacion, options = list(pageLength = 10))
  })
  
  output$downloadInflacion <- downloadHandler(
    filename = function() {
      paste0("Inflacion_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(dfInflacion, file, row.names = FALSE)
    }
  )
  
  
  
  
  # Módulo de Salario Mínimo (dentro de server.R)
  output$salariominimo <- renderPlot({
    ggplot(dfSalarioMinimo, aes(x = Ano, y = Salario, color = Zona, group = Zona)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(x = "Año", y = "Salario $ (MXN)", title = "Salario Mínimo") +
      scale_x_continuous(
        breaks = c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023, 2024, 2025),
        minor_breaks = NULL
      ) +
      theme_minimal()
  })
  
  
  output$salarioMinimoTable <- DT::renderDT({
    # Transformar el dataframe a formato ancho: una fila por Zona y una columna para cada Año.
    wide_data <- dfSalarioMinimo %>%
      tidyr::pivot_wider(
        names_from = Ano,
        values_from = Salario
      )
    
    DT::datatable(wide_data, options = list(pageLength = 10))
  })
  
  output$downloadSalariominimo <- downloadHandler(
    filename = function() {
      paste0("SalarioMinimo_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Transformar el dataframe a formato ancho para la descarga
      wide_data <- dfSalarioMinimo %>%
        tidyr::pivot_wider(
          names_from = Ano,
          values_from = Salario
        )
      write.csv(wide_data, file, row.names = FALSE)
    }
  )
  
  
  #Salario Profesores
  # Reactivo: Filtra dfSalarioProfesores según la Entidad seleccionada
  filteredSalarioProfesores <- reactive({
    dfSalarioProfesores %>%
      filter(ENTIDAD %in% input$ent_salProf)
  })
  
  # Gráfico: Salario de Profesores a lo largo de los años
  output$salariosProfesoresPlot <- renderPlot({
    ggplot(filteredSalarioProfesores(), aes(x = Ano, y = SalarioHoraPromedio, color = ENTIDAD, group = ENTIDAD)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(x = "Año", y = "Salario Promedio por Hora (MXN)", title = "Salario de Profesores") +
      scale_x_continuous(breaks = sort(unique(filteredSalarioProfesores()$Ano))) +
      theme_minimal()
  })
  
  # Tabla: Datos en formato ancho para Salario de Profesores
  output$salariosProfesoresTable <- DT::renderDT({
    wide_data <- filteredSalarioProfesores() %>%
      # Convertir Año a carácter para que los nombres de columna sean exactamente los años
      mutate(Ano = as.character(Ano)) %>%
      tidyr::pivot_wider(
        id_cols = ENTIDAD,                   # Usamos ENTIDAD como clave
        names_from = Ano,                    # Cada año se convierte en una columna
        values_from = SalarioHoraPromedio,   # Los valores son el salario promedio
        # En caso de duplicados, se puede resumir (aquí se usa mean, pero también podrías usar first)
        values_fn = list(SalarioHoraPromedio = mean),
        values_fill = list(SalarioHoraPromedio = NA)  # Si falta dato, se asigna NA
      )
    DT::datatable(wide_data, options = list(pageLength = 10)) %>%
      DT::formatRound(
        columns = setdiff(names(wide_data), "ENTIDAD"),
        digits = 2,   # Redondear a 2 decimales
        mark = ","
      )
  })
  
  output$downloadSalariosProfesores <- downloadHandler(
    filename = function() { paste0("SalariosProfesores_", Sys.Date(), ".csv") },
    content = function(file) {
      wide_data <- filteredSalarioProfesores() %>%
        mutate(Ano = as.character(Ano)) %>%
        tidyr::pivot_wider(
          id_cols = ENTIDAD,
          names_from = Ano,
          values_from = SalarioHoraPromedio,
          values_fn = list(SalarioHoraPromedio = mean),
          values_fill = list(SalarioHoraPromedio = NA)
        )
      write.csv(wide_data, file, row.names = FALSE)
    }
  )
  
  
  
  
  # Gráfico: Barras agrupadas por PadresTrabajan, facetado por Quintil (excluyendo Total)
  output$situacionLaboralPlot <- renderPlot({
    data_plot <- dfSituacionLaboral %>%
      mutate(Quintil = case_when(
        QUINTIL == 1 ~ "Quintil 1",
        QUINTIL == 2 ~ "Quintil 2",
        QUINTIL == 3 ~ "Quintil 3",
        QUINTIL == 4 ~ "Quintil 4",
        QUINTIL == 5 ~ "Quintil 5",
        is.na(QUINTIL) ~ "Total"
      )) %>%
      filter(Quintil != "Total")
    
    ggplot(data_plot, aes(x = factor(Ano), y = n, fill = factor(PadresTrabajan))) +
      geom_col(position = "dodge") +
      facet_wrap(~ Quintil) +
      labs(x = "Año",
           y = "Número de Hogares",
           fill = "Padres Trabajan",
           title = "Situación laboral de los padres por quintiles de ingreso de los hogares") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  # Tabla: Datos en formato ancho para quintiles (excluyendo Total)
  output$situacionLaboralTable <- DT::renderDT({
    wide_data <- dfSituacionLaboral %>%
      mutate(Ano = as.character(Ano),
             Quintil = case_when(
               QUINTIL == 1 ~ "Quintil 1",
               QUINTIL == 2 ~ "Quintil 2",
               QUINTIL == 3 ~ "Quintil 3",
               QUINTIL == 4 ~ "Quintil 4",
               QUINTIL == 5 ~ "Quintil 5",
               is.na(QUINTIL) ~ "Total"
             )) %>%
      filter(Quintil != "Total") %>%
      tidyr::pivot_wider(
        id_cols = c(Quintil, PadresTrabajan),
        names_from = Ano,
        values_from = n,
        values_fn = list(n = sum),
        values_fill = list(n = 0)
      )
    DT::datatable(wide_data, options = list(pageLength = 10)) %>%
      DT::formatRound(
        columns = setdiff(names(wide_data), c("Quintil", "PadresTrabajan")),
        digits = 0,
        mark = ","
      )
  })
  
  # Descarga: Datos en formato ancho para quintiles (excluyendo Total)
  output$downloadSituacionLaboral <- downloadHandler(
    filename = function() { paste0("SituacionLaboral_", Sys.Date(), ".csv") },
    content = function(file) {
      wide_data <- dfSituacionLaboral %>%
        mutate(Ano = as.character(Ano),
               Quintil = case_when(
                 QUINTIL == 1 ~ "Quintil 1",
                 QUINTIL == 2 ~ "Quintil 2",
                 QUINTIL == 3 ~ "Quintil 3",
                 QUINTIL == 4 ~ "Quintil 4",
                 QUINTIL == 5 ~ "Quintil 5",
                 is.na(QUINTIL) ~ "Total"
               )) %>%
        filter(Quintil != "Total") %>%
        tidyr::pivot_wider(
          id_cols = c(Quintil, PadresTrabajan),
          names_from = Ano,
          values_from = n,
          values_fn = list(n = sum),
          values_fill = list(n = 0)
        )
      write.csv(wide_data, file, row.names = FALSE)
    }
  )
  
  
  # Gráfico: Barras para casos Total únicamente
  output$situacionLaboralTotalPlot <- renderPlot({
    data_total <- dfSituacionLaboral %>%
      mutate(Quintil = case_when(
        QUINTIL == 1 ~ "Quintil 1",
        QUINTIL == 2 ~ "Quintil 2",
        QUINTIL == 3 ~ "Quintil 3",
        QUINTIL == 4 ~ "Quintil 4",
        QUINTIL == 5 ~ "Quintil 5",
        is.na(QUINTIL) ~ "Total"
      )) %>%
      filter(Quintil == "Total")
    
    ggplot(data_total, aes(x = factor(Ano), y = n, fill = factor(PadresTrabajan))) +
      geom_col(position = "dodge") +
      labs(x = "Año",
           y = "Número de Hogares",
           fill = "Padres Trabajan",
           title = "Situación laboral de los padres (Total)") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  # Tabla: Datos en formato ancho para Total
  output$situacionLaboralTotalTable <- DT::renderDT({
    wide_data_total <- dfSituacionLaboral %>%
      mutate(Ano = as.character(Ano),
             Quintil = case_when(
               QUINTIL == 1 ~ "Quintil 1",
               QUINTIL == 2 ~ "Quintil 2",
               QUINTIL == 3 ~ "Quintil 3",
               QUINTIL == 4 ~ "Quintil 4",
               QUINTIL == 5 ~ "Quintil 5",
               is.na(QUINTIL) ~ "Total"
             )) %>%
      filter(Quintil == "Total") %>%
      tidyr::pivot_wider(
        id_cols = c(Quintil, PadresTrabajan),
        names_from = Ano,
        values_from = n,
        values_fn = list(n = sum),
        values_fill = list(n = 0)
      )
    DT::datatable(wide_data_total, options = list(pageLength = 10)) %>%
      DT::formatRound(
        columns = setdiff(names(wide_data_total), c("Quintil", "PadresTrabajan")),
        digits = 0,
        mark = ","
      )
  })
  
  # Descarga: Datos en formato ancho para Total
  output$downloadSituacionLaboralTotal <- downloadHandler(
    filename = function() { paste0("SituacionLaboralTotal_", Sys.Date(), ".csv") },
    content = function(file) {
      wide_data_total <- dfSituacionLaboral %>%
        mutate(Ano = as.character(Ano),
               Quintil = case_when(
                 QUINTIL == 1 ~ "Quintil 1",
                 QUINTIL == 2 ~ "Quintil 2",
                 QUINTIL == 3 ~ "Quintil 3",
                 QUINTIL == 4 ~ "Quintil 4",
                 QUINTIL == 5 ~ "Quintil 5",
                 is.na(QUINTIL) ~ "Total"
               )) %>%
        filter(Quintil == "Total") %>%
        tidyr::pivot_wider(
          id_cols = c(Quintil, PadresTrabajan),
          names_from = Ano,
          values_from = n,
          values_fn = list(n = sum),
          values_fill = list(n = 0)
        )
      write.csv(wide_data_total, file, row.names = FALSE)
    }
  )
  
  
  
  
  # Módulo de Gasto en Educación
  
  # Gráfico: Evolución del gasto en educación por Quintil (excluyendo Total)
  output$gastoEducacionPlot <- renderPlot({
    df_plot <- dfGastoEducacion %>%
      mutate(Quintil = case_when(
        QUINTIL == 1 ~ "Quintil 1",
        QUINTIL == 2 ~ "Quintil 2",
        QUINTIL == 3 ~ "Quintil 3",
        QUINTIL == 4 ~ "Quintil 4",
        QUINTIL == 5 ~ "Quintil 5",
        is.na(QUINTIL) ~ "Total"
      )) %>%
      # Filtrar por Entidad según el input de la UI
      filter(Entidad %in% input$ent_gastoEdu) %>%
      # Excluir los Total, ya que se muestran solo los quintiles 1 a 5
      filter(Quintil != "Total")
    
    ggplot(df_plot, aes(x = Ano, y = GastoEdu, color = Quintil, linetype = Entidad, group = interaction(Quintil, Entidad))) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(x = "Año",
           y = "Gasto mensual promedio en educación (MXN)",
           title = "Evolución del Gasto en Educación",
           color = "Quintil") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  # Tabla: Datos en formato ancho
  output$gastoEducacionTable <- DT::renderDT({
    wide_data <- dfGastoEducacion %>%
      mutate(Ano = as.character(Ano),
             Quintil = case_when(
               QUINTIL == 1 ~ "Quintil 1",
               QUINTIL == 2 ~ "Quintil 2",
               QUINTIL == 3 ~ "Quintil 3",
               QUINTIL == 4 ~ "Quintil 4",
               QUINTIL == 5 ~ "Quintil 5",
               is.na(QUINTIL) ~ "Total"
             )) %>%
      filter(Entidad %in% input$ent_gastoEdu) %>%
      filter(Quintil != "Total") %>%  # Excluir Total
      tidyr::pivot_wider(
        id_cols = c(Entidad, Quintil),          # Cada fila corresponde a un Quintil
        names_from = Ano,              # Las columnas serán los años
        values_from = GastoEdu,         # Los valores son el gasto educativo
        values_fn = list(GastoEdu = mean),  # En caso de duplicados, se toma la media
        values_fill = list(GastoEdu = NA)     # Si falta dato, se asigna NA
      )
    
    DT::datatable(wide_data, options = list(pageLength = 10)) %>%
      DT::formatRound(
        columns = setdiff(names(wide_data), c("Entidad", "Quintil")),
        digits = 2,
        mark = ","
      )
  })
  
  # Descarga: Exporta la tabla en formato ancho a CSV
  output$downloadGastoEducacion <- downloadHandler(
    filename = function() { paste0("GastoEducacion_", Sys.Date(), ".csv") },
    content = function(file) {
      wide_data <- dfGastoEducacion %>%
        mutate(Ano = as.character(Ano),
               Quintil = case_when(
                 QUINTIL == 1 ~ "Quintil 1",
                 QUINTIL == 2 ~ "Quintil 2",
                 QUINTIL == 3 ~ "Quintil 3",
                 QUINTIL == 4 ~ "Quintil 4",
                 QUINTIL == 5 ~ "Quintil 5",
                 is.na(QUINTIL) ~ "Total"
               )) %>%
        filter(Entidad %in% input$ent_gastoEdu) %>%
        filter(Quintil != "Total") %>%
        tidyr::pivot_wider(
          id_cols = c(Entidad, Quintil),
          names_from = Ano,
          values_from = GastoEdu,
          values_fn = list(GastoEdu = mean),
          values_fill = list(GastoEdu = NA)
        )
      write.csv(wide_data, file, row.names = FALSE)
    }
  )
  
  
  
  
  
  
  
  # Reactivo: Filtra dfAlumnosPorAula según la Entidad y el NIVEL (ambos de selección única)
  filteredAlumnosPorAula <- reactive({
    dfAlumnosPorAula %>%
      filter(N_ENTIDAD == input$ent_alumnosAula,
             NIVEL == input$nivel_alumnosAula) %>%
      # Extraer el año del PERIODO; asumiendo que PERIODO es del tipo "2019-2020"
      mutate(Year = as.numeric(substr(PERIODO, 1, 4)))
  })
  
  # Gráfico: Ratio de Alumnos por Aula
  output$alumnosPorAulaPlot <- renderPlot({
    ggplot(filteredAlumnosPorAula(), aes(x = Year, y = Ratio_AlumnosAulas, 
                                         color = GRADO, linetype = CONTROL,
                                         group = interaction(GRADO, CONTROL))) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(x = "Año",
           y = "Ratio de Alumnos por Aula",
           title = "Evolución del Ratio de Alumnos por Aula",
           color = "Grado",
           linetype = "Control") +
      scale_x_continuous(breaks = sort(unique(filteredAlumnosPorAula()$Year))) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  # Tabla: Datos en formato ancho con columnas: NIVEL, GRADO, CONTROL y una columna para cada Año
  output$alumnosPorAulaTable <- DT::renderDT({
    wide_data <- filteredAlumnosPorAula() %>%
      mutate(Year = as.character(Year)) %>%
      tidyr::pivot_wider(
        id_cols = c(NIVEL, GRADO, CONTROL),
        names_from = Year,
        values_from = Ratio_AlumnosAulas,
        values_fn = list(Ratio_AlumnosAulas = mean),  # En caso de duplicados, se toma la media
        values_fill = list(Ratio_AlumnosAulas = NA)
      )
    DT::datatable(wide_data, options = list(pageLength = 10)) %>%
      DT::formatRound(
        columns = setdiff(names(wide_data), c("NIVEL", "GRADO", "CONTROL")),
        digits = 2,
        mark = ","
      )
  })
  
  # Descarga: Exporta la tabla en formato ancho a CSV
  output$downloadAlumnosPorAula <- downloadHandler(
    filename = function() { paste0("AlumnosPorAula_", Sys.Date(), ".csv") },
    content = function(file) {
      wide_data <- filteredAlumnosPorAula() %>%
        mutate(Year = as.character(Year)) %>%
        tidyr::pivot_wider(
          id_cols = c(NIVEL, GRADO, CONTROL),
          names_from = Year,
          values_from = Ratio_AlumnosAulas,
          values_fn = list(Ratio_AlumnosAulas = mean),
          values_fill = list(Ratio_AlumnosAulas = NA)
        )
      write.csv(wide_data, file, row.names = FALSE)
    }
  )
  
  
  
  
  
  
  
  
  # Módulo: Alumnos por docente (server.R)
  
  # Reactivo: Filtra dfAlumnosPorDocente según la entidad y el nivel (selección única)
  filteredAlumnosPorDocente <- reactive({
    dfAlumnosPorDocente %>%
      filter(N_ENTIDAD == input$ent_alumnosDocente,
             NIVEL == input$nivel_alumnosDocente) %>%
      # Extraer el año del PERIODO (ej. "2019-2020" → 2019)
      mutate(Year = as.numeric(substr(PERIODO, 1, 4)))
  })
  
  # Gráfico: Evolución del Ratio de Alumnos por Docente
  output$alumnosPorDocentePlot <- renderPlot({
    ggplot(filteredAlumnosPorDocente(), aes(x = Year, y = Ratio_AlumnosAulas,
                                            color = GRADO, linetype = CONTROL,
                                            group = interaction(GRADO, CONTROL))) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(x = "Año",
           y = "Ratio de Alumnos por Docente",
           title = "Evolución del Ratio de Alumnos por Docente",
           color = "Grado",
           linetype = "Control") +
      scale_x_continuous(breaks = sort(unique(filteredAlumnosPorDocente()$Year))) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  # Tabla: Datos en formato ancho, con filas identificadas por NIVEL, GRADO y CONTROL y columnas para cada Año
  output$alumnosPorDocenteTable <- DT::renderDT({
    wide_data <- filteredAlumnosPorDocente() %>%
      mutate(Year = as.character(Year)) %>%
      tidyr::pivot_wider(
        id_cols = c(NIVEL, GRADO, CONTROL),
        names_from = Year,
        values_from = Ratio_AlumnosAulas,
        values_fn = list(Ratio_AlumnosAulas = mean),  # En caso de duplicados, se toma la media
        values_fill = list(Ratio_AlumnosAulas = NA)
      )
    DT::datatable(wide_data, options = list(pageLength = 10)) %>%
      DT::formatRound(
        columns = setdiff(names(wide_data), c("NIVEL", "GRADO", "CONTROL")),
        digits = 2,
        mark = ","
      )
  })
  
  # Descarga: Exporta la tabla transformada a formato ancho a CSV
  output$downloadAlumnosPorDocente <- downloadHandler(
    filename = function() { paste0("AlumnosPorDocente_", Sys.Date(), ".csv") },
    content = function(file) {
      wide_data <- filteredAlumnosPorDocente() %>%
        mutate(Year = as.character(Year)) %>%
        tidyr::pivot_wider(
          id_cols = c(NIVEL, GRADO, CONTROL),
          names_from = Year,
          values_from = Ratio_AlumnosAulas,
          values_fn = list(Ratio_AlumnosAulas = mean),
          values_fill = list(Ratio_AlumnosAulas = NA)
        )
      write.csv(wide_data, file, row.names = FALSE)
    }
  )
  
  
  
  
  # Módulo: Estudiantes inscritos por nivel y control
  
  # Reactivo: Filtra dfEstudiantesNivelControl según la entidad y el nivel seleccionado
  filteredEstudiantesNivelControl <- reactive({
    dfEstudiantesNivelControl %>%
      filter(N_ENTIDAD == input$ent_estudiantesNivelControl,
             NIVEL %in% input$nivel_estudiantesNivelControl)
  })
  
  # Gráfico: Evolución de alumnos inscritos
  output$estudiantesNivelControlPlot <- renderPlot({
    ggplot(filteredEstudiantesNivelControl(), 
           aes(x = factor(PERIODO, levels = sort(unique(PERIODO))), 
               y = ALUMNOS, 
               color = CONTROL, 
               linetype = NIVEL,
               group = interaction(CONTROL, NIVEL))) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(x = "Periodo",
           y = "Número de Alumnos",
           title = "Estudiantes inscritos en escuelas públicas vs privadas",
           color = "Control",
           linetype = "Nivel") +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Tabla: Datos en formato ancho, filas identificadas por NIVEL y CONTROL y columnas por PERIODO
  output$estudiantesNivelControlTable <- DT::renderDT({
    wide_data <- filteredEstudiantesNivelControl() %>%
      mutate(PERIODO = as.character(PERIODO)) %>%
      tidyr::pivot_wider(
        id_cols = c(NIVEL, CONTROL),
        names_from = PERIODO,
        values_from = ALUMNOS,
        values_fn = list(ALUMNOS = sum),
        values_fill = list(ALUMNOS = NA)
      )
    DT::datatable(wide_data, options = list(pageLength = 10)) %>%
      DT::formatRound(
        columns = setdiff(names(wide_data), c("NIVEL", "CONTROL")),
        digits = 0,
        mark = ","
      )
  })
  
  # Descarga: Exporta la tabla en formato ancho a CSV
  output$downloadEstudiantesNivelControl <- downloadHandler(
    filename = function() { paste0("EstudiantesNivelControl_", Sys.Date(), ".csv") },
    content = function(file) {
      wide_data <- filteredEstudiantesNivelControl() %>%
        mutate(PERIODO = as.character(PERIODO)) %>%
        tidyr::pivot_wider(
          id_cols = c(NIVEL, CONTROL),
          names_from = PERIODO,
          values_from = ALUMNOS,
          values_fn = list(ALUMNOS = sum),
          values_fill = list(ALUMNOS = NA)
        )
      write.csv(wide_data, file, row.names = FALSE)
    }
  )
  
  
  
  
  
  
  # Módulo: Tamaño de escuelas privadas
  
  # Reactivo: Filtra dfTamanoPrivadas según el NIVEL seleccionado
  filteredTamanoPrivadas <- reactive({
    dfTamanoPrivadas %>%
      filter(NIVEL == input$nivel_tamanoPrivadas)
  })
  
  # Gráfico: Histograma de ALUMNOS con facetas por PERIODO y usando coord_flip() para que
  # ALUMNOS se muestre en el eje y.
  output$tamanoPrivadasPlot <- renderPlot({
    # Asegurarse de tener datos (aunque el dataframe de ejemplo es de 0 filas, en la app real tendrá datos)
    df <- filteredTamanoPrivadas()
    
    # Usamos geom_histogram para obtener un histograma; ajusta el binwidth según tus datos.
    ggplot(df, aes(x = ALUMNOS)) +
      geom_histogram(binwidth = 100, fill = "steelblue", color = "black") +
      facet_wrap(~ PERIODO) +
      labs(x = "Frecuencia", y = "Número de Alumnos", 
           title = "Distribución de escuelas privadas según su número de alumnos") +
      theme_minimal()
  })
  
  # Tabla: Agrupa los datos en bins para la variable ALUMNOS y los organiza en formato ancho.
  output$tamanoPrivadasTable <- DT::renderDT({
    df <- filteredTamanoPrivadas()
    # Definir los bins. Por ejemplo, desde 0 hasta el máximo más un margen, con ancho de 100.
    max_alumnos <- ifelse(nrow(df) > 0, max(df$ALUMNOS, na.rm = TRUE), 1000)
    bin_breaks <- seq(0, max_alumnos + 100, by = 100)
    
    df_bins <- df %>%
      mutate(Bin = cut(ALUMNOS, breaks = bin_breaks, include.lowest = TRUE)) %>%
      group_by(PERIODO, Bin) %>%
      summarise(Count = n(), .groups = "drop")
    
    # Convertir la información a formato ancho: cada fila es un PERIODO y cada columna un bin
    wide_data <- df_bins %>%
      tidyr::pivot_wider(
        id_cols = Bin,          # Las filas serán los intervalos (bins)
        names_from = PERIODO,   # Cada columna corresponde a un PERIODO (año)
        values_from = Count,
        values_fill = list(Count = 0)
      )
    
    
    DT::datatable(wide_data, options = list(pageLength = 10))
  })
  
  # Descarga: Exporta la tabla en formato ancho a CSV
  output$downloadTamanoPrivadas <- downloadHandler(
    filename = function() { paste0("TamanoPrivadas_", Sys.Date(), ".csv") },
    content = function(file) {
      df <- filteredTamanoPrivadas()
      max_alumnos <- ifelse(nrow(df) > 0, max(df$ALUMNOS, na.rm = TRUE), 1000)
      bin_breaks <- seq(0, max_alumnos + 100, by = 100)
      
      df_bins <- df %>%
        mutate(Bin = cut(ALUMNOS, breaks = bin_breaks, include.lowest = TRUE)) %>%
        group_by(PERIODO, Bin) %>%
        summarise(Count = n(), .groups = "drop")
      
      wide_data <- df_bins %>%
        tidyr::pivot_wider(
          id_cols = PERIODO,
          names_from = Bin,
          values_from = Count,
          values_fill = list(Count = 0)
        )
      write.csv(wide_data, file, row.names = FALSE)
    }
  )
  
  
  
  
  
  # Módulo: Estudiantes inscritos por control por ingreso
  
  # Reactivo: Filtra dfEstudiantesControlIngreso según entidad (múltiple) y nivel (único)
  filteredEstudiantesControlIngreso <- reactive({
    dfEstudiantesControlIngreso %>%
      filter(Entidad %in% input$ent_estudiantesControlIngreso)
  })
  
  # Gráfico: Años en eje x, Alumnos en eje y, facets por Quintil, color por Control
  output$estudiantesControlIngresoPlot <- renderPlot({
    # Aseguramos que "Año" sea numérico (si es carácter, conviértelo)
    ggplot(filteredEstudiantesControlIngreso(), 
           aes(x = as.numeric(Ano), y = Alumnos, color = Control, group = Control)) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      facet_wrap(~ Quintil) +
      labs(x = "Año",
           y = "Número de Alumnos",
           title = "Estudiantes inscritos por control por ingreso",
           color = "Control") +
      scale_x_continuous(breaks = sort(unique(as.numeric(filteredEstudiantesControlIngreso()$Año)))) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  # Tabla: Datos en formato ancho
  # Cada fila corresponde a la combinación de NIVEL, Quintil y Control,
  # y cada columna es un Año
  output$estudiantesControlIngresoTable <- DT::renderDT({
    wide_data <- filteredEstudiantesControlIngreso() %>%
      mutate(Ano = as.character(Ano)) %>%
      tidyr::pivot_wider(
        id_cols = c(Quintil, Control),
        names_from = Ano,
        values_from = Alumnos,
        values_fn = list(Alumnos = sum),
        values_fill = list(Alumnos = NA)
      )
    DT::datatable(wide_data, options = list(pageLength = 10)) %>%
      DT::formatRound(
        columns = setdiff(names(wide_data), c("Quintil", "Control")),
        digits = 0,
        mark = ","
      )
  })
  
  # Descarga: Exporta la tabla en formato ancho a CSV
  output$downloadEstudiantesControlIngreso <- downloadHandler(
    filename = function() { paste0("EstudiantesControlIngreso_", Sys.Date(), ".csv") },
    content = function(file) {
      wide_data <- filteredEstudiantesControlIngreso() %>%
        mutate(Año = as.character(Año)) %>%
        tidyr::pivot_wider(
          id_cols = c(Quintil, Control),
          names_from = Año,
          values_from = Alumnos,
          values_fn = list(Alumnos = sum),
          values_fill = list(Alumnos = NA)
        )
      write.csv(wide_data, file, row.names = FALSE)
    }
  )
  
  
  
  
  
  
  # Módulo: Distribución de estudiantes inscritos en colegios privados según rangos de gasto en educación por hogar
  
  # Gráfico: Histograma con GastoEduMensual en el eje x, usando la suma de Estudiantes (weight)
  # y faceteado por el año (Ano)
  output$estudiantesGastoEduPlot <- renderPlot({
    ggplot(dfEstudiantesGastoEdu, aes(x = GastoEduMensual, weight = Estudiantes)) +
      geom_histogram(binwidth = 2000, fill = "steelblue", color = "black") +
      facet_wrap(~ Ano) +
      labs(x = "Gasto Mensual en Educación por Hogar (MXN)",
           y = "Número de Estudiantes Inscritos",
           title = "Distribución de estudiantes inscritos en colegios privados\nsegún rangos de gasto en educación") +
      scale_x_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  # Tabla: Agrupa la data en bins (intervalos) para GastoEduMensual y la transforma a formato ancho.
  output$estudiantesGastoEduTable <- DT::renderDT({
    df <- dfEstudiantesGastoEdu
    # Definir los intervalos (bins). Se calcula el máximo y se define un binwidth de 500 MXN.
    max_gasto <- ifelse(nrow(df) > 0, max(df$GastoEduMensual, na.rm = TRUE), 5000)
    bin_breaks <- seq(0, max_gasto + 2000, by = 2000)
    
    df_bins <- df %>%
      mutate(Bin = cut(GastoEduMensual, breaks = bin_breaks, include.lowest = TRUE)) %>%
      group_by(Ano, Bin) %>%
      summarise(Count = sum(Estudiantes), .groups = "drop")
    
    # Pivot para que cada fila sea un intervalo y cada columna un año
    wide_data <- df_bins %>%
      tidyr::pivot_wider(
        id_cols = Bin,
        names_from = Ano,
        values_from = Count,
        values_fill = list(Count = 0)
      )
    
    DT::datatable(wide_data, options = list(pageLength = 10))
  })
  
  # Descarga: Exporta la tabla transformada a formato ancho a CSV
  output$downloadEstudiantesGastoEdu <- downloadHandler(
    filename = function() { paste0("EstudiantesGastoEdu_", Sys.Date(), ".csv") },
    content = function(file) {
      df <- dfEstudiantesGastoEdu
      max_gasto <- ifelse(nrow(df) > 0, max(df$GastoEduMensual, na.rm = TRUE), 5000)
      bin_breaks <- seq(0, max_gasto + 500, by = 500)
      
      df_bins <- df %>%
        mutate(Bin = cut(GastoEduMensual, breaks = bin_breaks, include.lowest = TRUE)) %>%
        group_by(Ano, Bin) %>%
        summarise(Count = sum(Estudiantes), .groups = "drop")
      
      wide_data <- df_bins %>%
        tidyr::pivot_wider(
          id_cols = Bin,
          names_from = Ano,
          values_from = Count,
          values_fill = list(Count = 0)
        )
      
      write.csv(wide_data, file, row.names = FALSE)
    }
  )
  
  
  
  
  
  # Módulo: Costo de la colegiatura
  
  # Reactivo: Filtra dfGastoColegiatura según los filtros seleccionados
  filteredGastoColegiatura <- reactive({
    dfGastoColegiatura %>%
      filter(Entidad %in% input$ent_gastoColegiatura,
             nivel %in% input$nivel_gastoColegiatura)
  })
  
  # Gráfico: Líneas y puntos
  output$gastoColegiaturaPlot <- renderPlot({
    data <- filteredGastoColegiatura()
    ggplot(data, aes(x = Ano, y = GastoPromedioPonderado, 
                     color = nivel, linetype = Entidad,
                     group = interaction(nivel, Entidad))) +
      geom_line(size = 1) +
      geom_point(size = 3) +
      labs(x = "Año",
           y = "Colegiatura anual promedio (MXN)",
           title = "Costo de la colegiatura",
           color = "Nivel",
           linetype = "Entidad") +
      scale_x_continuous(breaks = sort(unique(data$Ano))) +
      scale_y_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  # Tabla: Datos en formato ancho (filas: Entidad y nivel, columnas: Año)
  output$gastoColegiaturaTable <- DT::renderDT({
    wide_data <- filteredGastoColegiatura() %>%
      mutate(Ano = as.character(Ano)) %>%
      tidyr::pivot_wider(
        id_cols = c(Entidad, nivel),
        names_from = Ano,
        values_from = GastoPromedioPonderado,
        values_fn = list(GastoPromedioPonderado = mean),
        values_fill = list(GastoPromedioPonderado = NA)
      )
    DT::datatable(wide_data, options = list(pageLength = 10)) %>%
      DT::formatRound(
        columns = setdiff(names(wide_data), c("Entidad", "nivel")),
        digits = 0,
        mark = ","
      )
  })
  
  # Descarga: Exporta la tabla en formato ancho a CSV
  output$downloadGastoColegiatura <- downloadHandler(
    filename = function() { paste0("GastoColegiatura_", Sys.Date(), ".csv") },
    content = function(file) {
      wide_data <- filteredGastoColegiatura() %>%
        mutate(Ano = as.character(Ano)) %>%
        tidyr::pivot_wider(
          id_cols = c(Entidad, nivel),
          names_from = Ano,
          values_from = GastoPromedioPonderado,
          values_fn = list(GastoPromedioPonderado = mean),
          values_fill = list(GastoPromedioPonderado = NA)
        )
      write.csv(wide_data, file, row.names = FALSE)
    }
  )
  
  
  
  
  
  
  # Módulo: Costo de la Vivienda
  
  # Subconjunto para Alquiler (Renta)
  viviendaRentaData <- reactive({
    dfVivienda %>%
      select(Entidad, Tipo, Valor_prom_m2_ponderado) %>%
      filter(Tipo == "Renta") %>% 
      arrange(desc(Valor_prom_m2_ponderado))
  })
  
  output$viviendaRentaPlot <- renderPlot({
    data <- viviendaRentaData()
    ggplot(data, aes(x = Valor_prom_m2_ponderado, 
                     y = reorder(Entidad, Valor_prom_m2_ponderado))) +
      geom_col(fill = "steelblue") +
      labs(x = "Valor promedio por m² ponderado (MXN)",
           y = "Entidad",
           title = "Costo de la vivienda (Alquiler)") +
      scale_x_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  output$viviendaRentaTable <- DT::renderDT({
    data <- viviendaRentaData()
    DT::datatable(data, options = list(pageLength = 10)) %>%
      DT::formatRound("Valor_prom_m2_ponderado", digits = 0, mark = ",")
  })
  
  output$downloadViviendaRenta <- downloadHandler(
    filename = function() { paste0("Vivienda_Renta_", Sys.Date(), ".csv") },
    content = function(file) {
      data <- viviendaRentaData()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  # Subconjunto para Adquisición (Venta)
  viviendaVentaData <- reactive({
    dfVivienda %>% 
      select(Entidad, Tipo, Valor_prom_m2_ponderado) %>%
      filter(Tipo == "Venta") %>% 
      arrange(desc(Valor_prom_m2_ponderado))
  })
  
  output$viviendaVentaPlot <- renderPlot({
    data <- viviendaVentaData()
    ggplot(data, aes(x = Valor_prom_m2_ponderado, 
                     y = reorder(Entidad, Valor_prom_m2_ponderado))) +
      geom_col(fill = "darkorange") +
      labs(x = "Valor promedio por m² ponderado (MXN)",
           y = "Entidad",
           title = "Costo de la vivienda (Adquisición)") +
      scale_x_continuous(labels = scales::comma) +
      theme_minimal()
  })
  
  output$viviendaVentaTable <- DT::renderDT({
    data <- viviendaVentaData()
    DT::datatable(data, options = list(pageLength = 10)) %>%
      DT::formatRound("Valor_prom_m2_ponderado", digits = 0, mark = ",")
  })
  
  output$downloadViviendaVenta <- downloadHandler(
    filename = function() { paste0("Vivienda_Venta_", Sys.Date(), ".csv") },
    content = function(file) {
      data <- viviendaVentaData()
      write.csv(data, file, row.names = FALSE)
    }
  )
  
  
  
  
  
  
  # { Para los demás indicadores sigue el mismo patrón: 
  # define un reactive() que genere los datos,
  # usa renderPlot() para el gráfico,
  # renderDataTable() para la tabla y
  # downloadHandler() para la descarga. }
  
}

