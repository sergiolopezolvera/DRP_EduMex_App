# ui.R
library(shinydashboard)
library(shiny)

ui <- dashboardPage(
  dashboardHeader(title = "Educación en México"),
  
  dashboardSidebar(
    # Sidebar: Menú de Navegación
    sidebarMenu(
      menuItem("Demografía y Migración", icon = icon("users"),
               menuSubItem("Población por edad", tabName = "demo"),
               menuSubItem("Inmigración internacional", tabName = "inmigracionInt"),
               menuSubItem("Migración Nacional", tabName = "migracionNac")
               ),
      menuItem("Hogares", icon = icon("house"),
               menuSubItem("Situación laboral de los padres", tabName = "situacionLaboral"),
               menuSubItem("Gasto en educación", tabName = "gastoEducacion")
               ),
      menuItem("Educación", icon = icon("graduation-cap"),
               menuSubItem("Alumnos (públicas vs privadas)", tabName = "estudiantesNivelControl"),
               menuSubItem("Alumnos por control por ingreso", tabName = "estudiantesNivelControlIngreso"),
               menuSubItem("Tamaño de escuelas privadas", tabName = "tamanoPrivadas"),
               menuSubItem("Alumnos en privadas por gasto" , tabName = "estudiantesGastoEdu"),
               menuSubItem("Costo de colegiatura privada", tabName = "gastoColegiatura"),
               menuSubItem("Alumnos por aula", tabName = "alumnosAula"),
               menuSubItem("Alumnos por docente", tabName = "alumnosDocente")
      ),
      menuItem("Macroeconomía y Vivienda", icon = icon("chart-line"),
               menuSubItem("Inflación", tabName = "infla"),
               menuSubItem("Salario Mínimo", tabName = "salMin"),
               menuSubItem("Salario de profesores", tabName = "salProf"),
               menuSubItem("Costo de la Vivienda", tabName = "vivienda"))
    )
  ),
  
  dashboardBody(
    # Body: Contenido de las pestañas
    tabItems(
      
      # { Demografía y Migración }
      tabItem(tabName = "demo",
              h2("Demografía y Migración"),
              fluidRow(
                # Filtros y botón de descarga en un box
                box(
                  width = 4,
                  selectInput("entidad", "Seleccione Entidad:",
                              choices = unique(dfPoblacion$Entidad),
                              multiple = TRUE,
                              selected = unique(dfPoblacion$Entidad)[1]),
                  selectInput("grupo", "Seleccione Grupo de Edad:",
                              choices = unique(dfPoblacion$Grupo),
                              multiple = TRUE,
                              selected = unique(dfPoblacion$Grupo)[1]),
                  downloadButton("downloadPopulation", "Descargar CSV")
                ),
                # Gráfico en otro box
                box(
                  width = 8,
                  plotOutput("populationPlot")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  DT::DTOutput("populationTable")
                )
              )
      ),
      
      # { Inmigración Internacional }
      tabItem(tabName = "inmigracionInt",
              h2("Inmigración Internacional"),
              fluidRow(
                # Filtros y botón de descarga
                box(
                  width = 4,
                  selectInput("entidadInm", "Seleccione Entidad:",
                              choices = unique(dfInmigracionInt$Entidad),
                              multiple = TRUE,
                              selected = unique(dfInmigracionInt$Entidad)[1]),
                  downloadButton("downloadInmigracionInt", "Descargar CSV")
                ),
                # Gráfico
                box(
                  width = 8,
                  plotOutput("inmigracionIntPlot")
                )
              ),
              fluidRow(
                # Tabla con datos en formato ancho
                box(
                  width = 12,
                  DT::DTOutput("inmigracionIntTable")
                )
              )
      ),
      # } Fin Inmigración Internacional
      tabItem(tabName = "migracionNac",
              h2("Migración Nacional"),
              fluidRow(
                # Filtros y botón de descarga en un box
                box(
                  width = 4,
                  selectInput("entidad_mn", "Seleccione Entidad:",
                              choices = unique(dfMigracionNac$Entidad),
                              multiple = TRUE,
                              selected = unique(dfMigracionNac$Entidad)[1]),
                  selectInput("tipo_mn", "Seleccione Tipo:",
                              choices = unique(dfMigracionNac$Tipo),
                              multiple = TRUE,
                              selected = unique(dfMigracionNac$Tipo)[1]),
                  downloadButton("downloadMigracionNac", "Descargar CSV")
                ),
                # Gráfico en otro box
                box(
                  width = 8,
                  plotOutput("migracionNacPlot")
                )
              ),
              fluidRow(
                box(
                  width = 12,
                  DT::DTOutput("migracionNacTable")
                )
              )
      ),
      
      
      
 #--------     
      
 #------    
      
      # { Inflación }
       tabItem(tabName = "infla",
               h2("Inflación"),
               fluidRow(
                 box(
                     width = 12,
                     plotOutput("inflacion"))
                 ),
               fluidRow(
                 box(
                   width = 2,
                   downloadButton("downloadInflacion", "Descargar CSV")
                 )
               ),
               fluidRow(
                 box(
                   width = 12,
                   DT::DTOutput("inflacionTable")
                 )
               )
       ),
 
       # { Salario Mínimo }
       tabItem(tabName = "salMin",
               h2("Salario Mínimo"),
               fluidRow(
                 box(
                   width = 12,
                   plotOutput("salariominimo"))
               ),
               fluidRow(
                 box(
                   width = 2,
                   downloadButton("downloadSalariominimo", "Descargar CSV")
                 )
               ),
               fluidRow(
                 box(
                   width = 12,
                   DT::DTOutput("salarioMinimoTable")
                 )
               )
       ),

 
 
 # En ui.R, dentro del tabItem correspondiente (por ejemplo, "salProf")
       tabItem(tabName = "salProf",
               h2("Salario de profesores"),
               fluidRow(
                 box(
                   width = 4,
                   selectInput("ent_salProf", "Seleccione Entidad:",
                               choices = unique(dfSalarioProfesores$ENTIDAD),
                               multiple = TRUE,
                               selected = unique(dfSalarioProfesores$ENTIDAD)[1]),
                   downloadButton("downloadSalariosProfesores", "Descargar CSV")
                 ),
                 box(
                   width = 8,
                   plotOutput("salariosProfesoresPlot")  # Si tienes un gráfico, por ejemplo
                 )
               ),
               fluidRow(
                 box(
                   width = 12,
                   DT::DTOutput("salariosProfesoresTable")
                 )
               )
       ),
       # En ui.R, dentro del dashboardBody > tabItems:
       # En ui.R, dentro de dashboardBody > tabItems:
       tabItem(tabName = "situacionLaboral",
               h2("Situación laboral de los padres"),
               tabBox(
                 width = 12,
                 # Pestaña para Quintiles (excluyendo Total)
                 tabPanel("Por Quintiles",
                          fluidRow(
                            box(
                              width = 12,
                              plotOutput("situacionLaboralPlot")
                            )
                          ),
                          fluidRow(
                            box(
                              width = 12,
                              downloadButton("downloadSituacionLaboral", "Descargar CSV")
                            )
                          ),
                          fluidRow(
                            box(
                              width = 12,
                              DT::DTOutput("situacionLaboralTable")
                            )
                          )
                 ),
                 # Pestaña para Total
                 tabPanel("Total",
                          fluidRow(
                            box(
                              width = 12,
                              plotOutput("situacionLaboralTotalPlot")
                            )
                          ),
                          fluidRow(
                            box(
                              width = 12,
                              downloadButton("downloadSituacionLaboralTotal", "Descargar CSV")
                            )
                          ),
                          fluidRow(
                            box(
                              width = 12,
                              DT::DTOutput("situacionLaboralTotalTable")
                            )
                          )
                 )
               )
       ),
       # En ui.R, dentro del tabItem correspondiente (por ejemplo, "salProf")
       tabItem(tabName = "gastoEducacion",
               h2("Gasto en educación"),
               fluidRow(
                 box(
                   width = 4,
                   selectInput("ent_gastoEdu", "Seleccione Entidad:",
                               choices = unique(dfGastoEducacion$Entidad),
                               multiple = TRUE,
                               selected = unique(dfGastoEducacion$Entidad)[1]),
                   downloadButton("downloadGastoEducacion", "Descargar CSV")
                 ),
                 box(
                   width = 8,
                   plotOutput("gastoEducacionPlot")  # Si tienes un gráfico, por ejemplo
                 )
               ),
               fluidRow(
                 box(
                   width = 12,
                   DT::DTOutput("gastoEducacionTable")
                 )
               )
       ),
       tabItem(tabName = "alumnosAula",
               h2("Estudiantes por aula por grado"),
               fluidRow(
                 box(
                   width = 4,
                   selectInput("ent_alumnosAula", "Seleccione Entidad:",
                               choices = unique(dfAlumnosPorAula$N_ENTIDAD),
                               selected = unique(dfAlumnosPorAula$N_ENTIDAD)[1]),
                   selectInput("nivel_alumnosAula", "Seleccione Nivel:",
                               choices = unique(dfAlumnosPorAula$NIVEL),
                               selected = unique(dfAlumnosPorAula$NIVEL)[1]),
                   downloadButton("downloadAlumnosPorAula", "Descargar CSV")
                 ),
                 box(
                   width = 8,
                   plotOutput("alumnosPorAulaPlot")
                 )
               ),
               fluidRow(
                 box(
                   width = 12,
                   DT::DTOutput("alumnosPorAulaTable")
                 )
               )
       ),
       tabItem(tabName = "alumnosDocente",
               h2("Estudiantes por docente por grado"),
               fluidRow(
                 box(
                   width = 4,
                   selectInput("ent_alumnosDocente", "Seleccione Entidad:",
                               choices = unique(dfAlumnosPorDocente$N_ENTIDAD),
                               selected = unique(dfAlumnosPorDocente$N_ENTIDAD)[1]),
                   selectInput("nivel_alumnosDocente", "Seleccione Nivel:",
                               choices = unique(dfAlumnosPorDocente$NIVEL),
                               selected = unique(dfAlumnosPorDocente$NIVEL)[1]),
                   downloadButton("downloadAlumnosPorDocente", "Descargar CSV")
                 ),
                 box(
                   width = 8,
                   plotOutput("alumnosPorDocentePlot")
                 )
               ),
               fluidRow(
                 box(
                   width = 12,
                   DT::DTOutput("alumnosPorDocenteTable")
                 )
               )
       ),
 
 
 
 
 
 
       tabItem(tabName = "estudiantesNivelControl",
               h2("Estudiantes inscritos en escuelas públicas vs privadas"),
               fluidRow(
                 box(
                   width = 4,
                   selectInput("ent_estudiantesNivelControl", "Seleccione Entidad:",
                               choices = unique(dfEstudiantesNivelControl$N_ENTIDAD),
                               selected = unique(dfEstudiantesNivelControl$N_ENTIDAD)[1]),
                   selectInput("nivel_estudiantesNivelControl", "Seleccione Nivel:",
                               choices = unique(dfEstudiantesNivelControl$NIVEL),
                               selected = unique(dfEstudiantesNivelControl$NIVEL)[1]),
                   downloadButton("downloadEstudiantesNivelControl", "Descargar CSV")
                 ),
                 box(
                   width = 8,
                   plotOutput("estudiantesNivelControlPlot")
                 )
               ),
               fluidRow(
                 box(
                   width = 12,
                   DT::DTOutput("estudiantesNivelControlTable")
                 )
               )
       ),
 
 
       tabItem(tabName = "tamanoPrivadas",
               h2("Tamaño de escuelas privadas"),
               fluidRow(
                 box(
                   width = 4,
                   selectInput("nivel_tamanoPrivadas", "Seleccione Nivel:",
                               choices = unique(dfTamanoPrivadas$NIVEL),
                               selected = unique(dfTamanoPrivadas$NIVEL)[1]),
                   downloadButton("downloadTamanoPrivadas", "Descargar CSV")
                 ),
                 box(
                   width = 8,
                   plotOutput("tamanoPrivadasPlot")
                 )
               ),
               fluidRow(
                 box(
                   width = 12,
                   DT::DTOutput("tamanoPrivadasTable")
                 )
               )
       ),
 
 
 
 
       tabItem(tabName = "estudiantesNivelControlIngreso",
               h2("Estudiantes por control por ingreso"),
               fluidRow(
                 box(
                   width = 4,
                   selectInput("ent_estudiantesControlIngreso", "Seleccione Entidad:",
                               choices = unique(dfEstudiantesControlIngreso$Entidad),
                               multiple = TRUE,
                               selected = unique(dfEstudiantesControlIngreso$Entidad)[1]),
                   downloadButton("downloadEstudiantesControlIngreso", "Descargar CSV")
                 ),
                 box(
                   width = 8,
                   plotOutput("estudiantesControlIngresoPlot")
                 )
               ),
               fluidRow(
                 box(
                   width = 12,
                   DT::DTOutput("estudiantesControlIngresoTable")
                 )
               )
       ),
 
 
 
 
 
 
         # En ui.R, dentro de dashboardBody > tabItems:
         tabItem(tabName = "estudiantesGastoEdu",
                 h2("Distribución de estudiantes inscritos en colegios privados según rangos de gasto en educación por hogar"),
                 fluidRow(
                   box(
                     width = 4,
                     # Puedes agregar aquí filtros adicionales si fueran necesarios
                     downloadButton("downloadEstudiantesGastoEdu", "Descargar CSV")
                   ),
                   box(
                     width = 8,
                     plotOutput("estudiantesGastoEduPlot")
                   )
                 ),
                 fluidRow(
                   box(
                     width = 12,
                     DT::DTOutput("estudiantesGastoEduTable")
                   )
                 )
         ),
 
 
 
 
         tabItem(tabName = "gastoColegiatura",
                 h2("Costo de la colegiatura"),
                 fluidRow(
                   box(
                     width = 4,
                     selectInput("ent_gastoColegiatura", "Seleccione Entidad:",
                                 choices = unique(dfGastoColegiatura$Entidad),
                                 multiple = TRUE,
                                 selected = unique(dfGastoColegiatura$Entidad)[1]),
                     selectInput("nivel_gastoColegiatura", "Seleccione Nivel:",
                                 choices = unique(dfGastoColegiatura$nivel),
                                 multiple = TRUE,
                                 selected = unique(dfGastoColegiatura$nivel)[1]),
                     downloadButton("downloadGastoColegiatura", "Descargar CSV")
                   ),
                   box(
                     width = 8,
                     plotOutput("gastoColegiaturaPlot")
                   )
                 ),
                 fluidRow(
                   box(
                     width = 12,
                     DT::DTOutput("gastoColegiaturaTable")
                   )
                 )
         ),
 
 
 
 
 
         tabItem(tabName = "vivienda",
                 h2("Costo de la Vivienda"),
                 tabBox(
                   width = 12,
                   # Pestaña para Quintiles (excluyendo Total)
                   tabPanel("Alquiler",
                            fluidRow(
                              box(
                                width = 12,
                                plotOutput("viviendaRentaPlot")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 12,
                                downloadButton("downloadViviendaRenta", "Descargar CSV")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 12,
                                DT::DTOutput("viviendaRentaTable")
                              )
                            )
                   ),
                   # Pestaña para Total
                   tabPanel("Adquisición",
                            fluidRow(
                              box(
                                width = 12,
                                plotOutput("viviendaVentaPlot")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 12,
                                downloadButton("downloadViviendaVenta", "Descargar CSV")
                              )
                            ),
                            fluidRow(
                              box(
                                width = 12,
                                DT::DTOutput("viviendaVentaTable")
                              )
                            )
                   )
                 ),
                 p("Datos de localidades con 100,000 o más habitantes (ciudades principales)")
         )
 
       
 
     
      
    )
  )
)

shinyUI(ui)
