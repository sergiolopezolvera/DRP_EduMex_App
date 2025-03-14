# global.R
# Este archivo se ejecuta antes que ui.R y server.R, por lo que es ideal para cargar librerías y datos

# Cargar las librerías necesarias
library(shiny)
library(dplyr)
library(DT)

# Cargar dataframes desde archivos RDS
# Asegúrate de que la carpeta 'data' esté en el mismo directorio que esta app
dfPoblacion <- readRDS("data/Poblacion.rds")
dfInmigracionInt <- readRDS("data/Inmigracion_int.rds")
dfMigracionNac <- readRDS("data/Migracion_nac.rds")
dfSalarioMinimo <- readRDS("data/SalariosMinimos.rds")
dfSalarioProfesores <- readRDS("data/SalariosProfesores.rds")
dfInflacion <- readRDS("data/Inflacion.rds")
dfSituacionLaboral <- readRDS("data/PadresTrabajanCom.rds")
dfGastoEducacion <- readRDS("data/GastoEducacion.rds")
dfEstudiantesNivelControl <- readRDS("data/EstudiantesNivelControl.rds") %>% mutate(CONTROL = recode(CONTROL, "PÃºblico" = "Público"),
                                                                                    NIVEL = recode(NIVEL, "TÃ©cnica" = "Técnica"))
dfAlumnosPorAula <- readRDS("data/AlumnosPorAula.rds")
dfAlumnosPorDocente <- readRDS("data/AlumnosPorDocente.rds")
dfTamanoPrivadas <- readRDS("data/TamanoColegiosPrivados.rds") %>% mutate(PERIODO = factor(PERIODO, c("2019-2020", "2020-2021", "2021-2022", "2022-2023", "2023-2024")))
dfEstudiantesControlIngreso <- readRDS("data/EstudiantesControlIngreso.rds")
dfEstudiantesGastoEdu <- readRDS("data/EstudiantesGastoEdu.rds")
dfGastoColegiatura <- readRDS("data/GastoColegiatura.rds")
dfVivienda <- readRDS("data/VentasRentas.rds")


# Aquí puedes agregar cualquier otra función o dato que necesites para la app
