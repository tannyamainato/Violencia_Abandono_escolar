#Cargando librerias
library(readr)
library(dplyr) 
library(tidyverse)
# Cargar el archivo CSV
panel_data <- read.csv("R_documents/data_proyecto_final/panel_IE.csv")
df <- read.csv("R_documents/data_proyecto_final/panel_IE.csv")

#Correlación variables respecto al abandono

# Ajustar los nombres de las columnas si es necesario (elimina espacios)
colnames(panel_data) <- gsub(" ", "_", colnames(panel_data))
View(colnames(panel_data))


# cambiar NA por 0.

df[is.na(df)] <- 0 
names(df)
# Seleccionar las variables de interés
#selected_columns <- df[, c("Abandono", "nviolencia", "victima_mujer", "Victima_Sin_Discapacidad", "Victima_Con_Discapacidad", 
               #                "NO_EXISTE_EMBARAZO", "SI_EXISTE_EMBARAZO", "Infractor.FUERA.del.sistema.educativo", 
                #               "Infractor.DENTRO.del.sistema.educativo", "Relación.Infractor.Pariente", "Relación.Infractor.Desconocido",
                 #              "Relación.Infractor.Enamorado.Novio", "Relación.Infractor.Estudiantes.del.establecimiento", 
                  #             "Relación.Infractor.Conocido.no.pariente", "Relación.Infractor.Docente", "Relación.Infractor.Compañero.de.aula",
                   #            "Relación.Infractor.Otro..Especifique.", "Relación.Infractor.Conserjes.Personal.de.limpieza", 
                   #            "Relación.Infractor.Choferes.de.transporte.escolar", "Relación.Infractor.Personal.administrativo.de.la.IE",
                   #            "Relación.Infractor.Autoridad.de.la.IE")] 

selected_columns <- df[, c(22, 23, 24, 25, 26, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52)] 

cor_matrix <- cor(selected_columns) 

# Regresión 
modelo <- lm(df[, 23] ~ df[, 22] + df[, 24] + df[, 25] + 
               df[, 26] + df[, 36] + df[, 37] + df[, 38] + df[, 39] + df[, 40] +
               df[, 41] + df[, 42] + df[, 43] + df[, 44] + df[, 45] + df[, 46] + 
               df[, 47] + df[, 48] + df[, 49] + df[, 50] + df[, 51] + df[, 52],data = df) 
summary(modelo) 
#
