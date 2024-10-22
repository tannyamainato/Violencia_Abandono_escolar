# Cristian Carrion-Cauja

# Librerias ---------------------------------------------------------------

library(dplyr)
library(collapse)
library(data.table)
library(ggplot2)
library(stringr)
library(fixest)

# Limpieza de datos -------------------------------------------------------

violencia = openxlsx::read.xlsx("C:/Users/cristiand.carrion/OneDrive - MINISTERIO DE EDUCACIÓN/Actividades_CCarrion/BasesImportantes/BuenVivir/06092024/data_vic_fechas.xlsx", detectDates = TRUE) %>% as.data.table();violencia %>% dim()

denuncias = openxlsx::read.xlsx("C:/Users/cristiand.carrion/OneDrive - MINISTERIO DE EDUCACIÓN/Actividades_CCarrion/BasesImportantes/BuenVivir/06092024/data_den_fechas_2.xlsx", detectDates = TRUE, sheet = "Sheet 1") %>% as.data.table(); denuncias %>% dim()

dta_abiertos_fin = fread("C:/Users/cristiand.carrion/OneDrive - MINISTERIO DE EDUCACIÓN/Actividades_CCarrion/BasesImportantes/Registro-Administrativo-Historico_2009-2022-Fin.csv") %>% 
  mutate(AMIE = toupper(AMIE)) #Exiten problema con instituciens que en codigo AMIE registra letras minúsculas (todas son MAYUSCULAS): Se transforma todos los caracteres en mayúscula

dta_abiertos_fin = dta_abiertos_fin %>% 
  arrange(desc(Año_lectivo)) %>% 
  select(#Año_lectivo, 
    AMIE, Régimen_Escolar) %>% ungroup() %>% 
  distinct(AMIE, .keep_all = T)

dta_abiertos_fin_frame = fread("C:/Users/cristiand.carrion/OneDrive - MINISTERIO DE EDUCACIÓN/Actividades_CCarrion/BasesImportantes/Registro-Administrativo-Historico_2009-2022-Fin.csv") %>% 
  mutate(AMIE = toupper(AMIE)) #Exiten problema con instituciens que en codigo AMIE registra letras minúsculas (todas son MAYUSCULAS): Se transforma todos los caracteres en mayúscula

#Correccion de nombres

to.plain <- function(s) {
  
  # 1 character substitutions
  old1 <- "šžþàáâãäåçèéêëìíîïðñòóôõöùúûüýŠŽÞÀÁÂÃÄÅÇÈÉÊËÌÍÎÏÐÑÒÓÔÕÖÙÚÛÜÝ"
  new1 <- "szyaaaaaaceeeeiiiidnooooouuuuyszyAAAAAACEEEEIIIIDNOOOOOUUUUY"
  s1 <- chartr(old1, new1, s)
  
  # 2 character substitutions
  old2 <- c("œ", "ß", "æ", "ø")
  new2 <- c("oe", "ss", "ae", "oe")
  s2 <- s1
  for(i in seq_along(old2)) s2 <- gsub(old2[i], new2[i], s2, fixed = TRUE)
  
  s2
}

# Transformar caracteres a mayúsculas - nombres y apellidos
tictoc::tic()
violencia[, `:=`(
  TXT_NOM_VICTIMA = toupper(TXT_NOM_VICTIMA)
)]
tictoc::toc()

# Transformar caracteres a mayúsculas - grado
tictoc::tic()
violencia[, `:=`(
  GRADO = toupper(GRADO)
)]
tictoc::toc()

# Transformar caracteres para union 
tictoc::tic()
violencia[, `:=`(
  TXT_NOM_VICTIMA = to.plain(TXT_NOM_VICTIMA)
)]
tictoc::toc()


violencia[, grado_violencia_aux := fcase(
  GRADO == "MAYOR A 36 MESES", 0,
  GRADO == "GRUPO 3 AÑOS", 1,
  GRADO == "GRUPO 4 AÑOS", 2,
  GRADO == "1ER AÑO BÁSICA", 3,
  GRADO == "2DO AÑO BÁSICA", 4,
  GRADO == "3ER AÑO BÁSICA", 5,
  GRADO == "4TO AÑO BÁSICA", 6,
  GRADO == "5TO AÑO BÁSICA", 7,
  GRADO == "6TO AÑO BÁSICA", 8,
  GRADO == "7MO AÑO BÁSICA", 9,
  GRADO == "8VO AÑO BÁSICA", 10,
  GRADO == "9NO AÑO BÁSICA", 11,
  GRADO == "10MO AÑO BÁSICA", 12,
  GRADO == "1ER AÑO BACHILLERATO", 13,
  GRADO == "2DO AÑO BACHILLERATO", 14,
  GRADO == "3ER AÑO BACHILLERATO", 15)
]

violencia[, DELITO_EDUCATIVO_aux := fcase(
  DELITO_EDUCATIVO == "Violación", "4",
  DELITO_EDUCATIVO == "Pornografía", "3",
  DELITO_EDUCATIVO == "Abuso Sexual", "2",
  DELITO_EDUCATIVO == "Acoso Sexual", "1",
  default = "0")
]


violencia = violencia %>% select(-RELACION_INFRACTOR) %>% 
  filter(ESTADO_CASO == 1) %>% 
  left_join(., denuncias %>% 
              filter(STS_ESTADO == 1) %>% 
              select(CODIGO_CASO, COD_AMIE, TXT_SEXO_INFRACTOR, GRUPO_INFRACTOR, RELACION_INFRACTOR, DELITO_FISCALIA), by = c("CODIGO_CASO")) %>% 
  filter(!is.na(COD_AMIE)) %>% 
  mutate(DELITO_EDUCATIVO_aux = ifelse(DELITO_EDUCATIVO_aux=="0", DELITO_FISCALIA, DELITO_EDUCATIVO_aux),
         DELITO_EDUCATIVO_aux = case_when(DELITO_EDUCATIVO_aux == "Artículo 171.- Violación" ~ "4",
                                          DELITO_EDUCATIVO_aux == "Artículo 170.- Abuso sexual" ~ "2",
                                          DELITO_EDUCATIVO_aux == "Artículo 166.- Acoso sexual." ~ "1",
                                          is.na(DELITO_EDUCATIVO_aux) ~ "0",
                                          T ~ DELITO_EDUCATIVO_aux))

violencia = violencia %>% 
  left_join(., dta_abiertos_fin, by = c("COD_AMIE" = "AMIE")); violencia$COD_AMIE %>% Hmisc::describe() ### se tiene 40 casos con IE, que no se tiene un historico (no se puede identificar los estudiantes que pertenecen a esta IE)

violencia <- violencia %>%
  mutate(
    AÑO_DEN = year(FEC_INGRESO_DENUNCIA_DIST),
    MES_DEN = month(FEC_INGRESO_DENUNCIA_DIST),
    Año_lectivo_DEN = case_when(
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2014 & MES_DEN >= 1 & MES_DEN <= 8 ~ "2013-2014 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2014 & MES_DEN >= 9 & MES_DEN <= 12 ~ "2014-2015 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2015 & MES_DEN >= 1 & MES_DEN <= 8 ~ "2014-2015 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2015 & MES_DEN >= 9 & MES_DEN <= 12 ~ "2015-2016 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2016 & MES_DEN >= 1 & MES_DEN <= 8 ~ "2015-2016 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2016 & MES_DEN >= 9 & MES_DEN <= 12 ~ "2016-2017 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2017 & MES_DEN >= 1 & MES_DEN <= 8 ~ "2016-2017 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2017 & MES_DEN >= 9 & MES_DEN <= 12 ~ "2017-2018 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2018 & MES_DEN >= 1 & MES_DEN <= 8 ~ "2017-2018 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2018 & MES_DEN >= 9 & MES_DEN <= 12 ~ "2018-2019 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2019 & MES_DEN >= 1 & MES_DEN <= 8 ~ "2018-2019 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2019 & MES_DEN >= 9 & MES_DEN <= 12 ~ "2019-2020 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2020 & MES_DEN >= 1 & MES_DEN <= 8 ~ "2019-2020 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2020 & MES_DEN >= 9 & MES_DEN <= 12 ~ "2020-2021 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2021 & MES_DEN >= 1 & MES_DEN <= 8 ~ "2020-2021 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2021 & MES_DEN >= 9 & MES_DEN <= 12 ~ "2021-2022 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2022 & MES_DEN >= 1 & MES_DEN <= 8 ~ "2021-2022 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2022 & MES_DEN >= 9 & MES_DEN <= 12 ~ "2022-2023 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2023 & MES_DEN >= 1 & MES_DEN <= 8 ~ "2022-2023 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2023 & MES_DEN >= 9 & MES_DEN <= 12 ~ "2023-2024 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2024 & MES_DEN >= 1 & MES_DEN <= 8 ~ "2023-2024 Fin",
      Régimen_Escolar == "Sierra" & AÑO_DEN == 2024 & MES_DEN >= 9 & MES_DEN <= 12 ~ "2024-2025 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2014 & MES_DEN >= 1 & MES_DEN <= 4 ~ "2013-2014 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2014 & MES_DEN >= 5 & MES_DEN <= 12 ~ "2014-2015 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2015 & MES_DEN >= 1 & MES_DEN <= 4 ~ "2014-2015 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2015 & MES_DEN >= 5 & MES_DEN <= 12 ~ "2015-2016 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2016 & MES_DEN >= 1 & MES_DEN <= 4 ~ "2015-2016 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2016 & MES_DEN >= 5 & MES_DEN <= 12 ~ "2016-2017 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2017 & MES_DEN >= 1 & MES_DEN <= 4 ~ "2016-2017 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2017 & MES_DEN >= 5 & MES_DEN <= 12 ~ "2017-2018 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2018 & MES_DEN >= 1 & MES_DEN <= 4 ~ "2017-2018 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2018 & MES_DEN >= 5 & MES_DEN <= 12 ~ "2018-2019 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2019 & MES_DEN >= 1 & MES_DEN <= 4 ~ "2018-2019 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2019 & MES_DEN >= 5 & MES_DEN <= 12 ~ "2019-2020 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2020 & MES_DEN >= 1 & MES_DEN <= 4 ~ "2019-2020 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2020 & MES_DEN >= 5 & MES_DEN <= 12 ~ "2020-2021 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2021 & MES_DEN >= 1 & MES_DEN <= 4 ~ "2020-2021 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2021 & MES_DEN >= 5 & MES_DEN <= 12 ~ "2021-2022 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2022 & MES_DEN >= 1 & MES_DEN <= 4 ~ "2021-2022 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2022 & MES_DEN >= 5 & MES_DEN <= 12 ~ "2022-2023 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2023 & MES_DEN >= 1 & MES_DEN <= 4 ~ "2022-2023 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2023 & MES_DEN >= 5 & MES_DEN <= 12 ~ "2023-2024 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2024 & MES_DEN >= 1 & MES_DEN <= 4 ~ "2023-2024 Fin",
      Régimen_Escolar == "Costa" & AÑO_DEN == 2024 & MES_DEN >= 5 & MES_DEN <= 12 ~ "2024-2025 Fin",
      TRUE ~ NA_character_
    )
  )

violencia = violencia %>% 
  filter(!is.na(Régimen_Escolar),
         !is.na(Año_lectivo_DEN)) %>%  # Se eliminan los 40 casos, 41 con Año_lectivo_DEN debido a que hay un registro de 2013
  mutate(EXISTE_PLAN_ACOMPANAMIENTO = ifelse(is.na(EXISTE_PLAN_ACOMPANAMIENTO),"NO", EXISTE_PLAN_ACOMPANAMIENTO),
         EXISTE_EMBARAZO = ifelse(is.na(EXISTE_EMBARAZO),"NO", EXISTE_EMBARAZO),
         RELACION_INFRACTOR_aux = ifelse(RELACION_INFRACTOR=="Otro (Especifique)",0,1),
         #FEC_INGRESO_DENUNCIA_DIST = ifelse(is.na(FEC_INGRESO_DENUNCIA_DIST), as.Date("2999-01-01"), FEC_INGRESO_DENUNCIA_DIST),
         #FECHA_ING_DEN_DIST = ifelse(is.na(FECHA_ING_DEN_DIST), as.Date("2999-01-01"), FECHA_ING_DEN_DIST)
  ) %>% 
  group_by(TXT_NOM_VICTIMA, NUM_CEDULA) %>% 
  mutate(NUM_EDAD = min(NUM_EDAD,na.rm = T),
         EXISTE_PLAN_ACOMPANAMIENTO = ifelse(EXISTE_PLAN_ACOMPANAMIENTO == "SI",1,0),
         EXISTE_EMBARAZO = ifelse(EXISTE_EMBARAZO == "SI",1,0),
         STS_DISCAPACIDAD = ifelse(STS_DISCAPACIDAD == "SI",1,0),
         FEC_INGRESO_DENUNCIA_DIST = min(FEC_INGRESO_DENUNCIA_DIST, na.rm = T),
         FECHA_ING_DEN_DIST = min(FECHA_ING_DEN_DIST, na.rm = T),
         grado_violencia_aux = min(grado_violencia_aux,na.rm = T),
         
         STS_DISCAPACIDAD = max(STS_DISCAPACIDAD,na.rm = T),
         EXISTE_PLAN_ACOMPANAMIENTO = max(EXISTE_PLAN_ACOMPANAMIENTO, na.rm = T),
         EXISTE_EMBARAZO = max(EXISTE_EMBARAZO, na.rm = T),
  ) %>% 
  group_by(TXT_NOM_VICTIMA, NUM_CEDULA, COD_AMIE, Año_lectivo_DEN, #NUM_EDAD, 
           TXT_SEXO, STS_DISCAPACIDAD, #GRADO, 
           grado_violencia_aux, #EXISTE_PLAN_ACOMPANAMIENTO, 
           #FEC_INGRESO_DENUNCIA_DIST, #DELITO_EDUCATIVO,#
           RELACION_INFRACTOR, 
           GRUPO_INFRACTOR,
           TXT_NOM_ZONA, COD_PROVINCIA, COD_CANTON, #COD_PARROQUIA, 
           TXT_SEXO_INFRACTOR,
           #EXISTE_EMBARAZO, FECHA_ING_DEN_DIST
  ) %>% 
  summarise(
    NUM_EDAD,
    DELITO_EDUCATIVO_aux = max(DELITO_EDUCATIVO_aux, na.rm = T), 
    #RELACION_INFRACTOR_aux = max(RELACION_INFRACTOR_aux, na.rm = T), 
    EXISTE_PLAN_ACOMPANAMIENTO = max(EXISTE_PLAN_ACOMPANAMIENTO, na.rm = T),
    EXISTE_EMBARAZO = max(EXISTE_EMBARAZO, na.rm = T),
    FEC_INGRESO_DENUNCIA_DIST = min(FEC_INGRESO_DENUNCIA_DIST, na.rm = T),
    FECHA_ING_DEN_DIST = min(FECHA_ING_DEN_DIST, na.rm = T)
  ) %>% 
  filter(FECHA_ING_DEN_DIST!=Inf) %>% 
  group_by(TXT_NOM_VICTIMA) %>%
  filter(!is.na(NUM_CEDULA) | all(is.na(NUM_CEDULA))) %>%
  slice(1) %>% ungroup() %>% as.data.table()

# Considerar que para Fin solo se tiene 2022 (unica manera de ver el numero de estudaintes que Abandonaron [Descomopisicion de la matricula])
violencia %>% group_by(Año_lectivo_DEN) %>% tally()


## Union data de victimas y datosAbiertos ----------------------------------

chek_tbl = violencia[, .("NViolencia" = .N), by = .(Año_lectivo_DEN, COD_AMIE)]
#chek_tbl = dcast(chek_tbl, apellidos_nombres + FechaDeNacimiento ~ prd_nombre, value.var = "N")

chek_tbl2 = violencia[, .("NViolencia" = .N), by = .(Año_lectivo_DEN, COD_AMIE, TXT_SEXO)]
chek_tbl2 = chek_tbl2%>% 
  tidyr::pivot_wider(names_from = `TXT_SEXO`,
                     values_from = c(NViolencia)) %>% 
  rename(Victima_MUJER = MUJER,
         Victima_HOMBRE = HOMBRE)

chek_tbl3 =sadsa



violencia[, .("NViolencia" = .N), by = .(Año_lectivo_DEN, COD_AMIE, TXT_SEXO_INFRACTOR)]
chek_tbl3 = chek_tbl3%>% 
  tidyr::pivot_wider(names_from = `TXT_SEXO_INFRACTOR`,
                     values_from = c(NViolencia)) %>% 
  rename(Infractor_MUJER = MUJER,
         Infractor_HOMBRE = HOMBRE,
         Infractor_Desconoce = `NA`)

chek_tbl4 = violencia[, .("NViolencia" = .N), by = .(Año_lectivo_DEN, COD_AMIE, DELITO_EDUCATIVO_aux)]
chek_tbl4 = chek_tbl4%>% 
  tidyr::pivot_wider(names_from = `DELITO_EDUCATIVO_aux`,
                     values_from = c(NViolencia)) %>% 
  rename(Violación = `4`,
         Pornografía = `3`,
         `Abuso Sexual` = `2`,
         `Acoso Sexual` = `1`,
         `Otro` = `0`)

chek_tbl5 = violencia[, .("NViolencia" = .N), by = .(Año_lectivo_DEN, COD_AMIE, STS_DISCAPACIDAD)]
chek_tbl5 = chek_tbl5%>% 
  tidyr::pivot_wider(names_from = `STS_DISCAPACIDAD`,
                     values_from = c(NViolencia)) %>% 
  rename(Victima_Sin_Discapacidad = `0`,
         Victima_Con_Discapacidad = `1`)

chek_tbl6 = violencia[, .("NViolencia" = .N), by = .(Año_lectivo_DEN, COD_AMIE, EXISTE_EMBARAZO)]
chek_tbl6 = chek_tbl6%>% 
  tidyr::pivot_wider(names_from = `EXISTE_EMBARAZO`,
                     values_from = c(NViolencia)) %>% 
  rename(NO_EXISTE_EMBARAZO = `0`,
         SI_EXISTE_EMBARAZO = `1`)

chek_tbl7 = violencia[, .("NViolencia" = .N), by = .(Año_lectivo_DEN, COD_AMIE, GRUPO_INFRACTOR)]
chek_tbl7 = chek_tbl7%>% 
  tidyr::pivot_wider(names_from = `GRUPO_INFRACTOR`,
                     values_from = c(NViolencia))

chek_tbl8 = violencia[, .("NViolencia" = .N), by = .(Año_lectivo_DEN, COD_AMIE, RELACION_INFRACTOR)]
chek_tbl8 = chek_tbl8 %>% 
  tidyr::pivot_wider(names_from = `RELACION_INFRACTOR`,
                     values_from = c(NViolencia))
colnames(chek_tbl8)[3:ncol(chek_tbl8)] <- paste("Relación Infractor", colnames(chek_tbl8)[3:ncol(chek_tbl8)])



# Union de bases y exportación de data ------------------------------------

base_violencia_DAbiertos_14_22 = dta_abiertos_fin_frame %>% 
  filter(!Año_lectivo %in% c("2009-2010 Fin", "2010-2011 Fin", "2011-2012 Fin", "2012-2013 Fin", "2013-2014 Fin")) %>% 
  left_join(., chek_tbl, by = c("Año_lectivo" = "Año_lectivo_DEN", "AMIE" = "COD_AMIE")) %>% 
  left_join(., chek_tbl2, by = c("Año_lectivo" = "Año_lectivo_DEN", "AMIE" = "COD_AMIE")) %>% 
  left_join(., chek_tbl3, by = c("Año_lectivo" = "Año_lectivo_DEN", "AMIE" = "COD_AMIE")) %>% 
  left_join(., chek_tbl4, by = c("Año_lectivo" = "Año_lectivo_DEN", "AMIE" = "COD_AMIE")) %>% 
  left_join(., chek_tbl5, by = c("Año_lectivo" = "Año_lectivo_DEN", "AMIE" = "COD_AMIE")) %>% 
  left_join(., chek_tbl6, by = c("Año_lectivo" = "Año_lectivo_DEN", "AMIE" = "COD_AMIE")) %>% 
  left_join(., chek_tbl7, by = c("Año_lectivo" = "Año_lectivo_DEN", "AMIE" = "COD_AMIE")) %>% 
  left_join(., chek_tbl8, by = c("Año_lectivo" = "Año_lectivo_DEN", "AMIE" = "COD_AMIE"))

fwrite(base_violencia_DAbiertos_14_22, "C:/Users/cristiand.carrion/OneDrive - MINISTERIO DE EDUCACIÓN/Actividades_CCarrion/papers/1AbanonoViolencia/1LimpiezaData/panel_IE.csv")



# Modelo OLS ------------------------------------------------------------------

base_combinada_1 <- fread("C:/Users/cristiand.carrion/OneDrive - MINISTERIO DE EDUCACIÓN/Actividades_CCarrion/papers/1AbanonoViolencia/1LimpiezaData/panel_IE.csv")

#Extraer el año lectivo del hecho de la víctima
base_combinada_1 <- base_combinada_1 %>%
  mutate(año_lectivo_ini = str_sub(Año_lectivo, 1, 9))

#Función que extrae el primer año del componente de año lectivo

base_combinada_1 <- base_combinada_1 %>%
  mutate(
    anio_lectivo_ini_4d = substr(año_lectivo_ini, 1, 4)
  )

base_combinada_1 <- base_combinada_1 %>%
  mutate(
    anio_lectivo_ini_4d = substr(año_lectivo_ini, 1, 4),
    # Convertimos los años a valores numéricos restando 2008 para seguir el patrón (2009 = 1, 2010 = 2, ...)
    anio_ini_valor = as.numeric(anio_lectivo_ini_4d) - 2008
  )


# Paso 4: Encontrar el primer año con un caso de violencia por institución
base_combinada_1 <- base_combinada_1 %>%
  group_by(AMIE) %>%  # Agrupar por institución
  mutate(primer_anio_con_caso = min(anio_ini_valor[!is.na(NViolencia)], na.rm = TRUE)) %>%
  ungroup()

# Paso 5: Crear la variable dummie_evento que será 1 a partir del primer evento de violencia en adelante
base_combinada_1 <- base_combinada_1 %>%
  mutate(dummie_evento = ifelse(anio_ini_valor >= primer_anio_con_caso, 1, 0))

base_combinada_1 <- base_combinada_1 %>%
  group_by(AMIE) %>%  # Agrupar por institución
  
  # Paso 5: Crear la variable dummie_acumulado que suma 1 cuando aparece un caso de violencia
  mutate(
    dummie_acumulado = ifelse(!is.na(NViolencia), 1, 0),  # Coloca 1 donde hay casos
    dummie_acumulado = cumsum(dummie_acumulado)  # Realiza la suma acumulada por institución
  ) %>%
  ungroup()

base_combinada_1 = base_combinada_1 %>% 
  mutate(anio_lectivo_ini_4d = as.numeric(anio_lectivo_ini_4d),
         NViolencia = ifelse(is.na(NViolencia),0,NViolencia))

test = base_combinada_1 %>%
  group_by(anio_lectivo_ini_4d, AMIE) %>%
  mutate(Abandono_NoPromovidos = Abandono+No.promovidos,
         tasa_NoPromovidos = No.promovidos / Total_estudiantes,
         tasa_abandono = Abandono / Total_estudiantes,
         tasa_abandono_noPromovidos = (Abandono+No.promovidos) / Total_estudiantes) %>%
  arrange(anio_lectivo_ini_4d, AMIE) %>%
  group_by(AMIE) %>%
  mutate(
    ano_violencia = min(anio_lectivo_ini_4d[NViolencia > 0], na.rm = TRUE),
    time = anio_lectivo_ini_4d - ano_violencia,
    time_aux = ifelse(time == -Inf, anio_lectivo_ini_4d - 2018, time)
  ) %>%
  ungroup() %>%
  # Creando la variable D
  group_by(AMIE) %>%
  mutate(
    D = as.factor(ifelse(max(NViolencia > 0, na.rm = T) > 0, 1, 0)),
    # Corrigiendo la creación de dummie_evento
    dummie_evento = as.factor(ifelse(time_aux >= 0, 1, 0)),
    anio_lectivo_ini_4d = as.factor(anio_lectivo_ini_4d),
    n_registros = n()
  )


## Regresion ---------------------------------------------------------------

mdl1 = lm(Abandono~factor(D)+factor(anio_lectivo_ini_4d), 
          data = test)

summary(mdl1)


# Aplicando Diff-Diff -----------------------------------------------------

mdl2 = feols(tasa_abandono ~ i(time_aux, D, ref=-1,ref2=0) | AMIE + time_aux, 
             data = test)
summary(mdl2)
etable(mdl2)
coefplot(mdl2)
