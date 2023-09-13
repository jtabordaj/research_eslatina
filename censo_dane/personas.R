source("./censo_dane/dependencies.R")

# DF Personas

personas <- get(dfsEnvir[4])

personas <- personas %>% mutate(esMujer = ifelse(P_SEXO == 2, 1, 0)) # Si es mujer = 1

personas <- personas %>% mutate(grupoEdad = P_EDADR) # Mantiene categorias DANE

personas <- personas %>% mutate(noGrupoEtnico = ifelse(PA1_GRP_ETNIC == 6 | PA1_GRP_ETNIC == 9, 1, 0)) # 1 = No pertenece a grupos etnicos
personas <- personas %>% mutate(geIndigena = ifelse(PA1_GRP_ETNIC == 1, 1, 0)) # 1 = Indigena
personas <- personas %>% mutate(geGitano = ifelse(PA1_GRP_ETNIC == 2, 1, 0)) # 1 = Gitano/Rrom
personas <- personas %>% mutate(geRaizal = ifelse(PA1_GRP_ETNIC == 3, 1, 0)) # 1 = Raizal
personas <- personas %>% mutate(gePalenquero = ifelse(PA1_GRP_ETNIC == 4, 1, 0)) # 1 = Palenquero
personas <- personas %>% mutate(geAfro = ifelse(PA1_GRP_ETNIC == 5, 1, 0)) # 1 = Afro

personas <- personas %>% mutate(lugarNacimiento = PA_LUG_NAC) # Mantiene categorias DANE

personas <- personas %>% mutate(seEnfermo = ifelse(P_ENFERMO == 1, 1, 0)) # 1 = Si, en el ultimo mes se enfermo
personas <- personas %>% mutate(atencionMedica = P_QUEHIZO_PPAL) # Mantiene categorias DANE
personas <- personas %>% mutate(fueAtendido = ifelse(PA_LO_ATENDIERON == 1, 1, 0)) # 1 = Fue atendido por su problema de salud
personas <- personas %>% mutate(calidadSevicio = PA1_CALIDAD_SERV) # Mantiene categorias DANE

personas <- personas %>% mutate(problemasFisicos = ifelse(CONDICION_FISICA == 1, 1, 0)) # 1 = Tiene problemas fisicos dia a dia

personas <- personas %>% mutate(esAlfabeta = ifelse(P_ALFABETA == 1, 1, 0)) # 1 = Sabe leer y escribir

personas <- personas %>% mutate(nivelEducativo = ifelse(P_NIVEL_ANOSR == 1 | P_NIVEL_ANOSR == 2, 1, 
    ifelse(P_NIVEL_ANOSR >= 3 | P_NIVEL_ANOSR <= 6, 2, 
    ifelse(P_NIVEL_ANOSR == 7 | P_NIVEL_ANOSR == 8, 3, 
    ifelse(P_NIVEL_ANOSR == 9, 4, 0))))
) # 1 = Primaria, 2 = Bachillerato/Normal, 3 = Educacion Superior, 4 = Posgrado, 0 = No tiene/No Informa/Otros
personas <- personas %>% mutate(asisteClase = ifelse(PA_ASISTENCIA == 1, 1, 0)) # 1 = Esta asistiendo a clases ya sea virtual o presencial

personas <- personas %>% mutate(actividadReciente = P_TRABAJO) # Mantiene categorias DANE

personas <- personas %>% mutate(estadoCivil = P_EST_CIVIL) # Mantiene categorias DANE

personas <- personas %>% mutate(tuvoHijos = ifelse(PA_HNV == 1, 1, 0)) # 1 = Ha tenido hijos nacidos vivos


personas <- personas %>% select(U_DPTO, U_MPIO, COD_ENCUESTAS, U_VIVIENDA, P_NROHOG, P_NRO_PER, esMujer, grupoEdad, 
noGrupoEtnico, geIndigena, geGitano, geRaizal, gePalenquero, geAfro, lugarNacimiento, seEnfermo, atencionMedica, fueAtendido, 
calidadSevicio, problemasFisicos, esAlfabeta, nivelEducativo, asisteClase, actividadReciente, estadoCivil, tuvoHijos)

personas[is.na(personas)] <- 0