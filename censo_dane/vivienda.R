source("./censo_dane/dependencies.R")

# DF vivienda

vivienda <- get(dfsEnvir[5])

vivienda <- vivienda %>% mutate(tipoVivienda = V_TIPO_VIV) # Mantiene categoria DANE, 0 ruta para valores no registrados/otros
vivienda <- vivienda %>% mutate(materialPared = V_MAT_PARED) # Mantiene categoria DANE, 0 ruta para valores no registrados/otros
vivienda <- vivienda %>% mutate(materialPiso = V_MAT_PISO) # Mantiene categoria DANE, 0 ruta para valores no registrados/otros

vivienda <- vivienda %>% mutate(tieneEnergia = ifelse(VA_EE == 1, 1, 0)) # 1 = Tiene energia electrica
vivienda <- vivienda %>% mutate(estratoEE = ifelse(VA1_ESTRATO == 9, 0, VA1_ESTRATO)) # Estratos, pero si es = 0, no tiene estrato o no se sabe cual es

vivienda <- vivienda %>% mutate(tieneAcueducto = ifelse(VB_ACU == 1, 1, 0)) # 1 = Tiene acueducto
vivienda <- vivienda %>% mutate(tieneAlcantarillado = ifelse(VC_ALC == 1, 1, 0)) # 1 = Tiene alcantarillado
vivienda <- vivienda %>% mutate(tieneGas = ifelse(VD_GAS == 1, 1, 0)) # 1 =  Tiene Gas natural
vivienda <- vivienda %>% mutate(tieneServBasuras = ifelse(VE_RECBAS == 1, 1, 0)) # 1 = Tiene servicio de recoleccion de basuras
vivienda <- vivienda %>% mutate(tieneInternet = ifelse(VF_INTERNET == 1, 1, 0)) # 1 = Tiene internet

vivienda <- vivienda %>% mutate(tipoSSanitario = V_TIPO_SERSA) # Mantiene categoria DANE, 0 ruta para valores no registrados/otros

vivienda <- vivienda %>% select(U_DPTO, U_MPIO, COD_ENCUESTAS, U_VIVIENDA, U_EDIFICA, tipoVivienda, materialPared, materialPiso,
tieneEnergia, estratoEE, tieneAcueducto, tieneAlcantarillado, tieneGas, tieneServBasuras, tieneInternet, tipoSSanitario)

vivienda[is.na(vivienda)] <- 0