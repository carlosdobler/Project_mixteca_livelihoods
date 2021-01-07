

library(tidyverse)
library(readODS)
# library(here)

dir_encuestas <- "/home/cdobler/Documents/Insync_ciga/Research/Fieldwork_Mixteca/encuestas/"
  
# Loop to go over all files
list.files(dir_encuestas, full.names = T)[263] %>% 
  read_ods(sheet = 11,
           col_names = F,
           na = 0,
           col_types = NA) %>% 
  as_tibble() -> raw_table



tibble(
  
  # BASICS ----------------------------------------------------------------------------------------
  
  # id
  id = str_c(
    str_sub(raw_table[raw_table$A == "1.1" ,"B", drop = T], end = 3), "_",
    str_sub(raw_table[raw_table$A == "2.1" ,"B", drop = T], end = 2) %>% str_to_upper(),
    str_sub(raw_table[raw_table$A == "2.2" ,"B", drop = T], end = 2) %>% str_to_upper()),
  
  # community
  comunidad = raw_table[raw_table$A == "1.1" ,"B", drop = T] %>% 
    str_to_lower(),
  
  # comuneros in the household
  comuneros = raw_table[raw_table$A == "2.6" ,"B", drop = T] %>% 
    {if(is.na(.)) "ER: Falta dato" else if (. == "SÍ") as.logical(T) else as.logical(F)},
  
  # land ownership (de jure)
  tierra = raw_table[raw_table$A == "2.7" ,"B", drop = T] %>% 
    {if(is.na(.)) "ER: Falta dato" else if(str_detect(., "NO TIENEN")) "F" else "T"}, # transform to logical
  
  # number of fields
  terrenos = raw_table[raw_table$A == "2.7" ,"B", drop = T] %>% 
    {if(is.na(.)) "ER: Falta dato" else
      if(str_detect(., "NO TIENEN")) NA else
        if(. == "MÁS DE 5") "6" else .}, # transform to integer
  
  # land area
  superficie = raw_table[raw_table$A == "2.8" ,"B", drop = T] %>% 
    {if(is.na(.) & !is.na(terrenos) & str_detect(terrenos, "ER", negate = T)) "ER: Falta dato (no. de terrenos no nulo)" else
      if(!is.na(.) & is.na(terrenos)) "ER: Imposible (no tiene terrenos)" else
        if(is.na(.) & is.na(terrenos)) NA else
          case_when(str_detect(., "1 HA") ~ as.character(0.5),
                    str_detect(., "1.9 HA") ~ as.character(1.5),
                    str_detect(., "2.9 HA") ~ as.character(2.5),
                    str_detect(., "3.9 HA") ~ as.character(3.5),
                    str_detect(., "4.9 HA") ~ as.character(4.5),
                    str_detect(., "5.9 HA") ~ as.character(5.5),
                    str_detect(., "A 10 HA") ~ as.character(8),
                    str_detect(., "DE 10 HA") ~ as.character(12))}, # transform to double
  
  # land acquisition
  tibble(
    terrenos_a = terrenos,
    herencia = raw_table[raw_table$A == "2.9" ,"B", drop = T],
    compra = raw_table[raw_table$A == "2.10" ,"B", drop = T],
    asign_dir = raw_table[raw_table$A == "2.11" ,"B", drop = T],
    otra_adq = raw_table[raw_table$A == "2.12" ,"B", drop = T]) %>% 
    
    {if(all(is.na(.[,2:5])) & !is.na(.$terrenos_a) & str_detect(.$terrenos_a, "ER", negate = T)) 
      tibble(herencia = "ER: Se necesita al menos un tipo de adquisición (no. de terrenos no nulo)",
             compra = "ER: Se necesita al menos un tipo de adquisición (no. de terrenos no nulo)",
             asign_dir = "ER: Se necesita al menos un tipo de adquisición (no. de terrenos no nulo)",
             otra_adq = "ER: Se necesita al menos un tipo de adquisición (no. de terrenos no nulo)") else
               
        mutate(., 
               herencia = 
                 if(is.na(terrenos_a) & !is.na(herencia)) "ER: Imposible (no tiene terrenos)" else
                   if(is.na(terrenos_a) & is.na(herencia)) NA else
                     if(is.na(herencia)) "F" else "T", # transform to logical
      
               compra = 
                 if(is.na(terrenos_a) & !is.na(compra)) "ER: Imposible (no tiene terrenos)" else
                   if(is.na(terrenos_a) & is.na(compra)) NA else
                     if(is.na(compra)) "F" else "T", # transform to logical

               asign_dir = 
                 if(is.na(terrenos_a) & !is.na(asign_dir)) "ER: Imposible (no tiene terrenos)" else
                   if(is.na(terrenos_a) & is.na(asign_dir)) NA else
                     if(is.na(asign_dir)) "F" else "T", # transform to logical
               
               otra_adq = 
                 if(is.na(terrenos_a) & !is.na(otra_adq)) "ER: Imposible (no tiene terrenos)" else
                   if(is.na(terrenos_a) & is.na(otra_adq)) NA else
                     if(is.na(otra_adq)) "F" else "T") %>% # transform to logical
        
        select(-terrenos_a)
    },
  
  # farmed land
  tierra_sembrada = raw_table[raw_table$A == "2.13" ,"B", drop = T] %>% 
    {if(is.na(.) & !is.na(terrenos) & str_detect(terrenos, "ER", negate = T)) "ER: Falta dato (sí tiene terrenos)" else
      if(!is.na(.) & is.na(terrenos)) "ER: Imposible (no tiene terrenos)" else
        case_when(is.na(.) ~ NA_character_,
                  str_detect(., "NADA") ~ as.character(0), 
                  str_detect(., "1 HA") ~ as.character(0.5),
                  str_detect(., "1.9 HA") ~ as.character(1.5),
                  str_detect(., "2.9 HA") ~ as.character(2.5),
                  str_detect(., "3.9 HA") ~ as.character(3.5),
                  str_detect(., "4.9 HA") ~ as.character(4.5),
                  str_detect(., "5.9 HA") ~ as.character(5.5),
                  str_detect(., "A 10 HA") ~ as.character(8),
                  str_detect(., "DE 10 HA") ~ as.character(12))}, # transform to double
          
      
  # use of common lands
  tierra_com = raw_table[raw_table$A == "2.16" ,"B", drop = T] %>% 
    {if(is.na(.)) as.logical(T) else as.logical(F)},
  
  tibble(
    tierra_com_a = tierra_com,
    tierra_com_lena = raw_table[raw_table$A == "2.17" ,"B", drop = T],
    tierra_com_pastoreo = raw_table[raw_table$A == "2.18" ,"B", drop = T],
    tierra_com_material = raw_table[raw_table$A == "2.19" ,"B", drop = T],
    tierra_com_otra = raw_table[raw_table$A == "2.20" ,"B", drop = T]) %>%
    
    {if(all(is.na(.[,2:5])) & .$tierra_com_a == T)
      tibble(
        tierra_com_lena = "ER: Se necesita al menos un tipo de uso (uso de tierras com. no nulo)",
        tierra_com_pastoreo = "ER: Se necesita al menos un tipo de uso (uso de tierras com. no nulo)",
        tierra_com_material = "ER: Se necesita al menos un tipo de uso (uso de tierras com. no nulo)",
        tierra_com_otra = "ER: Se necesita al menos un tipo de uso (uso de tierras com. no nulo)") else
          
        mutate(.,
               tierra_com_lena = 
                 if(tierra_com_a == F & !is.na(tierra_com_lena)) "ER: Imposible (no usan tierras com.)" else
                   if(tierra_com_a == F & is.na(tierra_com_lena)) NA else
                     if(is.na(tierra_com_lena)) "F" else "T", # transform to logical
               
               tierra_com_pastoreo = 
                 if(tierra_com_a == F & !is.na(tierra_com_pastoreo)) "ER: Imposible (no usan tierras com.)" else
                   if(tierra_com_a == F & is.na(tierra_com_pastoreo)) NA else
                     if(is.na(tierra_com_pastoreo)) "F" else "T", # transform to logical
               
               tierra_com_material = 
                 if(tierra_com_a == F & !is.na(tierra_com_material)) "ER: Imposible (no usan tierras com.)" else
                   if(tierra_com_a == F & is.na(tierra_com_material)) NA else
                     if(is.na(tierra_com_material)) "F" else "T", # transform to logical
               
               tierra_com_otra = 
                 if(tierra_com_a == F & !is.na(tierra_com_otra)) "ER: Imposible (no usan tierras com.)" else
                   if(tierra_com_a == F & is.na(tierra_com_otra)) NA else
                     if(is.na(tierra_com_otra)) "F" else "T") %>% # transform to logical
        
        select(-tierra_com_a)
      },
  
  # use of additional lands
  tierra_adic = raw_table[raw_table$A == "2.21" ,"B", drop = T] %>% 
    {if(is.na(.)) "ER: Falta dato" else
      case_when(str_detect(., "NADA") ~ as.character(0), 
                str_detect(., "1 HA") ~ as.character(0.5),
                str_detect(., "1.9 HA") ~ as.character(1.5),
                str_detect(., "2.9 HA") ~ as.character(2.5),
                str_detect(., "3.9 HA") ~ as.character(3.5),
                str_detect(., "4.9 HA") ~ as.character(4.5),
                str_detect(., "5.9 HA") ~ as.character(5.5),
                str_detect(., "A 10 HA") ~ as.character(8),
                str_detect(., "DE 10 HA") ~ as.character(12))}, # transform to double
  
  # Farms?
  siembra = if((tierra_sembrada == "0" | is.na(tierra_sembrada)) & tierra_adic == "0") F else T,
  
  # arrangement additional land
  tierra_adic_arreglo = raw_table[raw_table$A == "2.22" ,"B", drop = T] %>% 
    {if(is.na(.) & (tierra_adic != "0" | str_detect(tierra_adic, "ER", negate = T))) "ER: Falta dato (usa terrenos no propios)" else
      if(!is.na(.) & tierra_adic == "0") "ER: Imposible (no usa terrenos no propios)" else (.)}, 
  
  # kinship - additional land
  tierra_adic_kin = raw_table[raw_table$A == "2.23" ,"B", drop = T] %>% 
    {if(is.na(.) & (tierra_adic != "0" & str_detect(tierra_adic, "ER", negate = T))) "ER: Falta dato (usa terrenos no propios)" else
      if(!is.na(.) & tierra_adic == "0") "ER: Imposible (no usa terrenos no propios)" else
        if('[.]' == "SÍ") "T" else if('[.]' == "NO") "F" else if(is.na(.)) NA}, # transform to logical
  
  # observations?
  obs1 = raw_table[raw_table$A == "2.24" ,"B", drop = T],
  
  
  # GOV'T PROGRAMS ---------------------------------------------------------------------------------
  
  # procampo (annual transfer)
  procampo = raw_table[raw_table$A == "3.1" ,"B", drop = T] %>%
    {if(is.na(.)) "ER: Falta dato" else
      case_when(str_detect(., "NADA") ~ as.character(0), 
                str_detect(., "MENOS DE") ~ as.character(500),
                str_detect(., "1,999") ~ as.character(1500),
                str_detect(., "2,999") ~ as.character(2500),
                str_detect(., "3,999") ~ as.character(3500),
                str_detect(., "4,999") ~ as.character(4500),
                str_detect(., "O MÁS") ~ as.character(5500))}, # transform to integer
  
  # prospera (number of beneficiaries)
  prospera = raw_table[raw_table$A == "3.2" ,"B", drop = T] %>%
    {if(is.na(.)) "ER: Falta dato" else
      case_when(str_detect(., "NINGUNO") ~ as.character(0), 
                str_detect(., "1") ~ as.character(1),
                str_detect(., "DE 2") ~ as.character(3),
                TRUE ~ "2")}, # transform to integer
  
  # adultos mayores (number of beneficiaries)
  adultosmay = raw_table[raw_table$A == "3.3" ,"B", drop = T] %>%
    {if(is.na(.)) "ER: Falta dato" else
      case_when(str_detect(., "NINGUNO") ~ as.character(0), 
                str_detect(., "1") ~ as.character(1),
                str_detect(., "DE 2") ~ as.character(3),
                TRUE ~ "2")}, # transform to integer
  
  # other programs
  progan = raw_table[raw_table$A == "3.4" ,"B", drop = T] %>% 
  {if(is.na(.)) "F" else "T"}, # transform to logical
  
  pesa = raw_table[raw_table$A == "3.5" ,"B", drop = T] %>% 
  {if(is.na(.)) "F" else "T"}, # transform to logical
  
  siniestro = raw_table[raw_table$A == "3.6" ,"B", drop = T] %>% 
  {if(is.na(.)) "F" else "T"}, # transform to logical
  
  # pension
  pension = tibble(amount = raw_table[raw_table$A == "3.8" ,"B", drop = T],
                   freq = raw_table[raw_table$A == "3.9" ,"B", drop = T]) %>% 
    
    {mutate(.,
            amount = case_when(str_detect(amount, "NADA") | is.na(amount) ~ 0, 
                               str_detect(amount, "MENOS DE") ~ 500,
                               str_detect(amount, "1,999") ~ 1500,
                               str_detect(amount, "2,999") ~ 2500,
                               str_detect(amount, "3,999") ~ 3500,
                               str_detect(amount, "4,999") ~ 4500,
                               TRUE ~ parse_number(amount)),
            
            freq = ifelse(is.na(freq) | freq == "AL AÑO", 1, 12))} %>% 
    
            {.$amount * .$freq},
  
  # observations?
  obs2 = raw_table[raw_table$A == "3.10" ,"B", drop = T],
  
  
  # AGRICULTURE ------------------------------------------------------------------------------------
  
  # Irrigation
  riego = raw_table[raw_table$A == "4.1" ,"B", drop = T] %>%
    {if(is.na(.)) "ER: Falta dato" else 
      if(. == "SÍ" & siembra == F) "ER: Imposible (no siembra)" else
        if(. == "SÍ") "T" else "F"},
  
  # Irrigation type
  tibble(riego_a = riego,
         riego_rio = raw_table[raw_table$A == "4.2" ,"B", drop = T],
         riego_cont = raw_table[raw_table$A == "4.3" ,"B", drop = T],
         riego_sub = raw_table[raw_table$A == "4.4" ,"B", drop = T]) %>% 
    
         {if(all(is.na(.[,2:4])) & .$riego_a == T)
           tibble(
             riego_rio = "ER: Se necesita al menos un tipo de riego (uso de riego no nulo)",
             riego_cont = "ER: Se necesita al menos un tipo de riego (uso de riego no nulo)",
             riego_sub = "ER: Se necesita al menos un tipo de riego (uso de riego no nulo)") else
               
               mutate(.,
                      riego_rio = 
                        if(riego_a == F & !is.na(riego_rio)) "ER: Imposible (no usan riego)" else
                          if(riego_a == F & is.na(riego_rio)) NA else
                            if(is.na(riego_rio)) "F" else "T", # transform to logical
                      
                      riego_cont = 
                        if(riego_a == F & !is.na(riego_cont)) "ER: Imposible (no usan riego)" else
                          if(riego_a == F & is.na(riego_cont)) NA else
                            if(is.na(riego_cont)) "F" else "T", # transform to logical
                      
                      riego_sub = 
                        if(riego_a == F & !is.na(riego_sub)) "ER: Imposible (no usan riego)" else
                          if(riego_a == F & is.na(riego_sub)) NA else
                            if(is.na(riego_sub)) "F" else "T") %>% # transform to logical
             
             select(-riego_a)
         },
  
  # Irrigation availability
  riego_perm = tibble(riego = riego,
                      int = raw_table[raw_table$A == "4.5" ,"B", drop = T],
                      perm = raw_table[raw_table$A == "4.6" ,"B", drop = T]) %>% 
    
    {if(is.na(.$int) & is.na(.$perm) & .$riego == "T") "ER: Se necesita al menos un tipo de riego (uso de riego no nulo)" else
       if(is.na(.$int) & is.na(.$perm) & .$riego == "F") NA else
         if(is.na(.$int) & !is.na(.$perm)) "T" else "F"},
  
  # Cropping cycles
  ciclos = raw_table[raw_table$A == "4.9" ,"B", drop = T] %>%
    {if(is.na(.)) "ER: Falta dato" else
      if(str_detect(., "0", negate = T) & siembra == F) "ER: Imposible (no siembra)" else
        if(str_detect(., "0") & siembra == T) "ER: Falta dato (sí siembra)" else
          if(str_detect(., "0")) NA else as.character(.)}, # transform to integer
  
  # Cropping sites
  sites = raw_table[raw_table$A == "4.10" ,"B", drop = T] %>%
    {if(!is.na(.) & siembra == F) "ER: Imposible (no siembra)" else
      if(is.na(.) & siembra == T) "ER: Falta dato (sí siembra)" else
        case_when(is.na(.) ~ NA_character_,
                  str_detect(., "MÁS") ~ "4",
                  TRUE ~ as.character(.))}, # transform to integer
      
  # Terrace cropping
  terraza = raw_table[raw_table$A == "4.11" ,"B", drop = T] %>%
    {if(!is.na(.) & siembra == F) "ER: Imposible (no siembra)" else
      if(is.na(.) & siembra == T) "ER: Falta dato (sí siembra)" else
        case_when(is.na(.) ~ NA_character_,
                  str_detect(., "0") ~ "F",
                  TRUE ~ "T")}, # transform to logical
  
  # Tilling
  till_tractor = raw_table[raw_table$A == "4.12" ,"B", drop = T] %>%
    {if(!is.na(.) & siembra == F) "ER: Imposible (no siembra)" else
      if(is.na(.) & siembra == T) "ER: Falta dato (sí siembra)" else
        case_when(is.na(.) ~ NA_character_,
                  . == "NO USAMOS" ~ "F",
                  TRUE ~ "T")}, # transform to logical
  
  till_yunta = raw_table[raw_table$A == "4.13" ,"B", drop = T] %>%
    {if(!is.na(.) & siembra == F) "ER: Imposible (no siembra)" else
      if(is.na(.) & siembra == T) "ER: Falta dato (sí siembra)" else
        case_when(is.na(.) ~ NA_character_,
                  . == "NO USAMOS" ~ "F",
                  TRUE ~ "T")}, # transform to logical
  
  # External inputs
  fertilizer = raw_table[raw_table$A == "4.14" ,"B", drop = T] %>%
    {if(!is.na(.) & siembra == F) "ER: Imposible (no siembra)" else
      if(is.na(.) & siembra == T) "ER: Falta dato (sí siembra)" else
        if(. == "NO USAMOS") "F" else "T"}, # transform to logical
  
  fertilizer_chem = raw_table[raw_table$A == "4.14" ,"B", drop = T] %>%
    {if(fertilizer == "F" | is.na(fertilizer)) NA else
      if(str_detect(., "QUÍMICO")) "T" else "F"}, # transform to logical
  
  herbicide = raw_table[raw_table$A == "4.15" ,"B", drop = T] %>%
    {if(!is.na(.) & siembra == F) "ER: Imposible (no siembra)" else
      if(is.na(.) & siembra == T) "ER: Falta dato (sí siembra)" else
        if(. == "NO") "F" else "T"}, # transform to logical
  
  insecticide = raw_table[raw_table$A == "4.16" ,"B", drop = T] %>%
    {if(!is.na(.) & siembra == F) "ER: Imposible (no siembra)" else
      if(is.na(.) & siembra == T) "ER: Falta dato (sí siembra)" else
        if(. == "NO") "F" else "T"}, # transform to logical
  
  # hired labor
  jornaleros = raw_table[raw_table$A == "4.17" ,"B", drop = T] %>%
    {if(!is.na(.) & siembra == F) "ER: Imposible (no siembra)" else
      if(is.na(.) & siembra == T) "ER: Falta dato (sí siembra)" else
        case_when(is.na(.) ~ NA_character_,
                  str_detect(., "0") ~ "0",
                  str_detect(., "1") ~ "1.5",
                  str_detect(., "3") ~ "3.5",
                  str_detect(., "5") ~ "5.5",
                  str_detect(., "MÁS") ~ "7.5")}, # transform to numeric
  
  # crop production
  tibble(
    siembra = siembra,
    
    maiz_cajete = tibble(y2019 = raw_table[raw_table$A == "4.34" ,"B", drop = T],
                         y2018 = raw_table[raw_table$A == "4.37" ,"B", drop = T]) %>%
      
      mutate(across(everything(), ~case_when(str_detect(.x, "0") | is.na(.x) ~ NA_character_,
                                             TRUE ~ "1"))) %>%
      
             {if(all(is.na(.[,1:2]))) "F" else
               if(siembra == F) "ER: Imposible (no siembra)" else "T"},
    
    
    maiz_riego = tibble(y2019 = raw_table[raw_table$A == "4.41" ,"B", drop = T],
                        y2018 = raw_table[raw_table$A == "4.44" ,"B", drop = T]) %>%
      
      mutate(across(everything(), ~case_when(str_detect(.x, "0") | is.na(.x) ~ NA_character_,
                                             TRUE ~ "1"))) %>%
      
             {if(all(is.na(.[,1:2]))) "F" else
               if(siembra == F) "ER: Imposible (no siembra)" else "T"},
    
    maiz_temp = tibble(y2019 = raw_table[raw_table$A == "4.48" ,"B", drop = T],
                       y2018 = raw_table[raw_table$A == "4.51" ,"B", drop = T]) %>%
      
      mutate(across(everything(), ~case_when(str_detect(.x, "0") | is.na(.x) ~ NA_character_,
                                             TRUE ~ "1"))) %>%
      
             {if(all(is.na(.[,1:2]))) "F" else
               if(siembra == F) "ER: Imposible (no siembra)" else "T"},
    
    frijol = tibble(y2019 = raw_table[raw_table$A == "4.55" ,"B", drop = T],
                    y2018 = raw_table[raw_table$A == "4.58" ,"B", drop = T]) %>%
      
      mutate(across(everything(), ~case_when(str_detect(.x, "0") | is.na(.x) ~ NA_character_,
                                             TRUE ~ "1"))) %>%
      
             {if(all(is.na(.[,1:2]))) "F" else
               if(siembra == F) "ER: Imposible (no siembra)" else "T"},
    
    trigo = tibble(y2019 = raw_table[raw_table$A == "4.62" ,"B", drop = T],
                   y2018 = raw_table[raw_table$A == "4.65" ,"B", drop = T]) %>%
      
      mutate(across(everything(), ~case_when(str_detect(.x, "0") | is.na(.x) ~ NA_character_,
                                             TRUE ~ "1"))) %>%
      
             {if(all(is.na(.[,1:2]))) "F" else
               if(siembra == F) "ER: Imposible (no siembra)" else "T"},
    
    avena = raw_table[raw_table$A == "4.69" ,"B", drop = T] %>% 
    {if(is.na(.)) "F" else
      if(siembra == F) "ER: Imposible (no siembra)" else "T"},
    
    alfalfa = raw_table[raw_table$A == "4.70" ,"B", drop = T] %>% 
    {if(is.na(.)) "F" else
      if(siembra == F) "ER: Imposible (no siembra)" else "T"},
    
    cebada = raw_table[raw_table$A == "4.71" ,"B", drop = T] %>% 
    {if(is.na(.)) "F" else
      if(siembra == F) "ER: Imposible (no siembra)" else "T"},
    
    alpiste = raw_table[raw_table$A == "4.72" ,"B", drop = T] %>% 
    {if(is.na(.)) "F" else
      if(siembra == F) "ER: Imposible (no siembra)" else "T"},
    
    otros_cultivos = raw_table[raw_table$A == "4.73" ,"B", drop = T] %>% 
    {if(is.na(.)) "F" else
      if(siembra == F) "ER: Imposible (no siembra)" else (.)}
  
    ) %>% 
    
    {if((all(is.na(.[,-(1:2)])) | all(.[, -(1:2)] == "F")) & siembra == T)
      
      mutate(.,
             maiz_cajete = "ER: Se necesita al menos un tipo de cultivo (sí siembra)",
             maiz_riego = "ER: Se necesita al menos un tipo de cultivo (sí siembra)",
             maiz_temp = "ER: Se necesita al menos un tipo de cultivo (sí siembra)",
             frijol = "ER: Se necesita al menos un tipo de cultivo (sí siembra)",
             trigo = "ER: Se necesita al menos un tipo de cultivo (sí siembra)",
             avena = "ER: Se necesita al menos un tipo de cultivo (sí siembra)",
             alfalfa = "ER: Se necesita al menos un tipo de cultivo (sí siembra)",
             cebada = "ER: Se necesita al menos un tipo de cultivo (sí siembra)",
             alpiste = "ER: Se necesita al menos un tipo de cultivo (sí siembra)",
             otros_cultivos = "ER: Se necesita al menos un tipo de cultivo (sí siembra)") %>% 
        
        select(-siembra)},
  
  # crop area
  maiz_cajete_area = if(maiz_cajete == "T")
    tibble(y2019 = raw_table[raw_table$A == "4.34" ,"B", drop = T],
           y2018 = raw_table[raw_table$A == "4.37" ,"B", drop = T]) %>% 
    
    mutate(across(everything(), ~case_when(str_detect(.x, "MENOS") ~ "0.5",
                                           str_detect(.x, "1.9") ~ "1.5",
                                           str_detect(.x, "2.9") ~ "2.5",
                                           str_detect(.x, "3.9") ~ "3.5",
                                           str_detect(.x, "4.9") ~ "4.5",
                                           str_detect(.x, "5") ~ "5.5",
                                           TRUE ~ NA_character_))) %>% 
    mutate(across(everything(), ~as.numeric(.x))) %>% unlist() %>% mean() else NA,
  
  maiz_riego_area = if(maiz_riego == "T")
    tibble(y2019 = raw_table[raw_table$A == "4.41" ,"B", drop = T],
           y2018 = raw_table[raw_table$A == "4.44" ,"B", drop = T]) %>% 
    
    mutate(across(everything(), ~case_when(str_detect(.x, "MENOS") ~ "0.5",
                                           str_detect(.x, "1.9") ~ "1.5",
                                           str_detect(.x, "2.9") ~ "2.5",
                                           str_detect(.x, "3.9") ~ "3.5",
                                           str_detect(.x, "4.9") ~ "4.5",
                                           str_detect(.x, "5") ~ "5.5",
                                           TRUE ~ NA_character_))) %>% 
    mutate(across(everything(), ~as.numeric(.x))) %>% unlist() %>% mean() else NA,
  
  maiz_temp_area = if(maiz_temp == "T")
    tibble(y2019 = raw_table[raw_table$A == "4.48" ,"B", drop = T],
           y2018 = raw_table[raw_table$A == "4.51" ,"B", drop = T]) %>% 
    
    mutate(across(everything(), ~case_when(str_detect(.x, "MENOS") ~ "0.5",
                                           str_detect(.x, "1.9") ~ "1.5",
                                           str_detect(.x, "2.9") ~ "2.5",
                                           str_detect(.x, "3.9") ~ "3.5",
                                           str_detect(.x, "4.9") ~ "4.5",
                                           str_detect(.x, "5") ~ "5.5",
                                           TRUE ~ NA_character_))) %>% 
    mutate(across(everything(), ~as.numeric(.x))) %>% unlist() %>% mean() else NA,
  
  frijol_area = if(frijol == "T")
    tibble(y2019 = raw_table[raw_table$A == "4.55" ,"B", drop = T],
           y2018 = raw_table[raw_table$A == "4.58" ,"B", drop = T]) %>% 
    
    mutate(across(everything(), ~case_when(str_detect(.x, "MENOS") ~ "0.5",
                                           str_detect(.x, "1.9") ~ "1.5",
                                           str_detect(.x, "2.9") ~ "2.5",
                                           str_detect(.x, "3.9") ~ "3.5",
                                           str_detect(.x, "4.9") ~ "4.5",
                                           str_detect(.x, "5") ~ "5.5",
                                           TRUE ~ NA_character_))) %>% 
    mutate(across(everything(), ~as.numeric(.x))) %>% unlist() %>% mean() else NA,
  
  trigo_area = if(trigo == "T")
    tibble(y2019 = raw_table[raw_table$A == "4.62" ,"B", drop = T],
           y2018 = raw_table[raw_table$A == "4.65" ,"B", drop = T]) %>% 
    
    mutate(across(everything(), ~case_when(str_detect(.x, "MENOS") ~ "0.5",
                                           str_detect(.x, "1.9") ~ "1.5",
                                           str_detect(.x, "2.9") ~ "2.5",
                                           str_detect(.x, "3.9") ~ "3.5",
                                           str_detect(.x, "4.9") ~ "4.5",
                                           str_detect(.x, "5") ~ "5.5",
                                           TRUE ~ NA_character_))) %>% 
    mutate(across(everything(), ~as.numeric(.x))) %>% unlist() %>% mean() else NA,
  
  # Subsistence
  maiz_cajete_consumo = raw_table[raw_table$A == "4.40" ,"B", drop = T] %>% 
    {if(maiz_cajete != "T" & !is.na(.)) "ER: Imposible (no siembra este cultivo)" else
        if(maiz_cajete == "T" & is.na(.)) "ER: Falta dato (sí siembra este cultivo)" else
          case_when(is.na(.) ~ NA_character_,
                    str_detect(., "10") ~ "5",
                    str_detect(., "20") ~ "15",
                    str_detect(., "30") ~ "25",
                    str_detect(., "40") ~ "35",
                    str_detect(., "50") ~ "45",
                    str_detect(., "60") ~ "55",
                    str_detect(., "70") ~ "65",
                    str_detect(., "80") ~ "75",
                    str_detect(., "90") ~ "85",
                    str_detect(., "100") ~ "95")},
  
  maiz_riego_consumo = raw_table[raw_table$A == "4.47" ,"B", drop = T] %>% 
    {if(maiz_riego != "T" & !is.na(.)) "ER: Imposible (no siembra este cultivo)" else
      if(maiz_riego == "T" & is.na(.)) "ER: Falta dato (sí siembra este cultivo)" else
        case_when(is.na(.) ~ NA_character_,
                  str_detect(., "10") ~ "5",
                  str_detect(., "20") ~ "15",
                  str_detect(., "30") ~ "25",
                  str_detect(., "40") ~ "35",
                  str_detect(., "50") ~ "45",
                  str_detect(., "60") ~ "55",
                  str_detect(., "70") ~ "65",
                  str_detect(., "80") ~ "75",
                  str_detect(., "90") ~ "85",
                  str_detect(., "100") ~ "95")},
  
  maiz_temp_consumo = raw_table[raw_table$A == "4.54" ,"B", drop = T] %>% 
    {if(maiz_temp != "T" & !is.na(.)) "ER: Imposible (no siembra este cultivo)" else
      if(maiz_temp == "T" & is.na(.)) "ER: Falta dato (sí siembra este cultivo)" else
        case_when(is.na(.) ~ NA_character_,
                  str_detect(., "10") ~ "5",
                  str_detect(., "20") ~ "15",
                  str_detect(., "30") ~ "25",
                  str_detect(., "40") ~ "35",
                  str_detect(., "50") ~ "45",
                  str_detect(., "60") ~ "55",
                  str_detect(., "70") ~ "65",
                  str_detect(., "80") ~ "75",
                  str_detect(., "90") ~ "85",
                  str_detect(., "100") ~ "95")},
  
  frijol_consumo = raw_table[raw_table$A == "4.61" ,"B", drop = T] %>% 
    {if(frijol != "T" & !is.na(.)) "ER: Imposible (no siembra este cultivo)" else
      if(frijol == "T" & is.na(.)) "ER: Falta dato (sí siembra este cultivo)" else
        case_when(is.na(.) ~ NA_character_,
                  str_detect(., "10") ~ "5",
                  str_detect(., "20") ~ "15",
                  str_detect(., "30") ~ "25",
                  str_detect(., "40") ~ "35",
                  str_detect(., "50") ~ "45",
                  str_detect(., "60") ~ "55",
                  str_detect(., "70") ~ "65",
                  str_detect(., "80") ~ "75",
                  str_detect(., "90") ~ "85",
                  str_detect(., "100") ~ "95")},
  
  trigo_consumo = raw_table[raw_table$A == "4.68" ,"B", drop = T] %>% 
    {if(trigo != "T" & !is.na(.)) "ER: Imposible (no siembra este cultivo)" else
      if(trigo == "T" & is.na(.)) "ER: Falta dato (sí siembra este cultivo)" else
        case_when(is.na(.) ~ NA_character_,
                  str_detect(., "10") ~ "5",
                  str_detect(., "20") ~ "15",
                  str_detect(., "30") ~ "25",
                  str_detect(., "40") ~ "35",
                  str_detect(., "50") ~ "45",
                  str_detect(., "60") ~ "55",
                  str_detect(., "70") ~ "65",
                  str_detect(., "80") ~ "75",
                  str_detect(., "90") ~ "85",
                  str_detect(., "100") ~ "95")},
  
  # Commercialization of production
  venta_ag_local = tibble(local1 = raw_table[raw_table$A == "4.75" ,"B", drop = T],
                       local2 = raw_table[raw_table$A == "4.76" ,"B", drop = T]) %>% 
    
                       {if(any(!is.na(.)) & siembra == F) "ER: Imposible (no siembra)" else
                         if(all(is.na(.)) & siembra == F) NA else
                           if(all(is.na(.))) "F" else "T"},
  
  venta_ag_no_local = raw_table[raw_table$A == "4.77" ,"B", drop = T] %>% 
    {if(!is.na(.) & siembra == F) "ER: Imposible (no siembra)" else
      if(is.na(.) & siembra == F) NA else
        if(is.na(.)) "F" else "T"},
  
  obs3 = raw_table[raw_table$A == "4.83" ,"B", drop = T],
  
  
  # LIVESTOCK --------------------------------------------------------------------------------------
  
  bovino = raw_table[raw_table$A == "5.1" ,"B", drop = T],
  ovino = raw_table[raw_table$A == "5.4" ,"B", drop = T],
  caprino = raw_table[raw_table$A == "5.7" ,"B", drop = T],
  
  venta_gan_local = tibble(local1 = raw_table[raw_table$A == "5.11" ,"B", drop = T],
                           local2 = raw_table[raw_table$A == "5.12" ,"B", drop = T]) %>% 
    
                           {if(any(!is.na(.)) & is.na(bovino) & is.na(ovino) & is.na(caprino)) "ER: Imposible (no cria ganado)" else
                             if(all(is.na(.)) & is.na(bovino) & is.na(ovino) & is.na(caprino)) NA else
                               if(all(is.na(.))) "F" else "T"},
  
  venta_gan_no_local = raw_table[raw_table$A == "5.13" ,"B", drop = T] %>% 
    {if(!is.na(.) & is.na(bovino) & is.na(ovino) & is.na(caprino)) "ER: Imposible (no cria ganado)" else
      if(is.na(.) & is.na(bovino) & is.na(ovino) & is.na(caprino)) NA else
        if(is.na(.)) "F" else "T"},
  
  obs4 = raw_table[raw_table$A == "5.21" ,"B", drop = T],
  
  
  # HOUSEHOLD COMP. + INCOME -----------------------------------------------------------------------
  
  # Composition
  tibble(m1 = c(raw_table[raw_table$A == "6.1" ,"B", drop = T],
                raw_table[raw_table$A == "6.4" ,"B", drop = T],
                raw_table[raw_table$A == "6.7" ,"B", drop = T],
                raw_table[raw_table$A == "6.10" ,"B", drop = T],
                raw_table[raw_table$A == "6.13" ,"B", drop = T],
                raw_table[raw_table$A == "6.16" ,"B", drop = T],
                raw_table[raw_table$A == "6.19" ,"B", drop = T],
                raw_table[raw_table$A == "6.22" ,"B", drop = T],
                raw_table[raw_table$A == "6.25" ,"B", drop = T],
                raw_table[raw_table$A == "6.28" ,"B", drop = T]),
         
         m2 = c(raw_table[raw_table$A == "6.2" ,"B", drop = T],
                raw_table[raw_table$A == "6.5" ,"B", drop = T],
                raw_table[raw_table$A == "6.8" ,"B", drop = T],
                raw_table[raw_table$A == "6.11" ,"B", drop = T],
                raw_table[raw_table$A == "6.14" ,"B", drop = T],
                raw_table[raw_table$A == "6.17" ,"B", drop = T],
                raw_table[raw_table$A == "6.20" ,"B", drop = T],
                raw_table[raw_table$A == "6.23" ,"B", drop = T],
                raw_table[raw_table$A == "6.26" ,"B", drop = T],
                raw_table[raw_table$A == "6.29" ,"B", drop = T])) %>% 
    
    filter(across(everything(), ~!is.na(.x))) %>% 
    mutate(m1 = case_when(str_detect(m1, "A$") | m1 == "MADRE" ~ "FEM",
                          TRUE ~ "MALE") %>% as.factor(),
           m2 = as.numeric(m2)) %>%
    
    summarize(adult_may = sum(m2 >= 65),
              male_adult = sum(m1 == "MALE" & m2 >= 18 & m2 < 65),
              female_adult = sum(m1 == "FEM" & m2 >= 18 & m2 < 65),
              male_teen = sum(m1 == "MALE" & m2 >= 12 & m2 < 18),
              female_teen = sum(m1 == "FEM" & m2 >= 12 & m2 < 18),
              children = sum(m2 < 12)),
  
  # Escolaridad
  tibble(m1 = c(raw_table[raw_table$A == "6.3" ,"B", drop = T],
                raw_table[raw_table$A == "6.6" ,"B", drop = T],
                raw_table[raw_table$A == "6.9" ,"B", drop = T],
                raw_table[raw_table$A == "6.12" ,"B", drop = T],
                raw_table[raw_table$A == "6.15" ,"B", drop = T],
                raw_table[raw_table$A == "6.18" ,"B", drop = T],
                raw_table[raw_table$A == "6.21" ,"B", drop = T],
                raw_table[raw_table$A == "6.24" ,"B", drop = T],
                raw_table[raw_table$A == "6.27" ,"B", drop = T],
                raw_table[raw_table$A == "6.30" ,"B", drop = T])) %>% 
           
    filter(!is.na(m1)) %>% 
    summarize(educ_mas_sec = sum(str_detect(m1, "AVANZADO"))),
  
  # Income
  
  
    
    
    
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
    
    
  
  
  
  
  
  
  

  
    
  
  
  
  
  
) %>% 
  #str()
  View()
      
tractor ownership (4.12)
yunta ownership (4.13)



