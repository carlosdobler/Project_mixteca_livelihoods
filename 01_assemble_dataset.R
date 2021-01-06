

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
  
  # ***** BASICS *****
  
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
    {if(is.na(.) & !is.na(terrenos) & str_detect(terrenos, "ER", negate = T)) "ER: Falta dato (no. de terrenos no nulo)" else
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
  obs1 = raw_table[raw_table$A == "2.24" ,"B", drop = T]) %>% View()
  
  
  # ***** GOV'T PROGRAMS *****
  
  # procampo (annual transfer)
  procampo = raw_table[raw_table$A == "3.1" ,"B", drop = T] %>%
    {if(is.na(.)) "ERROR" else
      case_when(str_detect(., "NADA") ~ as.character(0), 
                str_detect(., "DE $1,000") ~ as.character(500),
                str_detect(., "$1,999") ~ as.character(1500),
                str_detect(., "$2,999") ~ as.character(2500),
                str_detect(., "$3,999") ~ as.character(3500),
                str_detect(., "$4,999") ~ as.character(4500),
                str_detect(., "O MÁS") ~ as.character(5500))}, # transform to integer
  
  # prospera (number of beneficiaries)
  prospera = raw_table[raw_table$A == "3.2" ,"B", drop = T] %>%
    {if(is.na(.)) "ERROR" else
      case_when(str_detect(., "NINGUNO") ~ as.character(0), 
                str_detect(., "1") ~ as.character(1),
                str_detect(., "DE 2") ~ as.character(3),
                TRUE ~ "2")}, # transform to integer
  
  # adultos mayores (number of beneficiaries)
  adultosmay = raw_table[raw_table$A == "3.3" ,"B", drop = T] %>%
    {if(is.na(.)) "ERROR" else
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
  
  other_progs = raw_table[raw_table$A == "3.7" ,"B", drop = T],
  
  # pension
  pension_a = raw_table[raw_table$A == "3.8" ,"B", drop = T] %>% 
  {case_when(str_detect(., "NADA") | is.na(.) ~ 0, 
             str_detect(., "DE $1,000") ~ 500,
             str_detect(., "$1,999") ~ 1500,
             str_detect(., "$2,999") ~ 2500,
             str_detect(., "$3,999") ~ 3500,
             str_detect(., "$4,999") ~ 4500,
             TRUE ~ parse_number(.))},
  
  pension = raw_table[raw_table$A == "3.9" ,"B", drop = T] %>%
    {if(is.na(.) | . == "AL AÑO") pension_a else pension_a*12},
  
  # observations?
  obs2 = raw_table[raw_table$A == "3.10" ,"B", drop = T],
  
  
  # ***** AGRICULTURE *****
  
  # Irrigation
  riego = raw_table[raw_table$A == "4.1" ,"B", drop = T] %>%
    {if(is.na(.)) "ERROR" else if(. == "SÍ") "T" else "F"},
  
  # Irrigation type
  riego_rio = raw_table[raw_table$A == "4.2" ,"B", drop = T] %>% 
    {if(riego == "ERROR" | (riego == "F" & !is.na(.))) "ERROR" else
      if(is.na(.)) "F" else "T"},
  
  riego_cont = raw_table[raw_table$A == "4.3" ,"B", drop = T] %>% 
    {if(riego == "ERROR" | (riego == "F" & !is.na(.))) "ERROR" else
      if(is.na(.)) "F" else "T"},
  
  riego_sub = raw_table[raw_table$A == "4.4" ,"B", drop = T] %>% 
    {if(riego == "ERROR" | (riego == "F" & !is.na(.))) "ERROR" else
      if(is.na(.)) "F" else "T"},
  
  # Irrigation availability
  riego_perm = tibble(int = raw_table[raw_table$A == "4.5" ,"B", drop = T],
                      perm = raw_table[raw_table$A == "4.6" ,"B", drop = T],
                      riego = riego) %>% 
    
    {if(is.na(.$int) & is.na(.$perm) & .$riego == "T") "ERROR" else
       if(is.na(.$int) & is.na(.$perm) & .$riego == "F") NA else
         if(is.na(.$int) & !is.na(.$perm)) "T" else "F"}
  
  # Cropping cycles
  ciclos = raw_table[raw_table$A == "4.9" ,"B", drop = T] %>%
    {if(is.na(.)) "ERROR" else
      if(tierra_sembrada == "0" & tierra_adic == "0" & str_detect(., "0", negate = T)) "ERROR" else
        if(str_detect(., "0")) NA else as.character(.)}, # transform to integer
  
  # Cropping sites
  sites = raw_table[raw_table$A == "4.10" ,"B", drop = T] %>%
  {if(tierra_sembrada == "0" & tierra_adic == "0" & str_detect(., "0", negate = T)) "ERROR" else
      if(str_detect(., "0")) NA else as.character(.)}, # transform to integer
    
  
  
  
  
  
) %>% 
  select(-pension_a) %>% 
  #str()
  View()
      






# tierras
raw_table[raw_table$A == "2.7" ,"B", drop = T] %>% 
  {if(. != "0 (NO TIENEN TIERRA)") as.logical(T) else as.logical(F)}
  
