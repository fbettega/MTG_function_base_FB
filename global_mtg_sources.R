################################################################################
#######################  Not in function  ######################################
deck_parser <- function(deck_path) {
  deck <- read.delim(deck_path, header = FALSE, blank.lines.skip = FALSE) %>%
    filter(!str_detect(.$V1, regex("deck|Sideboard", ignore_case = TRUE))) %>%
    mutate(sep_side = str_detect(.$V1, regex("^$", ignore_case = TRUE))) %>%
    mutate(
      quantite = as.numeric(str_extract_all(.$V1, "^[:digit:]*\\S*")),
      nom = tolower(str_extract(.$V1, "(?<=[:digit:]\\s).*")),
    ) %>%
    select(-V1)
}

################################################################################
#######################  Not in function  ######################################
`%notin%` <- Negate(`%in%`)
################################################################################
#######################  Not in function  ######################################
# simple function that name list
name_list_of_model_with_string <- function(list, string) {
  names(list) <- string
  return(list)
}

################################################################################
############# function that get scry fall id from a df with card_name ##########
join_with_scryfall <- function(
    Df_with_cardname,
    cardname_col ,
    scry_fall_df 
){
  scry_fall_df_lower <- scry_fall_df %>% 
    mutate(name =  tolower(name))
  
  
  df_with_card_name_to_match_fun <- Df_with_cardname %>% #modify 
    select(
      #modify 
      all_of(cardname_col)
    ) %>% 
    rename(CardName = !!cardname_col) %>% 
    distinct() %>% 
    mutate(
      CardName_base = CardName,
      CardName = tolower(CardName)
      )
  
  
  
  
  initial_match <-  df_with_card_name_to_match_fun %>%
    left_join(
      scry_fall_df_lower,
      by = c(
        #modify 
        "CardName" = "name"
      )
    ) %>% 
    select(
      #modify 
      CardName,CardName_base,id) %>%
    filter(!is.na(id))

  
  
  match_double_face <-  df_with_card_name_to_match_fun  %>% 
    filter(CardName %notin% initial_match$CardName) %>% 
    # select(-id) %>%
    left_join(
      scry_fall_df_lower %>%
        filter(id %notin% initial_match$id) %>%
        mutate(
          name = str_remove(name,"\\s+//.*$")
        ),
      by = c("CardName" = "name")
    ) %>% 
    select(
      #modify 
      CardName,CardName_base,id
    ) %>%
    filter(!is.na(id))
  
  
  room_matching <- df_with_card_name_to_match_fun %>% 
    filter(CardName %notin% c(initial_match$CardName,match_double_face$CardName)) %>% 
    mutate(
      CardName_temp = str_replace(CardName,"&&","//")
    )  %>% 
    # select(-id) %>%
    left_join(
      scry_fall_df_lower,
      by = c("CardName_temp" = "name")
    )  %>% 
    select(
      #modify 
      CardName,CardName_base,id
    )
  
  
  
  
  res <- rbind(
    initial_match,
    match_double_face,
    room_matching
  ) %>% 
    select(- CardName) %>% 
    rename(
      CardName = CardName_base ,           
      scry_fall_id = id
      ) 
  
  
  return(res)
}
