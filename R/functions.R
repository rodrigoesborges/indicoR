#' Configurar cliente Indico
#' @param base_url URL base do servidor Indico (ex: "https://indico.example.com")
#' @param token Token de API (Bearer token)
#' @export
indico_config <- function(base_url, token) {
  options(
    indico.base_url = sub("/$", "", base_url), # remover trailing slash
    indico.token = token
  )
  invisible(TRUE)
}

#' Obter configuração
indico_get_config <- function() {
  list(
    base_url = getOption("indico.base_url"),
    token = getOption("indico.token")
  )
}

#' Procurar ID de pessoa pelo email
#' @param email Email do utilizador
#' @export
indico_find_person_id <- function(email) {
  config <- indico_get_config()
  
  resp <- request(paste0(config$base_url, "/api/users/search")) %>%
    req_headers(Authorization = paste("Bearer", config$token)) %>%
    req_url_query(q = email) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE)
  
  if (length(resp) == 0) {
    warning("Nenhum utilizador encontrado para: ", email)
    return(NA_integer_)
  }
  
  # retorna o primeiro resultado
  resp[1, "id"]
}

#' Obter tipos de contribuição de um evento
#' @param event_id ID do evento
#' @export
indico_get_contribution_types <- function(event_id) {
  config <- indico_get_config()
  
  resp <- request(paste0(config$base_url, "/api/events/", event_id, "/contribution-types")) %>%
    req_headers(Authorization = paste("Bearer", config$token)) %>%
    req_perform() %>%
    resp_body_json(simplifyVector = TRUE)
  
  setNames(resp$id, resp$name)
}

#' Criar uma contribuição num evento
#' @param event_id ID do evento
#' @param title Título da contribuição
#' @param duration Duração em minutos (default: 30)
#' @param description Descrição (opcional)
#' @param speaker_ids Vector de IDs de oradores
#' @param type_id ID do tipo de contribuição (opcional)
#' @param verbose Mostrar progresso (default: TRUE)
#' @export
indico_create_contribution <- function(event_id, title, duration = 30,
                                       description = "", speaker_ids = NULL,
                                       type_id = NULL, verbose = TRUE) {
  config <- indico_get_config()
  
  if (is.null(config$token) || is.null(config$base_url)) {
    stop("Configure primeiro com indico_config(base_url, token)")
  }
  
  # preparar dados
  persons <- if (!is.null(speaker_ids) && length(speaker_ids) > 0) {
    lapply(speaker_ids, function(id) {
      list(personId = id, roles = list("speaker"))
    })
  } else {
    list()
  }
  
  body <- list(
    title = title,
    duration = duration,
    description = description,
    persons = persons
  )
  
  if (!is.null(type_id)) {
    body$type = type_id
  }
  
  if (verbose) {
    message(sprintf("A criar '%s' no evento %s...", substr(title, 1, 50), event_id))
  }
  
  # fazer pedido
  resp <- request(paste0(config$base_url, "/api/events/", event_id, "/contributions")) %>%
    req_headers(
      Authorization = paste("Bearer", config$token),
      "Content-Type" = "application/json"
    ) %>%
    req_body_json(body) %>%
    req_error(is_error = ~.status >= 400) %>%
    req_perform()
  
  if (resp_status(resp) >= 400) {
    error_body <- resp_body_json(resp)
    stop(sprintf("Erro %s: %s", resp_status(resp), error_body$message))
  }
  
  if (verbose) message("✓ Sucesso")
  
  resp_body_json(resp, simplifyVector = TRUE)
}

#' Adicionar contribuições a partir de um data.frame
#' @param df Data.frame com colunas: event_id, title, speaker_email, duration, description, type
#' @param default_event_id Evento único se df não tiver event_id (opcional)
#' @param default_type Tipo padrão se não especificado (ex: "Talk")
#' @param skip_errors Continuar em caso de erro (default: FALSE)
#' @export
indico_add_contributions_from_df <- function(df, default_event_id = NULL,
                                             default_type = NULL,
                                             skip_errors = FALSE) {
  
  # validar data.frame
  required_cols <- "title"
  if (is.null(default_event_id) && !"event_id" %in% names(df)) {
    stop("Data.frame deve conter 'event_id' ou forneça default_event_id")
  }
  if (!all(required_cols %in% names(df))) {
    stop("Data.frame deve conter pelo menos a coluna 'title'")
  }
  
  # garantir colunas opcionais
  if (!"event_id" %in% names(df) && !is.null(default_event_id)) {
    df$event_id <- default_event_id
  }
  if (!"duration" %in% names(df)) df$duration <- 30
  if (!"description" %in% names(df)) df$description <- ""
  if (!"speaker_email" %in% names(df)) df$speaker_email <- NA
  if (!"type" %in% names(df) && !is.null(default_type)) {
    df$type <- default_type
  }
  
  # cache para IDs de tipos e pessoas
  type_cache <- list()
  person_cache <- list()
  
  results <- vector("list", nrow(df))
  
  for (i in seq_len(nrow(df))) {
    row <- df[i, ]
    
    tryCatch({
      # obter ID do tipo se necessário
      type_id <- NULL
      if (!is.na(row$type)) {
        event_id <- row$event_id
        if (is.null(type_cache[[event_id]])) {
          type_cache[[event_id]] <- indico_get_contribution_types(event_id)
        }
        type_id <- type_cache[[event_id]][row$type]
      }
      
      # obter IDs de oradores
      speaker_ids <- NULL
      if (!is.na(row$speaker_email)) {
        emails <- strsplit(row$speaker_email, ";|,")[[1]]
        emails <- trimws(emails)
        speaker_ids <- sapply(emails, function(email) {
          if (is.null(person_cache[[email]])) {
            person_cache[[email]] <- indico_find_person_id(email)
          }
          person_cache[[email]]
        })
        speaker_ids <- speaker_ids[!is.na(speaker_ids)]
      }
      
      # criar contribuição
      results[[i]] <- indico_create_contribution(
        event_id = row$event_id,
        title = row$title,
        duration = row$duration,
        description = row$description,
        speaker_ids = speaker_ids,
        type_id = type_id,
        verbose = TRUE
      )
      
    }, error = function(e) {
      msg <- sprintf("Erro na linha %d ('%s'): %s", i, row$title, e$message)
      if (skip_errors) {
        warning(msg)
        results[[i]] <- NULL
      } else {
        stop(msg)
      }
    })
  }
  
  # retornar data.frame com resultados
  successful <- results[!sapply(results, is.null)]
  if (length(successful) > 0) {
    bind_rows(lapply(successful, as.data.frame.list))
  } else {
    data.frame()
  }
}
```
