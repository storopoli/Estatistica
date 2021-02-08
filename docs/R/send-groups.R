library(tidyverse)
library(googlesheets4)
library(blastula)
library(glue)
set.seed(123)


#' Crate Randomized Groups of Students
#'
#' @param degree `character` "PPGA" or "CIS"
#' @param topicos `character vector` for the course topics to compose groups
#' @param test `logical` wether to test the function or not with fake student data
#' @param n_test `integer` how many fake student samples to generate
#' @param seed `integer` random seed to RGN
#'
#' @return list of tibbles containing the groups
#' @export
#'
#' @examples
make_groups <- function(degree = c("PPGA", "CIS"), topicos = NULL, test = FALSE, n_test = 60, seed = 123) {
  library(tidyverse)
  library(googlesheets4)
  library(blastula)
  library(glue)
  set.seed(seed)
  if (is.null(topics)) {
    topics <- c("Teste de Hipóteses- Teste t",
                "Testes não Paramétricos",
                "Análise Multivariada de Variância - ANOVA",
                "Correlação / Correlação Parcial",
                "Regressão Linear Simples / Múltipla",
                "Regressão Logística")
  }
  
  n_groups = length(topics)
  if (test == TRUE) {
    df <- tibble(
      name = rep("Fulano de Tal", n_test),
      email = rep("xxx@gmail.com", n_test),
      ra = runif(n_test, 111111111, 999999999),
      degree = sample(c("PPGA", "CIS"), n_test, replace = T),
      lp = as.character(sample(c(1:4), n_test, replace = T))
    )
  }
  else {
    df <- read_sheet("1rd1Cg0emQAKZ652Fqd6cq9n9IVLHmEgKHkTITL2zb4I", .name_repair = janitor::make_clean_names) %>% 
      select(email = email_address,
             name = qual_seu_nome,
             ra = qual_seu_ra,
             degree = qual_seu_curso,
             lp = qual_sua_linha_de_pesquisa) %>% 
      mutate(degree = if_else(str_detect(degree, "Administração"), "PPGA", "CIS"),
             lp = str_extract(lp, "\\d"))
  }
  
  groups <- df %>%
    filter(degree == {{ degree }}) %>% 
    sample_frac(1L) %>%   ## Randomize!
    group_by( group = (row_number()-1) %/% (n()/n_groups) + 1) %>% 
    nest %>% 
    pull(data) %>% 
    map2(topics, ~ mutate(.x, topic = .y))
  
  return(groups)
}



#' Create email for students
#'
#' @param tbl a tibble of student data and a group
#'
#' @return A `blastula::email_message` object.
#' @export
#'
#' @examples
create_email <- function(tbl) {
  body_text <- md(glue("
  Olá,
  
  O seu tópico é **{ tbl$topic[1] }**!
  
  Veja seu grupo abaixo
  
  { knitr::kable(tbl, format = 'html') }
  
  Quaisquer dúvidas entre em contato com os professores da sua disciplina.
                       "))
  footer_text <- glue("Enviado em { format(Sys.time(), '%d/%m/%y %HH:%MM') }.")
  email <- compose_email(body = body_text, footer = footer_text)
  return(email)
}

PPGA <- make_groups("PPGA", test = FALSE)
CIS <- make_groups("CIS", test = FALSE)

PPGA %>% 
  map(~ smtp_send(
    create_email(.x),
    to = .x$email,
    from = "josees@uni9.pro.br",
    cc = "josees@uni9.pro.br",
    subject = glue("[Estatística - PPGA] Criação de Grupos - { .x$topic[[1]] }"),
    credentials = creds_key("uni9pro")))

CIS %>% 
  map(~ smtp_send(
    create_email(.x),
    to = .x$email,
    from = "josees@uni9.pro.br",
    cc = c("josees@uni9.pro.br", "leonardo.vils@uni9.pro.br"),
    subject = glue("[Estatística - CIS] Criação de Grupos - { .x$topic[1] }"),
    credentials = creds_key("uni9pro")))
