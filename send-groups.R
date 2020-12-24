library(tidyverse)
library(googlesheets4)
library(blastula)
library(glue)
set.seed(123)

df <- read_sheet("1nUlGsyoJ-SQGeLtUAKYsW1ltMG47p3OzcZE6LXj0xMs") %>% 
  mutate(lp = as.factor(lp)) %>% 
  select(-timestamp,-password) %>% 
  sample_frac(1L)  ## Randomize!

# n_test <- 30
# df <- tibble(
#   name = rep("Fulano de Tal", n_test),
#   email = rep("xxx@gmail.com", n_test),
#   lp = as.factor(sample(c(
#     "LP 1",
#     "LP 2",
#     "LP 3"
#   ), n_test, replace = T))
# )

topicos <- c("Teste de Hipóteses- Teste t",
             "Testes não Paramétricos",
             "Análise Multivariada de Variância - ANOVA",
             "Correlação / Correlação Parcial",
             "Regressão Linear Simples / Múltipla",
             "Regressão Logística")
n_groups <- 6

groups <- df %>%
  group_by( group = (row_number()-1) %/% (n()/n_groups) + 1) %>% 
  nest %>% 
  pull(data) %>% 
  map2(topicos, ~ mutate(.x, topic = .y))

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

groups %>% 
  map(~ smtp_send(
    create_email(.x),
    to = .x$email,
    from = "josees@uni9.pro.br",
    cc = c("josees@uni9.pro.br", "leonardo.vils@uni9.pro.br"),
    subject = glue("[Estatística - CIS] Criação de Grupos - { .x$topic[1] }"),
    credentials = creds_key("uni9pro")))
