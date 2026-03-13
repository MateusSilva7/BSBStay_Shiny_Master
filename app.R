# app.R — seletor de modo para Render/Docker
app_mode <- tolower(trimws(Sys.getenv("APP_MODE", "master")))
app_file <- switch(
  app_mode,
  "master" = "app_master.R",
  "public" = "app_public.R",
  "app_master.R"
)

e <- new.env(parent = globalenv())
sys.source(app_file, envir = e)

if (!exists("app", envir = e, inherits = FALSE)) {
  stop("O arquivo ", app_file, " nao criou o objeto 'app'.")
}

e$app
