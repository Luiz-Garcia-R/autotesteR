qui.quadrado <- function(var1, var2, titulo = "Teste Qui-Quadrado", 
                         x = NULL, y = "Proporção", mostrar_tabela = TRUE, ajuda = FALSE) {

  if (ajuda || missing(var1) || missing(var2)) {
    cat("
Função qui.quadrado()

Descrição:
  Realiza o Teste Qui-Quadrado de Pearson para avaliar associação entre duas variáveis categóricas.

Quando usar:
  - Quando você tem duas variáveis categóricas com **amostras grandes**.
  - A tabela de contingência pode ter **mais de 2 categorias** por variável.
  - As **frequências esperadas** nas células da tabela devem ser **≥ 5**.

Assunções:
  - As observações devem ser independentes.
  - As categorias devem ser mutuamente exclusivas.
  - As frequências esperadas devem ser suficientemente grandes (≥ 5 por célula).

Limitações:
  - **Não recomendado para tabelas pequenas**, especialmente 2x2 com frequências esperadas < 5.
  - Para essas situações, use `teste.fisher()`.

Comparação:
  - **Qui-quadrado**: usa aproximação estatística (boa para amostras grandes).
  - **Fisher**: calcula p-valores exatos (melhor para amostras pequenas).

Argumentos:
  var1, var2     : variáveis categóricas
  titulo         : título do gráfico
  x, y           : rótulos dos eixos
  mostrar_tabela : se TRUE, exibe a tabela de contingência
  ajuda          : se TRUE, exibe esta explicação

Exemplo:
  grupo <- c(\"A\", \"A\", \"B\", \"B\", \"A\", \"B\")
  resposta <- c(\"Sim\", \"Não\", \"Sim\", \"Sim\", \"Não\", \"Não\")
  qui.quadrado(grupo, resposta)
")
    return(invisible(NULL))
  }
  
  pacotes <- c("ggplot2", "dplyr")
  for (pkg in pacotes) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("O pacote", pkg, "não está instalado. Instale com install.packages(\"", pkg, "\")"))
    }
  }
  
  library(ggplot2)
  library(dplyr)
  
  
  if (length(var1) != length(var2)) stop("As variáveis devem ter o mesmo comprimento.")
  
  nome1 <- deparse(substitute(var1))
  nome2 <- deparse(substitute(var2))
  nome1 <- sub(".*\\$", "", nome1)
  nome2 <- sub(".*\\$", "", nome2)
  
  if (is.null(x)) x <- nome1
  if (is.null(y)) y <- "Proporção"
  
  tabela <- table(var1, var2)
  
  if (mostrar_tabela) {
    cat("Tabela de contingência (observada):\n")
    print(tabela)
    cat("\n")
  }
  
  teste <- suppressWarnings(chisq.test(tabela))
  
  freq_esperadas <- teste$expected
  if (any(freq_esperadas < 5)) {
    message("⚠️ Algumas frequências esperadas são < 5. Considere usar o teste exato de Fisher para maior precisão.")
  }
  
  cat("Teste Qui-Quadrado de Pearson\n")
  cat("------------------------------\n")
  cat("Estatística X²:", round(teste$statistic, 3), "\n")
  cat("p-valor:", signif(teste$p.value, 4), "\n")
  cat("Graus de liberdade:", teste$parameter, "\n")
  
  df_plot <- data.frame(var1 = var1, var2 = var2)
  df_prop <- df_plot %>%
    group_by(var1, var2) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(var1) %>%
    mutate(prop = n / sum(n))
  
  g <- ggplot(df_prop, aes(x = var1, y = prop, fill = var2)) +
    geom_bar(stat = "identity", position = "stack") +
    labs(
      title = titulo,
      x = x,
      y = y,
      fill = nome2
    ) +
    theme_minimal() +
    theme(
      legend.title = element_text(size = 10),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(g)
  invisible(teste)
}





