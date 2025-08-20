teste.fisher <- function(var1, var2, titulo = "Teste Exato de Fisher", 
                         x = NULL, y = "Proporção", mostrar_tabela = TRUE, ajuda = FALSE) {
  library(ggplot2)
  library(dplyr)
  
  if (ajuda || missing(var1) || missing(var2)) {
    cat("
Função teste.fisher()

Descrição:
  Realiza o Teste Exato de Fisher para avaliar se há associação significativa entre duas variáveis categóricas.

Quando usar:
  - Quando você tem **duas variáveis categóricas** (fatores).
  - Quando a **tabela de contingência é pequena (especialmente 2x2)**.
  - Quando há **frequências esperadas menores que 5** — situação onde o teste qui-quadrado não é confiável.
  - É o teste preferido para amostras pequenas.

Diferença entre Fisher e Qui-quadrado:
  - **Fisher** calcula a probabilidade exata das combinações observadas.
  - **Qui-quadrado** usa uma aproximação baseada em distribuições teóricas, o que exige maiores tamanhos amostrais.

Limitações:
  - Para tabelas maiores que 2x2, o cálculo exato pode ser **computacionalmente lento** — o R usa aproximações.

Argumentos:
  var1, var2     : variáveis categóricas
  titulo         : título do gráfico
  x, y           : nomes dos eixos
  mostrar_tabela : exibe ou não a tabela de contingência
  ajuda          : se TRUE, mostra esta explicação

Exemplo:
  grupo <- c(\"A\", \"A\", \"B\", \"B\", \"A\", \"B\")
  resposta <- c(\"Sim\", \"Não\", \"Sim\", \"Sim\", \"Não\", \"Não\")
  teste.fisher(grupo, resposta)
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
  
  if (any(dim(tabela) > 2)) {
    message("⚠️ A tabela tem dimensão maior que 2x2. O teste de Fisher pode ser computacionalmente pesado e o p-valor é uma aproximação.")
  }
  
  teste <- fisher.test(tabela)
  
  cat("Teste Exato de Fisher\n")
  cat("---------------------\n")
  cat("p-valor:", signif(teste$p.value, 4), "\n")
  
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

