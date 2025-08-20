teste.t <- function(..., titulo = "Teste t", x = "Grupo", y = "Valor", ajuda = FALSE) {
  # Se o usuário solicitar ajuda ou não fornecer dois grupos, mostra explicação
  grupos <- list(...)
  if (ajuda || length(grupos) != 2) {
    cat("
Função teste.t()

Descrição:
  Realiza o teste t de Student para comparar as médias de dois grupos.

Quando usar:
  - Quando os dados seguem distribuição aproximadamente normal.
  - Quando se deseja comparar a média de dois grupos independentes.
  - Ideal para amostras com tamanho moderado a grande (n > 30) ou quando a normalidade pode ser assumida.

Argumentos:
  ...     : dois vetores numéricos (ex: grupo1, grupo2)
  titulo  : título do gráfico (padrão = 'Teste t')
  x       : nome do eixo x
  y       : nome do eixo y
  ajuda   : se TRUE, exibe essa explicação

Exemplo:
  g1 <- rnorm(10, 5)
  g2 <- rnorm(10, 6)
  teste.t(g1, g2)

")
    return(invisible(NULL))
  }
  
  # Verifica e carrega pacotes necessários
  pacotes_necessarios <- c("ggplot2")
  for (pkg in pacotes_necessarios) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      stop(paste("O pacote", pkg, "não está instalado. Instale com install.packages(\"", pkg, "\")"))
    }
  }
  
  library(ggplot2)
  
  # Captura dos nomes das variáveis
  nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
  nomes <- sub("^.*\\$", "", nomes_raw)
  
  # Verifica se os dados são numéricos
  if (!all(sapply(grupos, is.numeric))) stop("Todos os grupos devem ser vetores numéricos.")
  
  # Criação do data frame
  valores <- unlist(grupos)
  grupo <- factor(rep(nomes, times = sapply(grupos, length)), levels = nomes)
  dados <- data.frame(valor = valores, grupo = grupo)
  
  # Teste t
  resultado <- t.test(grupos[[1]], grupos[[2]])
  pval <- resultado$p.value
  
  # Rótulo do p-valor
  p_label <- if (pval < 0.001) {
    "p < 0.001"
  } else {
    paste0("p = ", signif(pval, 3))
  }
  
  # Asteriscos de significância
  signif_label <- if (pval < 0.001) {
    "***"
  } else if (pval < 0.01) {
    "**"
  } else if (pval < 0.05) {
    "*"
  } else {
    ""
  }
  
  # Posição do asterisco no gráfico
  y_max <- max(valores, na.rm = TRUE)
  y_pos <- y_max + 0.1 * abs(y_max)
  
  # Gráfico
  g <- ggplot(dados, aes(x = grupo, y = valor, fill = grupo)) +
    geom_boxplot(alpha = 0.7, outlier.shape = NA) +
    geom_jitter(width = 0.1, alpha = 0.5, color = "black") +
    annotate("text", x = 1.5, y = y_pos, label = signif_label, size = 6) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set2") +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 45, hjust = 1)
    ) +
    labs(
      title = titulo,
      subtitle = p_label,
      x = x,
      y = y
    )
  
  print(g)
  return(resultado)
}


