#' Funcao pre.teste() para sugestao de teste estatistico
#'
#' Identifica automaticamente se os dados sao numericos ou categoricos e sugere o teste estatistico mais adequado.
#'
#' Para dados numericos:
#' - Verifica normalidade (Shapiro, Anderson-Darling ou KS)
#' - Verifica homogeneidade de variancia
#' - Sugere teste t/ANOVA ou Mann-Whitney/Kruskal-Wallis
#'
#' Para dados categoricos:
#' - Cria tabela de contingencia
#' - Sugere qui-quadrado, teste exato de Fisher ou teste G
#'
#' @param ... Dois ou mais vetores (numericos ou categoricos)
#' @param alpha Nivel de significancia. Padrao = 0.05
#' @param ajuda Logico. Se TRUE, mostra ajuda detalhada
#'
#' @return Mensagens com sugestao de teste estatistico apropriado
#' @export
#' @importFrom stats sd
#'
#' @examples
#' g1 <- c(1, 2, 3)
#' g2 <- c(4, 5, 6)
#' pre.teste(g1, g2)
#'
#' grupo1 <- c("A", "B", "A", "B")
#' grupo2 <- c("Sim", "Sim", "Nao", "Nao")
#' pre.teste(grupo1, grupo2)

pre.teste <- function(..., alpha = 0.05, ajuda = FALSE) {
  grupos <- list(...)
  nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
  nomes <- sub("^.*\\$", "", nomes_raw)

  if (ajuda || length(grupos) < 2) {
    cat("\nFuncao pre.teste()\n\nDescricao:\n  Identifica automaticamente se os dados sao numericos ou categoricos e sugere o teste estatistico adequado.\n\nPara dados numericos:\n  - Verifica normalidade (Shapiro, Anderson-Darling ou KS conforme n)\n  - Verifica homogeneidade de variancia\n  - Sugere teste t/ANOVA ou Mann-Whitney/Kruskal-Wallis\n\nPara dados categoricos:\n  - Cria tabela de contingencia\n  - Avalia condicoes para teste do qui-quadrado ou teste exato de Fisher\n\nArgumentos:\n  ...   : dois ou mais vetores numericos ou categoricos\n  alpha : nivel de significancia (padrao = 0.05)\n  ajuda : se TRUE, exibe esta explicacao\n\nExemplo com valores numericos:\n    g1 <- c(1, 2, 3)\n    g2 <- c(4, 5, 6)\n    pre.teste(g1, g2)\n\nExemplo com valores categoricos\n    grupo1 <- c('A', 'B', 'A', 'B')\n    grupo2 <- c('Sim', 'Sim', 'Nao', 'Nao')\n    pre.teste(grupo1, grupo2)\n")
    return(invisible(NULL))
  }

  # Detectar tipo de dado
  sao_numericos <- all(sapply(grupos, is.numeric))
  sao_categoricos <- all(sapply(grupos, function(x) is.factor(x) || is.character(x)))

  if (!sao_numericos && !sao_categoricos) {
    stop("Os dados devem ser todos numericos ou todos categoricos.")
  }

  if (sao_numericos) {
    ## --- FLUXO PARA DADOS NUMERICOS ---
    valores <- unlist(grupos)
    grupo <- factor(rep(nomes, times = sapply(grupos, length)))
    dados <- data.frame(valor = valores, grupo = grupo)

    aplicar_teste_normalidade <- function(x) {
      n <- length(x)
      if (n <= 50) {
        p <- shapiro.test(x)$p.value
        metodo <- "Shapiro-Wilk"
      } else if (n <= 300) {
        p <- nortest::ad.test(x)$p.value
        metodo <- "Anderson-Darling"
      } else {
        ks_result <- ks.test(x, "pnorm", mean = mean(x), sd = sd(x))
        p <- ks_result$p.value
        metodo <- "Kolmogorov-Smirnov"
      }
      list(p = p, metodo = metodo)
    }

    normalidade <- lapply(grupos, aplicar_teste_normalidade)
    p_values <- sapply(normalidade, function(x) x$p)
    metodos <- sapply(normalidade, function(x) x$metodo)

    normalidade_df <- data.frame(
      grupo = nomes,
      n = sapply(grupos, length),
      metodo = metodos,
      p_valor = signif(p_values, 3),
      normal = p_values > alpha
    )

    p_levene <- if (length(grupos) > 2) {
      car::leveneTest(valor ~ grupo, data = dados)$`Pr(>F)`[1]
    } else {
      var.test(grupos[[1]], grupos[[2]])$p.value
    }
    homogeneo <- p_levene > alpha

    cat("Resultados dos testes de normalidade (baseado em n):\n\n")
    print(normalidade_df)

    cat("\n Homogeneidade de variancias:\n")
    cat(ifelse(homogeneo,
               paste0("Variancias homogeneas (p = ", signif(p_levene, 3), ")\n"),
               paste0("Variancias heterogeneas (p = ", signif(p_levene, 3), ")\n")))

    cat("\n Recomendacao de teste estatistico:\n")
    if (all(normalidade_df$normal)) {
      if (homogeneo) {
        cat(ifelse(length(grupos) == 2, "Use: teste t\n", "Use: ANOVA\n"))
      } else {
        cat(ifelse(length(grupos) == 2, "Use: Welch's t-test\n", "Use: ANOVA com correcao de Welch ou Kruskal-Wallis\n"))
      }
    } else {
      cat(ifelse(length(grupos) == 2, "Use: teste de Mann-Whitney\n", "Use: teste de Kruskal-Wallis\n"))
    }

    return(invisible(list(normalidade = normalidade_df, p_levene = p_levene)))
  }

  if (sao_categoricos) {
    ## --- FLUXO PARA DADOS CATEGORICOS ---
    categorias <- lapply(grupos, as.factor)
    tabela <- table(categorias[[1]], categorias[[2]])

    cat("Tabela de Contingencia:\n")
    print(tabela)

    cat("\n Recomendacao de teste de associacao:\n")
    if (any(tabela < 5)) {
      cat("Ha valores esperados < 5. Recomendado: Teste exato de Fisher.\n")
    } else {
      cat("Frequencias adequadas. Recomendado: Teste do Qui-quadrado.\n")
    }

    return(invisible(list(tabela = tabela)))
  }
}
