#' Funcao pre.teste() para sugestao de teste estatistico
#'
#' Identifica automaticamente se os dados sao numericos ou categoricos e sugere o teste estatistico mais adequado.
#'
#' @param ... Dois ou mais vetores (numericos ou categoricos)
#' @param alpha Nivel de significancia. Padrao = 0.05
#' @param ajuda Logico. Se TRUE, mostra ajuda detalhada
#' @param verbose Logico. Se TRUE, imprime mensagens informativas
#' @return Lista invisivel com resultados dos testes de normalidade, homogeneidade ou tabela de contingencia e recomendacao do teste
#' @export

pre.teste <- function(..., alpha = 0.05, ajuda = FALSE, verbose = TRUE) {
  grupos <- list(...)
  nomes_raw <- as.character(match.call(expand.dots = FALSE)$...)
  nomes <- sub("^.*\\$", "", nomes_raw)

  if (ajuda || length(grupos) < 2) {
    if (verbose) {
      message("
Funcao pre.teste()

Descricao:
  Identifica automaticamente se os dados sao numericos ou categoricos e sugere o teste estatistico adequado.

Para dados numericos:
  - Verifica normalidade (Shapiro, Anderson-Darling ou KS conforme n)
  - Verifica homogeneidade de variancia
  - Sugere teste t/ANOVA ou Mann-Whitney/Kruskal-Wallis

Para dados categoricos:
  - Cria tabela de contingencia
  - Avalia condicoes para teste do qui-quadrado ou teste exato de Fisher

Argumentos:
  ...      : dois ou mais vetores numericos ou categoricos
  alpha    : nivel de significancia (padrao = 0.05)
  ajuda    : se TRUE, exibe esta explicacao
  verbose  : se TRUE, imprime mensagens informativas

Exemplo com valores numericos:
    g1 <- c(1,2,3)
    g2 <- c(4,5,6)
    pre.teste(g1, g2)

Exemplo com valores categoricos:
    grupo1 <- c('A','B','A','B')
    grupo2 <- c('Sim','Sim','Nao','Nao')
    pre.teste(grupo1, grupo2)
")
    }
    return(invisible(NULL))
  }

  # Detectar tipo de dado
  sao_numericos <- all(sapply(grupos, is.numeric))
  sao_categoricos <- all(sapply(grupos, function(x) is.factor(x) || is.character(x)))

  if (!sao_numericos && !sao_categoricos) {
    stop("Os dados devem ser todos numericos ou todos categoricos.")
  }

  if (sao_numericos) {
    # --- DADOS NUMERICOS ---
    valores <- unlist(grupos)
    grupo <- factor(rep(nomes, times = sapply(grupos, length)))
    dados <- data.frame(valor = valores, grupo = grupo)

    aplicar_teste_normalidade <- function(x) {
      n <- length(x)
      if (n <= 50) {
        p <- stats::shapiro.test(x)$p.value
        metodo <- "Shapiro-Wilk"
      } else if (n <= 300) {
        p <- nortest::ad.test(x)$p.value
        metodo <- "Anderson-Darling"
      } else {
        ks_res <- stats::ks.test(x, "pnorm", mean = mean(x), sd = sd(x))
        p <- ks_res$p.value
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

    # Teste de homogeneidade
    if (length(grupos) > 2) {
      p_levene <- car::leveneTest(valor ~ grupo, data = dados)$`Pr(>F)`[1]
    } else {
      p_levene <- stats::var.test(grupos[[1]], grupos[[2]])$p.value
    }
    homogeneo <- p_levene > alpha

    # Sugestao de teste
    recomendacao <- if (all(normalidade_df$normal)) {
      if (homogeneo) if (length(grupos) == 2) "teste t" else "ANOVA" else
        if (length(grupos) == 2) "Welch's t-test" else "ANOVA com correcao de Welch ou Kruskal-Wallis"
    } else {
      if (length(grupos) == 2) "teste de Mann-Whitney" else "teste de Kruskal-Wallis"
    }

    if (verbose) {
      message("Resultados dos testes de normalidade:")
      print(normalidade_df)
      message("Homogeneidade de variancias: ",
              ifelse(homogeneo, paste0("homogeneas (p = ", signif(p_levene,3), ")"),
                     paste0("heterogeneas (p = ", signif(p_levene,3), ")")))
      message("Recomendacao do teste estatistico: ", recomendacao)
    }

    return(invisible(list(
      normalidade = normalidade_df,
      p_levene = p_levene,
      homogeneo = homogeneo,
      recomendacao = recomendacao
    )))
  }

  if (sao_categoricos) {
    # --- DADOS CATEGORICOS ---
    categorias <- lapply(grupos, as.factor)
    tabela <- table(categorias[[1]], categorias[[2]])

    recomendacao <- if (any(tabela < 5)) "Teste exato de Fisher" else "Teste do Qui-quadrado"

    if (verbose) {
      message("Tabela de contingencia:")
      print(tabela)
      message("Recomendacao do teste de associacao: ", recomendacao)
    }

    return(invisible(list(
      tabela = tabela,
      recomendacao = recomendacao
    )))
  }
}

