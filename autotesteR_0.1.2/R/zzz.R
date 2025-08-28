.onAttach <- function(libname, pkgname) {
  if (requireNamespace("crayon", quietly = TRUE)) {
    green <- crayon::green
    bold <- crayon::bold
  } else {
    green <- identity
    bold <- identity
  }

  packageStartupMessage(
    "\n",
    green("autotesteR "), "carregado com sucesso!\n",
    "----------------------------------------------------------\n",
    "Um pacote desenvolvido para facilitar analises estatisticas\n",
    "com uma linguagem acessivel, diagnsticos automaticos e graficos claros.\n",
    "\n",
    "Digite ", green("?autotesteR"), " para ver uma introducaoo geral\n",
    "Digite o nome da formula sem parametros (", bold("ex. teste.t()"), ") para ver uma breve descricao do teste\n",
    "Site oficial e atualizacoes: https://github.com/Luiz-Garcia-R/autotesteR.git\n",
    "----------------------------------------------------------\n"
  )
}

