.onAttach <- function(libname, pkgname) {
  packageStartupMessage(
    "\n",
    crayon::green("autotesteR "), "carregado com sucesso!\n",
    "----------------------------------------------------------\n",
    "Um pacote desenvolvido para facilitar analises estatisticas\n",
    "com uma linguagem acessivel, diagnosticos automaticos e graficos claros.\n",
    "\n",
    "Digite ", crayon::green("?autotesteR"), " para ver uma introducao geral\n",
    "Digite o nome da formula sem parametros (", crayon::bold("ex. teste.t()"), ") para ver uma breve descricao do teste\n",
    "Site oficial e atualizacoes: https://github.com/Luiz-Garcia-R/autotesteR.git\n",
    "----------------------------------------------------------\n"
  )
}
