dados <- read.table("data/dadoscor.txt", head = TRUE)

attach(dados)

ppcor::pcor.test(Var1, Var2, list(Var3, Var4))
ppcor::pcor.test(Var1, Var3, list(Var2, Var4))
ppcor::pcor.test(Var1, Var4, list(Var2, Var3))