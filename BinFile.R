## Pob fonctionne, mais quand il s'agit de scinder ma data pour n'avoir que les extrêmes values, je ne parviens pas:
# Question 1: Comment faire pour obtenir les extrêmes valeurs, sachant que cette formule réduit ma amtrice?
W1 <- sales10adj %>% filter(Product_1 > u) %>% mutate( W1= Product_1 - u)

w_product1 <- W1$W1
filter(Product_1>u,Product_2>u ,Product_3>u,Product_4>u ,Product_5>u ,Product_6>u ,Product_7>u ,Product_8>u ,Product_9>u ,Product_10>u)

mutate(W1 = Product_1 - u) %>% mutate(W2 = Product_2 - u) %>% mutate(W3 = Product_3 - u) %>% mutate(W4 = Product_4 - u) %>% mutate(W5 = Product_5 - u) %>% mutate(W6 = Product_6 - u) %>% mutate(W7 = Product_7 - u) %>% mutate(W8 = Product_8 - u) %>% mutate(W9 = Product_9 - u) %>% mutate(W10 = Product_10 - u)
W
Sales <- sales10adj
Sales %>& mapply(c(Product_1 >u))
select_(c("W1", "W2", "W3", "W4", "W5", "W6", "W7", "W8", "W9", "W10"))

# Je voulais à la limite faire des vecteurs puis les assembler, mais les dimensions ne correspondraient pas.
# ci-dessous, la logique appliquée dans le practical 2
W <-  df %>% dplyr::filter(sales10adj > u) %>% dplyr::mutate(W = sales10adj - u) %>% dplyr::select(W)



####### Autres tentatives infructueuses, mais en ressource
cop <-loglikCopula(u, copula,
             error = c("-Inf", "warn-Inf", "let-it-be"))
## Generic [and "rotCopula" method] :
fitCopula(cop, pob)
# S4 method for copula
giraffe <- fitCopula(cop, pob,
          method = c("mpl", "ml", "itau", "irho", "itau.mpl"),
          posDef = is(copula, "ellipCopula"),
          start = NULL, lower = NULL, upper = NULL,
          optim.method = optimMeth(copula, method, dim = d),
          optim.control = list(maxit=1000),
          estimate.variance = NA, hideWarnings = FALSE, …)
giraffe
optimMeth(copula, method, dim)