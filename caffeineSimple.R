pacman::p_load(bnlearn, BiocManager, Rgraphviz)
## Caffeine
caffeine.dag <- empty.graph(nodes=c('Bgeno', 'Bfeno'))
caffeine.dag <- set.arc(caffeine.dag, from="Bgeno", to="Bfeno")
G.lv = c("SS", "SF", "FF")
I.lv = c("S", "F")
F.lv = c("Slow", "Fast")
Bgeno.prob = array(c(0.1035, 0.4365, 0.46),
                   dim=3,
                   dimnames = list(Bgeno = G.lv))
geno.feno.prob = array(c(1, 0, 1, 0, 0, 1),
                   dim=c(2,3),
                   dimnames = list(Bfeno = F.lv, Bgeno= G.lv))
cpt = list(Bgeno = Bgeno.prob, Bfeno = geno.feno.prob)
bn = custom.fit(caffeine.dag, cpt)

cpquery(bn, event=(Bfeno=='Slow'),
        evidence = list(Bgeno = 'SF'),
        method = 'lw',
        n=1000)

cpquery(bn, event=(Bgeno=='SS'),
        evidence = list(Bfeno = 'Slow'),
        method = 'lw',
        n=10000000)
cpquery(bn, event=(Bgeno=='SF'),
        evidence = list(Bfeno = 'Slow'),
        method = 'lw',
        n=10000000)
##
A.lv = c("young", "adult", "old")
S.lv = c("M", "F")
E.lv = c("high", "uni")
O.lv = c("emp", "self")
R.lv = c("small", "big")
T.lv = c("car", "train", "other")
survey.dag = empty.graph(nodes = c("A", "S", "E", "O", "R", "T"))
survey.dag = set.arc(survey.dag, from = "A", to = "E")
survey.dag = set.arc(survey.dag, from = "S", to = "E")
survey.dag = set.arc(survey.dag, from = "E", to = "O")
survey.dag = set.arc(survey.dag, from = "E", to = "R")
survey.dag = set.arc(survey.dag, from = "O", to = "T")
survey.dag = set.arc(survey.dag, from = "R", to = "T")
survey.dag

A.lv = c("young", "adult", "old")
S.lv = c("M", "F")
E.lv = c("high", "uni")
O.lv = c("emp", "self")
R.lv = c("small", "big")
T.lv = c("car", "train", "other")
A.prob = array(c(0.30, 0.50, 0.20), dim = 3, dimnames = list(A = A.lv))
S.prob = array(c(0.60, 0.40), dim = 2, dimnames = list(S = S.lv))
O.prob = array(c(0.96, 0.04, 0.92, 0.08), dim = c(2, 2),
               dimnames = list(O = O.lv, E = E.lv))
R.prob = array(c(0.25, 0.75, 0.20, 0.80), dim = c(2, 2),
               dimnames = list(R = R.lv, E = E.lv))
E.prob = array(c(0.75, 0.25, 0.72, 0.28, 0.88, 0.12, 0.64,
                 0.36, 0.70, 0.30, 0.90, 0.10), dim = c(2, 3, 2),
               dimnames = list(E = E.lv, A = A.lv, S = S.lv))
T.prob = array(c(0.48, 0.42, 0.10, 0.56, 0.36, 0.08, 0.58,
                 0.24, 0.18, 0.70, 0.21, 0.09), dim = c(3, 2, 2),
               dimnames = list(T = T.lv, O = O.lv, R = R.lv))
cpt = list(A = A.prob, S = S.prob, E = E.prob, O = O.prob,
           R = R.prob, T = T.prob)
bn = custom.fit(survey.dag, cpt)
