pacman::p_load(bnlearn, BiocManager, Rgraphviz)
# Caffeine
## Levels
G.lv = c("SS", "SF", "FF")
A.lv = c("S", "F")
F.lv = c("Slow", "Fast")
## a priori probabilities
pop.prob = array(c(0.73, 0.27), dim=2, dimnames = list(population = I.lv))
## conditional probabilities
geno.feno.prob = array(c(1, 0, 1, 0, 0, 1),
                       dim=c(2,3),
                       dimnames = list(Bfeno = F.lv, Bgeno= G.lv))
Bgeno.prob = array(c(1,0,0,0,1,0,
                     0,1,0, 0,0,1),
                   dim=c(3,2,2),
                   dimnames = list('Bgeno' = G.lv, p1=I.lv, p2=I.lv))
Bgeno.prob
caffeine.dag <- empty.graph(nodes=c('p1', 'p2', 'Bgeno', 'Bfeno'))
caffeine.dag <- set.arc(caffeine.dag, from="Bgeno", to="Bfeno")
caffeine.dag <- set.arc(caffeine.dag, from="p1", to="Bgeno")
caffeine.dag <- set.arc(caffeine.dag, from="p2", to="Bgeno")




cpt = list(p1=pop.prob, p2=pop.prob, 
           Bgeno = Bgeno.prob, Bfeno = geno.feno.prob)
bn = custom.fit(caffeine.dag, cpt)

cpquery(bn, event=(Bfeno=='Slow'),
        evidence = list(Bgeno = 'SF'),
        method = 'lw',
        n=1000)

cpquery(bn, event=(Bgeno=='SS'),
        evidence = list(Bfeno = 'Slow'),
        method = 'lw',
        n=1000)

# AB blood type

## Levels
G.lv = c('00', 'A0', 'AA', 'AB', 'BB', 'B0')
A.lv = c('0', 'A', 'B')
F.lv = c('0', 'A', 'B', 'AB')

## a priori probabilities
pop.prob <- array(c(0.36, 0.3466, 0.0834, 0.0642, 0.0124, 0.1334), dim=6, dimnames=list(PopGeno = G.lv))
allele.prob <- array(c(0.6, 0.11, 0.29), dim=3, dimnames=list(allel=A.lv))

## Conditional probabilities 
geno.feno.prob <- array(c(1,0,0,0, # 00
                          0,1,0,0, # A0
                          0,1,0,0, # AA
                          0,0,0,1, # AB
                          0,0,1,0, # BB
                          0,0,1,0  # B0
                          ),
                        dim=c(4,6),
                        dimnames=list(feno=F.lv, geno=G.lv)
                        )
geno.feno.generator <- function(g, f) {
  copy <- geno.feno.prob
  dimnames(copy) = list(f=F.lv, g=G.lv)
  copy
}

geno.feno.prob
## Graph
ab.dag <- empty.graph(nodes=c('Bgeno', 'Bfeno'))
ab.dag <- set.arc(ab.dag, from="Bgeno", to="Bfeno")
ab.dag

## Bayesien net
Bfeno <- geno.feno.generator('Bgeno', 'Bfeno')
Bgeno <- pop.prob
dimnames(Bgeno) <- list(Bgeno=G.lv)
cpt = list(Bgeno = pop.prob, Bfeno = Bfeno)
print(cpt$Bfeno%*%cpt$Bgeno)
bn = custom.fit(ab.dag, cpt)

## Queries

cpquery(bn, event=(Bfeno=='A'),
        evidence = list(Bgeno = 'AA'),
        method = 'lw',
        n=1000)

cpquery(bn, event=(Bgeno=='A0'),
        evidence = list(Bfeno = 'A'),
        method = 'lw',
        n=10000000)
