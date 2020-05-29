##PROJET: Manipulation de polynomes

#1) Quelle serait la représentation creuse sous forme de liste du polynome 2+x^4−3x^5 ? list(c(2, 0), c(1, 4), c(-3, 5))
#2) Et sa représentation dense ? c(2, 0, 0, 0, 1, -3)
#3) Quand choisit-on de travailler plutot en représentation creuse ? Quand on souhaite travailler avec les degrés
#4) Quelle est la représentation la plus économique en terme d’espace mémoire ? Representation dense

# SOURCES :
# http://www.i3s.unice.fr/~malapert/R/pdf/A4-Polynomes.pdf
# https://www.youtube.com/watch?v=Z393AcN_Gz0 (Horner methode comprehension)

# Pour executer le programme faire Rscript ./polynomes.r
# Ou l'executer dans Rstudio.
# Va executer toutes les demonstrations de l'activite.

# Vous devez avoir installer le package 'polynom' sinon il y aura une erreur.

# Pour utiliser les fonctions une a une
# (cf. Activite_manipulation_des_polynomes.pdf)
# qui est une documentation du code.
p <- list(c(2,5), c(-1,4),c(-2,1))
q <- list(c(1,4), c(7,2),c(-1,0))

# indique si le polynome est nul
is_poly0 <- function(p) length(p) == 0

# renvoyant le degre d’un monome ou d’un polynome.
degre <- function(mp) {
    if(is_poly0(mp)) return(-Inf)
    if (is.list(mp)) mp <- mp[[1]]
    if (is.vector(mp) && length(mp) == 2) return(mp[2])
    else return(NULL)
}

# renvoyant le coefficient d’un monome ou d’un polynome.
coeff <- function(mp) {
    if(is_poly0(mp)) return(-Inf)
    if (is.list(mp)) mp <- mp[[1]]
    if (is.vector(mp) && length(mp) == 2) return(mp[1])
    else return(NULL)
}

# renvoyant une chaîne de caractères représentant le polynome creux sous la forme ∑aiX^i
poly2str <- function(p){
    if(length(p) == 0){
        return ("")
    }
    acc = ""
    for(e in p){
        acc = paste(acc, e[1], "*X^", e[2], " + ", sep="")
    }
    return(substr(acc, 0, nchar(acc) - 3))
}

# multiplication par un scalaire k d’un polynome
mult_ext <- function(p, k) {
    return(lapply(p, function(x) c(k*x[1], x[2])))
}

# transforme un polynome plein en un polynome creux
make_poly <- function(x) {
    poly_creux <- list()
    power <- length(x) - 1
    j <- 1
    for (i in length(x):1) {
        if (x[i] != 0) {
            poly_creux[[j]] <- c(x[i], power)
            j <- j + 1
        }
        power <- power - 1
    }
    return(poly_creux)
}

# générant un polynome aléatoire de degré inférieur à n
# et dont les coefficients sont tirés dans le vecteur coeffs
rand_poly <- function(n, coeffs) {
    poly <- list()
    len_poly <- sample(1:n-1, 1)
    j <- 1
    for (i in len_poly:1) {
        rand_coeff <- sample(coeffs, 1)
        if (rand_coeff != 0) {
            poly[[j]] <- c(rand_coeff, i)
            j <- j + 1
        }
    }
    if (length(poly) == 0) return(rand_poly(n, coeffs))
    return(poly)
}

# trie une liste de monomes par degré décroissant
sort_monoms <- function(p) {
    for (i in 1:length(p)) {
        for (j in i:length(p)) {
            if (p[[i]][2] < p[[j]][2]) {
                x <- p[[i]]
                p[[i]] <- p[[j]]
                p[[j]] <- x
            }
        }
    }
    return(p)
}

# somme les termes de même degré, et supprime les termes dont le coefficient est nulle
merge_monoms <- function(p) {
    monoms <- list(p[[1]])
    l_p <- length(p)
    i <- 2
    j <- 1
    # Additionne les monoms entre eux
    while (i <= l_p) {
        if (monoms[[j]][2] == p[[i]][2] && i != j) {
            monoms[[j]] <- c(monoms[[j]][1] + p[[i]][1], p[[i]][2])
        } else {
            j <- j + 1
            monoms[[j]] <- p[[i]]
        }
        i <- i + 1
    }
    k <- 1
    # Enlève les monoms de coeff nulle
    while (k <= length(monoms)) {
        if (monoms[[k]][1] == 0) {
            monoms[[k]] <- NULL
            next
        }
        k <- k + 1
    }
    return(monoms)
}

# Addition de deux polynomes
add <- function(p, q) {
    combine <- c(p, q) # combine 2 listes
    return(merge_monoms(sort_monoms(combine)))
}

# Soustraction de 2 polynomes
sub <- function(p,q) add(p,mult_ext(q,-1))

# polynome dérivé du polynome p
deriv <- function(p) {
    drv <- list()
    for (i in p) {
        if (i[[2]] == 1) {
            drv <- c(drv, list(c(i[[1]], 0)))
        }
        else if (i[[2]] == 0) {
            i <- NULL
        }
        else {
            drv <- c(drv, list(c(i[[2]] * i[[1]], i[[2]] - 1)))
        }
    }
    return(drv)
}

# primitive du polynome p
integ <- function(p) {
    integ <- list()
    for (c in p) {
        integ <- c(integ, list(c(c[[1]] / (c[[2]] + 1), c[[2]] + 1)))
    }
    return(integ)
}

# Multiplication de 2 monomes
mult_monoms <- function(m1, m2) {
    return(list(c(m1[1] * m2[1], m1[2] + m2[2])))
}

# Multiplication d'un polynome par un monome
mult_poly_mono <- function(p, m) {
    return(lapply(p, function(x) c(m[1]*x[1], m[2] + x[2])))
}

# Multiplication interne de deux polynomes
mult <- function(p, q) {
    res <- list()
    # S'appuyant sur la methode vu dans le cours de l'activite
    for (monom in q) {
        res <- add(res, mult_poly_mono(p, monom))
    }
    return(res)
}

# Valeur d'un polynome en un point x version naive
polyval <- function(p, x) {
    res <- c()
    for (monom in p) {
        res <- c(res, c(monom[1] * x ** monom[2]))
    }
    return(sum(res))
}

# Fonction personnel pour faire polyhorn
# qui convertir un polynome creux a dense
creux_to_dense <- function(p) {
    degre <- degre(p)
    res <- c()
    old <- p[[1]][2]
    i <- 1
    j <- 1
    while (i <= length(p)) {
        if (old - p[[j]][2] >= 2) {
            res <- c(res, 0)
            old <- old - 1
            next
        }
        else {
            res <- c(res, p[[j]][1])
            j <- j + 1
        }
        old <- p[[i]][2]
        i <- i + 1
    }
    adjust <- degre - length(res) + 1
    if (adjust > 0) res <- c(res, numeric(adjust))
    return(rev(res))
}

# Valeur d'un polynome en un point x version Horner
polyhorn <- function(p, x) {
    # Verification que le polynome p est un polynome creux, si oui conversion en dense
    if (is.list(p)) p <- creux_to_dense(p)
    len_p <- length(p)
    res <- p[[len_p]][1]
    for (i in (len_p-1):1) {
        res <- (res * x) + p[[i]][1]
    }
    return(res)
}

# construit la fonction polynome associee a p
fpoly <- function(p) {
    return( function(x) {polyval(p, x)}) # on peut faire pareil avec polyhorn
}

# dessine les courbes de la primitive, derivee et du polynome
dessiner <- function(p, x) {
    cols <- c("green", "red", "blue")
    finteg <- fpoly(integ(p))
    fderiv <- fpoly(deriv(p))
    fpoly_p <- fpoly(p)

    integ <- sapply(x, finteg)
    derivate <- sapply(x, fderiv)
    poly_p <- sapply(x, fpoly_p)
    matplot(x, cbind(integ, derivate, poly_p), t="l", lwd=1.5, lty=1, xlab="x", ylab="y", col=cols)
    legend("topleft", inset=.05, legend=c("Primitive", "Derivee", "Polynome"), horiz=FALSE, lwd=2, lty=1, col=cols)
}

# tout les cas de test de l'activite polynome
# sont effectue dans cette fonction
demonstration <- function() {
    stopifnot("2*X^5 + -1*X^4 + -2*X^1" == poly2str(p))
    stopifnot(poly2str(mult_ext(p,-2)) == "-4*X^5 + 2*X^4 + 4*X^1")
    stopifnot(poly2str(make_poly(c(0, -2, 0, 0, -1, 2))) == "2*X^5 + -1*X^4 + -2*X^1")
    stopifnot(poly2str(make_poly(c(-1, 0, -7, 0, 1))) == "1*X^4 + -7*X^2 + -1*X^0")
    stopifnot(poly2str(add(p, list())) == "2*X^5 + -1*X^4 + -2*X^1")
    stopifnot(poly2str(add(list(), q)) == "1*X^4 + 7*X^2 + -1*X^0")
    stopifnot(poly2str(add(p,q)) == "2*X^5 + 7*X^2 + -2*X^1 + -1*X^0")
    stopifnot(poly2str(sub(p,p)) == "")
    stopifnot(poly2str(sub(p,q)) == "2*X^5 + -2*X^4 + -7*X^2 + -2*X^1 + 1*X^0")
    stopifnot(poly2str(sub(q,p)) == "-2*X^5 + 2*X^4 + 7*X^2 + 2*X^1 + -1*X^0")
    stopifnot(poly2str(integ(p)) == "0.333333333333333*X^6 + -0.2*X^5 + -1*X^2")
    stopifnot(poly2str(integ(q)) == "0.2*X^5 + 2.33333333333333*X^3 + -1*X^1")
    stopifnot(poly2str(deriv(p)) == "10*X^4 + -4*X^3 + -2*X^0")
    stopifnot(poly2str(deriv(q)) == "4*X^3 + 14*X^1")
    p1 <- make_poly(c(1,1))
    p2 <- make_poly(c(-1,1))
    p3 <- make_poly(c(1,-1,1))
    stopifnot(poly2str(mult(p1,p2)) == "1*X^2 + -1*X^0")
    stopifnot(poly2str(mult(p3,p3)) == "1*X^4 + -2*X^3 + 3*X^2 + -2*X^1 + 1*X^0")
    cat('---TEST CASES DE L\'ACTIVITE POLYNOME---\n')
    cat('TEST CASE : poly2str(p) == "2*X^5 + -1*X^4 + -2*X^1" TRUE\n')
    cat('TEST CASE : poly2str(mult_ext(p,-2)) == "-4*X^5 + 2*X^4 + 4*X^1" TRUE\n')
    cat('TEST CASE : poly2str(make_poly(c(0, -2, 0, 0, -1, 2))) == "2*X^5 + -1*X^4 + -2*X^1" TRUE\n')
    cat('TEST CASE : poly2str(make_poly(c(-1, 0, -7, 0, 1))) == "1*X^4 + -7*X^2 + -1*X^0" TRUE\n')
    cat('TEST CASE : poly2str(rand_poly(5,0:3)) = ', poly2str(rand_poly(5,0:3)), '\n')
    cat('TEST CASE : poly2str(rand_poly(10,0:1)) = ', poly2str(rand_poly(10,0:1)), '\n')
    cat('TEST CASE : poly2str(add(p, list())) == "2*X^5 + -1*X^4 + -2*X^1" TRUE\n')
    cat('TEST CASE : poly2str(add(list(), q)) == "1*X^4 + 7*X^2 + -1*X^0" TRUE\n')
    cat('TEST CASE : poly2str(add(p,q)) == "2*X^5 + 7*X^2 + -2*X^1 + -1*X^0" TRUE\n')
    cat('TEST CASE : poly2str(sub(p,p)) == "" TRUE\n')
    cat('TEST CASE : poly2str(sub(p,q)) == "2*X^5 + -2*X^4 + -7*X^2 + -2*X^1 + 1*X^0" TRUE\n')
    cat('TEST CASE : poly2str(sub(q,p)) == "-2*X^5 + 2*X^4 + 7*X^2 + 2*X^1 + -1*X^0" TRUE\n')
    cat('TEST CASE : poly2str(integ(p)) == "0.333333333333333*X^6 + -0.2*X^5 + -1*X^2" TRUE\n')
    cat('TEST CASE : poly2str(integ(q)) == "0.2*X^5 + 2.33333333333333*X^3 + -1*X^1" TRUE\n')
    cat('TEST CASE : poly2str(deriv(p)) == "10*X^4 + -4*X^3 + -2*X^0" TRUE\n')
    cat('TEST CASE : poly2str(deriv(q)) == "4*X^3 + 14*X^1" TRUE\n')
    cat('TEST CASE : poly2str(mult(p1,p2)) == "1*X^2 + -1*X^0" TRUE\n')
    cat('TEST CASE : poly2str(mult(p3,p3)) == "1*X^4 + -2*X^3 + 3*X^2 + -2*X^1 + 1*X^0" TRUE\n')
    cat('---TEST POLYNOMES P EN UN POINT X---\n')
    cat('UTILISATION DE POLYVAL\n')
    print(poly2str(p))
    for(i in -1:1) {
        print(paste("p(",i,") = ",polyval(p,i), sep=""))
    }
    print(poly2str(q))
    for(i in -1:1) {
        print(paste("q(",i,") = ",polyval(q,i), sep=""))
    }
    cat('UTALISATION DE POLYHORN\n')
    print(poly2str(p))
    for(i in -1:1) {
        print(paste("p(",i,") = ",polyhorn(p,i), sep=""))
    }
    print(poly2str(q))
    for(i in -1:1) {
        print(paste("q(",i,") = ",polyhorn(q,i), sep=""))
    }
    cat('dessiner(p, x) -> (voir plot)\n')
    x <- seq(-2,4,length.out=1000)
    p1 <- make_poly(c(-1,-1,1))
    dessiner(p1,x)
}

demonstration()

### N'AFFICHE PAS COMME DANS L'EXEMPLE DE L'ACTIVITE MAIS A LES MEMES RESULTAT
### EN COMPARANT vh, vn et vr ci-dessous (sachant que polyhorn et polyval fonctionnent bien avec de multiple tests)
x <- seq(2-0.02,2.02,length.out=1001)
vec <- c(-2,0,1)
## p(x) = (x^2-2)^16
p1 <- make_poly(vec)
for(i in 1:4) {
  p1 <- mult(p1,p1)
}
## Evaluate p using our methods (observed)
vn <- sapply(x, polyval, p=p1)
vh <- sapply(x, polyhorn, p=p1)


library("polynom")
## Evaluate p using R package (expected)
pr <-as.polynomial(c(-2,0,1))
## p is an R object ! I directly used the power operator.
pr <- pr ** 16
## I also use a generic function
vr <- predict(pr,x)

## Last, visualize the results
cols <- c("red", "gold")
matplot(x, cbind(vn-vr,vh-vr),t="l", lwd=2, lty=1,col=cols, xlab="x",ylab="P(x)-P^R(x)")
legend("topleft", inset=.05, legend=c("Naive", "Horner"), horiz=TRUE, lwd=2, lty=1, col=cols)