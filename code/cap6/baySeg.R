bay <- function (y, ncol=2, max_iter=10^3) 
{
    numb = dim(y)[1]
    x = 0 * y
    mu = matrix(0, max_iter, 6)
    sigma2 = rep(0, max_iter)
    mu[1, ] = c(35, 50, 65, 84, 92, 120)
    sigma2[1] = 20
    beta = rep(1, max_iter)
    xcum = matrix(0, numb^2, 6)
    n = rep(0, 6)
    cat("Computing normalizing constant \n")
    Z = generatefbeta(y, max_iter, ncol)
    thefunc = approxfun(seq(0.1, 2, by = 0.1), Z)
    cat("Gibbs sampler on course \n")

    ## Main loop

    for (i in 2:max_iter) {
        cat("Iteration i", i, "\n")
        lvr = 0
        for (k in 1:numb) {
            for (l in 1:numb) {

                for(j in 1:ncol){
                    n[j] = nei4(x, k, l, j)
                }
                prob = exp(beta[i-1] * n) * dnorm(y[k, l], mu[i-1,], sqrt(sigma2[i-1]))

                if(NaN %in% prob){
                    cat("Hay un NA", "\n")
                    prob = exp(beta[i-1] * n)
                }

                x[k, l] = sample(1:ncol, 1, prob = prob)

                xcum[(k - 1) * numb + l, x[k, l]] = xcum[(k-1) * numb + l, x[k, l]] + 1
                lvr = lvr + n[x[k, l]]
            }
        }

        ## Parameter simulation
        mu[i, 1] = truncnorm(1, mean(y[x == 1]), sqrt(sigma2[i - 1]/sum(x == 1)), 0, mu[i - 1, 2])

        for(j in 2:(ncol-1)){
            mu[i, j] = truncnorm(1, mean(y[x == j]), sqrt(sigma2[i - 1]/sum(x == j)), mu[i, j - 1], mu[i - 1, j + 1])
        }

        mu[i, ncol] = truncnorm(1, mean(y[x == ncol]), sqrt(sigma2[i-1]/sum(x == ncol)), mu[i, ncol-1], 255)

        sese = (y - mu[i, 1])^2 * (x == 1)

        for(j in 2:ncol){
            sese = sese + (y - mu[i, j])^2 * (x == j)
        }

        sese = sum(sese)

        if(is.nan(1/rgamma(1, (numb)^2/2, sese/2))){
            sigma2[i] = sigma2[i-1]
        }
        else{
             sigma2[i] = 1/rgamma(1, (numb)^2/2, sese/2)
        }



        betatilde = beta[i - 1] + runif(1, -0.05, 0.05)
        if(betatilde > 1.96){
            betatilde = betatilde - 0.1
        }
        laccept = lvr * (betatilde - beta[i - 1]) + integrate(thefunc, 
            betatilde, beta[i - 1])$value
        if (runif(1) <= exp(laccept)){ 
            beta[i] = betatilde}
        else {beta[i] = beta[i - 1]}
    }
    list(beta = beta, mu = mu, sigma2 = sigma2, xcum = xcum)
}