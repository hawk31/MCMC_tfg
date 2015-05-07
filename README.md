# Repositorio del TFG (Markov Chain Monte Carlo)

En cada carpeta de `code/` encontraremos el código utilizado en cada capítulo del TFG. Se ha intentado que las funciones sean lo más genéricas posibles.

La mayoría del código del trabajo está en `R`, por lo que si es necesario algún paquete adicional se indica (todos disponibles en CRAN). Para algunos capítulos, se requieren algunos lenguajes de programación adicionales además de `R`. En los capítulos 5 y 6 se hace uso intensivo de `C++` por motivos de rendimiento computacional, por lo que un compilador estándar es necesario. Las pruebas se han realizado usando el compilador por defecto de cualquier distribución Linux `g++`.

En el capítulo 8 se hace uso de `Python 2.7`, y de las librerías `numpy, scipy, lda, BeautifulSoup`, por lo que si no se tienen instaladas en el sistema, el código no funcionará. Para instalar en cualquier distribución Linux, recurrir a `pip install numpy scipy lda BeautifulSoup`. El funcionamiento en Windows no ha sido testado.
