# Finite Mixture Models (FMM) from scratch 

> Finite mixture models are very useful when applied to data where observations originate from various groups and the group affiliations are not known. For example, in single cell RNA-seq data, transcripts in each cell can be modeled as a mixture of two probabilistic processes: 1) a negative binomial process for when a transcript is amplified and detected at a level correlating with its abundance and 2) a low-magnitude Poisson process for when drop-outs occur. These error model can be then used to provide a basis for further statistical analysis including those described in [Fan et al](http://www.nature.com/nmeth/journal/v13/n3/full/nmeth.3734.html).

In this repository I use simulations and sample data to learn about methods for model-based clustering of finite mixture Gaussian distributions.

This is ultimately my attempt at utilizing the EM algorithm for finite mixture modeling and model-based clustering in the R programming language **from scratch** and without the help of libraries or packages (e.g. [`flexmix`](https://cran.r-project.org/package=flexmix)).

Feel free to [contact me](mailto:shanasabri@gmail.com) with any questions or concerns.

###  Fitting a mixture of two normals to 1-dimensional data
[![image](https://i.imgur.com/MzXLn5n.png "image")](https://github.com/ShanSabri/fmm/blob/master/plots/fmm1d.pdf "ff1d")

###  Find both means of clustered two-dimensional data and fit a mixture of two bivariate normals
[![image](https://i.imgur.com/CtFYyEt.png "image")](https://github.com/ShanSabri/fmm/blob/master/plots/fmm2d.pdf "ff2d")

##### Valuable refs.:

  - [McLachlan et al.](https://www.annualreviews.org/doi/abs/10.1146/annurev-statistics-031017-100325)
  - [jef.works](https://jef.works/blog/2017/08/05/a-practical-introduction-to-finite-mixture-models/)
  - [flexmix vignettes](https://cran.r-project.org/web/packages/flexmix/vignettes/flexmix-intro.pdf)
 

License
----

MIT
