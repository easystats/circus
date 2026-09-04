# circus

***The Circus of Monsters!***

`circus` contains a variety fitted models to help the systematic testing
of other packages.

## Installation

Run the following:

\
[`install.packages`](https://rdrr.io/r/utils/install.packages.html)`(``"remotes"``)`\
`remotes``::``install_github``(``"easystats/circus"``)`\
[`library`](https://rdrr.io/r/base/library.html)`(``"circus"``)`

## How to use it

You could use the package in your `testthat` block by directly calling
the models. However, the usual workflow is to directly download them
from github with the
[`download_model()`](https://easystats.github.io/circus/reference/download_model.md)
function in the [`insight`](https://github.com/easystats/insight)
package:

\
`test_that``(``"my_function_works"``, ``{`\
`  ``testthat``::`[`skip_if_not_installed`](https://testthat.r-lib.org/reference/skip.html)`(``"insight"``)`\
`  ``testthat``::`[`skip_if_not_installed`](https://testthat.r-lib.org/reference/skip.html)`(``"curl"``)`` ``# to check internet access`\
`  ``testthat``::`[`skip_if_offline`](https://testthat.r-lib.org/reference/skip.html)`(``)`\
`  ``testthat``::`[`skip_if_not_installed`](https://testthat.r-lib.org/reference/skip.html)`(``"httr2"``)`` ``# to download stuff`\
\
`  ``# model <- circus::lmerMod_1  # Local solution`\
`  ``model`` ``<-`` ``insight``::``download_model``(``"lmerMod_1"``)`\
`  ``testthat``::`[`skip_if`](https://testthat.r-lib.org/reference/skip.html)`(`[`is.null`](https://rdrr.io/r/base/NULL.html)`(``model``)``)`\
`  ``testthat``::`[`expect_equal`](https://testthat.r-lib.org/reference/equality-expectations.html)`(``myFunction``(``model``)``, ``0.333``)`\
`}``)`

## Contribute

Feel free to add any model you find missing! Any scary creature for the
depth of your mind has its place here!

In order to add models, do the following:

1.  Add your model-name in the
    [`usethis::use_data()`](https://usethis.r-lib.org/reference/use_data.html)
    function (last chunk) in the `README.Rmd`
2.  Add documentation for your model in the
    [`R/data.R/`](https://github.com/easystats/circus/blob/main/R/data.R)
    file
3.  Now fit your model and save it to the data-folder, using
    `usethis::use_data(<yourmodel>)`.
4.  Check and build documentation for the package (to generate the
    `.rd`-files)
5.  Upload following files to github: `/data/<yourmodel.rda>`, `data.R`
    and `data.rd`.

**Note** When you build or install the package, it is recommended to do
so with following build-options:
`R CMD INSTALL --no-multiarch --with-keep.source --no-libs --no-data`.
Furthermore, when building the documentation, make sure to **not** build
the vignettes.

## Sample data

### Illusion Game

\
`# Data from Makowski et al., (2023) for the Illusion Game`\
`df`` ``<-`` `[`read.csv`](https://rdrr.io/r/utils/read.table.html)`(``"https://raw.githubusercontent.com/RealityBending/IllusionGameValidation/refs/heads/main/data/study1.csv"``)`\
`df`` ``<-`` ``df``[`[`c`](https://rdrr.io/r/base/c.html)`(``"Participant"``, ``"Illusion_Type"``, ``"Trial"``, ``"RT"``, ``"Error"``, ``"Illusion_Strength"``, ``"Illusion_Difference"``)``]`\
`df``$``RT`` ``<-`` ``df``$``RT`` ``/`` ``1000`\
`df`` ``<-`` ``df``[``df``$``Illusion_Strength`` ``>`` ``0``, ``]`\
`df`` ``<-`` ``df``[``df``$``Illusion_Type`` `[`%in%`](https://rdrr.io/r/base/match.html)` `[`c`](https://rdrr.io/r/base/c.html)`(``"Müller-Lyer"``, ``"Delboeuf"``, ``"Ebbinghaus"``, ``"Vertical-Horizontal"``, ``"Ponzo"``)``, ``]`\
[`write.csv`](https://rdrr.io/r/utils/write.table.html)`(``df``, ``"../data/illusiongame.csv"``, row.names ``=`` ``FALSE``)`

## How to add models

\
`lm_1`` ``<-`` `[`lm`](https://rdrr.io/r/stats/lm.html)`(``mpg`` ``~`` ``wt``, data ``=`` ``mtcars``)`\
`usethis``::`[`use_data`](https://usethis.r-lib.org/reference/use_data.html)`(``lm_1``, overwrite ``=`` ``TRUE``)`
