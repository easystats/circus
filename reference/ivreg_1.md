# ivreg_1

Model of class ivreg

## Usage

``` r
ivreg_1
```

## Format

An object of class `ivreg` of length 18.

## Examples

``` r
if (FALSE) {
data(CigarettesSW)
CigarettesSW$rprice <- with(CigarettesSW, price / cpi)
CigarettesSW$rincome <- with(CigarettesSW, income / population / cpi)
CigarettesSW$tdiff <- with(CigarettesSW, (taxs - tax) / cpi)

ivreg_1 <- ivreg(
  log(packs) ~ log(rprice) + log(rincome) | log(rincome) + tdiff + I(tax / cpi),
  data = CigarettesSW,
  subset = year == "1995"
)
}
```
