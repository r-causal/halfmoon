# Add ESS Table Header

This function replaces the counts in the default header of
[`gtsummary::tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_svysummary.html)
tables to counts representing the Effective Sample Size (ESS). See
[`ess()`](https://r-causal.github.io/halfmoon/reference/ess.md) for
details.

## Usage

``` r
add_ess_header(
  x,
  header = "**{level}**  \nESS = {format(n, digits = 1, nsmall = 1)}"
)
```

## Arguments

- x:

  (`tbl_svysummary`)  
  Object of class `'tbl_svysummary'` typically created with
  [`gtsummary::tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_svysummary.html).

- header:

  (`string`)  
  String specifying updated header. Review
  [`gtsummary::modify_header()`](https://www.danieldsjoberg.com/gtsummary/reference/modify.html)
  for details on use.

## Value

a 'gtsummary' table

## Examples

``` r
svy <- survey::svydesign(~1, data = nhefs_weights, weights = ~ w_ate)

gtsummary::tbl_svysummary(svy, include = c(age, sex, smokeyrs)) |>
  add_ess_header()


  

Characteristic
```

**Overall**  
ESS = 1013.3¹

AGE IN 1971

43 (33, 53)

0: MALE 1: FEMALE

  

    0

1,527 (49%)

    1

1,599 (51%)

YEARS OF SMOKING

24 (14, 33)

¹ Median (Q1, Q3); n (%)

Abbreviation: ESS = Effective Sample Size

hdr \<- [paste0](https://rdrr.io/r/base/paste.html)( "\*\*{level}\*\*
\n", "N = {n_unweighted}; ESS = {format(n, digits = 1, nsmall = 1)}" )
gtsummary::[tbl_svysummary](https://www.danieldsjoberg.com/gtsummary/reference/tbl_svysummary.html)(svy,
by = qsmk, include = [c](https://rdrr.io/r/base/c.html)(age, sex,
smokeyrs)) \|\> add_ess_header(header = hdr)

[TABLE]
