# Add ESS Table Header

This function replaces the counts in the default header of
[`gtsummary::tbl_svysummary()`](https://www.danieldsjoberg.com/gtsummary/reference/tbl_svysummary.html)
tables to counts representing the Effective Sample Size (ESS). See
[`ess()`](https://r-causal.github.io/causalgenerics/reference/ess.html)
for details.

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

## Details

The header statistics available to `header` are the ESS of the column
(`n`), the total the columns are a share of (`N`), and that share (`p`).
ESS is not additive, so the ESS of the whole sample is not the total
that the group ESS values divide up. For a table with a
`gtsummary::tbl_svysummary(by =)` variable, `N` is therefore the sum of
the group ESS values and `p` is each group's share of that sum, whether
or not the table also has an overall column from
[`gtsummary::add_overall()`](https://www.danieldsjoberg.com/gtsummary/reference/add_overall.html).
The overall column reports the ESS of the whole sample as both `n` and
`N`, so its `p` is 1.

## Examples

``` r
svy <- survey::svydesign(~1, data = nhefs_weights, weights = ~ w_ate)

gtsummary::tbl_svysummary(svy, include = c(age, sex, smokeyrs)) |>
  add_ess_header()


  

Characteristic
```
