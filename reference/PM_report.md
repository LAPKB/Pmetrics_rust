# Generate a report

**\[stable\]**

Generates a report from a specified Rmd template

## Usage

``` r
PM_report(x, template, path, show = TRUE, quiet = FALSE)
```

## Arguments

- x:

  A
  [PM_result](https://lapkb.github.io/Pmetrics_rust/reference/PM_result.md)
  object obtained from
  [PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md).

- template:

  If missing, the default Pmetrics report template as specified in
  [getPMoptions](https://lapkb.github.io/Pmetrics_rust/reference/getPMoptions.md)
  is used. It can be changed with
  [setPMoptions](https://lapkb.github.io/Pmetrics_rust/reference/setPMoptions.md).
  Otherwise, the value for `template` can be "plotly", "ggplot", or
  "none".

- path:

  The path for the generated report, defaults to a temporary file in the
  current working directory.

- show:

  Controls if the report should be automatically opened on generation,
  defaults to `TRUE`

- quiet:

  If `TRUE`, suppresses the message about report generation, defaults to
  `FALSE`.

## Value

Generates an HTML-report in the current working directory.

## See also

[PM_load](https://lapkb.github.io/Pmetrics_rust/reference/PM_load.md)

## Author

Markus Hovd, Julian Otalvaro, and Michael Neely
