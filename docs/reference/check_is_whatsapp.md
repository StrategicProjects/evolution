# Check if numbers are on WhatsApp

Verifies whether one or more phone numbers are registered on WhatsApp
using the Evolution API v2 chat controller endpoint.

## Usage

``` r
check_is_whatsapp(client, numbers, verbose = FALSE)
```

## Arguments

- client:

  An
  [`evo_client()`](https://strategicprojects.github.io/evolution/reference/evo_client.md)
  object.

- numbers:

  Character vector of phone numbers to check (with country code, e.g.,
  `"5581999990000"`).

- verbose:

  Logical. If `TRUE`, logs request/response details.

## Value

A named list (or data frame) from the API indicating which numbers are
registered. The HTTP status code is stored in
`attr(result, "http_status")`.

## Examples

``` r
if (FALSE) { # \dontrun{
check_is_whatsapp(client, c("5581999990000", "5511988887777"))
} # }
```
