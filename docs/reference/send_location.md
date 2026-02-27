# Send a location

Sends a geographic location pin via Evolution API v2.

## Usage

``` r
send_location(
  client,
  number,
  latitude,
  longitude,
  name = NULL,
  address = NULL,
  verbose = FALSE
)
```

## Arguments

- client:

  An
  [`evo_client()`](https://strategicprojects.github.io/evolution/reference/evo_client.md)
  object.

- number:

  Character. Recipient number with country code (e.g., `"5581999990000"`
  or `"+5581999990000"`).

- latitude:

  Numeric. Latitude coordinate.

- longitude:

  Numeric. Longitude coordinate.

- name:

  Optional character. Location label name.

- address:

  Optional character. Address description.

- verbose:

  Logical. If `TRUE`, logs request/response details with **cli**.

## Value

A named list with the API response. The HTTP status code is stored in
`attr(result, "http_status")`.

## Examples

``` r
if (FALSE) { # \dontrun{
send_location(client, "5581999990000",
              latitude = -8.05, longitude = -34.88,
              name = "Recife Antigo", address = "Marco Zero")
} # }
```
