# Send a poll

Sends a poll (question with selectable options) via Evolution API v2.

## Usage

``` r
send_poll(client, number, name, values, selectable_count = 1L, verbose = FALSE)
```

## Arguments

- client:

  An
  [`evo_client()`](https://monitoramento.sepe.pe.gov.br/evolution/reference/evo_client.md)
  object.

- number:

  Character. Recipient number with country code (e.g., `"5581999990000"`
  or `"+5581999990000"`).

- name:

  Question text displayed in the poll.

- values:

  Character vector of poll options (minimum 2).

- selectable_count:

  Integer. Number of options a user can select (default `1L` for
  single-choice).

- verbose:

  Logical. If `TRUE`, logs request/response details with **cli**.

## Value

A named list with the API response. The HTTP status code is stored in
`attr(result, "http_status")`.

## Examples

``` r
if (FALSE) { # \dontrun{
send_poll(client, "5581999990000",
  name = "Favourite language?",
  values = c("R", "Python", "Julia"),
  selectable_count = 1)
} # }
```
