# Perform a JSON POST request (internal)

Sends a JSON POST to the Evolution API and returns the parsed response.
Includes structured CLI logging when `verbose = TRUE` and robust error
handling that surfaces the actual API error message.

## Usage

``` r
.evo_post(client, path, body, verbose = FALSE)
```

## Arguments

- client:

  An `evo_client` object.

- path:

  Character. Path to append to the base URL.

- body:

  List to be JSON-encoded as the request body.

- verbose:

  Logical. If `TRUE`, prints request/response diagnostics via **cli**.

## Value

Parsed JSON as list (with raw HTTP status stored in attribute
`http_status`).
