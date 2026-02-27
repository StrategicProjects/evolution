# React to a message

Sends an emoji reaction to an existing message.

## Usage

``` r
send_reaction(client, key, reaction, verbose = FALSE)
```

## Arguments

- client:

  An
  [`evo_client()`](https://strategicprojects.github.io/evolution/reference/evo_client.md)
  object.

- key:

  List with `remoteJid`, `fromMe`, and `id` identifying the target
  message.

- reaction:

  Emoji string (e.g., `"\U0001f44d"` for thumbs up). Use an empty string
  `""` to remove a reaction.

- verbose:

  Logical. If `TRUE`, logs request/response details with **cli**.

## Value

A named list with the API response. The HTTP status code is stored in
`attr(result, "http_status")`.

## Examples

``` r
if (FALSE) { # \dontrun{
send_reaction(client, key = list(
  remoteJid = "5581999990000@s.whatsapp.net",
  fromMe = TRUE,
  id = "BAE594145F4C59B4"
), reaction = "\U0001f44d")
} # }
```
