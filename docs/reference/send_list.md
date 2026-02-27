# Send a list message

Sends an interactive list message via Evolution API v2. List messages
display a menu of selectable options organised into sections.

## Usage

``` r
send_list(
  client,
  number,
  title,
  description,
  button_text,
  sections,
  footer = "",
  delay = NULL,
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

- title:

  Character. List message title.

- description:

  Character. List message body text.

- button_text:

  Character. Text displayed on the list button (e.g., `"View options"`).

- sections:

  A list of section objects. Each section is a named list with `title`
  and `rows`, where `rows` is a list of named lists each containing
  `title`, optional `description`, and optional `rowId`.

- footer:

  Character. Footer text (required by the API, defaults to `""`).

- delay:

  Integer (ms). Optional presence delay before sending. Simulates typing
  before the message is sent.

- verbose:

  Logical. If `TRUE`, logs request/response details with **cli**.

## Value

A named list with the API response. The HTTP status code is stored in
`attr(result, "http_status")`.

## Note

**Baileys connector:** Interactive list messages are **not supported**
on the Baileys (WhatsApp Web) connector and are likely to be
discontinued. This endpoint is fully supported only on the **Cloud API**
connector. If you are on Baileys, consider using
[`send_poll()`](https://strategicprojects.github.io/evolution/reference/send_poll.md)
as an alternative.

## See also

[`send_poll()`](https://strategicprojects.github.io/evolution/reference/send_poll.md)

## Examples

``` r
if (FALSE) { # \dontrun{
send_list(client, "5581999990000",
  title = "Our Menu",
  description = "Select from the options below:",
  button_text = "View options",
  footer = "Powered by R",
  sections = list(
    list(title = "Drinks", rows = list(
      list(title = "Coffee", description = "Hot coffee", rowId = "1"),
      list(title = "Tea",    description = "Green tea",  rowId = "2")
    )),
    list(title = "Food", rows = list(
      list(title = "Cake", description = "Chocolate cake", rowId = "3")
    ))
  ))
} # }
```
