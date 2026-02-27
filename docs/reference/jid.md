# Build a WhatsApp JID from a raw phone number

Normalises a raw phone number by removing spaces, dashes, parentheses,
and the leading `+` sign, then appends `@s.whatsapp.net`.

## Usage

``` r
jid(number)
```

## Arguments

- number:

  Character scalar or vector. Raw phone number(s) (e.g.,
  `"+5581999990000"`).

## Value

Character JID(s) (e.g., `"5581999990000@s.whatsapp.net"`).

## Examples

``` r
jid("+55 81 99999-0000")
#> [1] "5581999990000@s.whatsapp.net"
#> "5581999990000@s.whatsapp.net"

jid("5581999990000")
#> [1] "5581999990000@s.whatsapp.net"
#> "5581999990000@s.whatsapp.net"
```
