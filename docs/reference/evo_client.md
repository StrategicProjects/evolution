# Create an Evolution API client

Creates a preconfigured **httr2** client to call Evolution API v2. It
sets the `apikey` header, a custom User-Agent and basic automatic
retries.

The returned object is used by every `send_*()` function and stores the
base request (`req`) and the instance name so you don't have to repeat
them.

## Usage

``` r
evo_client(base_url, api_key, instance)
```

## Arguments

- base_url:

  Character. Server base URL (no trailing slash), e.g.
  `"https://your-host"`.

- api_key:

  Character. API key (sent as `apikey` header). Prefer
  `Sys.getenv("EVO_APIKEY")` to avoid hardcoding secrets.

- instance:

  Character. Instance name/ID used in endpoint paths.

## Value

An object of class `evo_client` with fields `req` (httr2 request) and
`instance`.

## See also

[`send_text()`](https://monitoramento.sepe.pe.gov.br/evolution/reference/send_text.md),
[`send_media()`](https://monitoramento.sepe.pe.gov.br/evolution/reference/send_media.md),
[`send_location()`](https://monitoramento.sepe.pe.gov.br/evolution/reference/send_location.md)

## Examples

``` r
if (FALSE) { # \dontrun{
client <- evo_client(
  base_url = "https://your-evolution-host.com",
  api_key  = Sys.getenv("EVO_APIKEY"),
  instance = "myInstance"
)
} # }
```
