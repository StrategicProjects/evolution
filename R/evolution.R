# ---- Client factory ---------------------------------------------------------

#' Create an Evolution API client
#'
#' @description Creates a preconfigured **httr2** client to call Evolution API v2.
#' It sets the `apikey` header, a custom User-Agent and basic automatic retries.
#' @param base_url Character. Server base URL (no trailing slash), e.g. `"https://your-host"`.
#' @param api_key  Character. API key (sent as `apikey` header).
#' @param instance Character. Instance name/ID used in endpoint paths.
#' @return An object of class `evo_client` with fields `req` (httr2 request) and `instance`.
#' @examples
#' \dontrun{
#' client <- evo_client("https://evolution_api_host", "KEY", "chatArgus")
#' }
#' @export
evo_client <- function(base_url, api_key, instance) {
  stopifnot(nzchar(base_url), nzchar(api_key), nzchar(instance))
  req <- httr2::request(sub("/+$", "", base_url)) |>
    httr2::req_headers(apikey = api_key, `Content-Type` = "application/json") |>
    httr2::req_user_agent("evoapi R client (httr2)") |>
    httr2::req_retry(max_tries = 3)
  structure(list(req = req, instance = instance), class = "evo_client")
}

# ---- Internals --------------------------------------------------------------

#' @keywords internal
.evo_path <- function(...) {
  paste0(c(...), collapse = "/")
}

# compact a list by removing NULLs (jsonlite drops them too)
.compact <- function(x) x[!vapply(x, is.null, logical(1))]

#' Perform a JSON POST request (internal)
#'
#' @keywords internal
#' @param client An `evo_client` object.
#' @param path   Character. Path to append to the base URL.
#' @param body   List to be JSON-encoded as the request body.
#' @param verbose Logical. If TRUE, print request/response debug via cli + httr2::req_verbose().
#' @return Parsed JSON as list (invisibly returns raw response as attr `http_status`).
.evo_post <- function(client, path, body, verbose = FALSE) {
  stopifnot(inherits(client, "evo_client"))
  timeout <- getOption("evoapi.timeout", 60)
  body <- .compact(body)
  req <- client$req |>
    httr2::req_url_path_append(path) |>
    httr2::req_body_json(body, auto_unbox = TRUE, null = "null") |>
    httr2::req_timeout(timeout) |>
    httr2::req_error(
      is_error = function(resp) httr2::resp_status(resp) >= 400,
      body     = function(resp) paste0("Evolution API error (", httr2::resp_status(resp), ")")
    )
  if (isTRUE(verbose)) {
    cli::cli_h1("evoapi request")
    cli::cli_inform(paste("POST", path))
    cli::cli_inform(paste("Timeout:", timeout, "s"))
    # Show a redacted/pretty body
    show <- body
    if (!is.null(show$apikey)) show$apikey <- "<redacted>"
    cli::cli_inform("Body:")
    capture <- utils::capture.output(str(show, give.attr = FALSE))
    for (line in capture) {
      cli::cli_inform(line)
    }
    req <- httr2::req_verbose(req)
  }
  resp <- httr2::req_perform(req)
  status <- httr2::resp_status(resp)
  if (isTRUE(verbose)) {
    cli::cli_alert_success(paste("HTTP", status))
    ct <- httr2::resp_header(resp, "content-type")
    if (!is.null(ct)) cli::cli_inform(paste("Response content-type:", ct))
  }
  out <- httr2::resp_body_json(resp, simplifyVector = TRUE)
  attr(out, "http_status") <- status
  out
}

# ---- Helpers ----------------------------------------------------------------

#' Build a WhatsApp JID from a raw number
#'
#' @description Normalizes a raw number (remove espaços, `-`, `(`, `)`) e anexa `@s.whatsapp.net`.
#' @param number Character. Número cru (p. ex., `"+5581999..."`).
#' @return Character JID.
#' @examples
#' jid("+5581999...")
#' @export
jid <- function(number) {
  cleaned <- gsub("[[:space:]]+", "", number)
  cleaned <- gsub("[-()]", "", cleaned)
  paste0(cleaned, "@s.whatsapp.net")
}

# ---- Endpoints (with verbose) ----------------------------------------------

#' Send a plain text message
#'
#' @description Sends a plain text WhatsApp message using Evolution API v2.
#' @param client An [evo_client()] object.
#' @param number Character. Recipient in E.164 format (e.g., `"+5581999..."`).
#' @param text   Character. Message body.
#' @param delay Integer (ms). Optional presence delay before sending.
#' @param link_preview Logical. Enable URL link preview.
#' @param mentions_everyone Logical. Mention everyone (if applicable).
#' @param mentioned Character vector of JIDs to mention (e.g., `jid("+55...")`).
#' @param quoted Optional list with Baileys message `key` and `message` (reply-to).
#' @param verbose Logical. If TRUE, logs request/response details with `cli` and enables `req_verbose()`.
#' @return A list parsed from JSON response.
#' @examples
#' \dontrun{
#' client <- evo_client("https://evolution_api_host", Sys.getenv("EVO_APIKEY"), "chatArgus")
#' send_text(client, "+55819...", "Olá", delay = 123, link_preview = FALSE, verbose = TRUE)
#' }
#' @export
send_text <- function(client, number, text, delay = NULL,
                      link_preview = NULL, mentions_everyone = NULL,
                      mentioned = NULL, quoted = NULL, verbose = FALSE) {
  stopifnot(is.character(number), length(number) == 1L, nzchar(number))
  stopifnot(is.character(text), length(text) == 1L, nzchar(text))
  body <- list(
    number = number,
    text = text,
    delay = delay,
    linkPreview = link_preview,
    mentionsEveryOne = mentions_everyone,
    mentioned = mentioned,
    quoted = quoted
  )
  .evo_post(client, .evo_path("message", "sendText", client$instance), body, verbose = verbose)
}

#' Send a WhatsApp Status (story)
#' @inheritParams send_text
#' @param type One of `"text"`, `"image"`, `"video"`, `"document"`, `"audio"`.
#' @param content Text (for `type = "text"`) or URL/base64 for media.
#' @param caption Optional caption for media.
#' @param background_color Hex color for text status background.
#' @param font Integer font id.
#' @param all_contacts Logical. Send to all contacts.
#' @param status_jid_list Optional character vector of JIDs.
#' @export
send_status <- function(client, type = c("text", "image", "video", "document", "audio"),
                        content, caption = NULL, background_color = NULL, font = NULL,
                        all_contacts = FALSE, status_jid_list = NULL, verbose = FALSE) {
  type <- match.arg(type)
  body <- list(
    type = type,
    content = content,
    caption = caption,
    backgroundColor = background_color,
    font = font,
    allContacts = isTRUE(all_contacts),
    statusJidList = status_jid_list
  )
  .evo_post(client, .evo_path("message", "sendStatus", client$instance), body, verbose = verbose)
}

# precisa de base64enc
# install.packages("base64enc")

#' Send media (image, video, document) — robusto para base64
#' @inheritParams send_text
#' @param mediatype One of "image","video","document".
#' @param mimetype e.g., "image/png", "video/mp4", "application/pdf".
#' @param media Pode ser: (a) URL http/https; (b) base64 cru (sem prefixo);
#'   (c) base64 no formato data:*;base64,<...>; (d) caminho de arquivo local.
#' @param file_name Nome sugerido (consistente com o mimetype).
#' @param verbose Log detalhado (cli + req_verbose).
#' @export
send_media <- function(client, number, mediatype, mimetype,
                       caption = NULL, media, file_name,
                       delay = NULL, link_preview = NULL, verbose = FALSE) {
  stopifnot(is.character(number) && length(number) == 1L && nzchar(number))
  stopifnot(mediatype %in% c("image", "video", "document"))
  stopifnot(is.character(mimetype) && nzchar(mimetype))
  stopifnot(is.character(file_name) && nzchar(file_name))

  normalize_media_input <- function(x) {
    if (!is.character(x) || length(x) != 1L) {
      cli::cli_abort("`media` deve ser uma string (URL, base64 ou caminho).")
    }
    # Caso (a): URL http(s) — devolve como está
    if (grepl("^https?://", x, ignore.case = TRUE)) {
      return(x)
    }

    # Caso (d): caminho local — lê e base64-encode
    if (file.exists(x)) {
      b64 <- base64enc::base64encode(x) # sem quebras
      return(b64)
    }

    # Caso (c): data:*;base64,....
    if (grepl("^data:.*;base64,", x)) {
      x <- sub("^data:.*;base64,", "", x)
    }

    # Agora supomos ser base64 cru — removemos espaços/linhas
    x <- gsub("\\s+", "", x)
    # Validação mínima: base64 tem apenas [A-Za-z0-9+/=]
    if (!grepl("^[A-Za-z0-9+/=]+$", x)) {
      cli::cli_abort("`media` não parece base64 válido nem URL/caminho de arquivo.")
    }
    x
  }

  media_norm <- normalize_media_input(media)

  body <- .compact(list(
    number = number,
    mediatype = mediatype,
    mimetype = mimetype,
    caption = caption,
    media = media_norm,
    fileName = file_name,
    delay = delay,
    linkPreview = link_preview
  ))

  .evo_post(client, .evo_path("message", "sendMedia", client$instance), body, verbose = verbose)
}

#' Send WhatsApp audio (voice note)
#' @inheritParams send_text
#' @param audio URL or base64.
#' @export
send_whatsapp_audio <- function(client, number, audio, delay = NULL,
                                link_preview = NULL, mentions_everyone = NULL,
                                mentioned = NULL, quoted = NULL, verbose = FALSE) {
  body <- list(
    number = number,
    audio = audio,
    delay = delay,
    linkPreview = link_preview,
    mentionsEveryOne = mentions_everyone,
    mentioned = mentioned,
    quoted = quoted
  )
  .evo_post(client, .evo_path("message", "sendWhatsAppAudio", client$instance), body, verbose = verbose)
}

#' Send a sticker
#' @inheritParams send_text
#' @param sticker URL or base64 image.
#' @export
send_sticker <- function(client, number, sticker, delay = NULL, verbose = FALSE) {
  body <- list(number = number, sticker = sticker, delay = delay)
  .evo_post(client, .evo_path("message", "sendSticker", client$instance), body, verbose = verbose)
}

#' Send a location
#' @inheritParams send_text
#' @param latitude,longitude Numeric coordinates.
#' @param name,address Optional character (label/description).
#' @export
send_location <- function(client, number, latitude, longitude, name = NULL, address = NULL, verbose = FALSE) {
  body <- list(
    number = number,
    latitude = latitude,
    longitude = longitude,
    name = name,
    address = address
  )
  .evo_post(client, .evo_path("message", "sendLocation", client$instance), body, verbose = verbose)
}

#' Send a WhatsApp contact (auto-generate wuid as @s.whatsapp.net)
#'
#' @description Sends one or more contacts following the Evolution API v2 format.
#' Automatically generates the `wuid` field as `<digits>@s.whatsapp.net`
#' from each contact’s phone number (or from `number` if not provided).
#'
#' @param client An [evo_client()] object.
#' @param number Recipient number (E.164, e.g. "+55819...").
#' @param contact Either:
#'   - a named list with fields `fullName`, `phoneNumber`, `organization`,
#'     `email`, `url`; or
#'   - a list of such lists (to send multiple contacts).
#'   The `wuid` field will be auto-generated if missing.
#' @param verbose Logical; if TRUE, shows detailed logs (cli + httr2 verbose).
#'
#' @return Parsed JSON response.
#' @examples
#' \dontrun{
#' send_contact(
#'   client,
#'   number = "+55819...",
#'   contact = list(
#'     fullName = "Your Name",
#'     phoneNumber = "+55819...",
#'     organization = "Company Name",
#'     email = "andre@example.com",
#'     url = "https://company_site.tec.br"
#'   ),
#'   verbose = TRUE
#' )
#' }
#' @export
send_contact <- function(client, number, contact, verbose = FALSE) {
  stopifnot(is.character(number), length(number) == 1L, nzchar(number))

  # Função auxiliar para limpar o número e gerar o wuid
  to_wuid <- function(num) {
    clean <- gsub("[^0-9]", "", num)
    if (nzchar(clean)) paste0(clean, "@s.whatsapp.net") else NULL
  }

  # Normaliza formato de contato (um ou vários)
  if (is.list(contact) && !is.null(contact$fullName)) {
    contact <- list(contact)
  }

  contact <- lapply(contact, function(ct) {
    if (is.null(ct$wuid)) {
      phone <- ct$phoneNumber %||% number
      ct$wuid <- to_wuid(phone)
    }
    .compact(ct)
  })

  body <- .compact(list(
    number = number,
    contact = contact
  ))

  .evo_post(client, .evo_path("message", "sendContact", client$instance),
    body,
    verbose = verbose
  )
}


#' React to a message
#' @inheritParams send_text
#' @param key List with `remoteJid`, `fromMe`, `id` of the target message.
#' @param reaction Emoji like `"\xF0\x9F\x98\x81"`.
#' @export
send_reaction <- function(client, key, reaction, verbose = FALSE) {
  body <- list(key = key, reaction = reaction)
  .evo_post(client, .evo_path("message", "sendReaction", client$instance), body, verbose = verbose)
}

#' Send Buttons
#' @inheritParams send_text
#' @param title,description,footer Character.
#' @param buttons List of buttons (see API docs).
#' @details Buttons may be discontinued on Baileys mode; supported on Cloud API.
#' @export
send_buttons <- function(client, number, title, description, footer, buttons,
                         delay = NULL, link_preview = NULL, mentions_everyone = NULL, verbose = FALSE) {
  body <- list(
    number = number,
    title = title,
    description = description,
    footer = footer,
    buttons = buttons,
    delay = delay,
    linkPreview = link_preview,
    mentionsEveryOne = mentions_everyone
  )
  .evo_post(client, .evo_path("message", "sendButtons", client$instance), body, verbose = verbose)
}

#' Send a poll
#' @inheritParams send_text
#' @param name Question text.
#' @param values Character vector of options.
#' @param selectable_count Integer (# options a user can select).
#' @export
send_poll <- function(client, number, name, values, selectable_count = 1L, verbose = FALSE) {
  body <- list(
    number = number,
    name = name,
    values = as.list(values),
    selectableCount = as.integer(selectable_count)
  )
  .evo_post(client, .evo_path("message", "sendPoll", client$instance), body, verbose = verbose)
}
