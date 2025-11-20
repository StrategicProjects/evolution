## R CMD check results

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

## 1. Uwe Ligges

The following issues reported by CRAN have been corrected:
	1.	Title field
 	 •	Fixed: The Title field in DESCRIPTION now starts with the package name.
	 •	Old: Evolution API v2 Client for R
	 •	New: A Client for 'Evolution Cloud API'
	2.	URL field
	•	Fixed: The URL field is now a valid list of URLs separated by commas, without invalid tags.

## 2. Konstanze Lauseker

> Please add \value to .Rd files regarding exported methods and explain
> the functions results in the documentation. Please write about the
> structure of the output (class) and also what the output means. (If a
> function does not return a value, please document that too, e.g.
> \value{No return value, called for side effects} or similar)

0 errors ✔ | 0 warnings ✔ | 0 notes ✔

Dear Konstanze,

Thank you for the review. I have added \\value sections to all affected .Rd files (send_buttons, send_location, send_media, send_poll, send_reaction, send_status, send_sticker, send_whatsapp_audio).
Each now documents the returned object structure (parsed JSON response as an R list with an HTTP status attribute) or states explicitly when a function is called only for side effects.

All documentation has been updated for consistency, and the package has been resubmitted.

Best regards,
André Leite
