## R CMD check results

0 errors | 0 warnings | 0 notes

* This is a patch release that addresses some CRAN errors related to internet
resources. Speicifically, the `parttree-art` vignette previously downloaded
images from Wikimedia Commons during build, which could fail if the resources
were unavailable. The images are now bundled with the package to bypass this
issue.

## Notes

* Winbuilder reports an SSL error for https://www.dimitris-ladopoulos.xyz/projects/portraits.html. This URL is valid and accessible; the error appears to be a
transient network issue on the Winbuilder machine. (I get no NOTES with either
local testing, or as part of my GitHub CI.)
