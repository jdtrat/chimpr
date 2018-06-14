# set up vcr
library("vcr")
invisible(vcr::vcr_configure(
    dir = "../fixtures",
    filter_sensitive_data = list("<chimpr_api_key>" = Sys.getenv('MAILCHIMP_KEY'))
))
