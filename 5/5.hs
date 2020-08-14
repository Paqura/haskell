
getUrl domain pathname token = domain ++ "/" ++ pathname ++ "?token=" ++ token

{- HLINT ignore -}
bindBasicDomain domain =
  \getUrl pathname token -> getUrl domain pathname token

domainBuilder = bindBasicDomain "http://example.com"

getApiRequestBuilder geturl domainBuilder token =
  \pathname -> domainBuilder geturl pathname token

getUrlByPathname = getApiRequestBuilder getUrl domainBuilder "toke1"
