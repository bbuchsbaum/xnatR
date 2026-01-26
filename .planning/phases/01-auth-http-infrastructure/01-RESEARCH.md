# Phase 1: Auth & HTTP Infrastructure - Research

**Researched:** 2026-01-26
**Domain:** R package authentication, HTTP client patterns, XNAT REST API
**Confidence:** HIGH

## Summary

This phase builds authentication and HTTP infrastructure for the xnatR package using httr. The existing codebase already uses httr and has basic authentication with package environment caching (xnatR_env), so this phase extends rather than replaces that foundation.

XNAT supports two authentication approaches: (1) Basic Auth with credentials sent on each request, and (2) JSESSIONID-based sessions created via POST to /data/JSESSION. The package will adopt JSESSIONID sessions to reduce credential exposure. XNAT also provides alias tokens for temporary authentication, issued via GET /data/services/tokens/issue.

R's httr package provides built-in support for cookies, handles, retry logic (via RETRY()), and credential management. The .netrc file format is natively supported by curl (httr's backend), and R packages conventionally use environment variables for credential discovery with a clear precedence order: explicit params > env vars > credential stores > config files.

**Primary recommendation:** Use httr (already a dependency) with handle-based cookie persistence for JSESSIONID, implement RETRY() for exponential backoff, and follow the credential precedence pattern used by gh/gitcreds packages (explicit > env > .netrc > YAML config).

## Standard Stack

The established libraries/tools for this domain:

### Core
| Library | Version | Purpose | Why Standard |
|---------|---------|---------|--------------|
| httr | 1.4.7+ | HTTP client | Already in use; mature, stable, well-documented; native curl .netrc support |
| yaml | 2.3.0+ | YAML config parsing | Industry standard for config files; already in use (not declared) |
| base64enc | 0.1-3+ | Basic Auth encoding | Lightweight; already in use for current auth |

### Supporting
| Library | Version | Purpose | When to Use |
|---------|---------|---------|-------------|
| jsonlite | 1.8.0+ | JSON parsing | Already imported; for parsing JSESSION/token responses |
| keyring | 1.3.0+ | Secure credential storage | OPTIONAL - recommended for future enhancement, not v1 requirement |

### Alternatives Considered
| Instead of | Could Use | Tradeoff |
|------------|-----------|----------|
| httr | httr2 | httr2 is more modern with pipeable API, better OAuth, built-in retry/cache, BUT would require complete rewrite of existing httr code; httr is superseded but stable |
| yaml | config | config provides multiple environments, but yaml is simpler and already in use |
| base64enc | base64url, openssl | Alternatives exist but base64enc is already working |

**Installation:**
```r
# In DESCRIPTION Imports (add missing):
Imports:
    httr (>= 1.4.0),
    jsonlite,
    dplyr,
    yaml,        # ADD THIS - currently used but not declared
    base64enc
```

## Architecture Patterns

### Recommended Project Structure
```
R/
├── environment.R        # Package environment (existing - keep)
├── config.R            # YAML config functions (existing - keep)
├── authenticate.R      # Auth functions (existing - enhance)
├── http-helpers.R      # NEW: Internal HTTP request wrapper
├── tokens.R           # NEW: Alias token management
└── errors.R           # NEW: Classed condition constructors
```

### Pattern 1: Credential Discovery with Precedence

**What:** Implement credential resolution following standard R package patterns (gh, gitcreds, aws.signature)

**When to use:** Always - in authenticate_xnat() before making any requests

**Example:**
```r
# Source: gh package pattern
resolve_credentials <- function(base_url = NULL, username = NULL,
                               password = NULL, token = NULL) {
  # Priority 1: Explicit params (already provided)
  if (!is.null(base_url) && !is.null(username) &&
      (!is.null(password) || !is.null(token))) {
    return(list(base_url = base_url, username = username,
                password = password, token = token, source = "explicit"))
  }

  # Priority 2: Environment variables
  env_host <- Sys.getenv("XNAT_HOST", unset = "")
  env_user <- Sys.getenv("XNAT_USER", unset = "")
  env_pass <- Sys.getenv("XNAT_PASS", unset = "")
  env_token <- Sys.getenv("XNAT_TOKEN", unset = "")

  if (nzchar(env_host) && nzchar(env_user)) {
    base_url <- base_url %||% env_host
    username <- username %||% env_user
    password <- password %||% if (nzchar(env_pass)) env_pass else NULL
    token <- token %||% if (nzchar(env_token)) env_token else NULL
    if (!is.null(password) || !is.null(token)) {
      return(list(base_url = base_url, username = username,
                  password = password, token = token, source = "environment"))
    }
  }

  # Priority 3: .netrc (let curl handle this via httr config)
  # httr with config(netrc = 1) will auto-use .netrc
  # Check if .netrc exists and might match host
  netrc_path <- path.expand("~/.netrc")
  if (file.exists(netrc_path) && !is.null(base_url)) {
    # .netrc provides credentials automatically via curl
    return(list(base_url = base_url, username = username,
                password = NULL, token = NULL, source = "netrc"))
  }

  # Priority 4: YAML config (existing load_credentials)
  creds <- try(load_credentials(), silent = TRUE)
  if (!inherits(creds, "try-error")) {
    return(c(creds, source = "yaml"))
  }

  # Nothing found
  stop("No credentials found. Provide explicitly or set XNAT_HOST/XNAT_USER/XNAT_PASS environment variables.",
       call. = FALSE)
}
```

### Pattern 2: JSESSIONID Session Management with Handles

**What:** Use httr handles to maintain cookies across requests, create JSESSION via POST, cache session cookie

**When to use:** After initial authentication, for all subsequent API calls

**Example:**
```r
# Source: httr documentation - handles preserve cookies automatically
create_session <- function(base_url, username, password = NULL,
                          token = NULL, ssl_verify = TRUE) {
  # Construct Basic Auth header
  auth_string <- paste0(username, ":", password %||% token)
  auth_encoded <- base64enc::base64encode(charToRaw(auth_string))
  auth_header <- paste("Basic", auth_encoded)

  # SSL config
  ssl_config <- if (ssl_verify) httr::config() else httr::config(ssl_verifypeer = FALSE)

  # Enable .netrc support
  netrc_config <- httr::config(netrc = 1, netrc_file = path.expand("~/.netrc"))

  # Create session via POST /data/JSESSION
  session_url <- paste0(sub("/+$", "", base_url), "/data/JSESSION")

  response <- httr::POST(
    url = session_url,
    httr::add_headers(Authorization = auth_header),
    ssl_config,
    netrc_config,
    httr::accept("text/plain")
  )

  if (httr::status_code(response) != 200) {
    stop_for_auth_error(response, "Failed to create session")
  }

  # Extract JSESSIONID from response (32 hex characters)
  jsessionid <- httr::content(response, as = "text", encoding = "UTF-8")
  jsessionid <- trimws(jsessionid)

  # Create handle for this host - httr will auto-manage cookies
  handle <- httr::handle(base_url)

  # Store in package environment
  xnatR_env$jsessionid <- jsessionid
  xnatR_env$handle <- handle
  xnatR_env$base_url <- base_url
  xnatR_env$ssl_verify <- ssl_verify
  xnatR_env$auth_header <- auth_header  # Keep for re-auth

  invisible(jsessionid)
}
```

### Pattern 3: Centralized HTTP Helper with Retry

**What:** Internal request wrapper that handles auth, retry with exponential backoff, error handling

**When to use:** For ALL HTTP requests - replaces direct httr::GET/POST calls

**Example:**
```r
# Source: httr RETRY() function
xnat_request <- function(method = c("GET", "POST", "PUT", "DELETE"),
                        path,
                        body = NULL,
                        query = list(),
                        accept = "application/json",
                        max_retries = 3,
                        timeout = 60,
                        ...) {
  method <- match.arg(method)

  # Get cached auth from environment
  if (is.null(xnatR_env$handle) || is.null(xnatR_env$base_url)) {
    stop("Not authenticated. Call authenticate_xnat() first.", call. = FALSE)
  }

  base_url <- xnatR_env$base_url
  handle <- xnatR_env$handle
  ssl_config <- if (xnatR_env$ssl_verify) httr::config() else httr::config(ssl_verifypeer = FALSE)

  # Build full URL
  url <- paste0(sub("/+$", "", base_url), "/", sub("^/+", "", path))

  # Use httr::RETRY for automatic exponential backoff
  response <- httr::RETRY(
    verb = method,
    url = url,
    handle = handle,  # Preserves JSESSIONID cookie
    httr::accept(accept),
    httr::timeout(timeout),
    query = query,
    body = body,
    ssl_config,
    times = max_retries,
    pause_base = 1,
    pause_cap = 60,
    pause_min = 1,
    terminate_on = c(400, 403, 404),  # Don't retry client errors except 401
    quiet = !getOption("xnatR.verbose", FALSE),
    ...
  )

  # Handle 401 - session expired, re-auth and retry ONCE
  if (httr::status_code(response) == 401) {
    if (getOption("xnatR.verbose", FALSE)) {
      message("Session expired (401), re-authenticating...")
    }

    # Re-create session using cached auth_header
    reauthenticate()

    # Retry the request ONCE
    response <- httr::RETRY(
      verb = method,
      url = url,
      handle = xnatR_env$handle,  # Use updated handle
      httr::accept(accept),
      httr::timeout(timeout),
      query = query,
      body = body,
      ssl_config,
      times = 1,  # Only one retry after re-auth
      quiet = TRUE,
      ...
    )
  }

  # Check for errors and raise classed conditions
  check_response(response)

  response
}
```

### Pattern 4: Classed Error Conditions

**What:** Use httr's http_condition() to create tryCatch-able error classes

**When to use:** For all error handling, so users can catch specific error types

**Example:**
```r
# Source: httr documentation on http_condition()
check_response <- function(response) {
  status <- httr::status_code(response)

  if (status == 401) {
    cond <- httr::http_condition(response, "error")
    class(cond) <- c("xnat_auth_error", class(cond))
    stop(cond)
  } else if (status == 403) {
    cond <- httr::http_condition(response, "error")
    class(cond) <- c("xnat_permission_error", class(cond))
    stop(cond)
  } else if (status == 404) {
    cond <- httr::http_condition(response, "error")
    class(cond) <- c("xnat_not_found_error", class(cond))
    stop(cond)
  } else if (status >= 500) {
    cond <- httr::http_condition(response, "error")
    class(cond) <- c("xnat_server_error", class(cond))
    stop(cond)
  } else if (status >= 400) {
    cond <- httr::http_condition(response, "error")
    class(cond) <- c("xnat_http_error", class(cond))
    stop(cond)
  }

  invisible(response)
}

# Usage in user code:
tryCatch(
  xnat_request("GET", "/data/projects"),
  xnat_auth_error = function(e) message("Authentication failed"),
  xnat_permission_error = function(e) message("Access denied"),
  xnat_not_found_error = function(e) message("Resource not found")
)
```

### Anti-Patterns to Avoid

- **Storing passwords in .Rhistory**: Never prompt for passwords in the console directly - use rstudioapi::askForPassword() or getPass::getPass()
- **Retrying 4xx errors**: Don't retry client errors (400, 403, 404) - they're deterministic failures
- **Printing raw HTML error pages**: Parse error messages from JSON responses or use status code text
- **Manual cookie management**: Let httr handles manage cookies automatically; don't manually set JSESSIONID headers
- **Bypassing centralized request helper**: All HTTP calls should go through xnat_request() for consistent retry/error handling

## Don't Hand-Roll

Problems that look simple but have existing solutions:

| Problem | Don't Build | Use Instead | Why |
|---------|-------------|-------------|-----|
| Retry with backoff | Custom sleep() loops | httr::RETRY() | Implements exponential backoff with jitter (AWS-recommended pattern), handles 429 retry-after headers, configurable |
| HTTP error handling | if/else status checks | httr::http_condition() + stop_for_status() | Creates tryCatch-able condition classes, standard pattern across httr packages |
| Cookie persistence | Manual cookie headers | httr::handle() | Automatically preserves cookies across requests to same host, reduces connection overhead |
| .netrc parsing | Read/parse ~/.netrc manually | httr config(netrc = 1) | curl backend natively supports .netrc, handles platform differences (Windows _netrc) |
| Credential caching | Custom caching logic | Package environment (existing xnatR_env) | R-native pattern, works with package load/unload, simple and explicit |
| Base64 encoding | manual base64 | base64enc::base64encode() | Already a dependency, handles edge cases correctly |

**Key insight:** httr is designed for API clients and solves most common patterns. Leverage its built-in features (RETRY, handles, conditions, config) rather than reimplementing.

## Common Pitfalls

### Pitfall 1: httr2 Migration Temptation

**What goes wrong:** Seeing that httr2 is "the future" and attempting to migrate existing httr code

**Why it happens:** httr2 has better features (pipeable, OAuth, caching), and httr is marked "superseded"

**How to avoid:** Stick with httr for brownfield projects. httr is stable, well-documented, and will be maintained. Migrating requires rewriting ALL existing HTTP code (list_projects, list_subjects, etc.). Not worth the cost for existing codebase.

**Warning signs:** Reading httr2 documentation when you already have working httr code

### Pitfall 2: Not Using Handles for Session Persistence

**What goes wrong:** Creating new HTTP connections for each request, cookies don't persist, JSESSIONID is lost

**Why it happens:** Using httr::GET(url) directly without a handle

**How to avoid:** Create a handle with httr::handle(base_url) and pass it to all requests, or use the default handle behavior. Store handle in xnatR_env for reuse.

**Warning signs:** Having to re-authenticate on every request; JSESSIONID not being sent

### Pitfall 3: .netrc File Permissions

**What goes wrong:** .netrc credentials not being used even though file exists

**Why it happens:** curl requires strict file permissions (600 or 400 on Unix, user-only read)

**How to avoid:** Check file permissions during credential discovery. Use Sys.chmod() to set mode "0600" if needed. Warn user if permissions are too open.

**Warning signs:** credentials = "netrc" but authentication still failing; curl security warnings

### Pitfall 4: Retrying 401 Indefinitely

**What goes wrong:** Infinite loop of re-auth attempts when credentials are actually invalid

**Why it happens:** Automatic re-auth on 401 without limiting retries

**How to avoid:** Only attempt re-auth ONCE per request. If still 401 after re-auth, fail with authentication error. Track re-auth attempts.

**Warning signs:** Long hangs during authentication; repeated POST /data/JSESSION calls in logs

### Pitfall 5: Provider Prefix Confusion

**What goes wrong:** Using old XNAT 1.6 syntax like "provider.ldap/username" instead of modern "ldap/username"

**Why it happens:** Old XNAT documentation still exists online

**How to avoid:** Support both syntaxes: if username contains "/", assume provider prefix. XNAT 1.7+ simplified to just "providerid/username" without "provider." prefix.

**Warning signs:** Users report LDAP auth not working with provider prefix

### Pitfall 6: Missing yaml Dependency

**What goes wrong:** Package fails with "could not find function yaml.load_file" on systems without yaml package

**Why it happens:** Current DESCRIPTION doesn't list yaml in Imports, even though load_credentials() uses it

**How to avoid:** Add yaml to DESCRIPTION Imports section immediately

**Warning signs:** Works in dev environment (yaml installed) but fails on fresh installs

## Code Examples

Verified patterns from official sources:

### httr RETRY with Exponential Backoff
```r
# Source: https://httr.r-lib.org/reference/RETRY.html
library(httr)

# Automatic retry with exponential backoff (1s, 2s, 4s)
response <- RETRY(
  "GET",
  "https://xnat.example.com/data/projects",
  times = 3,              # Max retry attempts
  pause_base = 1,         # Base delay (2^attempt * pause_base)
  pause_cap = 60,         # Max delay cap
  pause_min = 1,          # Min delay with jitter
  terminate_on = c(400, 403, 404),  # Don't retry these
  quiet = FALSE           # Show retry messages
)
```

### .netrc Support via httr config
```r
# Source: curl documentation + httr config()
library(httr)

# Enable .netrc credential discovery
response <- GET(
  "https://xnat.example.com/data/projects",
  config(
    netrc = 1,  # Enable .netrc
    netrc_file = path.expand("~/.netrc")  # Explicit path (optional)
  )
)

# .netrc file format (~/.netrc with mode 600):
# machine xnat.example.com
# login myusername
# password mypassword
```

### httr Handle for Cookie Persistence
```r
# Source: httr documentation on handles
library(httr)

# Create handle for persistent connection and cookies
h <- handle("https://xnat.example.com")

# All requests with this handle share cookies
r1 <- GET(handle = h, path = "/data/JSESSION", authenticate("user", "pass"))
r2 <- GET(handle = h, path = "/data/projects")  # JSESSIONID auto-included

# Extract cookies from handle
cookies(h)
```

### Classed Error Conditions with tryCatch
```r
# Source: https://httr.r-lib.org/reference/http_condition.html
library(httr)

# Create classed condition
create_xnat_error <- function(response, type) {
  cond <- http_condition(response, "error")
  class(cond) <- c(paste0("xnat_", type, "_error"), class(cond))
  cond
}

# Use with tryCatch
result <- tryCatch(
  {
    r <- GET("https://xnat.example.com/data/invalid")
    stop_for_status(r)
    content(r)
  },
  http_404 = function(e) {
    message("Resource not found")
    NULL
  },
  http_401 = function(e) {
    message("Authentication failed")
    NULL
  }
)
```

### Environment Variable Credential Discovery
```r
# Source: https://httr.r-lib.org/articles/secrets.html
# Standard pattern from httr secrets vignette

# In .Renviron:
# XNAT_HOST=https://xnat.example.com
# XNAT_USER=myusername
# XNAT_PASS=mypassword

# In R code:
get_credentials <- function() {
  list(
    host = Sys.getenv("XNAT_HOST", unset = ""),
    user = Sys.getenv("XNAT_USER", unset = ""),
    pass = Sys.getenv("XNAT_PASS", unset = "")
  )
}

# Check if set
creds <- get_credentials()
if (nzchar(creds$host) && nzchar(creds$user) && nzchar(creds$pass)) {
  # Use credentials
  authenticate_xnat(creds$host, creds$user, password = creds$pass)
}
```

### XNAT JSESSION Creation
```r
# Source: XNAT REST API documentation
# POST /data/JSESSION with Basic Auth, returns 32-char hex JSESSIONID

library(httr)

create_jsession <- function(base_url, username, password) {
  # Construct Basic Auth header
  auth_string <- paste0(username, ":", password)
  auth_header <- paste("Basic", base64enc::base64encode(charToRaw(auth_string)))

  # POST to /data/JSESSION
  session_url <- paste0(sub("/+$", "", base_url), "/data/JSESSION")

  response <- POST(
    url = session_url,
    add_headers(Authorization = auth_header),
    accept("text/plain")
  )

  if (status_code(response) == 200) {
    jsessionid <- trimws(content(response, as = "text", encoding = "UTF-8"))
    message("JSESSIONID: ", jsessionid)
    return(jsessionid)
  } else {
    stop("Failed to create session: HTTP ", status_code(response))
  }
}

# Use JSESSIONID in subsequent requests as cookie
# httr handles this automatically when using handles
```

### XNAT Alias Token Issue
```r
# Source: XNAT REST API documentation
# GET /data/services/tokens/issue (requires existing auth)

issue_alias_token <- function() {
  # Requires existing JSESSION or Basic Auth
  response <- xnat_request(
    method = "GET",
    path = "/data/services/tokens/issue",
    accept = "application/json"
  )

  token_data <- jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8")
  )

  # Returns: alias, secret, xdatUserId, estimatedExpirationTime, etc.
  list(
    alias = token_data$alias,
    secret = token_data$secret,
    expires = token_data$estimatedExpirationTime
  )
}

# Validate token
validate_alias_token <- function(alias, secret) {
  path <- sprintf("/data/services/tokens/validate/%s/%s", alias, secret)
  response <- xnat_request("GET", path, accept = "application/json")

  result <- jsonlite::fromJSON(
    httr::content(response, as = "text", encoding = "UTF-8")
  )

  result$valid  # TRUE/FALSE
}

# Invalidate token
invalidate_alias_token <- function(alias, secret) {
  path <- sprintf("/data/services/tokens/invalidate/%s/%s", alias, secret)
  response <- xnat_request("GET", path)

  httr::status_code(response) == 200
}
```

## State of the Art

| Old Approach | Current Approach | When Changed | Impact |
|--------------|------------------|--------------|--------|
| httr for new projects | httr2 for new projects | November 2023 (httr2 1.0.0) | httr marked "superseded" but stable; httr2 not worth migration for existing httr code |
| Basic Auth on every request | JSESSIONID sessions | XNAT 1.0+ | Reduced credential exposure, better performance, standard practice |
| Manual retry loops | httr::RETRY() | httr 1.3.0 (2017) | Standardized exponential backoff with jitter |
| .Rprofile password storage | .Renviron + keyring | Ongoing best practice | Better security, no plaintext in code |
| XNAT provider.ldap/ prefix | XNAT ldap/ prefix | XNAT 1.7 (2018) | Simplified config, both syntaxes still work |

**Deprecated/outdated:**
- httr::authenticate() with type="basic" on every request - use JSESSION instead for persistence
- Storing JSESSIONID in headers manually - use httr handles for automatic cookie management
- Setting download.file.method for .netrc - httr's config(netrc=1) is more robust

## Open Questions

Things that couldn't be fully resolved:

1. **XNAT Provider Prefix Exact Syntax**
   - What we know: XNAT 1.8+ supports provider prefixes like "ldap/username" for LDAP authentication
   - What's unclear: Whether old "provider.ldap/username" syntax still works, or if it's just "ldap/username"
   - Recommendation: Support both formats - strip "provider." prefix if present, pass rest as-is

2. **JSESSION Expiry Timing**
   - What we know: JSESSION can expire; server returns 401 when expired
   - What's unclear: Default timeout value, whether it's configurable per-server
   - Recommendation: Don't track client-side; handle 401 reactively with re-auth

3. **Alias Token Default Timeout**
   - What we know: Tokens valid for 48 hours by default, configurable via aliasTokenTimeout site preference
   - What's unclear: Whether timeout is returned in token issue response for client-side tracking
   - Recommendation: Store estimatedExpirationTime from response, but don't preemptively fail - let server decide

4. **.netrc Multi-Host Support**
   - What we know: .netrc can have multiple machine entries
   - What's unclear: How httr/curl matches when base_url has port or path components
   - Recommendation: Test with real .netrc; document that machine should be hostname only (no protocol/port/path)

5. **R 4.6.0 netrc Option**
   - What we know: R 4.6.0 added native netrc option support (as of July 2025 in RStudio tracking)
   - What's unclear: Whether this affects httr's curl backend or is separate
   - Recommendation: Use httr's config(netrc=1) for now; compatible with all R versions

## Sources

### Primary (HIGH confidence)
- [httr RETRY function documentation](https://httr.r-lib.org/reference/RETRY.html) - Exponential backoff implementation
- [httr Managing Secrets vignette](https://httr.r-lib.org/articles/secrets.html) - Environment variables, credential storage
- [httr http_condition documentation](https://httr.r-lib.org/reference/http_condition.html) - Classed error conditions
- [httr set_cookies documentation](https://httr.r-lib.org/reference/set_cookies.html) - Cookie handling
- [XNAT User Session Management API](https://wiki.xnat.org/display/XAPI/User+Session+Management+API) - JSESSION creation/deletion
- [XNAT User Alias Token API](https://wiki.xnat.org/display/XAPI/User+Alias+Token+API) - Token issue/validate/invalidate

### Secondary (MEDIUM confidence)
- [httr2 1.0.0 release notes](https://tidyverse.org/blog/2023/11/httr2-1-0-0/) - Why httr2 exists, comparison
- [gargle auth patterns](https://gargle.r-lib.org/articles/gargle-auth-in-client-package.html) - Google package auth design
- [gitcreds package documentation](https://gitcreds.r-lib.org/articles/package.html) - Credential precedence pattern
- [XNAT Configuring Authentication Providers](https://wiki.xnat.org/documentation/configuring-authentication-providers) - Provider prefix syntax
- [curl .netrc documentation](https://everything.curl.dev/usingcurl/netrc.html) - .netrc format and usage
- [R-hub blog: Retries in API packages](https://blog.r-hub.io/2020/04/07/retry-wheel/) - Best practices for retry logic
- [Earthdata R authentication docs](https://urs.earthdata.nasa.gov/documentation/for_users/data_access/r) - .netrc with R example

### Tertiary (LOW confidence)
- GitHub issue #339 on httr handles/cookies - Community discussion, not official docs
- XNAT Google Groups discussions on LDAP - User reports, may not reflect current state

## Metadata

**Confidence breakdown:**
- Standard stack: HIGH - httr/yaml/base64enc are all currently in use and well-documented
- Architecture patterns: HIGH - Patterns verified from official httr docs and XNAT API docs
- XNAT API endpoints: MEDIUM - Documentation exists but WebFetch couldn't access wiki content (JavaScript issues)
- .netrc integration: MEDIUM - Verified curl supports it, httr uses curl, but specific httr integration details from community sources
- Provider prefix syntax: LOW - Documentation found but conflicting info between XNAT versions
- Pitfalls: HIGH - Based on official httr guidance and common API client patterns

**Research date:** 2026-01-26
**Valid until:** Approximately 60 days (2026-03-27) - httr and XNAT API are stable; no major changes expected
