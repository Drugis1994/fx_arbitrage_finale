# R/DS.R  (OANDA Practice DS 0.3)  ---- robust "_" <-> "/" normalization
# - Safe for callers that sometimes pass "EUR/USD" and sometimes "EUR_USD"
# - Ensures endpoints that require URL path-safe instrument always get underscore form

DS <- local({
  ## ---------- små helpers ----------
  # ==============================
  # 🔐 OANDA Auth-konfig
  # ==============================
  # Leser autentiseringsdetaljer fra:
  # 1. R options() (dersom du har satt dem via options(oanda.account_id=...))
  # 2. Ellers miljøvariabler i ~/.Renviron eller .env
  # 3. Har fallback på standard API-url (OANDA Practice)
  # ==============================
  # --- OANDA base URLs & auth helpers ---
  `%||%` <- function(x, y) {
    if (is.null(x)) y else x
  }
  stream_base_url_f <- function() {
    getOption(
      "oanda.stream_base_url",
      Sys.getenv("OANDA_STREAM_BASE_URL", "https://stream-fxpractice.oanda.com/v3")
    )
  }
  
  base_url_f <- function() {
    getOption(
      "oanda.base_url",
      Sys.getenv("OANDA_BASE_URL", "https://api-fxpractice.oanda.com/v3")
    )
  }
  
  api_token_f <- function() {
    getOption(
      "OANDA.API_KEY",
      Sys.getenv("OANDA_API_KEY", "")
    )
  }
  
  account_id_f <- function() {
    trimws(
      getOption(
        "oanda.account_id",
        Sys.getenv("OANDA_ACCOUNT_ID", "")
      )
    )
  }

  # ==============================
  # 🔁 Symbol normalization helpers
  # ==============================
  # Pair form: "EUR/USD"
  # Instrument form: "EUR_USD"
  normalize_symbol <- function(sym) gsub("_", "/", sym, fixed = TRUE)
  denorm_symbol    <- function(pair) gsub("/", "_", pair, fixed = TRUE)
  
  # Always produce instrument-safe name for URL path segments
  as_instr <- function(x) {
    if (is.null(x) || !nzchar(x)) return("")
    # accept both "AAA_BBB" and "AAA/BBB"
    out <- gsub("/", "_", x, fixed = TRUE)
    # basic cleanup
    out <- toupper(trimws(out))
    out
  }
  
  .check_cfg <- function() {
    if (!nzchar(api_token_f()))
      stop("OANDA api_key mangler. Sett options(OANDA.API_KEY='...') eller OANDA_API_KEY.", call. = FALSE)
    if (!nzchar(account_id_f()))
      stop("OANDA account_id mangler. Sett options(oanda.account_id='...') eller OANDA_ACCOUNT_ID.", call. = FALSE)
    if (!grepl("^https?://", base_url_f()))
      stop("base_url ser rar ut: ", base_url_f(), call. = FALSE)
    if (!grepl("^https?://", stream_base_url_f()))
      stop("stream_base_url ser rar ut: ", stream_base_url_f(), call. = FALSE)
    invisible(TRUE)
  }
  
  has_auth <- function() {
    nzchar(api_token_f()) && nzchar(account_id_f())
  }
  
  # ==============================
  # 🌐 Request builder
  # ==============================
  .req_base <- function(url) {
    ua <- getOption("arb.user_agent", "tri-fx-arb/0.1 (R httr2)")
    timeout_sec <- getOption("oanda.http_timeout", 30)
    max_tries   <- getOption("oanda.http_max_tries", 4)
    bo_min      <- getOption("oanda.http_backoff_min", 0.4)
    bo_max      <- getOption("oanda.http_backoff_max", 1.2)
    
    httr2::request(url) |>
      httr2::req_headers(
        Authorization = paste("Bearer", api_token_f()),
        Accept        = "application/json",
        `User-Agent`  = ua
      ) |>
      httr2::req_timeout(timeout_sec) |>
      httr2::req_retry(
        max_tries = max_tries,
        backoff = ~ runif(1, bo_min, bo_max) * (2^(.x - 1))
      )
  }
  
  # ==============================
  # 🧮 Price parsing helper
  # ==============================
  .last_price <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_real_)
    
    if (is.list(x) && !is.data.frame(x)) {
      last <- x[[length(x)]]
      if (is.list(last) && !is.null(last$price)) {
        return(suppressWarnings(as.numeric(last$price)))
      }
      return(suppressWarnings(as.numeric(last)))
    }
    
    if (is.data.frame(x) && "price" %in% names(x)) {
      return(suppressWarnings(as.numeric(utils::tail(x$price, 1))))
    }
    
    suppressWarnings(as.numeric(utils::tail(x, 1)))
  }
  
  # ==============================
  # ✅ Account / instrument endpoints
  # ==============================
  ping <- function() {
    .check_cfg()
    url  <- sprintf("%s/accounts/%s/summary", base_url_f(), account_id_f())
    resp <- .req_base(url) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
    cat("HTTP", httr2::resp_status(resp), httr2::resp_status_desc(resp), "\n")
    body <- httr2::resp_body_string(resp)
    if (nzchar(body)) cat(body, "\n")
    invisible(resp)
  }
  
  list_symbols <- function() {
    .check_cfg()
    url  <- sprintf("%s/accounts/%s/instruments", base_url_f(), account_id_f())
    resp <- .req_base(url) |> httr2::req_perform()
    httr2::resp_check_status(resp)
    j    <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
    syms <- j$instruments
    out  <- syms$name[syms$type == "CURRENCY" & grepl("^[A-Z]{3}_[A-Z]{3}$", syms$name)]
    as.character(sort(unique(out)))
  }
  
  instruments_meta <- function() {
    .check_cfg()
    url  <- sprintf("%s/accounts/%s/instruments", base_url_f(), account_id_f())
    resp <- .req_base(url) |> httr2::req_perform()
    httr2::resp_check_status(resp)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)$instruments
  }
  
  # ==============================
  # 🕯️ Candles / history
  # ==============================
  .get_candles <- function(instr, granularity = "M1", count = 10, price = "BA") {
    .check_cfg()
    instr <- as_instr(instr)  # <--- critical fix (path-safe)
    url   <- sprintf("%s/instruments/%s/candles", base_url_f(), instr)
    resp  <- .req_base(url) |>
      httr2::req_url_query(granularity = granularity, count = count, price = price) |>
      httr2::req_perform()
    httr2::resp_check_status(resp)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
  }
  
  fetch_history <- function(instruments,
                            granularity = getOption("oanda.history.granularity", "M1"),
                            count       = getOption("oanda.history.count", 10)) {
    rows <- list()
    meta <- try(instruments_meta(), silent = TRUE)
    pip_by <- NULL
    if (!inherits(meta, "try-error") && is.data.frame(meta)) {
      pip_by <- setNames(as.numeric(meta$pipLocation), meta$name)
    }
    
    for (instr0 in instruments) {
      instr <- as_instr(instr0)
      
      j <- tryCatch(.get_candles(instr, granularity = granularity, count = count, price = "BA"),
                    error = function(e) NULL)
      
      use_mid <- FALSE
      if (is.null(j) || is.null(j$candles) ||
          !is.list(j$candles) || !all(c("bid","ask") %in% names(j$candles))) {
        j <- tryCatch(.get_candles(instr, granularity = granularity, count = count, price = "M"),
                      error = function(e) NULL)
        use_mid <- !is.null(j) && !is.null(j$candles) && "mid" %in% names(j$candles)
        if (!use_mid) next
      }
      
      parts <- strsplit(instr, "_", fixed = TRUE)[[1]]
      pair  <- paste(parts[1], parts[2], sep = "/")
      
      half_spread <- 0.0
      if (use_mid) {
        if (!is.null(pip_by) && !is.na(pip_by[instr])) {
          half_spread <- 0.5 * 10^(as.numeric(pip_by[instr]))  # 0.5 pip
        } else {
          half_spread <- getOption("oanda.mid_default_half_spread", 0.00005)
        }
      }
      
      if (use_mid) {
        mid <- suppressWarnings(as.numeric(j$candles$mid$c))
        tm  <- as.POSIXct(j$candles$time, tz = "UTC")
        ok  <- which(is.finite(mid))
        if (length(ok)) {
          rows[[length(rows) + 1]] <- data.frame(
            pair = pair,
            bid  = mid[ok] - half_spread,
            ask  = mid[ok] + half_spread,
            time = tm[ok],
            stringsAsFactors = FALSE
          )
        }
      } else {
        bid <- suppressWarnings(as.numeric(j$candles$bid$c))
        ask <- suppressWarnings(as.numeric(j$candles$ask$c))
        tm  <- as.POSIXct(j$candles$time, tz = "UTC")
        ok  <- which(is.finite(bid) & is.finite(ask))
        if (length(ok)) {
          rows[[length(rows) + 1]] <- data.frame(
            pair = pair, bid = bid[ok], ask = ask[ok], time = tm[ok],
            stringsAsFactors = FALSE
          )
        }
      }
    }
    
    if (length(rows)) dplyr::bind_rows(rows) else
      data.frame(pair = character(), bid = numeric(), ask = numeric(),
                 time = as.POSIXct(character()), stringsAsFactors = FALSE)
  }
  
  # ==============================
  # 📸 Snapshot
  # ==============================
  snapshot <- function(instruments) {
    .check_cfg()
    if (!length(instruments)) {
      return(data.frame(pair = character(), bid = numeric(), ask = numeric(),
                        time = character(), stringsAsFactors = FALSE))
    }
    
    # pricing endpoint expects comma-separated instrument names; use underscore
    instruments <- vapply(instruments, as_instr, character(1))
    instr_csv <- paste(instruments, collapse = ",")
    
    url  <- sprintf("%s/accounts/%s/pricing", base_url_f(), account_id_f())
    resp <- .req_base(url) |>
      httr2::req_url_query(instruments = instr_csv) |>
      httr2::req_perform()
    httr2::resp_check_status(resp)
    
    j <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    if (is.null(j$prices) || !length(j$prices)) {
      return(data.frame(pair = character(), bid = numeric(), ask = numeric(),
                        time = character(), stringsAsFactors = FALSE))
    }
    
    rows <- lapply(j$prices, function(q) {
      pair <- normalize_symbol(q$instrument)  # q$instrument is underscore
      bidv <- .last_price(q$bids)
      askv <- .last_price(q$asks)
      tm   <- if (!is.null(q$time)) as.POSIXct(q$time, tz = "UTC") else NA
      data.frame(pair = pair, bid = bidv, ask = askv, time = tm, stringsAsFactors = FALSE)
    })
    dplyr::bind_rows(rows)
  }
  
  # ==============================
  # 📡 Live streaming
  # ==============================
  stream_prices <- function(instruments,
                            on_tick,
                            include_heartbeats = getOption("arb.stream.include_heartbeats", TRUE),
                            verbose            = getOption("arb.stream.verbose", TRUE),
                            snapshot           = getOption("arb.stream.snapshot", TRUE)) {
    .check_cfg()
    stopifnot(is.function(on_tick))
    if (!length(instruments)) stop("stream_prices(): empty instruments vector", call. = FALSE)
    
    # stream expects underscore instruments
    instruments <- vapply(instruments, as_instr, character(1))
    instr_csv <- paste(instruments, collapse = ",")
    
    base <- sprintf("%s/accounts/%s/pricing/stream", stream_base_url_f(), account_id_f())
    url  <- paste0(
      base,
      "?instruments=", utils::URLencode(instr_csv, reserved = TRUE),
      if (isTRUE(snapshot)) "&snapshot=true" else ""
    )
    
    ua_stream <- getOption("arb.user_agent_stream", getOption("arb.user_agent", "tri-fx-arb/0.1 (R curl)"))
    accept_encoding <- getOption("oanda.stream.accept_encoding", "identity")
    
    headers <- c(
      Authorization = paste("Bearer", api_token_f()),
      Accept        = "application/json",
      "User-Agent"  = ua_stream
    )
    
    attempt <- 0L
    repeat {
      attempt <- attempt + 1L
      if (isTRUE(verbose)) message(sprintf("📡 stream connect attempt #%d …", attempt))
      
      h <- curl::new_handle(verbose = isTRUE(verbose))
      curl::handle_setheaders(h, .list = as.list(headers))
      curl::handle_setopt(h, accept_encoding = accept_encoding)
      
      got_any <- FALSE
      buf <- raw(0)
      
      handler <- function(x) {
        if (!length(x)) return(length(x))
        buf <<- c(buf, x)
        txt <- rawToChar(buf)
        
        lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
        complete <- lines
        remainder <- raw(0)
        
        if (nchar(txt) == 0L || substr(txt, nchar(txt), nchar(txt)) != "\n") {
          if (length(lines)) complete <- head(lines, -1)
          remainder <- charToRaw(if (length(lines)) tail(lines, 1) else "")
        }
        buf <<- remainder
        
        for (ln in complete) {
          ln <- trimws(ln)
          if (!nzchar(ln)) next
          
          j <- try(jsonlite::fromJSON(ln, simplifyVector = FALSE), silent = TRUE)
          if (inherits(j, "try-error")) next
          
          if (!is.null(j$type) && j$type %in% c("HEARTBEAT", "pricing.heartbeat")) {
            if (include_heartbeats && isTRUE(verbose) && !is.null(j$time)) {
              message("💓 ", j$time)
            }
            next
          }
          
          if (!is.null(j$type) && j$type %in% c("PRICE", "pricing.clientprice")) {
            bid <- .last_price(j$bids)
            ask <- .last_price(j$asks)
            if (is.finite(bid) && is.finite(ask)) {
              got_any <<- TRUE
              # j$instrument comes underscore; keep it as-is (core_runtime handles both)
              on_tick(j$instrument, bid, ask, time = j$time)
            } else if (isTRUE(verbose)) {
              message("⚠️ non-finite bid/ask; instrument: ", j$instrument)
            }
          } else if (isTRUE(verbose)) {
            message("⚠️ unexpected object type from stream: ", paste(class(j), collapse = "+"))
          }
        }
        length(x)
      }
      
      ok <- TRUE
      tryCatch(
        curl::curl_fetch_stream(url = url, handle = h, fun = handler),
        error = function(e) {
          ok <<- FALSE
          if (isTRUE(verbose)) message("⚠️ stream error: ", conditionMessage(e))
        }
      )
      
      if (isTRUE(verbose)) {
        if (ok) message("ℹ️ stream closed by server.")
        if (!got_any) message("ℹ️ no data received before close.")
      }
      
      delay <- min(30, 1 * 2^(attempt - 1L))
      Sys.sleep(delay)
    }
    
    invisible(TRUE)
  }
  
  # ==============================
  # 🧾 Orders / account
  # ==============================
  place_market <- function(instrument, side = c("buy", "sell"), units) {
    .check_cfg()
    side <- match.arg(side)
    stopifnot(is.numeric(units), is.finite(units), units > 0)
    
    instrument <- as_instr(instrument)  # <--- normalization (path-safe)
    u <- if (side == "buy") units else -units
    tif <- getOption("oanda.order.time_in_force", "FOK")
    
    url  <- sprintf("%s/accounts/%s/orders", base_url_f(), account_id_f())
    body <- list(order = list(
      type        = "MARKET",
      instrument  = instrument,
      units       = as.character(u),
      timeInForce = tif
    ))
    
    resp <- .req_base(url) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(body, auto_unbox = TRUE) |>
      httr2::req_perform()
    
    httr2::resp_check_status(resp)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
  }
  
  list_open_orders <- function() {
    .check_cfg()
    url <- sprintf("%s/accounts/%s/openOrders", base_url_f(), account_id_f())
    resp <- .req_base(url) |> httr2::req_perform()
    httr2::resp_check_status(resp)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)$orders
  }
  
  account_summary <- function() {
    .check_cfg()
    url <- sprintf("%s/accounts/%s/summary", base_url_f(), account_id_f())
    resp <- .req_base(url) |> httr2::req_perform()
    httr2::resp_check_status(resp)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)$account
  }
  
  # ==============================
  # 📚 OrderBook / PositionBook (path-safe)
  # ==============================
  orderbook <- function(instrument, time = NULL) {
    .check_cfg()
    instrument <- as_instr(instrument)  # <--- critical fix (URL path-safe)
    url <- sprintf("%s/instruments/%s/orderBook", base_url_f(), instrument)
    req <- .req_base(url)
    if (!is.null(time)) req <- httr2::req_url_query(req, time = time)
    resp <- httr2::req_perform(req)
    httr2::resp_check_status(resp)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
  }
  
  positionbook <- function(instrument, time = NULL) {
    .check_cfg()
    instrument <- as_instr(instrument)  # <--- critical fix (URL path-safe)
    url <- sprintf("%s/instruments/%s/positionBook", base_url_f(), instrument)
    req <- .req_base(url)
    if (!is.null(time)) req <- httr2::req_url_query(req, time = time)
    resp <- httr2::req_perform(req)
    httr2::resp_check_status(resp)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
  }
  
  # ==============================
  # 🧾 Info
  # ==============================
  describe <- function() {
    list(
      base_url        = base_url_f(),
      stream_base_url = stream_base_url_f(),
      account_id      = account_id_f(),
      has_auth        = has_auth()
    )
  }
  
  # ==============================
  # 📦 Export DS object
  # ==============================
  list(
    provider               = "oanda-practice",
    
    base_url               = base_url_f,
    stream_base_url        = stream_base_url_f,
    
    base_url_value         = base_url_f(),
    stream_base_url_value  = stream_base_url_f(),
    
    api_token              = api_token_f,
    account_id             = account_id_f,
    
    has_auth               = has_auth,
    version                = "tri-fx-arb DS 0.3",
    
    normalize_symbol       = normalize_symbol,
    denorm_symbol          = denorm_symbol,
    
    ping                   = ping,
    list_symbols           = list_symbols,
    instruments_meta       = instruments_meta,
    fetch_history          = fetch_history,
    snapshot               = snapshot,
    stream_prices          = stream_prices,
    
    place_market           = place_market,
    list_open_orders       = list_open_orders,
    account_summary        = account_summary,
    
    orderbook              = orderbook,
    positionbook           = positionbook,
    
    describe               = describe
  )
})



DS$get_server_time <- function() {
  resp <- .oanda_get("/v3/accounts")
  
  if (is.null(resp$lastTransactionID)) {
    return(NULL)
  }
  
  list(
    time = Sys.time(), 
    server = resp$lastTransactionID
  )
}