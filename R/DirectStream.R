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
  
  normalize_symbol <- function(sym) gsub("_","/", sym, fixed = TRUE)
  denorm_symbol    <- function(pair) gsub("/","_", pair, fixed = TRUE)
  
  .check_cfg <- function(){
    if (!nzchar(api_token_f()))  stop("OANDA api_key mangler. Sett options(OANDA.API_KEY='...') eller OANDA_API_KEY.", call.=FALSE)
    if (!nzchar(account_id_f())) stop("OANDA account_id mangler. Sett options(oanda.account_id='...') eller OANDA_ACCOUNT_ID.", call.=FALSE)
    if (!grepl("^https?://", base_url_f()))        stop("base_url ser rar ut: ",        base_url_f(),        call.=FALSE)
    if (!grepl("^https?://", stream_base_url_f())) stop("stream_base_url ser rar ut: ", stream_base_url_f(), call.=FALSE)
    invisible(TRUE)
  }
  
  has_auth <- function() {
    nzchar(api_token_f()) && nzchar(account_id_f())
  }
  
  ## felles request-builder
  .req_base <- function(url) {
    httr2::request(url) |>
      httr2::req_headers(
        Authorization = paste("Bearer", api_token_f()),
        Accept        = "application/json",
        `User-Agent`  = "tri-fx-arb/0.1 (R httr2)"
      ) |>
      httr2::req_timeout(30) |>
      httr2::req_retry(max_tries = 4, backoff = ~ runif(1, 0.4, 1.2) * (2^(.x-1)))
  }
  
  ## trygg uthenting av siste pris uansett struktur
  .last_price <- function(x) {
    if (is.null(x) || length(x) == 0) return(NA_real_)
    # Liste av lister: ta siste element og hent $price hvis finnes
    if (is.list(x) && !is.data.frame(x)) {
      last <- x[[length(x)]]
      if (is.list(last) && !is.null(last$price)) {
        return(suppressWarnings(as.numeric(last$price)))
      }
      # Hvis elementet i seg selv er atomisk, prøv å tolke som tall
      return(suppressWarnings(as.numeric(last)))
    }
    # Data.frame med kolonne "price"
    if (is.data.frame(x) && "price" %in% names(x)) {
      return(suppressWarnings(as.numeric(utils::tail(x$price, 1))))
    }
    # Atomisk vektor
    suppressWarnings(as.numeric(utils::tail(x, 1)))
  }

  .normalize_depth <- function(buckets, depth_limit = 10L) {
    if (is.null(buckets) || length(buckets) == 0) {
      empty <- data.frame(
        level = integer(), bid_price = numeric(), bid_size = numeric(),
        ask_price = numeric(), ask_size = numeric(), stringsAsFactors = FALSE
      )
      return(list(frame = empty, arrays = list(
        bid = list(price = numeric(), size = numeric()),
        ask = list(price = numeric(), size = numeric())
      )))
    }

    if (is.data.frame(buckets)) {
      df <- buckets
    } else {
      df <- do.call(rbind, lapply(buckets, function(x) as.data.frame(x, stringsAsFactors = FALSE)))
    }
    price <- suppressWarnings(as.numeric(df$price))
    bid_sz <- suppressWarnings(as.numeric(df$longCountPercent %||% df$bid_size %||% df$longCount %||% df$bidVolume %||% df$longCountPercent))
    ask_sz <- suppressWarnings(as.numeric(df$shortCountPercent %||% df$ask_size %||% df$shortCount %||% df$askVolume %||% df$shortCountPercent))

    depth_df <- data.frame(
      level = seq_len(NROW(df)),
      bid_price = price,
      bid_size = bid_sz,
      ask_price = price,
      ask_size = ask_sz,
      stringsAsFactors = FALSE
    )

    depth_df <- depth_df[seq_len(min(nrow(depth_df), depth_limit)), , drop = FALSE]

    list(
      frame = depth_df,
      arrays = list(
        bid = list(price = depth_df$bid_price, size = depth_df$bid_size),
        ask = list(price = depth_df$ask_price, size = depth_df$ask_size)
      )
    )
  }

  .normalize_orderbook <- function(raw, instrument) {
    ts <- raw$time %||% raw$timestamp %||% raw$orderBook$time %||% NA_character_
    buckets <- raw$orderBook$buckets %||% raw$buckets %||% NULL
    depth <- .normalize_depth(buckets)

    l1_bid <- if (nrow(depth$frame) > 0) depth$frame$bid_price[[1]] else NA_real_
    l1_ask <- if (nrow(depth$frame) > 0) depth$frame$ask_price[[1]] else NA_real_

    list(
      instrument = instrument,
      timestamp = ts,
      provider = "oanda-practice",
      l1 = list(
        bid = list(price = l1_bid, size = depth$frame$bid_size[[1]] %||% NA_real_),
        ask = list(price = l1_ask, size = depth$frame$ask_size[[1]] %||% NA_real_)
      ),
      depth = depth$arrays,
      depth_frame = depth$frame,
      raw = raw
    )
  }
  
  ## ---------- konto/instrument ----------
  ping <- function(){
    .check_cfg()
    url  <- sprintf("%s/accounts/%s/summary", base_url_f(), account_id_f())
    resp <- .req_base(url) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
    cat("HTTP", httr2::resp_status(resp), httr2::resp_status_desc(resp), "\n")
    body <- httr2::resp_body_string(resp); if (nzchar(body)) cat(body, "\n")
    invisible(resp)
  }
  
  list_symbols <- function(){
    .check_cfg()
    url  <- sprintf("%s/accounts/%s/instruments", base_url_f(), account_id_f())
    resp <- .req_base(url) |> httr2::req_perform(); httr2::resp_check_status(resp)
    j    <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
    syms <- j$instruments
    out  <- syms$name[syms$type == "CURRENCY" & grepl("^[A-Z]{3}_[A-Z]{3}$", syms$name)]
    as.character(sort(unique(out)))
  }
  
  instruments_meta <- function(){
    .check_cfg()
    url  <- sprintf("%s/accounts/%s/instruments", base_url_f(), account_id_f())
    resp <- .req_base(url) |> httr2::req_perform(); httr2::resp_check_status(resp)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)$instruments
  }
  
  .get_candles <- function(instr, granularity="M1", count=10, price="BA"){
    url  <- sprintf("%s/instruments/%s/candles", base_url_f(), instr)
    resp <- .req_base(url) |>
      httr2::req_url_query(granularity = granularity, count = count, price = price) |>
      httr2::req_perform()
    httr2::resp_check_status(resp)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
  }
  
  fetch_history <- function(instruments, granularity="M1", count=10){
    rows <- list()
    meta <- try(instruments_meta(), silent = TRUE)
    pip_by <- NULL
    if (!inherits(meta, "try-error") && is.data.frame(meta)) {
      pip_by <- setNames(as.numeric(meta$pipLocation), meta$name)
    }
    
    for (instr in instruments){
      j <- tryCatch(.get_candles(instr, granularity=granularity, count=count, price="BA"),
                    error=function(e) NULL)
      use_mid <- FALSE
      if (is.null(j) || is.null(j$candles) ||
          !is.list(j$candles) || !all(c("bid","ask") %in% names(j$candles))) {
        j <- tryCatch(.get_candles(instr, granularity=granularity, count=count, price="M"),
                      error=function(e) NULL)
        use_mid <- !is.null(j) && !is.null(j$candles) && "mid" %in% names(j$candles)
        if (!use_mid) next
      }
      parts <- strsplit(instr, "_", fixed=TRUE)[[1]]
      pair  <- paste(parts[1], parts[2], sep="/")
      
      half_spread <- 0.0
      if (use_mid) {
        if (!is.null(pip_by) && !is.na(pip_by[instr])) {
          half_spread <- 0.5 * 10^(as.numeric(pip_by[instr]))  # 0.5 pip
        } else {
          half_spread <- 0.00005
        }
      }
      
      if (use_mid){
        mid <- suppressWarnings(as.numeric(j$candles$mid$c))
        tm  <- as.POSIXct(j$candles$time, tz = "UTC")
        ok  <- which(is.finite(mid))
        if (length(ok)){
          rows[[length(rows)+1]] <- data.frame(
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
        if (length(ok)){
          rows[[length(rows)+1]] <- data.frame(
            pair = pair, bid = bid[ok], ask = ask[ok], time = tm[ok],
            stringsAsFactors = FALSE
          )
        }
      }
    }
    if (length(rows)) dplyr::bind_rows(rows) else
      data.frame(pair=character(), bid=numeric(), ask=numeric(), time=as.POSIXct(character()))
  }
  
  ## ---------- live pricing ----------
  snapshot <- function(instruments){
    .check_cfg()
    if (!length(instruments)) {
      return(data.frame(pair=character(), bid=numeric(), ask=numeric(), time=character(), stringsAsFactors = FALSE))
    }
    instr_csv <- paste(instruments, collapse = ",")
    url  <- sprintf("%s/accounts/%s/pricing", base_url_f(), account_id_f())
    resp <- .req_base(url) |>
      httr2::req_url_query(instruments = instr_csv) |>
      httr2::req_perform()
    httr2::resp_check_status(resp)
    
    # Viktig: IKKE flatten automatisk
    j <- httr2::resp_body_json(resp, simplifyVector = FALSE)
    
    if (is.null(j$prices) || !length(j$prices)) {
      return(data.frame(pair=character(), bid=numeric(), ask=numeric(), time=character(), stringsAsFactors = FALSE))
    }
    
    rows <- lapply(j$prices, function(q) {
      pair <- normalize_symbol(q$instrument)
      bidv <- .last_price(q$bids)
      askv <- .last_price(q$asks)
      tm   <- if (!is.null(q$time)) as.POSIXct(q$time, tz = "UTC") else NA
      data.frame(pair = pair, bid = bidv, ask = askv, time = tm, stringsAsFactors = FALSE)
    })
    dplyr::bind_rows(rows)
  }
  
  stream_prices <- function(instruments,
                            on_tick,
                            include_heartbeats = FALSE,
                            verbose = FALSE,
                            snapshot = TRUE) {   # <— NEW
    .check_cfg(); stopifnot(is.function(on_tick))
    if (!length(instruments)) stop("stream_prices(): empty instruments vector", call. = FALSE)
    
    if (length(instruments) > 100L) {
      stop(sprintf("OANDA stream: maks 50 instrumenter per stream. Fikk %d.", length(instruments)), call. = FALSE)
    }
    
    instr_csv <- paste(instruments, collapse = ",")
    base <- sprintf("%s/accounts/%s/pricing/stream", stream_base_url_f(), account_id_f())
    url  <- paste0(
      base,
      "?instruments=", utils::URLencode(instr_csv, reserved = TRUE),
      if (isTRUE(snapshot)) "&snapshot=true" else ""
    )
    
    
    # NB: character-vektor (ikke list)
    headers <- c(
      Authorization = paste("Bearer", api_token_f()),
      Accept        = "application/json",
      "User-Agent"  = "tri-fx-arb/0.1 (R curl)"
    )
    
    attempt <- 0L
    repeat {
      attempt <- attempt + 1L
      if (verbose) message(sprintf("📡 stream connect attempt #%d …", attempt))
      
      h <- curl::new_handle(verbose = isTRUE(verbose))
      curl::handle_setheaders(h, .list = as.list(headers))
      curl::handle_setopt(h, accept_encoding = "identity")
      # ev. slå av komprimering:
      # curl::handle_setopt(h, accept_encoding = "identity")
      
      got_any <- FALSE
      
      handler <- function(x) {
        if (!length(x)) return()
        txt <- rawToChar(x)
        for (ln in strsplit(txt, "\n", fixed = TRUE)[[1]]) {
          ln <- trimws(ln); if (!nzchar(ln)) next
          
          j <- try(jsonlite::fromJSON(ln, simplifyVector = FALSE), silent = TRUE)
          if (inherits(j, "try-error")) next
          
          # heartbeats
          if (!is.null(j$type) && j$type == "HEARTBEAT") {
            if (include_heartbeats && isTRUE(verbose) && !is.null(j$time)) {
              message("💓 ", j$time)
            }
            next
          }
          
          # prices
          if (is.list(j) && !is.null(j$type) && j$type == "PRICE") {
            bid <- .last_price(j$bids)
            ask <- .last_price(j$asks)
            if (is.finite(bid) && is.finite(ask)) {
              got_any <<- TRUE
              on_tick(j$instrument, bid, ask, time = j$time) 
            } else if (verbose) {
              message("⚠️ non-finite bid/ask; instrument: ", j$instrument)
            }
          } else if (verbose) {
            message("⚠️ unexpected object type from stream: ", paste(class(j), collapse = "+"))
            message("⚠️ problematic data: ", ln)
          }
        } # <- slutt for-løkke
      } # <- slutt handler
      
      ok <- TRUE
      tryCatch(
        curl::curl_fetch_stream(url = url, handle = h, fun = handler),
        error = function(e) {
          ok <<- FALSE
          if (verbose) message("⚠️ stream error: ", conditionMessage(e))
        }
      )
      
      if (verbose) {
        if (ok) message("ℹ️ stream closed by server.")
        if (!got_any) message("ℹ️ no data received before close.")
      }
      
      # eksponentiell backoff (maks 30s)
      delay <- min(30, 1 * 2^(attempt - 1))
      Sys.sleep(delay)
    }
    
    invisible(TRUE)
  }
  
  
  ## ---------- ordre/endepunkter ----------
  
  
  
  place_market <- function(instrument, side = c("buy","sell"), units){
    .check_cfg()
    side <- match.arg(side)
    stopifnot(is.numeric(units), is.finite(units), units > 0)
    u <- if (side == "buy") units else -units
    
    url  <- sprintf("%s/accounts/%s/orders", base_url_f(), account_id_f())
    body <- list(order = list(
      type        = "MARKET",
      instrument  = instrument,
      units       = as.character(u),
      timeInForce = "FOK"  # eller "IOC"
    ))
    resp <- .req_base(url) |>
      httr2::req_method("POST") |>
      httr2::req_body_json(body, auto_unbox = TRUE) |>
      httr2::req_perform()
    httr2::resp_check_status(resp)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
  }
  
  list_open_orders <- function(){
    .check_cfg()
    url <- sprintf("%s/accounts/%s/openOrders", base_url_f(), account_id_f())
    resp <- .req_base(url) |> httr2::req_perform()
    httr2::resp_check_status(resp)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)$orders
  }
  
  account_summary <- function(){
    .check_cfg()
    url <- sprintf("%s/accounts/%s/summary", base_url_f(), account_id_f())
    resp <- .req_base(url) |> httr2::req_perform()
    httr2::resp_check_status(resp)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)$account
  }
  
  ## ---------- aggregert kundedata ----------
  orderbook <- function(instrument, time = NULL){
    .check_cfg()
    url <- sprintf("%s/instruments/%s/orderBook", base_url_f(), instrument)
    req <- .req_base(url)
    if (!is.null(time)) req <- httr2::req_url_query(req, time = time)
    resp <- httr2::req_perform(req)
    httr2::resp_check_status(resp)
    raw <- jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
    .normalize_orderbook(raw, instrument)
  }
  
  positionbook <- function(instrument, time = NULL){
    .check_cfg()
    url <- sprintf("%s/instruments/%s/positionBook", base_url_f(), instrument)
    req <- .req_base(url)
    if (!is.null(time)) req <- httr2::req_url_query(req, time = time)
    resp <- httr2::req_perform(req)
    httr2::resp_check_status(resp)
    jsonlite::fromJSON(httr2::resp_body_string(resp), simplifyVector = TRUE)
  }
  
  ## ---------- eksponer API ----------
  
  # liten helper som ikke printer nøkkelverdier
  describe <- function() {
    list(
      base_url        = base_url_f(),
      stream_base_url = stream_base_url_f(),
      account_id      = account_id_f(),
      has_auth        = has_auth()
    )
  }
  
  list(
    provider               = "oanda-practice",
    
    # funcs (kalles som DS$base_url(), DS$stream_base_url())
    base_url               = base_url_f,
    stream_base_url        = stream_base_url_f,
    
    # ferdig evaluerte verdier (til rask inspeksjon)
    base_url_value         = base_url_f(),
    stream_base_url_value  = stream_base_url_f(),
    
    # identifikatorer (NB: ikke print DS$api_token() i konsollen)
    api_token              = api_token_f,
    account_id             = account_id_f,
    
    # kjapp auth-sjekk
    has_auth               = has_auth,
    
    # valgfri versjon/info
    version                = "tri-fx-arb DS 0.2",
    
    # dine eksisterende exports
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
    
    # oversikt
    describe               = describe
  )
})# <-- Viktig: avslutter DS <- local({ ... }) med "})"

