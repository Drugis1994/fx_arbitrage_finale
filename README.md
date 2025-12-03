# FX Arbitrage Engine (R + C++)

High-performance triangular FX arbitrage engine with a single computation thread and lock-free ringbuffers. R drives configuration/streaming while C++ performs matrix math.

## Repo layout
- `run.R` – portable bootstrap (loads `.env` with `dotenv`, resolves root dynamically, starts streaming loop)
- `R/` – R runtime (core runtime, wrappers, tri-route builder, trading logic, utilities)
- `C++/` – Engine core (ringbuffers, matrix math, thread loop, R bindings)

## Branch strategy
- `main`: production-ready snapshots
- `codex/work`: active refactors/feature work (current)

## Call graph (R → C++ → R)
| Step | R entry | C/C++ work | Notes |
| --- | --- | --- | --- |
| `engine_init` | `Engine_Wrapper.R` | `engine_create` (Rcpp) | routes matrix (S=start, X=mid, Y=end) prepared in R |
| `engine_start` | `Engine_Wrapper.R` | `engine_start` thread spawn | single computation thread |
| `engine_push_tick` | `Engine_Wrapper.R` | tick ringbuffer enqueue + matrix update | validates indices/spreads, prevents overflow |
| `eval_all_routes` | — | `engine.c` | reads matrices, computes PnL/edge, emits to result ring |
| `engine_poll` | `Engine_Wrapper.R` | `engine_poll` (C to R) | drains result ring into R data.frame |

## Multi-provider/HPC design notes
- Single engine instance, one memory space, one engine thread; no IPC overhead.
- Lock-free SPSC ringbuffers for ticks/results; overflow guarded with drop counters.
- Provider adapters (future): each adapter normalizes raw feeds into unified `Tick` shape `{instrument_id, bid, ask, depth[], timestamp_ns, provider_id}` and pushes into the shared input ringbuffer.
- Matrices/order book logic are provider-agnostic and support future depth-based filters (L1/L2 captured in normalized orderbook payloads).
- Current Oanda adapter stays intact; new adapters should be plug-and-play without touching engine math.

## Running
1. Set auth in `.env` or environment variables (`OANDA_API_KEY`, `OANDA_ACCOUNT_ID`, optional `FX_ARBITRAGE_ROOT`).
2. From any working directory run `Rscript run.R` (root resolves automatically).
3. The script loads R files, dynloads the platform-specific engine library (`.Platform$dynlib.ext`), builds routes, and starts streaming.

## Maintenance checklist
- Keep `.Rproj.user/`, `dev/`, `.DS_Store`, swap/backup artifacts out of Git (see `.gitignore`).
- Prefer `codex/work` for changes; open PRs from that branch back to `main`.
- Document route shapes and provider adapters alongside code changes.
