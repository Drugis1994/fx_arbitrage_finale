import ctypes
import os
import sys

# Adjust name if you build different
if sys.platform == "darwin":
    LIBNAME = "libengine_v4.dylib"
elif sys.platform.startswith("linux"):
    LIBNAME = "libengine_v4.so"
else:
    raise RuntimeError("Unsupported platform")

_here = os.path.dirname(os.path.abspath(__file__))
_libpath = os.path.join(_here, LIBNAME)

lib = ctypes.CDLL(_libpath)

# ---- signatures ----
lib.engine_create_tri.restype = ctypes.c_void_p
lib.engine_create_tri.argtypes = [
    ctypes.c_int, ctypes.c_int,
    ctypes.POINTER(ctypes.c_int), ctypes.c_int,
    ctypes.c_int
]

lib.engine_start.restype = ctypes.c_int
lib.engine_start.argtypes = [ctypes.c_void_p]

lib.engine_stop.restype = None
lib.engine_stop.argtypes = [ctypes.c_void_p]

lib.engine_destroy.restype = None
lib.engine_destroy.argtypes = [ctypes.c_void_p]

lib.engine_push_tick.restype = ctypes.c_int
lib.engine_push_tick.argtypes = [ctypes.c_void_p, ctypes.c_int, ctypes.c_int, ctypes.c_double, ctypes.c_double]

class Result(ctypes.Structure):
    _fields_ = [
        ("route_id", ctypes.c_int),
        ("mid_i", ctypes.c_int),
        ("end_i", ctypes.c_int),
        ("edge", ctypes.c_double),
        ("final_base", ctypes.c_double),
        ("pnl", ctypes.c_double),
        ("pct", ctypes.c_double),
        ("ts_ns", ctypes.c_uint64),
    ]

lib.engine_pop_result.restype = ctypes.c_int
lib.engine_pop_result.argtypes = [ctypes.c_void_p, ctypes.POINTER(Result)]

class Engine:
    def __init__(self, n_ccy, start_i_1b, routes_mid_end_1b):
        # routes_mid_end_1b: list of (mid,end) 1-based
        flat = []
        for mid, end in routes_mid_end_1b:
            flat.extend([mid, end])
        arr = (ctypes.c_int * len(flat))(*flat)
        self._eng = lib.engine_create_tri(n_ccy, start_i_1b, arr, len(routes_mid_end_1b), 1)
        if not self._eng:
            raise RuntimeError("engine_create_tri failed")

    def start(self):
        return bool(lib.engine_start(self._eng))

    def stop(self):
        lib.engine_stop(self._eng)

    def destroy(self):
        if self._eng:
            lib.engine_destroy(self._eng)
            self._eng = None

    def push_tick(self, i_1b, j_1b, bid, ask):
        return bool(lib.engine_push_tick(self._eng, i_1b, j_1b, float(bid), float(ask)))

    def poll(self):
        out = []
        r = Result()
        while lib.engine_pop_result(self._eng, ctypes.byref(r)):
            out.append({
                "route_id": r.route_id,
                "mid": r.mid_i,
                "end": r.end_i,
                "pnl": r.pnl,
                "edge": r.edge,
                "final_base": r.final_base,
                "pct": r.pct,
                "ts_ns": int(r.ts_ns),
            })
        return out