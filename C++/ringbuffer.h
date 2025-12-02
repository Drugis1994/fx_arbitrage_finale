#pragma once
#include <stddef.h>
#include <stdatomic.h>

#ifdef __cplusplus
extern "C"
{
#endif

// SPSC ringbuffer (single-producer, single-consumer)
// Capacity is N, but usable slots are N-1 (classic ring).
#define RB_DEFINE(name, type, N)                                               \
    typedef struct name                                                        \
    {                                                                          \
        type buffer[(N)];                                                      \
        atomic_size_t head;                                                    \
        atomic_size_t tail;                                                    \
    } name;                                                                    \
                                                                               \
    static inline void name##_init(name *rb)                                   \
    {                                                                          \
        atomic_init(&rb->head, 0);                                             \
        atomic_init(&rb->tail, 0);                                             \
    }                                                                          \
                                                                               \
    static inline int name##_push(name *rb, const type *v)                     \
    {                                                                          \
        size_t h = atomic_load_explicit(&rb->head, memory_order_relaxed);      \
        size_t n = (h + 1) % (N);                                              \
        size_t t = atomic_load_explicit(&rb->tail, memory_order_acquire);      \
        if (n == t)                                                            \
            return 0;                                                          \
        rb->buffer[h] = *v;                                                    \
        atomic_store_explicit(&rb->head, n, memory_order_release);             \
        return 1;                                                              \
    }                                                                          \
                                                                               \
    static inline int name##_pop(name *rb, type *out)                          \
    {                                                                          \
        size_t t = atomic_load_explicit(&rb->tail, memory_order_relaxed);      \
        size_t h = atomic_load_explicit(&rb->head, memory_order_acquire);      \
        if (t == h)                                                            \
            return 0;                                                          \
        *out = rb->buffer[t];                                                  \
        atomic_store_explicit(&rb->tail, (t + 1) % (N), memory_order_release); \
        return 1;                                                              \
    }

#ifdef __cplusplus
}
#endif