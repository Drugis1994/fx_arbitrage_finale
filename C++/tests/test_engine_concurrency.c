#include "engine.h"
#include "results.h"

#include <assert.h>
#include <pthread.h>
#include <stdio.h>
#include <time.h>

static void sleep_ms(long ms)
{
    struct timespec ts;
    ts.tv_sec = ms / 1000;
    ts.tv_nsec = (ms % 1000) * 1000000L;
    nanosleep(&ts, NULL);
}

typedef struct
{
    engine_t *eng;
} producer_ctx_t;

static void *producer_thread(void *arg)
{
    producer_ctx_t *ctx = (producer_ctx_t *)arg;
    assert(engine_push_tick(ctx->eng, 1, 2, 1.10, 1.11));
    assert(engine_push_tick(ctx->eng, 2, 3, 1.20, 1.21));
    assert(engine_push_tick(ctx->eng, 3, 1, 1.30, 1.31));
    return NULL;
}

static void test_tick_flow(void)
{
    int routes[] = {2, 3};
    engine_t *eng = engine_create_tri(3, 1, routes, 1, 1);
    assert(eng != NULL);
    assert(engine_start(eng));

    producer_ctx_t ctx = {eng};
    pthread_t thread;
    assert(pthread_create(&thread, NULL, producer_thread, &ctx) == 0);
    pthread_join(thread, NULL);

    result_t r;
    int got = 0;
    for (int attempts = 0; attempts < 20 && !got; ++attempts)
    {
        if (engine_pop_result(eng, &r))
        {
            got = 1;
            break;
        }
        sleep_ms(5);
    }

    assert(got && "engine should emit at least one result after ticks");

    engine_stop(eng);
    engine_destroy(eng);
}

int main(void)
{
    test_tick_flow();
    printf("engine concurrency test passed\n");
    return 0;
}
