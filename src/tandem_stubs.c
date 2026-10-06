/* Fills through the vendored tandem.c. The OCaml side checks ranges and positions and computes
 * the position after the fill, so these stubs never fail and never allocate.
 *
 * The generator's row cache is the OCaml record Engine.ctx = { key; shift; st; row; buf; base }:
 * st is a Bytes block of 64 int64 slots that hold the words of tandem_rng's o and h in the same
 * order, and row is the cached row or -1. Each stub runs tandem.c on that cache and writes it
 * back, so neither side reseeds after the other. The byte versions of the normal allocate. */
#include <caml/alloc.h>
#include <caml/bigarray.h>
#include <caml/mlvalues.h>

#include "tandem.h"

static tandem_rng from_ctx(value ctx, intnat word0) {
    value key = Field(ctx, 0);
    const int64_t *st = (const int64_t *)Bytes_val(Field(ctx, 2));
    intnat row = Long_val(Field(ctx, 3));
    uint32_t k[4];
    for (int i = 0; i < 4; i++) k[i] = (uint32_t)Long_val(Field(key, i));
    tandem_rng r = tandem_from_key(k, (uint64_t)word0 << 5, 1u << Long_val(Field(ctx, 1)));
    if (row >= 0) {
        for (int i = 0; i < 32; i++) {
            (&r.o[0][0])[i] = (uint32_t)st[i];
            (&r.h[0][0])[i] = (uint32_t)st[32 + i];
        }
        r.row = (uint64_t)row;
        r.cached = 1u;
    }
    return r;
}

/* Immediate values need no write barrier. */
static void to_ctx(const tandem_rng *r, value ctx) {
    int64_t *st = (int64_t *)Bytes_val(Field(ctx, 2));
    if (!r->cached) return;
    for (int i = 0; i < 32; i++) {
        st[i] = (&r->o[0][0])[i];
        st[32 + i] = (&r->h[0][0])[i];
    }
    Field(ctx, 3) = Val_long((intnat)r->row);
}

#define BA(type, a, off) ((type *)Caml_ba_data_val(a) + (off))
/* A Float.Array is always a flat block of doubles. */
#define FA(a, off) ((double *)(a) + (off))

#define FILL(name, call, target)                                                               \
    value name(value ctx, intnat word0, value a, intnat off, intnat n) {                       \
        tandem_rng r = from_ctx(ctx, word0);                                                   \
        call(&r, target, (size_t)n);                                                           \
        to_ctx(&r, ctx);                                                                       \
        return Val_unit;                                                                       \
    }                                                                                          \
    value name##_byte(value ctx, value word0, value a, value off, value n) {                   \
        return name(ctx, Long_val(word0), a, Long_val(off), Long_val(n));                      \
    }

FILL(tandem_ml_fill_u32, tandem_fill_u32, BA(uint32_t, a, off))
FILL(tandem_ml_fill_u64, tandem_fill_u64, BA(uint64_t, a, off))
FILL(tandem_ml_fill_f64, tandem_fill_f64, BA(double, a, off))
FILL(tandem_ml_fill_f64_fa, tandem_fill_f64, FA(a, off))
FILL(tandem_ml_fill_normal, tandem_fill_normal_f64, BA(double, a, off))
FILL(tandem_ml_fill_normal_fa, tandem_fill_normal_f64, FA(a, off))
FILL(tandem_ml_fill_exponential, tandem_fill_exponential_f64, BA(double, a, off))
FILL(tandem_ml_fill_exponential_fa, tandem_fill_exponential_f64, FA(a, off))
/* Point the word buffer of the scalar draws, ctx.buf, at the block of words that holds `word`.
 * Its length in words is a power of two. Bytes data is 8-byte aligned. */
value tandem_ml_refill(value ctx, intnat word) {
    value buf = Field(ctx, 4);
    intnat n = (intnat)(caml_string_length(buf) / 4), base = word & ~(n - 1);
    tandem_rng r = from_ctx(ctx, base);
    tandem_fill_u32(&r, (uint32_t *)Bytes_val(buf), (size_t)n);
    to_ctx(&r, ctx);
    Field(ctx, 5) = Val_long(base);
    return Val_unit;
}

value tandem_ml_refill_byte(value ctx, value word) { return tandem_ml_refill(ctx, Long_val(word)); }

/* The normal at 32-bit word `word`. The scalar draws call it only when the ziggurat leaves
 * its inner rectangles. */
double tandem_ml_normal(value ctx, intnat word) {
    tandem_rng r = from_ctx(ctx, word);
    double x = tandem_normal_f64(&r);
    to_ctx(&r, ctx);
    return x;
}

value tandem_ml_normal_byte(value ctx, value word) {
    return caml_copy_double(tandem_ml_normal(ctx, Long_val(word)));
}

value tandem_ml_fill_below32(value ctx, intnat word0, value a, intnat off, intnat n,
                             intnat range) {
    tandem_rng r = from_ctx(ctx, word0);
    tandem_fill_u32_below(&r, BA(uint32_t, a, off), (size_t)n, (uint32_t)range);
    to_ctx(&r, ctx);
    return Val_unit;
}

value tandem_ml_fill_below32_byte(value *argv, int argc) {
    (void)argc;
    return tandem_ml_fill_below32(argv[0], Long_val(argv[1]), argv[2], Long_val(argv[3]),
                                  Long_val(argv[4]), Long_val(argv[5]));
}

value tandem_ml_fill_below64(value ctx, intnat word0, value a, intnat off, intnat n,
                             int64_t range) {
    tandem_rng r = from_ctx(ctx, word0);
    tandem_fill_u64_below(&r, BA(uint64_t, a, off), (size_t)n, (uint64_t)range);
    to_ctx(&r, ctx);
    return Val_unit;
}

value tandem_ml_fill_below64_byte(value *argv, int argc) {
    (void)argc;
    return tandem_ml_fill_below64(argv[0], Long_val(argv[1]), argv[2], Long_val(argv[3]),
                                  Long_val(argv[4]), Int64_val(argv[5]));
}
