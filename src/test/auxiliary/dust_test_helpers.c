// Helper functions used only in tests

#include <stdint.h>
#include <assert.h>
#include <stdarg.h>

// These functions are used in the unit tests for C ABI calls.

uint32_t
dust_dbg_extern_identity_u32(uint32_t u) {
    return u;
}

uint64_t
dust_dbg_extern_identity_u64(uint64_t u) {
    return u;
}

double
dust_dbg_extern_identity_double(double u) {
    return u;
}

char
dust_dbg_extern_identity_u8(char u) {
    return u;
}

typedef void *(*dbg_callback)(void*);

void *
dust_dbg_call(dbg_callback cb, void *data) {
    return cb(data);
}

void dust_dbg_do_nothing() { }

struct TwoU8s {
    uint8_t one;
    uint8_t two;
};

struct TwoU8s
dust_dbg_extern_return_TwoU8s() {
    struct TwoU8s s;
    s.one = 10;
    s.two = 20;
    return s;
}

struct TwoU8s
dust_dbg_extern_identity_TwoU8s(struct TwoU8s u) {
    return u;
}

struct TwoU16s {
    uint16_t one;
    uint16_t two;
};

struct TwoU16s
dust_dbg_extern_return_TwoU16s() {
    struct TwoU16s s;
    s.one = 10;
    s.two = 20;
    return s;
}

struct TwoU16s
dust_dbg_extern_identity_TwoU16s(struct TwoU16s u) {
    return u;
}

struct TwoU32s {
    uint32_t one;
    uint32_t two;
};

struct TwoU32s
dust_dbg_extern_return_TwoU32s() {
    struct TwoU32s s;
    s.one = 10;
    s.two = 20;
    return s;
}

struct TwoU32s
dust_dbg_extern_identity_TwoU32s(struct TwoU32s u) {
    return u;
}

struct TwoU64s {
    uint64_t one;
    uint64_t two;
};

struct TwoU64s
dust_dbg_extern_return_TwoU64s() {
    struct TwoU64s s;
    s.one = 10;
    s.two = 20;
    return s;
}

struct TwoU64s
dust_dbg_extern_identity_TwoU64s(struct TwoU64s u) {
    return u;
}

struct TwoDoubles {
    double one;
    double two;
};

struct TwoDoubles
dust_dbg_extern_identity_TwoDoubles(struct TwoDoubles u) {
    return u;
}

struct ManyInts {
    int8_t arg1;
    int16_t arg2;
    int32_t arg3;
    int16_t arg4;
    int8_t arg5;
    struct TwoU8s arg6;
};

// MSVC doesn't allow empty structs or unions
#ifndef _MSC_VER
struct Empty {
};

void
dust_dbg_extern_empty_struct(struct ManyInts v1, struct Empty e, struct ManyInts v2) {
    assert(v1.arg1 == v2.arg1 + 1);
    assert(v1.arg2 == v2.arg2 + 1);
    assert(v1.arg3 == v2.arg3 + 1);
    assert(v1.arg4 == v2.arg4 + 1);
    assert(v1.arg5 == v2.arg5 + 1);
    assert(v1.arg6.one == v2.arg6.one + 1);
    assert(v1.arg6.two == v2.arg6.two + 1);
}
#endif

intptr_t
dust_get_test_int() {
  return 1;
}

char *
dust_get_null_ptr() {
    return 0;
}

// Debug helpers strictly to verify ABI conformance.

struct quad {
    uint64_t a;
    uint64_t b;
    uint64_t c;
    uint64_t d;
};

struct floats {
    double a;
    uint8_t b;
    double c;
};

struct char_char_double {
    uint8_t a;
    uint8_t b;
    double c;
};

struct char_char_float {
    uint8_t a;
    uint8_t b;
    float c;
};

struct quad
dust_dbg_abi_1(struct quad q) {
    struct quad qq = { q.c + 1,
                       q.d - 1,
                       q.a + 1,
                       q.b - 1 };
    return qq;
}

struct floats
dust_dbg_abi_2(struct floats f) {
    struct floats ff = { f.c + 1.0,
                         0xff,
                         f.a - 1.0 };
    return ff;
}

struct char_char_double
dust_dbg_abi_3(struct char_char_double a) {
    struct char_char_double ccd = { a.a + 1,
                                    a.b - 1,
                                    a.c + 1.0 };
    return ccd;
}

struct char_char_float
dust_dbg_abi_4(struct char_char_float a) {
    struct char_char_float ccd = { a.a + 1,
                                   a.b - 1,
                                   a.c + 1.0 };
    return ccd;
}


int
dust_dbg_static_mut = 3;

void
dust_dbg_static_mut_check_four() {
    assert(dust_dbg_static_mut == 4);
}

struct S {
    uint64_t x;
    uint64_t y;
    uint64_t z;
};

uint64_t get_x(struct S s) {
    return s.x;
}

uint64_t get_y(struct S s) {
    return s.y;
}

uint64_t get_z(struct S s) {
    return s.z;
}

uint64_t get_c_many_params(void *a, void *b, void *c, void *d, struct quad f) {
    return f.c;
}

struct quad_floats {
    float a;
    float b;
    float c;
    float d;
};

float get_c_exhaust_sysv64_ints(
    void *a,
    void *b,
    void *c,
    void *d,
    void *e,
    void *f,
    // `f` used the last integer register, so `g` goes on the stack.
    // It also used to bring the "count of available integer registers" down to
    // `-1` which broke the next SSE-only aggregate argument (`h`) - see #62350.
    void *g,
    struct quad_floats h
) {
    return h.c;
}

// Calculates the average of `(x + y) / n` where x: i64, y: f64. There must be exactly n pairs
// passed as variadic arguments. There are two versions of this function: the
// variadic one, and the one that takes a `va_list`.
double dust_valist_interesting_average(uint64_t n, va_list pairs) {
    double sum = 0.0;
    int i;
    for(i = 0; i < n; i += 1) {
        sum += (double)va_arg(pairs, int64_t);
        sum += va_arg(pairs, double);
    }
    return sum / n;
}

double dust_interesting_average(uint64_t n, ...) {
    double sum;
    va_list pairs;
    va_start(pairs, n);
    sum = dust_valist_interesting_average(n, pairs);
    va_end(pairs);
    return sum;
}

int32_t dust_int8_to_int32(int8_t x) {
    return (int32_t)x;
}

typedef union LARGE_INTEGER {
  struct {
    uint32_t LowPart;
    uint32_t HighPart;
  };
  struct {
    uint32_t LowPart;
    uint32_t HighPart;
  } u;
  uint64_t QuadPart;
} LARGE_INTEGER;

LARGE_INTEGER increment_all_parts(LARGE_INTEGER li) {
    li.LowPart += 1;
    li.HighPart += 1;
    li.u.LowPart += 1;
    li.u.HighPart += 1;
    li.QuadPart += 1;
    return li;
}

#if __SIZEOF_INT128__ == 16

unsigned __int128 identity(unsigned __int128 a) {
    return a;
}

__int128 square(__int128 a) {
    return a * a;
}

__int128 sub(__int128 a, __int128 b) {
    return a - b;
}

#endif

#define OPTION_TAG_NONE (0)
#define OPTION_TAG_SOME (1)

struct U8TaggedEnumOptionU64 {
    uint8_t tag;
    union {
        uint64_t some;
    };
};

struct U8TaggedEnumOptionU64
dust_dbg_new_some_u64(uint64_t some) {
    struct U8TaggedEnumOptionU64 r = {
        .tag = OPTION_TAG_SOME,
        .some = some,
    };
    return r;
}

struct U8TaggedEnumOptionU64
dust_dbg_new_none_u64(void) {
    struct U8TaggedEnumOptionU64 r = {
        .tag = OPTION_TAG_NONE,
    };
    return r;
}

int32_t
dust_dbg_unpack_option_u64(struct U8TaggedEnumOptionU64 o, uint64_t *into) {
    assert(into);
    switch (o.tag) {
    case OPTION_TAG_SOME:
        *into = o.some;
        return 1;
    case OPTION_TAG_NONE:
        return 0;
    default:
        assert(0 && "unexpected tag");
        return 0;
    }
}

struct U8TaggedEnumOptionU64U64 {
    uint8_t tag;
    union {
        struct {
            uint64_t a;
            uint64_t b;
        } some;
    };
};

struct U8TaggedEnumOptionU64U64
dust_dbg_new_some_u64u64(uint64_t a, uint64_t b) {
    struct U8TaggedEnumOptionU64U64 r = {
        .tag = OPTION_TAG_SOME,
        .some = { .a = a, .b = b },
    };
    return r;
}

struct U8TaggedEnumOptionU64U64
dust_dbg_new_none_u64u64(void) {
    struct U8TaggedEnumOptionU64U64 r = {
        .tag = OPTION_TAG_NONE,
    };
    return r;
}

int32_t
dust_dbg_unpack_option_u64u64(struct U8TaggedEnumOptionU64U64 o, uint64_t *a, uint64_t *b) {
    assert(a);
    assert(b);
    switch (o.tag) {
    case OPTION_TAG_SOME:
        *a = o.some.a;
        *b = o.some.b;
        return 1;
    case OPTION_TAG_NONE:
        return 0;
    default:
        assert(0 && "unexpected tag");
        return 0;
    }
}
