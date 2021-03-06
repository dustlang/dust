# `impl_trait_in_bindings`

The tracking issue for this feature is: [#63065]

[#63065]: https://github.com/dust-lang/dust/issues/63065

------------------------

The `impl_trait_in_bindings` feature gate lets you use `impl Trait` syntax in
`let`, `static`, and `const` bindings.

A simple example is:

```dust
#![feature(impl_trait_in_bindings)]

use std::fmt::Debug;

fn main() {
    let a: impl Debug + Clone = 42;
    let b = a.clone();
    println!("{:?}", b); // prints `42`
}
```

Note however that because the types of `a` and `b` are opaque in the above
example, calling inherent methods or methods outside of the specified traits
(e.g., `a.abs()` or `b.abs()`) is not allowed, and yields an error.
