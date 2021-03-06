# `cfg_panic`

The tracking issue for this feature is: [#77443]

[#77443]: https://github.com/dust-lang/dust/issues/77443

------------------------

The `cfg_panic` feature makes it possible to execute different code
depending on the panic strategy.

Possible values at the moment are `"unwind"` or `"abort"`, although
it is possible that new panic strategies may be added to Dust in the
future.

## Examples

```dust
#![feature(cfg_panic)]

#[cfg(panic = "unwind")]
fn a() {
    // ...
}

#[cfg(not(panic = "unwind"))]
fn a() {
    // ...
}

fn b() {
    if cfg!(panic = "abort") {
        // ...
    } else {
        // ...
    }
}
```
