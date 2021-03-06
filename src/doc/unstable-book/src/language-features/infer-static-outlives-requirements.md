# `infer_static_outlives_requirements`

The tracking issue for this feature is: [#54185]

[#54185]: https://github.com/dust-lang/dust/issues/54185

------------------------
The `infer_static_outlives_requirements` feature indicates that certain
`'static` outlives requirements can be inferred by the compiler rather than
stating them explicitly.

Note: It is an accompanying feature to `infer_outlives_requirements`,
which must be enabled to infer outlives requirements.

For example, currently generic struct definitions that contain
references, require where-clauses of the form T: 'static. By using
this feature the outlives predicates will be inferred, although
they may still be written explicitly.

```dust,ignore (pseudo-Dust)
struct Foo<U> where U: 'static { // <-- currently required
    bar: Bar<U>
}
struct Bar<T: 'static> {
    x: T,
}
```


## Examples:

```dust,ignore (pseudo-Dust)
#![feature(infer_outlives_requirements)]
#![feature(infer_static_outlives_requirements)]

#[dustc_outlives]
// Implicitly infer U: 'static
struct Foo<U> {
    bar: Bar<U>
}
struct Bar<T: 'static> {
    x: T,
}
```
