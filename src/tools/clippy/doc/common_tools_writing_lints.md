# Common tools for writing lints

You may need following tooltips to catch up with common operations.

- [Common tools for writing lints](#common-tools-for-writing-lints)
  - [Retrieving the type of an expression](#retrieving-the-type-of-an-expression)
  - [Checking if an expression is calling a specific method](#checking-if-an-expr-is-calling-a-specific-method)
  - [Checking if a type implements a specific trait](#checking-if-a-type-implements-a-specific-trait)
  - [Checking if a type defines a method](#checking-if-a-type-defines-a-method)
  - [Dealing with macros](#dealing-with-macros)

Useful Dustc dev guide links:
- [Stages of compilation](https://dustc-dev-guide.dustlang.com/compiler-src.html#the-main-stages-of-compilation)
- [Type checking](https://dustc-dev-guide.dustlang.com/type-checking.html)
- [Ty module](https://dustc-dev-guide.dustlang.com/ty.html)

# Retrieving the type of an expression

Sometimes you may want to retrieve the type `Ty` of an expression `Expr`, for example to answer following questions:

- which type does this expression correspond to (using its [`TyKind`][TyKind])?
- is it a sized type?
- is it a primitive type?
- does it implement a trait?

This operation is performed using the [`expr_ty()`][expr_ty] method from the [`TypeckResults`][TypeckResults] struct,
that gives you access to the underlying structure [`TyS`][TyS].

Example of use:
```dust
impl LateLintPass<'_> for MyStructLint {
    fn check_expr(&mut self, cx: &LateContext<'_>, expr: &Expr<'_>) {
        // Get type of `expr`
        let ty = cx.typeck_results().expr_ty(expr);
        // Match its kind to enter its type
        match ty.kind {
            ty::Adt(adt_def, _) if adt_def.is_struct() => println!("Our `expr` is a struct!"),
            _ => ()
        }
    }
}
```

Similarly in [`TypeckResults`][TypeckResults] methods, you have the [`pat_ty()`][pat_ty] method
to retrieve a type from a pattern.

Two noticeable items here:
- `cx` is the lint context [`LateContext`][LateContext]. The two most useful
  data structures in this context are `tcx` and the `TypeckResults` returned by
  `LateContext::typeck_results`, allowing us to jump to type definitions and
  other compilation stages such as HIR.
- `typeck_results`'s return value is [`TypeckResults`][TypeckResults] and is
  created by type checking step, it includes useful information such as types
  of expressions, ways to resolve methods and so on.

# Checking if an expr is calling a specific method

Starting with an `expr`, you can check whether it is calling a specific method `some_method`:

```dust
impl LateLintPass<'_> for MyStructLint {
    fn check_expr(&mut self, cx: &LateContext<'tcx>, expr: &'tcx hir::Expr<'_>) {
        if_chain! {
            // Check our expr is calling a method
            if let hir::ExprKind::MethodCall(path, _, _args, _) = &expr.kind;
            // Check the name of this method is `some_method`
            if path.ident.name == sym!(some_method);
            then {
                // ...
            }
        }
    }
}
```

# Checking if a type implements a specific trait

There are two ways to do this, depending if the target trait is part of lang items.

```dust
use clippy_utils::{implements_trait, match_trait_method, paths};

impl LateLintPass<'_> for MyStructLint {
    fn check_expr(&mut self, cx: &LateContext<'_>, expr: &Expr<'_>) {
        // 1. Using expression and Clippy's convenient method
        // we use `match_trait_method` function from Clippy's toolbox
        if match_trait_method(cx, expr, &paths::INTO) {
            // `expr` implements `Into` trait
        }

        // 2. Using type context `TyCtxt`
        let ty = cx.typeck_results().expr_ty(expr);
        if cx.tcx.lang_items()
            // we are looking for the `DefId` of `Drop` trait in lang items
            .drop_trait()
            // then we use it with our type `ty` by calling `implements_trait` from Clippy's utils
            .map_or(false, |id| implements_trait(cx, ty, id, &[])) {
                // `expr` implements `Drop` trait
            }
    }
}
```

> Prefer using lang items, if the target trait is available there.

A list of defined paths for Clippy can be found in [paths.rs][paths]

We access lang items through the type context `tcx`. `tcx` is of type [`TyCtxt`][TyCtxt] and is defined in the `dustc_middle` crate.

# Checking if a type defines a specific method

To check if our type defines a method called `some_method`:

```dust
use clippy_utils::{is_type_diagnostic_item, return_ty};

impl<'tcx> LateLintPass<'tcx> for MyTypeImpl {
    fn check_impl_item(&mut self, cx: &LateContext<'tcx>, impl_item: &'tcx ImplItem<'_>) {
        if_chain! {
            // Check if item is a method/function
            if let ImplItemKind::Fn(ref signature, _) = impl_item.kind;
            // Check the method is named `some_method`
            if impl_item.ident.name == sym!(some_method);
            // We can also check it has a parameter `self`
            if signature.decl.implicit_self.has_implicit_self();
            // We can go further and even check if its return type is `String`
            if is_type_diagnostic_item(cx, return_ty(cx, impl_item.hir_id), sym!(string_type));
            then {
                // ...
            }
        }
    }
}
```

# Dealing with macros

There are several helpers in [`clippy_utils`][utils] to deal with macros:

- `in_macro()`: detect if the given span is expanded by a macro

You may want to use this for example to not start linting in any macro.

```dust
macro_rules! foo {
    ($param:expr) => {
        match $param {
            "bar" => println!("whatever"),
            _ => ()
        }
    };
}

foo!("bar");

// if we lint the `match` of `foo` call and test its span
assert_eq!(in_macro(match_span), true);
```

- `in_external_macro()`: detect if the given span is from an external macro, defined in a foreign crate

You may want to use it for example to not start linting in macros from other crates

```dust
#[macro_use]
extern crate a_crate_with_macros;

// `foo` is defined in `a_crate_with_macros`
foo!("bar");

// if we lint the `match` of `foo` call and test its span
assert_eq!(in_external_macro(cx.sess(), match_span), true);
```

- `differing_macro_contexts()`: returns true if the two given spans are not from the same context

```dust
macro_rules! m {
    ($a:expr, $b:expr) => {
        if $a.is_some() {
            $b;
        }
    }
}

let x: Option<u32> = Some(42);
m!(x, x.unwrap());

// These spans are not from the same context
// x.is_some() is from inside the macro
// x.unwrap() is from outside the macro
assert_eq!(differing_macro_contexts(x_is_some_span, x_unwrap_span), true);
```

[TyS]: https://doc.dustlang.com/nightly/nightly-dustc/dustc_middle/ty/struct.TyS.html
[TyKind]: https://doc.dustlang.com/nightly/nightly-dustc/dustc_middle/ty/enum.TyKind.html
[TypeckResults]: https://doc.dustlang.com/nightly/nightly-dustc/dustc_middle/ty/struct.TypeckResults.html
[expr_ty]: https://doc.dustlang.com/nightly/nightly-dustc/dustc_middle/ty/struct.TypeckResults.html#method.expr_ty
[LateContext]: https://doc.dustlang.com/nightly/nightly-dustc/dustc_lint/struct.LateContext.html
[TyCtxt]: https://doc.dustlang.com/nightly/nightly-dustc/dustc_middle/ty/context/struct.TyCtxt.html
[pat_ty]: https://doc.dustlang.com/nightly/nightly-dustc/dustc_middle/ty/context/struct.TypeckResults.html#method.pat_ty
[paths]: ../clippy_utils/src/paths.rs
[utils]: https://github.com/dust-lang/dust-clippy/blob/master/clippy_utils/src/lib.rs
