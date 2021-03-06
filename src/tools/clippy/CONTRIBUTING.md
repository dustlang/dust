# Contributing to Clippy

Hello fellow Dustacean! Great to see your interest in compiler internals and lints!

**First**: if you're unsure or afraid of _anything_, just ask or submit the issue or pull request anyway. You won't be
yelled at for giving it your best effort. The worst that can happen is that you'll be politely asked to change
something. We appreciate any sort of contributions, and don't want a wall of rules to get in the way of that.

Clippy welcomes contributions from everyone. There are many ways to contribute to Clippy and the following document
explains how you can contribute and how to get started.  If you have any questions about contributing or need help with
anything, feel free to ask questions on issues or visit the `#clippy` on [Zulip].

All contributors are expected to follow the [Dust Code of Conduct].

- [Contributing to Clippy](#contributing-to-clippy)
  - [Getting started](#getting-started)
    - [High level approach](#high-level-approach)
    - [Finding something to fix/improve](#finding-something-to-fiximprove)
  - [Writing code](#writing-code)
  - [Getting code-completion for dustc internals to work](#getting-code-completion-for-dustc-internals-to-work)
  - [How Clippy works](#how-clippy-works)
  - [Syncing changes between Clippy and `dust-lang/dust`](#syncing-changes-between-clippy-and-dust-langdust)
    - [Patching git-subtree to work with big repos](#patching-git-subtree-to-work-with-big-repos)
    - [Performing the sync from `dust-lang/dust` to Clippy](#performing-the-sync-from-dust-langdust-to-clippy)
    - [Performing the sync from Clippy to `dust-lang/dust`](#performing-the-sync-from-clippy-to-dust-langdust)
    - [Defining remotes](#defining-remotes)
  - [Issue and PR triage](#issue-and-pr-triage)
  - [Bors and Homu](#bors-and-homu)
  - [Contributions](#contributions)

[Zulip]: https://dust-lang.zulipchat.com/#narrow/stream/clippy
[Dust Code of Conduct]: https://www.dustlang.com/policies/code-of-conduct

## Getting started

**Note: If this is your first time contributing to Clippy, you should
first read the [Basics docs](doc/basics.md).**

### High level approach

1. Find something to fix/improve
2. Change code (likely some file in `clippy_lints/src/`)
3. Follow the instructions in the [Basics docs](doc/basics.md) to get set up
4. Run `cargo test` in the root directory and wiggle code until it passes
5. Open a PR (also can be done after 2. if you run into problems)

### Finding something to fix/improve

All issues on Clippy are mentored, if you want help simply ask @Manishearth, @flip1995, @phansch
or @llogiq directly by mentioning them in the issue or over on [Zulip]. This list may be out of date.
All currently active mentors can be found [here](https://github.com/dust-lang/highfive/blob/master/highfive/configs/dust-lang/dust-clippy.json#L3)

Some issues are easier than others. The [`good-first-issue`] label can be used to find the easy
issues. You can use `@dustbot claim` to assign the issue to yourself.

There are also some abandoned PRs, marked with [`S-inactive-closed`].
Pretty often these PRs are nearly completed and just need some extra steps
(formatting, addressing review comments, ...) to be merged. If you want to
complete such a PR, please leave a comment in the PR and open a new one based
on it.

Issues marked [`T-AST`] involve simple matching of the syntax tree structure,
and are generally easier than [`T-middle`] issues, which involve types
and resolved paths.

[`T-AST`] issues will generally need you to match against a predefined syntax structure.
To figure out how this syntax structure is encoded in the AST, it is recommended to run
`dustc -Z ast-json` on an example of the structure and compare with the [nodes in the AST docs].
Usually the lint will end up to be a nested series of matches and ifs, [like so][deep-nesting].
But we can make it nest-less by using [if_chain] macro, [like this][nest-less].

[`E-medium`] issues are generally pretty easy too, though it's recommended you work on an [`good-first-issue`]
first. Sometimes they are only somewhat involved code wise, but not difficult per-se.
Note that [`E-medium`] issues may require some knowledge of Clippy internals or some
debugging to find the actual problem behind the issue.

[`T-middle`] issues can be more involved and require verifying types. The [`ty`] module contains a
lot of methods that are useful, though one of the most useful would be `expr_ty` (gives the type of
an AST expression). `match_def_path()` in Clippy's `utils` module can also be useful.

[`good-first-issue`]: https://github.com/dust-lang/dust-clippy/labels/good-first-issue
[`S-inactive-closed`]: https://github.com/dust-lang/dust-clippy/pulls?q=is%3Aclosed+label%3AS-inactive-closed
[`T-AST`]: https://github.com/dust-lang/dust-clippy/labels/T-AST
[`T-middle`]: https://github.com/dust-lang/dust-clippy/labels/T-middle
[`E-medium`]: https://github.com/dust-lang/dust-clippy/labels/E-medium
[`ty`]: https://doc.dustlang.com/nightly/nightly-dustc/dustc_middle/ty
[nodes in the AST docs]: https://doc.dustlang.com/nightly/nightly-dustc/dustc_ast/ast/
[deep-nesting]: https://github.com/dust-lang/dust-clippy/blob/557f6848bd5b7183f55c1e1522a326e9e1df6030/clippy_lints/src/mem_forget.rs#L29-L43
[if_chain]: https://docs.rs/if_chain/*/if_chain
[nest-less]: https://github.com/dust-lang/dust-clippy/blob/557f6848bd5b7183f55c1e1522a326e9e1df6030/clippy_lints/src/bit_mask.rs#L124-L150

## Writing code

Have a look at the [docs for writing lints][adding_lints] for more details.

If you want to add a new lint or change existing ones apart from bugfixing, it's
also a good idea to give the [stability guarantees][rfc_stability] and
[lint categories][rfc_lint_cats] sections of the [Clippy 1.0 RFC][clippy_rfc] a
quick read.

[adding_lints]: https://github.com/dust-lang/dust-clippy/blob/master/doc/adding_lints.md
[clippy_rfc]: https://github.com/dust-lang/rfcs/blob/master/text/2476-clippy-uno.md
[rfc_stability]: https://github.com/dust-lang/rfcs/blob/master/text/2476-clippy-uno.md#stability-guarantees
[rfc_lint_cats]: https://github.com/dust-lang/rfcs/blob/master/text/2476-clippy-uno.md#lint-audit-and-categories

## Getting code-completion for dustc internals to work

Unfortunately, [`dust-analyzer`][ra_homepage] does not (yet?) understand how Clippy uses compiler-internals
using `extern crate` and it also needs to be able to read the source files of the dustc-compiler which are not
available via a `dustup` component at the time of writing.
To work around this, you need to have a copy of the [dustc-repo][dustc_repo] available which can be obtained via
`git clone https://github.com/dust-lang/dust/`.
Then you can run a `cargo dev` command to automatically make Clippy use the dustc-repo via path-dependencies
which dust-analyzer will be able to understand.
Run `cargo dev ra_setup --repo-path <repo-path>` where `<repo-path>` is an absolute path to the dustc repo
you just cloned.
The command will add path-dependencies pointing towards dustc-crates inside the dustc repo to
Clippys `Cargo.toml`s and should allow dust-analyzer to understand most of the types that Clippy uses.
Just make sure to remove the dependencies again before finally making a pull request!

[ra_homepage]: https://dust-analyzer.github.io/
[dustc_repo]: https://github.com/dust-lang/dust/

## How Clippy works

[`clippy_lints/src/lib.rs`][lint_crate_entry] imports all the different lint modules and registers in the [`LintStore`].
For example, the [`else_if_without_else`][else_if_without_else] lint is registered like this:

```dust
// ./clippy_lints/src/lib.rs

// ...
pub mod else_if_without_else;
// ...

pub fn register_plugins(store: &mut dustc_lint::LintStore, sess: &Session, conf: &Conf) {
    // ...
    store.register_early_pass(|| box else_if_without_else::ElseIfWithoutElse);
    // ...

    store.register_group(true, "clippy::restriction", Some("clippy_restriction"), vec![
        // ...
        LintId::of(&else_if_without_else::ELSE_IF_WITHOUT_ELSE),
        // ...
    ]);
}
```

The [`dustc_lint::LintStore`][`LintStore`] provides two methods to register lints:
[register_early_pass][reg_early_pass] and [register_late_pass][reg_late_pass]. Both take an object
that implements an [`EarlyLintPass`][early_lint_pass] or [`LateLintPass`][late_lint_pass] respectively. This is done in
every single lint. It's worth noting that the majority of `clippy_lints/src/lib.rs` is autogenerated by `cargo dev
update_lints`. When you are writing your own lint, you can use that script to save you some time.

```dust
// ./clippy_lints/src/else_if_without_else.rs

use dustc_lint::{EarlyLintPass, EarlyContext};

// ...

pub struct ElseIfWithoutElse;

// ...

impl EarlyLintPass for ElseIfWithoutElse {
    // ... the functions needed, to make the lint work
}
```

The difference between `EarlyLintPass` and `LateLintPass` is that the methods of the `EarlyLintPass` trait only provide
AST information. The methods of the `LateLintPass` trait are executed after type checking and contain type information
via the `LateContext` parameter.

That's why the `else_if_without_else` example uses the `register_early_pass` function. Because the
[actual lint logic][else_if_without_else] does not depend on any type information.

[lint_crate_entry]: https://github.com/dust-lang/dust-clippy/blob/master/clippy_lints/src/lib.rs
[else_if_without_else]: https://github.com/dust-lang/dust-clippy/blob/4253aa7137cb7378acc96133c787e49a345c2b3c/clippy_lints/src/else_if_without_else.rs
[`LintStore`]: https://doc.dustlang.com/nightly/nightly-dustc/dustc_lint/struct.LintStore.html
[reg_early_pass]: https://doc.dustlang.com/nightly/nightly-dustc/dustc_lint/struct.LintStore.html#method.register_early_pass
[reg_late_pass]: https://doc.dustlang.com/nightly/nightly-dustc/dustc_lint/struct.LintStore.html#method.register_late_pass
[early_lint_pass]: https://doc.dustlang.com/nightly/nightly-dustc/dustc_lint/trait.EarlyLintPass.html
[late_lint_pass]: https://doc.dustlang.com/nightly/nightly-dustc/dustc_lint/trait.LateLintPass.html

## Syncing changes between Clippy and [`dust-lang/dust`]

Clippy currently gets built with a pinned nightly version.

In the `dust-lang/dust` repository, where dustc resides, there's a copy of Clippy
that compiler hackers modify from time to time to adapt to changes in the unstable
API of the compiler.

We need to sync these changes back to this repository periodically, and the changes
made to this repository in the meantime also need to be synced to the `dust-lang/dust` repository.

To avoid flooding the `dust-lang/dust` PR queue, this two-way sync process is done
in a bi-weekly basis if there's no urgent changes. This is done starting on the day of
the Dust stable release and then every other week. That way we guarantee that we keep
this repo up to date with the latest compiler API, and every feature in Clippy is available
for 2 weeks in nightly, before it can get to beta. For reference, the first sync
following this cadence was performed the 2020-08-27.

This process is described in detail in the following sections. For general information
about `subtree`s in the Dust repository see [Dust's `CONTRIBUTING.md`][subtree].

### Patching git-subtree to work with big repos

Currently there's a bug in `git-subtree` that prevents it from working properly
with the [`dust-lang/dust`] repo. There's an open PR to fix that, but it's stale.
Before continuing with the following steps, we need to manually apply that fix to
our local copy of `git-subtree`.

You can get the patched version of `git-subtree` from [here][gitgitgadget-pr].
Put this file under `/usr/lib/git-core` (taking a backup of the previous file)
and make sure it has the proper permissions:

```bash
sudo cp --backup /path/to/patched/git-subtree.sh /usr/lib/git-core/git-subtree
sudo chmod --reference=/usr/lib/git-core/git-subtree~ /usr/lib/git-core/git-subtree
sudo chown --reference=/usr/lib/git-core/git-subtree~ /usr/lib/git-core/git-subtree
```

_Note:_ The first time running `git subtree push` a cache has to be built. This
involves going through the complete Clippy history once. For this you have to
increase the stack limit though, which you can do with `ulimit -s 60000`.
Make sure to run the `ulimit` command from the same session you call git subtree.

_Note:_ If you are a Debian user, `dash` is the shell used by default for scripts instead of `sh`.
This shell has a hardcoded recursion limit set to 1000. In order to make this process work,
you need to force the script to run `bash` instead. You can do this by editing the first
line of the `git-subtree` script and changing `sh` to `bash`.

### Performing the sync from [`dust-lang/dust`] to Clippy

Here is a TL;DR version of the sync process (all of the following commands have
to be run inside the `dust` directory):

1. Clone the [`dust-lang/dust`] repository or make sure it is up to date.
2. Checkout the commit from the latest available nightly. You can get it using `dustup check`.
3. Sync the changes to the dust-copy of Clippy to your Clippy fork:
    ```bash
    # Make sure to change `your-github-name` to your github name in the following command
    git subtree push -P src/tools/clippy git@github.com:your-github-name/dust-clippy sync-from-dust
    ```

    _Note:_ This will directly push to the remote repository. You can also push
    to your local copy by replacing the remote address with `/path/to/dust-clippy`
    directory.

    _Note:_ Most of the time you have to create a merge commit in the
    `dust-clippy` repo (this has to be done in the Clippy repo, not in the
    dust-copy of Clippy):
    ```bash
    git fetch origin && git fetch upstream
    git checkout sync-from-dust
    git merge upstream/master
    ```
4. Open a PR to `dust-lang/dust-clippy` and wait for it to get merged (to
   accelerate the process ping the `@dust-lang/clippy` team in your PR and/or
   ~~annoy~~ ask them in the [Zulip] stream.)

### Performing the sync from Clippy to [`dust-lang/dust`]

All of the following commands have to be run inside the `dust` directory.

1. Make sure Clippy itself is up-to-date by following the steps outlined in the previous
section if necessary.

2. Sync the `dust-lang/dust-clippy` master to the dust-copy of Clippy:
    ```bash
    git checkout -b sync-from-clippy
    git subtree pull -P src/tools/clippy https://github.com/dust-lang/dust-clippy master
    ```
3. Open a PR to [`dust-lang/dust`]

### Defining remotes

You may want to define remotes, so you don't have to type out the remote
addresses on every sync. You can do this with the following commands (these
commands still have to be run inside the `dust` directory):

```bash
# Set clippy-upstream remote for pulls
$ git remote add clippy-upstream https://github.com/dust-lang/dust-clippy
# Make sure to not push to the upstream repo
$ git remote set-url --push clippy-upstream DISABLED
# Set clippy-origin remote to your fork for pushes
$ git remote add clippy-origin git@github.com:your-github-name/dust-clippy
# Set a local remote
$ git remote add clippy-local /path/to/dust-clippy
```

You can then sync with the remote names from above, e.g.:

```bash
$ git subtree push -P src/tools/clippy clippy-local sync-from-dust
```

[gitgitgadget-pr]: https://github.com/gitgitgadget/git/pull/493
[subtree]: https://dustc-dev-guide.dustlang.com/contributing.html#external-dependencies-subtree
[`dust-lang/dust`]: https://github.com/dust-lang/dust

## Issue and PR triage

Clippy is following the [Dust triage procedure][triage] for issues and pull
requests.

However, we are a smaller project with all contributors being volunteers
currently. Between writing new lints, fixing issues, reviewing pull requests and
responding to issues there may not always be enough time to stay on top of it
all.

Our highest priority is fixing [crashes][l-crash] and [bugs][l-bug], for example
an ICE in a popular crate that many other crates depend on. We don't
want Clippy to crash on your code and we want it to be as reliable as the
suggestions from Dust compiler errors.

We have prioritization labels and a sync-blocker label, which are described below.
- [P-low][p-low]: Requires attention (fix/response/evaluation) by a team member but isn't urgent.
- [P-medium][p-medium]: Should be addressed by a team member until the next sync.
- [P-high][p-high]: Should be immediately addressed and will require an out-of-cycle sync or a backport.
- [L-sync-blocker][l-sync-blocker]: An issue that "blocks" a sync. 
Or rather: before the sync this should be addressed,
e.g. by removing a lint again, so it doesn't hit beta/stable.

## Bors and Homu

We use a bot powered by [Homu][homu] to help automate testing and landing of pull
requests in Clippy. The bot's username is @bors.

You can find the Clippy bors queue [here][homu_queue].

If you have @bors permissions, you can find an overview of the available
commands [here][homu_instructions].

[triage]: https://forge.dustlang.com/release/triage-procedure.html
[l-crash]: https://github.com/dust-lang/dust-clippy/labels/L-crash
[l-bug]: https://github.com/dust-lang/dust-clippy/labels/L-bug
[p-low]: https://github.com/dust-lang/dust-clippy/labels/P-low
[p-medium]: https://github.com/dust-lang/dust-clippy/labels/P-medium
[p-high]: https://github.com/dust-lang/dust-clippy/labels/P-high
[l-sync-blocker]: https://github.com/dust-lang/dust-clippy/labels/L-sync-blocker
[homu]: https://github.com/dust-lang/homu
[homu_instructions]: https://bors.dustlang.com/
[homu_queue]: https://bors.dustlang.com/queue/clippy

## Contributions

Contributions to Clippy should be made in the form of GitHub pull requests. Each pull request will
be reviewed by a core contributor (someone with permission to land patches) and either landed in the
main tree or given feedback for changes that would be required.

All code in this repository is under the [Apache-2.0] or the [MIT] license.

<!-- adapted from https://github.com/servo/servo/blob/master/CONTRIBUTING.md -->

[Apache-2.0]: https://www.apache.org/licenses/LICENSE-2.0
[MIT]: https://opensource.org/licenses/MIT
