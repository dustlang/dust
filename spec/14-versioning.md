# 14. Versioning & Evolution

## 14.1 Overview

This section defines the **versioning model** for the Dust Programming Language (DPL).

DPL versioning is designed to:
- preserve semantic stability,
- enable careful evolution,
- prevent silent behavior changes.

Versioning rules apply to:
- the language specification,
- the compiler and toolchain,
- reference examples and DIR.

---

## 14.2 Language Versions

A DPL version is identified by a **major.minor** number:

- **Major version** changes (e.g., v1.0) MAY introduce breaking changes.
- **Minor version** changes (e.g., v0.2) MUST be backward compatible with the previous minor version.

DPL v0.1 is the initial, foundational version.

---

## 14.3 Frozen Versions

A version is considered **frozen** when:

- its specification is complete,
- no semantic changes are permitted,
- only clarifications or editorial fixes may be made.

DPL v0.1 is **frozen**.

Implementations claiming v0.1 conformance MUST implement exactly the semantics defined in this specification.

---

## 14.4 Backward Compatibility

For minor version increments:

- all v0.1 programs MUST remain valid in v0.2 unless explicitly deprecated,
- new features MUST be additive,
- existing semantics MUST NOT be altered.

If backward compatibility cannot be preserved, a major version increment is required.

---

## 14.5 Forward Compatibility

A DPL v0.1 implementation MUST:

- reject programs that require features introduced in later versions,
- NOT attempt to guess or infer future semantics.

Forward compatibility is explicit, not implicit.

---

## 14.6 Deprecation Policy

Deprecation, if introduced in future versions, MUST:

- be explicitly documented,
- provide a clear migration path,
- remain supported for at least one minor version.

DPL v0.1 introduces no deprecations.

---

## 14.7 Version Annotation

Source files MAY declare an expected language version in future versions of DPL.

In v0.1, version annotations are not supported and MUST NOT appear in source files.

---

## 14.8 Evolution of DIR

DIR evolution follows the same versioning rules as the language:

- DIR emitted for v0.1 MUST be stable,
- changes to DIR semantics require a minor or major version increment,
- tools MUST treat DIR versioning explicitly.

---

## 14.9 Non-Goals of v0.1

DPL v0.1 does not define:

- experimental feature flags,
- unstable language modes,
- rolling or preview releases.

All evolution is explicit and versioned.

---

End of Section 14.