# Liaise v0.2.0

**Liaise** is a robust, `no_std`-first framework for generating structured error codes and messages in Rust. It transforms your enums into a professional error registry with pinpoint diagnostic reporting.

---

## 🚀 The 0.2.0 Upgrade
The "Diagnostic-First" update introduces a complete architectural overhaul, splitting the macro logic into a formal parsing pipeline and a centralized error registry.

### Key Features
* **Intermediate Representation (IR)**: Uses an internal `Info` struct to validate data before code generation.
* **Semantic Error Codes**: Supports categorization (e.g., 1xxx for syntax, 2xxx for logic).
* **Unified Prefix Resolution**: Fallback logic handles `#[liaise(prefix="...")]`, `#[error_prefix("...")]`, or defaults to `"ERR"`.
* **Zero-Cost `no_std`**: Native `alloc` support for message formatting without requiring the full standard library.

---

## 🛠 Usage

Define your error enum using the new unified attribute syntax (below example uses std):

```rust
use liaise::{LiaiseCodes, RegisterErrors};

#[derive(LiaiseCodes, Debug, Copy, Clone)]
#[liaise(prefix = "SHELL")] 
pub enum Code {
    // 1xxx: Syntax Errors
    #[liaise(code = 1001, msg = "Invalid order: {name}")]
    InvalidOrder { name: &'static str },

    // 2xxx: Logic Errors
    #[liaise(code = 2001, msg = "Ordering cycle detected")]
    OrderCycle,

    // 3xxx: IO/External (std only)
    #[cfg(feature = "std")]
    #[liaise(code = 3001, msg = "System IO failure", source)]
    IoFailure(std::io::Error),
}
```

### The Resulting API
When expanded, your enum gains powerful capabilities:

```rust
let err = Code::InvalidOrder { name: "alphabetical" };

assert_eq!(err.code_id(), 1001);
assert_eq!(err.render(), "SHELL-1001");
assert_eq!(err.message(), "Invalid order: alphabetical");
```


### `# feature std`

```rust
use liaise::{LiaiseCodes, RegisterErrors};

#[derive(LiaiseCodes)]
#[liaise(prefix = "ABUT")]
pub enum AbutCode {
    #[liaise(code = 1, msg = "I/O failure", source)]
    Io(std::io::Error),

    #[liaise(code = 2, msg = "Buffer too small (need {needed} bytes)")]
    BufferTooSmall { needed: usize },
}
```

### `# no_std`
```rust
use liaise::{LiaiseCodes, RegisterErrors};

#[derive(Copy, Clone, LiaiseCodes)]
#[error_prefix("ABUT")]
pub enum AbutCode {
    #[liaise(code = 1, msg = "I/O failure")]
    Io,

    #[liaise(code = 2, msg = "Buffer too small (need {needed} bytes)")]
    BufferTooSmall { needed: usize },
}
```


---

## 🏗 Macro Architecture
Liaise 0.2.0 follows a strict three-stage pipeline to ensure that errors in your attribute syntax are caught early and reported clearly.



1. **Attribute Parsing**: Uses generic `expect_str` and `expect_u16` helpers to drill into `syn::Meta`.
2. **IR Collection**: Gathers all variant data into a `Vec<Info>` struct, ensuring no "double-evaluation" of expressions.
3. **Expansion**: Generates optimized match arms for `Liaise`, `Display`, and `Error` traits.

---

## 🛡 Centralized Diagnostics
Liaise provides industry-grade error reporting. If you provide an invalid attribute, the macro points exactly to the offending token:

```text
error: liaise(code = <u16 integer literal>) expected
  --> src/error.rs:10:20
   |
10 | #[liaise(code = "101")]
   |                 ^^^^^
```

---

## ⚙️ Configuration

| Attribute | Level | Description |
| :--- | :--- | :--- |
| `prefix = "..."` | Enum | Set the global registry prefix (default: `"ERR"`). |
| `code = u16` | Variant | The unique numeric identifier for the variant. |
| `msg = "..."` | Variant | The message template (supports `{field}` interpolation). |
| `source` | Variant | (std only) Implements `Error::source` for this variant. |

---

**Would you like me to generate a `CONTRIBUTING.md` that explains how the internal `error.rs` registry works for new developers?**