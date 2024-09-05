//! Error types.

/// An error that occurs when translating Wasm to IR.
#[derive(Clone, Debug)]
pub enum FrontendError {
    /// The given WebAssembly feature is not supported.
    UnsupportedFeature(String),
    /// Some dimension of the WebAssembly module is too large to be
    /// supported by this library.
    TooLarge(String),
    /// An internal error occurred.
    Internal(String),
}

impl std::fmt::Display for FrontendError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl std::error::Error for FrontendError {}
