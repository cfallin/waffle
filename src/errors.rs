//! Error types.

#[derive(Clone, Debug)]
pub enum FrontendError {
    UnsupportedFeature(String),
    Internal(String),
}

impl std::fmt::Display for FrontendError {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        std::fmt::Debug::fmt(self, f)
    }
}

impl std::error::Error for FrontendError {}
