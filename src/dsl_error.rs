#[derive(Debug)]
pub struct DSLError(pub String);

impl std::fmt::Display for DSLError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for DSLError {}
