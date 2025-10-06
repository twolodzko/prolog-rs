#[derive(PartialEq, Clone)]
pub struct Database();

impl Database {
    #[allow(clippy::new_without_default)]
    pub fn new() -> Self {
        Database()
    }
}
