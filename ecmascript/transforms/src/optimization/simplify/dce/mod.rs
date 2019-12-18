pub fn dce() -> impl Pass + 'static {
    Remover::default()
}

#[derive(Debug, Default)]
struct Remover {}
