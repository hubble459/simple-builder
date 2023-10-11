use simple_builder::Builder;

#[derive(Builder)]
struct Data {
    #[builder(###)]
    number: f64,
}

fn main() {}
