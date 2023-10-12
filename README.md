# simple-builder
A simple implementation of the builder pattern for arbitrary Rust structs.

![badge](https://github.com/Brendan-Blanchard/simple-builder/actions/workflows/main.yml/badge.svg) [![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)

This package could easily be called "yet-another-builder" (YAB for short), but it fills a niche for those looking less
for features in a builder implementation, and more for ease of use. __In particular this implementation is for you if 
you have structs with several required fields and many optional ones.__ Examples of this might be query parameters for 
endpoints with required fields like a nonce and id, then many optional filtering parameters like start and end dates, 
price ranges, product types, etc. This macro was purpose built for this very use case.

# Example: Simple Use Case
```rust
use simple_builder_macro::Builder;

// Debug, PartialEq, Eq are only for assertions
#[derive(Debug, PartialEq, Eq, Builder)]
struct Breakfast {
    #[builder(required)]
    pub coffee: i64, // coffee is required, and therefore not Option<T>
    pub toast: Option<i64>,
    pub eggs: Option<i64>,
    pub bacon: Option<i64>,
}

pub fn main() {
    let desired_breakfast = Breakfast {
        coffee: 1,
        toast: None,
        eggs: Some(3),
        bacon: Some(2),
    };
    
    // semantically equivalent to `Breakfast::builder(16)`
    let mut builder = BreakfastBuilder::new(16);
    let breakfast = builder.eggs(3).bacon(2).build();
    
    assert_eq!(desired_breakfast, breakfast);
}
```
