//! Simple-builder is a simple-as-possible implementation of the builder pattern for arbitrary Rust structs.
//! It seeks to provide an easy to use interface for common use cases, such as web request with
//! many optional parameters.
//!
//! ## Implementation
//! The derived builders offer compile-time guarantees that no incorrect instance will be created
//! without super-linear compile times since required parameters go directly in the constructor.
//!
//! There are two downsides to this:
//! - The derived builders have lengthy constructors for structs with many required parameters.
//! This is arguably no longer a builder pattern, and as such isn't suitable for structs with many
//! required parameters.
//! - Calling `build` twice will result in a panic, since it owns and consumes the passed in data.
//!
//! Other implementations may suit your needs better if you have many _required_ parameters,
//! need to clone your builders, or have strict requirements for compile-time type checking of
//! required parameters and intermediate states.
pub use simple_builder_macro::Builder;

#[derive(Builder, Debug, PartialEq)]
struct TestItemOptionals {
    a: Option<String>,
    b: Option<bool>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[derive(Builder, Debug, PartialEq)]
    struct TestItem {
        #[builder(required)]
        a: i32,
        #[builder(required)]
        b: i32,
    }

    #[derive(Builder, Debug, PartialEq)]
    struct TestItemOptionals {
        a: Option<String>,
        b: Option<bool>,
    }

    #[derive(Builder, Debug, PartialEq)]
    struct TestItemMixedRequiredOptionals {
        a: Option<String>,
        b: Option<bool>,
        #[builder(required)]
        c: i32,
    }

    #[derive(Builder, Debug, PartialEq)]
    struct TestItemMixedRequiredOptionalReferences<'t> {
        a: Option<&'t str>,
        #[builder(required)]
        b: &'t str,
    }

    #[test]
    fn test_developer_experience() {
        let t = trybuild::TestCases::new();
        t.compile_fail("tests/ui/*.rs");
    }

    #[test]
    fn test_builder_method_on_original_struct() {
        let expected_item = TestItem { a: 0, b: 1 };

        let mut builder = TestItem::builder(0, 1);

        let test_item = builder.build();

        assert_eq!(expected_item, test_item);
    }

    #[test]
    #[should_panic(
        expected = "Option must be Some(T) for required fields. Builder may have already been consumed by calling `build`"
    )]
    fn test_builder_panic_on_second_build_call() {
        let mut builder = TestItem::builder(0, 1);

        let _first_build = builder.build();
        let _second_build = builder.build();
    }

    #[test]
    fn test_empty_builder_none_required() {
        let expected_item = TestItem { a: 0, b: 1 };

        let mut builder = TestItemBuilder::new(0, 1);

        let test_item = builder.build();

        assert_eq!(expected_item, test_item);
    }

    #[test]
    fn test_builder_with_required() {
        let expected_item = TestItemOptionals { a: None, b: None };

        let mut builder = TestItemOptionals::builder();

        let test_item = builder.build();

        assert_eq!(expected_item, test_item);
    }

    #[test]
    fn test_builder_optional_all_present() {
        let expected_item = TestItemOptionals {
            a: Some("string".to_string()),
            b: Some(false),
        };

        let mut builder = TestItemOptionalsBuilder::new();

        let item = builder.a("string".to_string()).b(false).build();

        assert_eq!(expected_item, item);
    }

    #[test]
    fn test_builder_optional_with_none() {
        let expected_item = TestItemOptionals {
            a: None,
            b: Some(false),
        };

        let mut builder = TestItemOptionalsBuilder::new();

        let item = builder.b(false).build();

        assert_eq!(expected_item, item);
    }

    #[test]
    fn test_builder_mixed_required_and_optional() {
        let expected_item = TestItemMixedRequiredOptionals {
            a: Some("string".to_string()),
            b: None,
            c: 42,
        };

        let mut builder = TestItemMixedRequiredOptionalsBuilder::new(42);

        let item = builder.a("string".to_string()).build();

        assert_eq!(expected_item, item);
    }

    #[test]
    fn test_builder_mixed_required_and_optional_references() {
        let expected_item = TestItemMixedRequiredOptionalReferences {
            a: Some("a"),
            b: "string",
        };

        let mut builder = TestItemMixedRequiredOptionalReferencesBuilder::new("string");

        let item = builder.a("a").build();

        assert_eq!(expected_item, item);
    }

    #[test]
    fn test_builder_ownership() {
        #[derive(Debug, PartialEq, Eq)]
        struct Item {
            field: i32,
        }

        #[derive(Builder, Debug, Eq, PartialEq)]
        struct TestItem {
            a: Option<Item>,
            #[builder(required)]
            b: Item,
        }

        let a = Item { field: 0 };
        let a1 = Item { field: 0 };

        let b = Item { field: 1 };
        let b1 = Item { field: 1 };

        let mut builder = TestItem::builder(b);

        let item = builder.a(a).build();

        let expected_item = TestItem { a: Some(a1), b: b1 };

        assert_eq!(expected_item, item);
    }

    #[test]
    fn test_builder_with_required_trait_and_lifetimes() {
        trait Marker {}

        #[derive(Debug, Eq, PartialEq, PartialOrd)]
        struct GenericT {
            num: i64,
        }

        impl Marker for GenericT {}

        #[derive(Builder, Debug, Eq, PartialEq)]
        struct TestItemGenericAddress<'t, 'u, T, U>
        where
            T: Marker,
            U: Marker,
        {
            a: Option<&'t T>,
            #[builder(required)]
            b: &'u U,
        }

        let test_struct_t = GenericT { num: 42 };
        let test_struct_u = GenericT { num: 1 };

        let expected_item = TestItemGenericAddress {
            a: Some(&test_struct_t),
            b: &test_struct_u,
        };

        let mut builder = TestItemGenericAddress::builder(&test_struct_u);

        let item = builder.a(&test_struct_t).build();

        assert_eq!(expected_item, item);
    }
}
