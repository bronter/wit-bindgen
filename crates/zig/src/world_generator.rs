use std::collections::HashMap;

use heck::ToSnakeCase;
use wit_bindgen_core::{
    wit_parser::{TypeId, InterfaceId, SizeAlign, Resolve, Docs, WorldId, WorldKey},
    Source
};

use crate::{interface_generator::InterfaceGenerator, Opts};

#[derive(Default)]
pub struct TypeInfo {
    pub generated_names: HashMap<TypeId, String>,
    pub generated_field_names: HashMap<TypeId, Vec<String>>,

    // Track which types we've referenced but not resolved
    pub unresolved_type_names: HashMap<TypeId, String>,
}

#[derive(Default)]
pub struct WorldGenerator {
    pub opts: Opts,
    // TODO: Maybe in a separate PR, make it so that spaces
    // per indentation level is configurable in Source.
    // Zig's style guide recommends 4 space indentation,
    // whereas Source uses a hard-coded 2 space indentation.
    // (https://ziglang.org/documentation/master/#Whitespace)
    src: Source,
    interface_srcs: HashMap<InterfaceId, String>,
    world_name: String,
    pub type_info: TypeInfo,
    pub sizes: SizeAlign,

    // Zig's error unions aren't fully compatible with WIT's
    // result type (https://github.com/ziglang/zig/issues/2647).
    // So if we use a canonical result type we need to generate
    // our own implementation of it.
    pub needs_result_type: bool,
}

impl WorldGenerator {
    pub fn write_docs_str(dest: &mut String, docs: &Docs) {
        match docs.contents {
            Some(docstr) => {
                for line in docstr.lines() {
                    dest.push_str(&format!("// {}\n", line));
                }
            },
            None => {}
        };
    }
}

impl wit_bindgen_core::WorldGenerator for WorldGenerator {
    fn preprocess(&mut self, resolve: &Resolve, world: WorldId) {
        let name = &resolve.worlds[world].name;
        self.world_name = name.clone();
        self.sizes.fill(resolve);
    }

    fn import_interface(
        &mut self,
        resolve: &Resolve,
        name: &WorldKey,
        iface: InterfaceId,
        files: &mut wit_bindgen_core::Files,
    ) {
        let mut gen = InterfaceGenerator::new(self, resolve, iface, true);
        gen.generate();
        self.interface_srcs.insert(iface, gen.src);
        todo!("Looks like we need to write a function bindgen too before we have everything we need to do this.");
    }

    fn finish(&mut self, resolve: &Resolve, id: WorldId, files: &mut wit_bindgen_core::Files) {
        let world = &resolve.worlds[id];
        let snake = world.name.to_snake_case();

        if self.needs_result_type {
            // Generic error; WIT's Result type doesn't have named errors
            self.src.push_str("const WITErrors = error { WITError };\n");
            // In Zig you can make compile-time functions that can take in types and return new types.
            // In this case, we take the types for the ok and error payloads and use them to generate
            // a new tagged union type representing a Result.
            // This type is a bit different from WIT's canonical Result type since it has two different
            // error cases, one for WIT-style errors and one for Zig-style errors.
            // We do this so that if we have an adapter function for a function that returns a WIT
            // Result but the adapter also needs to do an operation like memory allocation that can
            // return a Zig error, we can coerce both types into one Result type without losing the
            // payload of the (WIT) result or losing the name and integer value of the (Zig) error.
            self.src.push_str(
                "fn WITResult(comptime OkPayload: type, comptime ErrPayload: type) type {
                    return union(enum) {
                        ok: OkPayload,
                        wit_error: ErrPayload,
                        zig_error: anyerror,

                        pub fn as_error_union(self: @This()) !OkPayload {
                            return switch (self) {
                                .ok => |p| p,
                                .wit_error => |_| WITErrors.WITError,
                                .zig_error => |ze| ze,
                            };
                        }
                    };
                }\n"
            );
        }

        todo!("interfaces as files or as structs within a file?");
        // files.push(&format!("{}.zig", snake), self.src.as_bytes());
    }
}