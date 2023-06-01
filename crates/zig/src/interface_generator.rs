use heck::{ToSnakeCase, ToUpperCamelCase, ToShoutySnakeCase};
use wit_bindgen_core::wit_parser::*;
use wit_component::StringEncoding;

use crate::world_generator::{TypeInfo, WorldGenerator};

pub struct InterfaceGenerator<'a> {
    pub world_gen: &'a mut WorldGenerator,
    pub src: String,

    // We want to be able to write adapters around imported functions
    // with the same name as the function. To facilitate that we isolate
    // the imported functions within a struct. Not the prettiest solution,
    // but I've found no other way to do it in Zig without mangling the
    // name of the adapter function, which makes the interface ugly to use.
    wasm_import_func_sigs: Vec<String>,

    type_info: TypeInfo,

    // Whether or not we are generating types for the world itself
    is_world: bool,

    string_type_name: Option<String>,

    pub resolve: &'a Resolve,
    id: InterfaceId,
    pub return_pointer_area_size: usize,
    pub return_pointer_area_align: usize,
}

trait FieldName {
    fn make_zig_ident(&self) -> String;
}

// TODO: Seems redundant to have the exact same code three times
// for make_zig_ident. Figure out a better way to do this.
impl FieldName for Field {
    fn make_zig_ident(&self) -> String {
        to_zig_ident(&self.name.to_snake_case())
    }
}

impl FieldName for Case {
    fn make_zig_ident(&self) -> String {
        to_zig_ident(&self.name.to_snake_case())
    }
}

impl FieldName for EnumCase {
    fn make_zig_ident(&self) -> String {
        to_zig_ident(&self.name.to_snake_case())
    }
}

impl<'a> InterfaceGenerator<'a> {
    pub fn new(
        world_gen: &mut WorldGenerator,
        resolve: &'a Resolve,
        iface: InterfaceId,
        is_world: bool,
    ) -> InterfaceGenerator<'a> {
        InterfaceGenerator {
            world_gen,
            src: String::new(),
            wasm_import_func_sigs: Vec::new(),
            type_info: Default::default(),
            is_world,
            string_type_name: Default::default(),
            resolve,
            id: iface,
            return_pointer_area_size: 0,
            return_pointer_area_align: 0,
        }
    }

    // Generate the source for this interface
    pub fn generate(&mut self) {
        wit_bindgen_core::InterfaceGenerator::types(self, self.id);

        todo!("When finished generating an interface, transfer unresolved types to the WorldGen's unresolved types");
    }

    // Generate the source for a function import
    fn import_func(&mut self, func: &Function) {
        WorldGenerator::write_docs_str(&mut self.src, &func.docs);
        todo!("finish this");
    }

    // Turn a type name into a valid, idiomatic Zig identifier
    fn make_identifier<'b>(self, name: &str) -> &'b str {
        &to_zig_ident(&name.to_upper_camel_case())
    }

    // Declare a type in the source
    fn type_decl(&mut self, id: &TypeId, name: &str, type_name: &str, docs: &Docs) {
        assert!(self.lookup_type_name(id).is_none());
        WorldGenerator::write_docs_str(&mut self.src, docs);
        let camel_name = self.make_identifier(name);
        self.src.push_str(&format!("pub const {camel_name} = {type_name};\n"));
        self.add_type_name(id, camel_name);
    }

    // Store the generated type name, cleaning up unresolved types as they are resolved.
    fn add_type_name(&mut self, id: &TypeId, name: &str) {
        if self.is_world {
            self.world_gen.type_info.generated_names.insert(*id, name.into());
            self.world_gen.type_info.unresolved_type_names.remove(id);
        } else {
            self.type_info.generated_names.insert(*id, name.into());
            self.type_info.unresolved_type_names.remove(id);
        }
    }

    // Lookup the type name, returns None if it hasn't been defined yet
    pub fn lookup_type_name(&self, id: &TypeId) -> Option<&String> {
        match self.type_info.generated_names.get(id) {
            Some(s) => Some(s),
            None => match self.type_info.unresolved_type_names.get(id) {
                Some(s) => Some(s),
                None => match self.world_gen.type_info.generated_names.get(id) {
                    Some(s) => Some(s),
                    None => self.world_gen.type_info.unresolved_type_names.get(id)
                },
            }
        }
    }

    // Lookup the type name, but if it isn't defined, generate a name
    // and add it to a map of undefined types, then return the generated
    // name.
    pub fn get_type_name(&mut self, id: &TypeId) -> Option<&String> {
        match self.lookup_type_name(id) {
            Some(s) => Some(s),
            None => {
                match self.resolve.types[*id].name {
                    Some(s) => {
                        let name = self.make_identifier(&s);
                        self.type_info.unresolved_type_names.insert(*id, name.into());
                        Some(&name.to_string())
                    },
                    None => None
                }
            }
        }
    }

    // Store generated fields for the type id so we can reference them later.
    fn add_type_fields(&mut self, id: &TypeId, fields: Vec<String>) {
        if self.is_world {
            self.world_gen.type_info.generated_field_names.insert(*id, fields);
        } else {
            self.type_info.generated_field_names.insert(*id, fields);
        }
    }

    // If we've defined a record or variant type we should have a map of its fields.
    fn lookup_type_fields(&self, id: &TypeId) -> Option<&Vec<String>> {
        match self.type_info.generated_field_names.get(id) {
            Some(field_names) => Some(field_names),
            None => self.world_gen.type_info.generated_field_names.get(id),
        }
    }

    pub fn get_type_fields(&mut self, id: &TypeId, fields: &Vec<impl FieldName>) -> &Vec<String> {
        match self.lookup_type_fields(id) {
            Some(names) => names,
            None => {
                let mut names = Vec::new();
                for field in fields.into_iter() {
                    names.push(field.make_zig_ident());
                }
                self.add_type_fields(id, names);
                &names
            },
        }
    }

    pub fn string_type_def<'b>(&mut self) -> &'b str {
        match self.string_type_name {
            Some(name) => &name,
            None => {
                // The canonical ABI stores a pointer and a length, so it seems
                // like it should be a slice.  We determine the width from the encoding.
                let str_type = match self.world_gen.opts.string_encoding {
                    StringEncoding::UTF8 => "[]u8",
                    StringEncoding::UTF16 => "[]u16",
        
                    // This one is weird because we have to figure out which one it is at
                    // run-time, I can either union them together like this, or mess around
                    // with type coersion or re-encoding. I think the union is probably the
                    // easiest option.
                    StringEncoding::CompactUTF16 => "union(enum) { latin1: []u8, utf16: []u16 }",
                };
                let type_name = "WITString";
                self.src.push_str(&format!("pub const {type_name} = {str_type};\n"));
                self.string_type_name = Some(type_name.to_string());
                type_name
            }
        }
    }

    fn record_type_def<'b>(&mut self, id: &TypeId, record: &Record) -> &'b str {
        // If the struct's fields all match their canonical ABI representations,
        // we can pack the struct so that arrays of them can be lifted/lowered without
        // iterating.
        let shouldPack = record.fields.iter()
            .all(|f| self.is_canonical(&f.ty));

        let mut res = if shouldPack {
            String::from("packed struct {")
        } else {
            String::from("struct {")
        };

        if record.fields.len() > 0 {
            res.push('\n');
        }
        let field_names = self.get_type_fields(id, &record.fields);
        for (field_name, field) in field_names.iter().zip(record.fields) {
            WorldGenerator::write_docs_str(&mut res, &field.docs);
            let type_name = self.type_def(&field.ty);
            res.push_str(&format!("{field_name}: {type_name},\n"));
        }
        res.push('}');
        &res
    }
    
    fn flag_name<'b>(&self, flag: &Flag) -> &'b str {
        &flag.name.to_shouty_snake_case()
    }
    // A struct with all the flag values
    fn flags_struct<'b>(&self, flags: &Flags) -> &'b str {
        let n = 32 * flags.repr().count();

        let mut ret = String::from(".{\n");

        for (i, flag) in flags.flags.iter().enumerate() {
            WorldGenerator::write_docs_str(&mut ret, &flag.docs);
            let name = self.flag_name(&flag);
            ret.push_str(&format!(".{name} = @as(u{n}, 0x1) << {i},\n"));
        }
        ret.push('}');

        &ret
    }

    fn flags_type_def<'b>(&self, flags: &Flags) -> &'b str {
        let n = 32 * flags.repr().count();
        assert!(n <= 65535);
        &format!("u{}", n)
    }

    // Tuples in Zig are just anonymous structs, sort of like in Rust but more explicit
    // e.g. "(u8, u8)" in Rust -> "struct{u8, u8}" in Zig
    fn tuple_type_def<'b>(&mut self, tuple: &Tuple) -> &'b str {
        // If the struct's fields all match their canonical ABI representations,
        // we can pack the struct so that arrays of them can be lifted/lowered without
        // iterating.
        let shouldPack = tuple.types.iter()
            .all(|ty| self.is_canonical(ty));

        let mut res = if shouldPack {
            String::from("packed struct {")
        } else {
            String::from("struct {")
        };

        for ty in tuple.types.iter() {
            res.push_str(self.type_def(ty));
            res.push(',');
        }
        res.push('}');

        &res
    }

    // Variants would be like tagged unions in Zig.
    fn variant_type_def<'b>(&mut self, id: &TypeId, variant: &Variant) -> &'b str {
        let mut res = String::from("union(enum) {");
        if variant.cases.len() > 0 {
            res.push('\n');
        }

        let case_names = self.get_type_fields(id, &variant.cases);
        for (case_name, case) in case_names.iter().zip(variant.cases) {
            WorldGenerator::write_docs_str(&mut res, &case.docs);
            let type_name = match case.ty {
                Some(ty) => self.type_def(&ty),

                // Technically you can leave out the void type in the union declaration
                // when there is no type, but for readability I'm leaving it on.
                None => "void",
            };
            res.push_str(&format!("{case_name}: {type_name},\n"));
        }
        res.push('}');
        &res
    }

    fn enum_type_def<'b>(&mut self, id: &TypeId, e: &Enum) -> &'b str {
        let tagType = match e.tag() {
            Int::U8 => "u8",
            Int::U16 => "u16",
            Int::U32 => "u32",
            Int::U64 => "u64",
        };
        let mut res = format!("enum({tagType}) {{");
        if e.cases.len() > 0 {
            res.push('\n');
        }

        let case_names = self.get_type_fields(id, &e.cases);
        for (i, (case_name, case)) in case_names.iter().zip(e.cases).enumerate() {
            WorldGenerator::write_docs_str(&mut res, &case.docs);
            res.push_str(&format!("{case_name} = {i},\n"));
        }
        res.push('}');
        &res
    }

    pub fn optional_type_def<'b>(&mut self, ty: &Type) -> &'b str {
        &format!("?{}", self.type_def(ty))
    }

    // Ideally this would emit an error union, but since Zig's error unions do not
    // support error payloads, we have to use a non-standard Result type unless
    // we can be okay with losing info.
    fn result_type_def<'b>(&mut self, result: &Result_) -> &'b str {
        self.world_gen.needs_result_type = true;
        let ok_type = match result.ok {
            Some(ty) => self.type_def(&ty),
            None => "void",
        };
        let error_type = match result.err {
            Some(ty) => self.type_def(&ty),
            None => "void",
        };

        // make_wit_result_type retuns a tagged union type with .ok and .err cases,
        // using the provided ok_type and error_type as the payload types.
        &format!("WITResult({ok_type}, {error_type})")
    }

    fn union_type_def<'b>(&mut self, union: &Union) -> &'b str {
        let tagType = match union.tag() {
            Int::U8 => "u8",
            Int::U16 => "u16",
            Int::U32 => "u32",
            Int::U64 => "u64",
        };
        let mut res = format!("union(enum({tagType})) {{");
        if union.cases.len() > 0 {
            res.push('\n');
        }
        for (i, case) in union.cases.iter().enumerate() {
            WorldGenerator::write_docs_str(&mut res, &case.docs);
            let type_name = self.type_def(&case.ty);

            // In the canonical ABI explainer, unions are despecialized
            // by creating a variant with fields named by the index of 
            // each case.  The C bindgen seems to do this by naming them
            // like f0, f1, etc. Zig has a special syntax for identifiers
            // that would otherwise break the rules; they can be represented
            // like @"0", @"1", etc.
            let name = format!("@\"{i}\"");
            res.push_str(&format!("{name}: {type_name},\n"));
        }
        res.push('}');
        &res
    }

    fn list_type_def<'b>(&mut self, ty: &Type) -> &'b str {
        let type_name = self.type_def(ty);
        // Apparently slices in Zig will keep track of their length at runtime?
        &format!("[]{type_name}")
    }

    pub fn type_def<'b>(&mut self, ty: &Type) -> &'b str {
        match ty {
            Type::Bool => "bool",

            // Zig doesn't exactly have a "char" type.
            // The closest thing would probably be a u21 which std.unicode uses for unicode codepoints
            // (Zig strings are basically C strings but utf-8 encoded by default).
            Type::Char => "u21",

            Type::U8 => "u8",
            Type::S8 => "i8",
            Type::U16 => "u16",
            Type::S16 => "i16",
            Type::U32 => "u32",
            Type::S32 => "i32",
            Type::U64 => "u64",
            Type::S64 => "i64",
            Type::Float32 => "f32",
            Type::Float64 => "f64",

            Type::String => self.string_type_def(),

            // I guess this is for non-primitive types other than string?
            Type::Id(id) => {
                match self.get_type_name(id) {
                    Some(n) => &n,
                    None => {
                        let ty = &self.resolve.types[*id];
                        match &ty.kind {
                            // Guessing this case is for type aliases?
                            TypeDefKind::Type(ty) => self.type_def(ty),
                            // Record here would be like an anonymous struct in Zig
                            TypeDefKind::Record(record) => self.record_type_def(id, record),
                            // Flags would probably be represented with unsigned integers in Zig;
                            // Since we need a single type, we'll put all the flags into a struct
                            TypeDefKind::Flags(flags) => self.flags_type_def(flags),
                            // Tuples are just anonymous structs in Zig, you use them all the time with std.debug.print
                            TypeDefKind::Tuple(tuple) => self.tuple_type_def(tuple),
                            // Variant here would be like a tagged union in Zig
                            TypeDefKind::Variant(variant) => self.variant_type_def(id, variant),
                            // Zig has normal untagged enums, so nothing really special here
                            TypeDefKind::Enum(e) => self.enum_type_def(id, e),
                            // Zig has optional types built-in to the syntax
                            TypeDefKind::Option(ty) => self.optional_type_def(ty),
                            // A Result would be sort of like an error union in Zig
                            // Like optional types, this also has its own special syntax
                            TypeDefKind::Result(result_) => self.result_type_def(result_),
                            TypeDefKind::Union(union) => self.union_type_def(union),
                            // If this is a list of anything but primitive types it is not likely to be canonical
                            TypeDefKind::List(ty) => self.list_type_def(ty),
                            TypeDefKind::Future(option) => unimplemented!("Waiting until implemented for C bindgen"),
                            TypeDefKind::Stream(stream) => unimplemented!("Waiting until implemented for C bindgen"),
                            TypeDefKind::Unknown => unreachable!(),
                        }
                    }
                }
            }
        }
    }

    // This was copied from all_bits_valid in the Resolve impl
    // Needed a slightly different one since flags and enums
    // are represented canonically in this bindgen.
    pub fn is_canonical(&self, ty: &Type) -> bool {
        match ty {
            Type::U8
            | Type::S8
            | Type::U16
            | Type::S16
            | Type::U32
            | Type::S32
            | Type::U64
            | Type::S64
            | Type::Float32
            | Type::Float64 => true,

            Type::Bool | Type::Char | Type::String => false,

            Type::Id(id) => match &self.resolve.types[*id].kind {
                TypeDefKind::Enum(_) => true,
                TypeDefKind::List(_)
                | TypeDefKind::Variant(_)
                | TypeDefKind::Option(_)
                | TypeDefKind::Result(_)
                | TypeDefKind::Future(_)
                | TypeDefKind::Stream(_)
                | TypeDefKind::Union(_) => false,
                TypeDefKind::Type(t) => self.is_canonical(t),
                TypeDefKind::Record(r) => r.fields.iter().all(|f| self.is_canonical(&f.ty)),
                TypeDefKind::Tuple(t) => t.types.iter().all(|t| self.is_canonical(t)),

                // For this implementation, flags should always match their canonical representation in memory.
                TypeDefKind::Flags(_) => true,

                TypeDefKind::Unknown => unreachable!(),
            },
        }
    }
}

impl<'a> wit_bindgen_core::InterfaceGenerator<'a> for InterfaceGenerator<'a> {
    fn resolve(&self) -> &'a Resolve {
        self.resolve
    }

    fn type_record(&mut self, id: TypeId, name: &str, record: &Record, docs: &Docs) {
        let record_type_def = self.record_type_def(&id, record);
        self.type_decl(&id, name, record_type_def, docs);
    }

    fn type_tuple(&mut self, id: TypeId, name: &str, tuple: &Tuple, docs: &Docs) {
        let tuple_type_def = self.tuple_type_def(tuple);
        self.type_decl(&id, name, tuple_type_def, docs);
    }

    fn type_flags(&mut self, id: TypeId, name: &str, flags: &Flags, docs: &Docs) {
        let flags_type_def = self.flags_struct(flags);
        self.type_decl(&id, name, flags_type_def, docs);
    }

    fn type_variant(&mut self, id: TypeId, name: &str, variant: &Variant, docs: &Docs) {
        let variant_type_def = self.variant_type_def(&id, variant);
        self.type_decl(&id, name, variant_type_def, docs);
    }

    fn type_union(&mut self, id: TypeId, name: &str, union: &Union, docs: &Docs) {
        let union_type_def = self.union_type_def(union);
        self.type_decl(&id, name, union_type_def, docs);
    }

    fn type_option(&mut self, id: TypeId, name: &str, payload: &Type, docs: &Docs) {
        let option_type_def = self.optional_type_def(payload);
        self.type_decl(&id, name, option_type_def, docs);
    }

    fn type_result(&mut self, id: TypeId, name: &str, result: &Result_, docs: &Docs) {
        let result_type_def = self.result_type_def(result);
        self.type_decl(&id, name, result_type_def, docs);
    }

    fn type_enum(&mut self, id: TypeId, name: &str, enum_: &Enum, docs: &Docs) {
        let enum_type_def = self.enum_type_def(&id, enum_);
        self.type_decl(&id, name, enum_type_def, docs);
    }

    fn type_alias(&mut self, id: TypeId, dest_type_def: &str, sourceType: &Type, docs: &Docs) {
        let src_type_def = self.type_def(sourceType);
        self.type_decl(&id, dest_type_def, src_type_def, docs);
    }

    fn type_list(&mut self, id: TypeId, name: &str, ty: &Type, docs: &Docs) {
        let list_type_def = self.list_type_def(ty);
        self.type_decl(&id, name, list_type_def, docs);
    }

    fn type_builtin(&mut self, id: TypeId, name: &str, ty: &Type, docs: &Docs) {
        // Not sure what this function is for exactly, the go impl has a todo!() here
        // and the C impl just drops all the args, so I'd assume TBD?
        unimplemented!()
    }
}

// TODO: Copy/pasting to a list was the easy thing to do, but this should probably
// use a match statement like the C bindgen does. That way I can group these,
// and make the issue of efficiently searching them be the compiler's problem.
const RESERVED: &[&str] = &[
    "addrspace",
    "align",
    "allocator", // The functions we generate might use an allocator called "allocator", not a Zig keyword
    "allowzero",
    "and",
    "anyerror",
    "anyframe",
    "anyopaque",
    "anytype",
    "asm",
    "async",
    "await",
    "bool",
    "break",
    "c_int",
    "c_long",
    "c_longdouble",
    "c_longlong",
    "c_short",
    "c_uint",
    "c_ulong",
    "c_ulonglong",
    "c_ushort",
    "callconv",
    "catch",
    "comptime_float",
    "comptime_int",
    "comptime",
    "const",
    "continue",
    "defer",
    "else",
    "enum",
    "errdefer",
    "error",
    "export",
    "extern",
    "f128",
    "f16",
    "f32",
    "f64",
    "f80",
    "false",
    "fn",
    "for",
    "i128",
    "i16",
    "i32",
    "i64",
    "i8",
    "if",
    "inline",
    "isize",
    "linksection",
    "noalias",
    "noinline",
    "noreturn",
    "nosuspend",
    "null",
    "opaque",
    "or",
    "orelse",
    "packed",
    "pub",
    "resume",
    "return",
    "struct",
    "suspend",
    "switch",
    "test",
    "threadlocal",
    "true",
    "try",
    "type",
    "u128",
    "u16",
    "u32",
    "u64",
    "u8",
    "undefined",
    "union",
    "unreachable",
    "usingnamespace",
    "usize",
    "var",
    "void",
    "volatile",
    "while",
    "WITErrors", // For when we get an error back from an extern func, not a Zig keyword
    "WITResult", // Custom result type constructor, not a Zig keyword
    "WITString", // Lifted string type. not a Zig keyword
];

// Zig supports arbitrary bit-width integers, up to 65535 bits.
// However, it can still get confused even if the value exceeds 65535.
// So we should check for any string starting with 'i' or 'u',
// followed by one or more digits.
fn is_zig_int_type(name: &str) -> bool {
    if name.starts_with(&['u', 'i'])  {
        let mut chars = name.chars();
        chars.next();
        while let Some(c) = chars.next() {
            if !c.is_numeric() {
                return false;
            }
        }
    } else {
        return false;
    }

    return true;
}

pub fn to_zig_ident(name: &str) -> String {
    match RESERVED.binary_search(&name) {
        Ok(_) => format!("@\"{}\"", name),
        Err(_) => {
            if is_zig_int_type(name) {
                format!("@\"{}\"", name)
            } else {
                name.into()
            }
        },
    }
}
