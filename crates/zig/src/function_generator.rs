use std::mem;

use wit_bindgen_core::{
    Ns,
    wit_parser::{
        abi::{WasmType, Bindgen, Instruction, Bitcast, WasmSignature},
        SizeAlign,
        Resolve,
        Type, Function, TypeDefKind, Results,
    }
};
use wit_component::StringEncoding;

use crate::interface_generator::{InterfaceGenerator, to_zig_ident};

// In Zig, allocators have different functions
// for handling a single item vs an array.
// For single items you have create and destroy.
// For arrays you have a bunch of different
// functions but primarily alloc, realloc, and
// free.
enum Allocation {
    Item(String),
    Array(String),
}

#[derive(Default)]
struct BlockParams {
    payload: Option<String>,
    element: Option<String>,
    base: Option<String>
}

pub struct FunctionGenerator<'a, 'b> {
    // Basically doing the same thing "cleanup" is doing
    // in the Java bindgen.
    // At the beginning of the function body, we should
    // declare var's for each allocation, and at the end
    // we should free each var.
    allocations: Vec<Allocation>,
    
    block_storage: Vec<(String, BlockParams)>,
    block_params: BlockParams,
    blocks: Vec<(String, BlockParams)>,
    locals: Ns,
    
    // If something in the function (like memory allocation)
    // can throw an error, make sure we propagate it in the
    // return type.
    can_error: bool,
    // Whether or not we need to convert a Zig error to a Result
    // before returning it
    should_return_result: bool,

    is_import: bool,

    func: &'b Function,
    interface_gen: &'b mut InterfaceGenerator<'a>,
    src: String,

    import_return_pointer_area_align: usize,
    import_return_pointer_area_size: usize,
}

impl<'a, 'b> FunctionGenerator<'a, 'b> {
    fn new(
        func: &'b Function,
        interface_gen: &'b mut InterfaceGenerator<'a>,
        is_import: bool,
    ) -> Self {
        Self {
            allocations: Vec::new(),
            block_storage: Vec::new(),
            block_params: Default::default(),
            blocks: Vec::new(),
            locals: Default::default(),
            can_error: Default::default(),
            should_return_result: FunctionGenerator::is_return_type_result(interface_gen, func),
            is_import,
            func,
            interface_gen,
            src: String::new(),
            import_return_pointer_area_align: 0,
            import_return_pointer_area_size: 0,
        }
    }

    fn is_return_type_result(ig: &'b InterfaceGenerator<'a>, func: &Function) -> bool {
        match func.results {
            Results::Anon(Type::Id(id)) => match ig.resolve.types[id].kind {
                TypeDefKind::Result(_) => true,
                _ => false,
            },
            _ => false,
        }
    }

    // Basically just adds "const result_name = " to the src, and
    // pushes either the result or the fields of the result to
    // the results vector.
    // Useful for unpacking the result of variants
    fn const_results(
        &mut self,
        result_name: &str,
        result_types: &&[WasmType],
        results: &mut Vec<<FunctionGenerator<'_, '_> as Bindgen>::Operand>
    ) {
        let n = result_types.len();
        if n > 0 {
            let res = self.locals.tmp(result_name);
            self.src.push_str(&format!("const {res} = "));

            if n > 1 {
                for i in 0..n {
                    results.push(format!("{res}.@\"{i}\""));
                }
            } else {
                results.push(res);
            }
        }
    }

    // TODO: Does this get called when generating function exports?
    // If so I'd guess our only option is to panic on error since
    // we can't mess with the return type of exported functions.
    fn wrap_error(&self, op: &str) -> &'b str {
        self.can_error = true;
        if self.should_return_result {
            &format!(
                "{op} catch |err| {{
                    return .{{ .zig_error = err }};
                }}"
            )
        } else {
            &format!("try {op}")
        }
    }

    // No interpolation making new strings so I think 'static is safe here?
    fn wasm_to_zig_type(ty: WasmType) -> &'static str {
        match ty {
            WasmType::F32 => "f32",
            WasmType::F64 => "f64",
            WasmType::I32 => "i32",
            WasmType::I64 => "i64",
        }
    }

    fn extern_sig(&mut self, module_name: &str, raw_function_name: &str, sig: &WasmSignature) -> String {
        // TODO: It doesn't look like Zig has a way do deal with multi-value return on its own, however
        // it *might* be possible to do this with inline assembly. Documentation on how inline asm works
        // with webassembly is almost non-existant, but it looks like it uses a wasm-specific version of
        // the GNU Assembler syntax; see:
        // https://www.rowleydownload.co.uk/arm/documentation/gnu/as/WebAssembly_002dDependent.html#WebAssembly_002dDependent
        // https://github.com/ziglang/zig/issues/13129
        // https://github.com/ziglang/zig/issues/13165
        //
        // So I see two possible ways of doing it with inline assembly:
        //
        // The Hard Way - Use module-level assembly to define the extern as well as an adapter function,
        // which calls the extern and coerces its multi-value return value into a packed struct (or maybe use
        // a return pointer). Zig would then be able to call the adapter function directly.
        //
        // The Harder Way - Use module-level assembly to define the extern and use regular inline assembly in
        // each place the function is called to unpack the multi-value return value into the caller's local variables.
        // Probably the more efficient way to do it.
        //
        // Of course, host modules will have to do something else entirely, but that's probably wasmtime's problem.
        //
        // In the long run, I think I'm gonna need to have a chat with the Zig compiler devs and see if there's
        // a way Zig can support this using a builtin or something. I'm thinking maybe something like:
        // extern fn divmod(i32, i32) @wasmMultiReturn(i32, i32);
        // When you call it, you get back a special multi-value return type, but it behaves the same way as a tuple
        // so you can unpack it with something like divmod_result.@"0", divmod_result.@"1", etc.
        // For exports we could either define another builtin for constructing values, or have anonymous structs
        // coerce into multi-value returns, so maybe either
        // export fn divmod(a: i32, b: i32) @wasmMultiReturn(i32, i32) {
        //     return @wasmMultiReturnValue(a / b, a % b);
        // }
        // or
        // export fn divmod(a: i32, b: i32) @wasmMultiReturn(i32, i32) {
        //     return .{ a / b, a % b };
        // }
        let return_sig = match sig.results.len() {
            0 => "void",
            1 => Self::wasm_to_zig_type(sig.results[0]),

            _ => unimplemented!("Multi-value return currently not supported."),
        };

        let function_name = to_zig_ident(raw_function_name);
        format!("extern \"{module_name}\" fn {function_name}() {return_sig};\n")
    }
}

impl Bindgen for FunctionGenerator<'_, '_> {
    type Operand = String;

    fn sizes(&self) -> &SizeAlign {
        return &self.interface_gen.world_gen.sizes;
    }

    fn push_block(&mut self) {
        let prev_src = mem::take(&mut self.src);
        self.block_storage.push((prev_src, self.block_params));
    }

    fn finish_block(&mut self, operands: &mut Vec<String>) {
        let (prev_src, prev_block_params) = self.block_storage.pop().unwrap();
        let src = mem::replace(&mut self.src, prev_src);
        let block_params = mem::replace(&mut self.block_params, prev_block_params);
        
        let expr = match operands.len() {
            0 => "{}".to_string(),
            1 => operands[0].clone(),
            _ => format!(".{{ {}}}", operands.join(", ")),
        };
        if src.is_empty() {
            self.blocks.push((expr, block_params));
        } else if operands.is_empty() {
            self.blocks.push((format!("{{\n{}\n}}", &src[..]), block_params));
        } else {
            let block_name = self.locals.tmp("blk");
            self.blocks.push((format!("{block_name}: {{\n{}\nbreak {block_name} {expr};\n}}", &src[..]), block_params));
        }
    }

    fn return_pointer(&mut self, size: usize, align: usize) -> String {
        let ptr = self.locals.tmp("ptr");

        // Use a stack-based return area for imports, because exports need
        // their return area to be live until the post-return call.
        // TODO: ret_area and RET_AREA declarations
        if self.is_import {
            self.import_return_pointer_area_size = self.import_return_pointer_area_size.max(size);
            self.import_return_pointer_area_align =
                self.import_return_pointer_area_align.max(align);
            // TODO: Test this cast with a simple example to be sure it doesn't error at compile-time
            // May need to use @truncate/@bitCast instead
            self.src.push_str(&format!("const {ptr}: i32 = @intCast(i32, @ptrToInt(ret_area));"));
        } else {
            self.interface_gen.return_pointer_area_size = self.interface_gen.return_pointer_area_size.max(size);
            self.interface_gen.return_pointer_area_align =
                self.interface_gen.return_pointer_area_align.max(align);
            self.src.push_str(&format!("const {ptr}: i32 = @intCast(i32, @ptrToInt(RET_AREA));"));
        }

        ptr
    }

    fn is_list_canonical(&self, resolve: &Resolve, ty: &Type) -> bool {
        self.interface_gen.is_canonical(ty)
    }

    fn emit(
        &mut self,
        resolve: &Resolve,
        inst: &Instruction<'_>,
        operands: &mut Vec<Self::Operand>,
        results: &mut Vec<Self::Operand>,
    ) {
        let mut int_cast = |cvt: &str| {
            // @intCast asserts that the integer will fit into the destination type, as the docs state:
            // "Attempting to convert a number which is out of range of the destination type results
            // in safety-protected Undefined Behavior." (https://ziglang.org/documentation/master/#intCast)
            results.push(format!("@intCast({}, {})", cvt, operands[0]));
        };

        match inst {
            Instruction::GetArg { nth } => {
                let (name, _) = self.func.params[*nth];
                results.push(name.clone());
            },
            Instruction::I32Const { val } => results.push(val.to_string()),
            Instruction::ConstZero { tys } => {
                for ty in tys.iter() {
                    match ty {
                        WasmType::I32 => results.push("@as(i32, 0)".to_string()),
                        WasmType::I64 => results.push("@as(i64, 0)".to_string()),
                        WasmType::F32 => results.push("@as(f32, 0.0)".to_string()),
                        WasmType::F64 => results.push("@as(f64, 0.0)".to_string()),
                    }
                }
            }

            Instruction::I32FromChar
            | Instruction::I32FromU8
            | Instruction::I32FromS8
            | Instruction::I32FromU16
            | Instruction::I32FromS16
            | Instruction::I32FromU32
            | Instruction::I32FromS32 => int_cast("i32"),

            Instruction::I64FromU64
            | Instruction::I64FromS64
            | Instruction::F32FromFloat32
            | Instruction::F64FromFloat64
            | Instruction::Float32FromF32
            | Instruction::Float64FromF64
            | Instruction::S32FromI32
            | Instruction::S64FromI64 => results.push(operands[0].clone()),

            Instruction::S8FromI32 => int_cast("i8"),
            Instruction::U8FromI32 => int_cast("u8"),
            Instruction::S16FromI32 => int_cast("i16"),
            Instruction::U16FromI32 => int_cast("u16"),
            Instruction::U32FromI32 => int_cast("u32"),
            Instruction::U64FromI64 => int_cast("u64"),
            Instruction::CharFromI32 => int_cast("u21"),

            Instruction::Bitcasts { casts } => {
                for (cast, op) in casts.iter().zip(operands) {
                    let op = op;
                    match cast {
                        // @bitCast asserts that types are the same size, so these all work fine
                        Bitcast::I32ToF32 => results.push(format!("@bitCast(f32, {op})")),
                        Bitcast::I64ToF64 => results.push(format!("@bitCast(f64, {op})")),
                        Bitcast::F32ToI32 => results.push(format!("@bitCast(i32, {op})")),
                        Bitcast::F64ToI64 => results.push(format!("@bitCast(i64, {op})")),

                        // Here we need to cast from 32 to 64 bits.
                        // @bitCast would error at compile-time since the sizes are mis-matched,
                        // so we cast to a u32 first, then intCast it to an i64.
                        // Since the range of a u32 is within the positive range of an i64,
                        // this should just pad the upper 32 bits of the i64 with zeros and copy
                        // the u32 verbatim into the lower 32 bits.
                        Bitcast::F32ToI64 => results.push(format!("@intCast(i64, @bitCast(u32, {op}))")),

                        // It's unclear to me whether or not this should do sign extension.
                        // The C bindgen casts the i32 directly to an i64, which in C will do sign extension.
                        // So I'll assume the C bindgen is doing it correctly and emulate that here.
                        Bitcast::I32ToI64 => results.push(format!("@intCast(i64, {op})")),

                        // Here we need to cast from 64 to 32 bits, so we need to truncate the most significant bits
                        Bitcast::I64ToF32 => results.push(format!("@bitCast(f32, @truncate(i32, {op}))")),
                        Bitcast::I64ToI32 => results.push(format!("@truncate(i32, {op})")),

                        // No-op? From the code in wit-parser's abi.rs it looks like this is filtered out,
                        // so this is probably an unreachable case. I think it's still a good idea to push
                        // something to the results in case the abi code changes.
                        Bitcast::None => results.push(op.to_string()),
                    }
                }
            }

            Instruction::I32FromBool => {
                // @boolToInt returns a u1 (unsigned 1-bit type), so cast it to an i32
                results.push(format!("@intCast(i32, @boolToInt({}))", operands[0]));
            }
            Instruction::BoolFromI32 => {
                // Zig doesn't have a built-in int-to-bool conversion, you have to
                // explicitly compare it to zero, and the result of the expression is a bool.
                // https://github.com/ziglang/zig/issues/2841#issuecomment-509002009
                results.push(format!("({} != 0)", operands[0]));
            }

            Instruction::RecordLower { record, ty, .. } => {
                let op = &operands[0];
                let field_names = self.interface_gen.get_type_fields(ty, &record.fields);
                for field_name in field_names.iter() {
                    results.push(format!("{op}.{field_name}"));
                }
            }
            Instruction::RecordLift { name, record, ty } => {
                let record_type_name = self.interface_gen.get_type_name(ty).unwrap();
                let mut result = format!("{}{{\n", record_type_name);
                let field_names = self.interface_gen.get_type_fields(ty, &record.fields);
                for (field_name, op) in field_names.iter().zip(operands) {
                    result.push_str(&format!(".{field_name} = {op},\n"));
                }
                result.push_str("}");
                results.push(result);
            }

            Instruction::TupleLower { tuple, .. } => {
                let op = &operands[0];
                for i in 0..tuple.types.len() {
                    results.push(format!("{op}.@\"{i}\""));
                }
            }
            Instruction::TupleLift { .. } => {
                let mut result = String::from(".{\n");
                for op in operands {
                    result.push_str(&format!("{op},\n"));
                }
                result.push_str("}");
                results.push(result);
            }

            // In the canonical ABI explainer, flags are loaded in as an int first;
            // the code example provided shows that the int is loaded as little-endian.
            // Since webassembly is little-endian, Zig's int types in webassembly
            // should also be in little-endian, and since we used a multiple of 32
            // for our flags type size, we can just bitCast to/from the canonical
            // representation.
            // TODO: This is all assuming we're generating bindings for guest modules;
            // if we're generating bindings for the host, we'd need to check endianness
            // ("target.cpu.arch.endian()") and possibly do some byte-swapping.
            Instruction::FlagsLower { flags, .. } => {
                let n = flags.repr().count();
                let res = self.locals.tmp("lowered_flags");
                let op = &operands[0];
                self.src.push_str(&format!("const {res} = @bitCast([{n}]i32, {op});\n"));

                // The FlagsLower instruction wants n results, so unpack the array
                for i in 0..n {
                    results.push(format!("{res}[{i}]"));
                }
            }
            Instruction::FlagsLift { flags, .. } => {
                let n = 32 * flags.repr().count();
                results.push(format!("@bitCast(u{n}, [_]i32{{ {} }})", operands.join(", ")));
            }

            Instruction::VariantPayloadName => {
                let payload = self.locals.tmp("val");
                self.block_params.payload = Some(payload);
                results.push(payload);
            },

            Instruction::VariantLower {
                variant,
                results: result_types,
                ty,
                ..
            } => {
                let blocks = self
                    .blocks
                    .drain(self.blocks.len() - variant.cases.len()..)
                    .collect::<Vec<_>>();

                let case_names = self.interface_gen.get_type_fields(ty, &variant.cases);

                self.const_results("lowered_variant", result_types, results);
                self.src.push_str(&format!("switch ({}) {{\n", operands[0]));
                for ((case_name, case), (block, params)) in
                    case_names.iter().zip(variant.cases).zip(blocks)
                {
                    let payload = params.payload.expect(&format!("No payload for case {case_name}"));
                    self.src.push_str(&format!(".{case_name}"));
                    if case.ty.is_some() {
                        self.src.push_str(&format!(" => |{}| {block},\n", payload));
                    } else {
                        self.src.push_str(&format!(" => {block},\n"));
                    }
                }
                self.src.push_str("};\n");
            }

            Instruction::VariantLift {
                name, variant, ty,
            } => {
                let blocks = self
                    .blocks
                    .drain(self.blocks.len() - variant.cases.len()..)
                    .collect::<Vec<_>>();

                let result = self.locals.tmp("lifted_variant");
                results.push(result);

                let name = self.interface_gen.get_type_name(ty).unwrap();
                let case_names = self.interface_gen.get_type_fields(ty, &variant.cases);
                self.src.push_str(&format!("const {result} = switch ({}) {{\n", operands[0]));
                for (i, (case_name, (block, _))) in
                    case_names.iter().zip(blocks).enumerate()
                {
                    self.src.push_str(&format!("{i} => {name}{{\n.{case_name} = {block}\n}},\n"));
                }
                self.src.push_str("else => unreachable,\n");
                self.src.push_str("};\n");
            }

            Instruction::UnionLower {
                union,
                results: result_types,
                ..
            } => {
                let blocks = self
                    .blocks
                    .drain(self.blocks.len() - union.cases.len()..)
                    .collect::<Vec<_>>();

                self.const_results("lowered_union", result_types, results);

                self.src.push_str(&format!("switch ({}) {{\n", operands[0]));
                for (i, (case, (block, params))) in
                    union.cases.iter().zip(blocks).enumerate()
                {
                    let payload = params.payload.expect(&format!("No payload for case {i}"));
                    self.src.push_str(&format!(".@\"{i}\""));
                    self.src.push_str(&format!(" => |{}| {block},\n", payload));
                }
                self.src.push_str("};\n");
            }

            Instruction::UnionLift {
                name, union, ty,
            } => {
                let blocks = self
                    .blocks
                    .drain(self.blocks.len() - union.cases.len()..)
                    .collect::<Vec<_>>();

                let result = self.locals.tmp("lifted_union");
                results.push(result);

                let name = self.interface_gen.get_type_name(ty).unwrap();
                self.src.push_str(&format!("const {result} = switch ({}) {{\n", operands[0]));
                for (i, (case, (block, _))) in 
                    union.cases.iter().zip(blocks).enumerate()
                {
                    self.src.push_str(&format!("{i} => {name}{{ .@\"{i}\" = {block} }},\n"));
                }
                self.src.push_str("else => unreachable,");
                self.src.push_str("};\n");
            }

            Instruction::OptionLower {
                results: result_types,
                ..
            } => {
                let op = &operands[0];
                let (some, params) = self.blocks.pop().unwrap();
                let some_payload = params.payload.expect("No payload for Some option");
                let (none, _) = self.blocks.pop().unwrap();

                self.const_results("lowered_option", result_types, results);
                self.src.push_str(&format!(
                    "if ({op} != null) |{some_payload}| {some} else {none};\n",
                ));
            }

            Instruction::OptionLift { ty, .. } => {
                let op = &operands[0];
                let (some, _) = self.blocks.pop().unwrap();
                let (none, _) = self.blocks.pop().unwrap();
                let result = self.locals.tmp("lifted_option");
                results.push(result);
                let optional_ty = match self.interface_gen.lookup_type_name(ty) {
                    Some(name) => name,
                    None => self.interface_gen.optional_type_def(&Type::Id(*ty)),
                };
                self.src.push_str(&format!(
                    "const {result}: {optional_ty} = switch ({op}) {{
                        0 => null,
                        1 => {some},
                        else => unreachable,
                    }};\n"
                ));
            }

            Instruction::ResultLower {
                results: result_types,
                result,
                ..
            } => {
                let (err, err_params) = self.blocks.pop().unwrap();
                let (ok, ok_params) = self.blocks.pop().unwrap();

                let op = &operands[0];
                let ok_binding = if result.ok.is_some() {
                    &ok_params.payload.expect("No Ok payload")
                } else { "_" };
                let err_binding = if result.err.is_some() {
                    &err_params.payload.expect("No Error payload")
                } else { "_" };
                self.const_results("lowered_result", result_types, results);
                self.src.push_str(&format!(
                    // This assumes the error (if any) can only be a WIT error
                    "switch ({op}) {{
                        .ok => |{ok_binding}| {ok},
                        .wit_error => |{err_binding}| {err},
                        else => unreachable,
                    }};\n"
                ));
            }

            Instruction::ResultLift { ty, .. } => {
                let (err, _) = self.blocks.pop().unwrap();
                let (ok, _) = self.blocks.pop().unwrap();
                let op = &operands[0];
                let res = self.locals.tmp("lifted_result");
                results.push(res);
                let result_type = self.interface_gen.get_type_name(ty).unwrap();
                self.src.push_str(&format!(
                    "const {res} = switch ({op}) {{
                        0 => {result_type}{{ .ok = {ok} }},
                        1 => {result_type}{{ .wit_error = {err} }},
                        else => unreachable,
                    }};\n"
                ));
            }

            Instruction::EnumLower { .. } => {
                // Since we declared our enum type with a tag, and
                // with each case numbered as it is in the canonical
                // ABI, we can cast the enum directly to its integer
                // tag type rather than having to switch on it.
                results.push(format!("@enumToInt({})", operands[0]));
            }

            Instruction::EnumLift { ty, .. } => {
                // Just doing the opposite of above, take an int and use a builtin
                // to convert it to an enum.
                let enum_type = self.interface_gen.get_type_name(ty).unwrap();
                results.push(format!("@intToEnum({enum_type}, {})", operands[0]));
            }

            Instruction::ListCanonLower { realloc, element } => {
                let op = &operands[0];
                if realloc.is_none() {
                    results.push(format!("@intCast(i32, @ptrToInt({op}.ptr))"));
                    results.push(format!("@intCast(i32, {op}.len)"));
                } else {
                    let result = self.locals.tmp("lowered_array");
                    let allocation = self.wrap_error(&format!("allocator.realloc({op}, {op}.len)"));
                    self.allocations.push(Allocation::Array(result.clone()));
                    self.src.push_str(&format!("{result} = {allocation};\n"));

                    results.push(format!("@intCast(i32, @ptrToInt({result}.ptr))"));
                    results.push(format!("@intCast(i32, {result}.len)"));
                }
            }

            Instruction::ListCanonLift { element, .. } => {
                let ptr = &operands[0];
                let len = &operands[1];
                let element_type = self.interface_gen.type_def(element);
                results.push(format!(
                    // We cast the ptr from a i32 to a usize (since that's what @intToPtr requires),
                    // cast that to a many-item pointer, then convert the pointer to a slice using
                    // the length (after casting the length to a usize).
                    "@intToPtr([*]{element_type}, @intCast(usize, {ptr}))[0..@intCast(usize, {len})]"
                ));
            }

            Instruction::IterElem { .. } => {
                let elem = self.locals.tmp("item");
                self.block_params.element = Some(elem.clone());
                results.push(elem);
            },

            Instruction::IterBasePointer => {
                let payload = self.locals.tmp("base");
                self.block_params.base = Some(payload.clone());
                results.push(payload);
            },

            Instruction::ListLower { element, realloc } => {
                let op = &operands[0];
                let (body, params) = self.blocks.pop().unwrap();
                let elem = params.element.expect("No list element binding");
                let base = params.base.expect("No base pointer binding");

                let size = self.interface_gen.world_gen.sizes.size(element);
                let align = self.interface_gen.world_gen.sizes.align(element);

                let result = self.locals.tmp("lowered_array");
                let allocation = self.wrap_error(&format!(
                    // wit-bindgen wants to use pointers like you do in C, so the type
                    // is u8 here since we're just going to cast it to a different pointer
                    // later and all we care about is that it has the correct size (in bytes)
                    // and alignment.
                    "allocator.alignedAlloc(u8, {align}, {op}.len * {size})"
                ));
                self.allocations.push(Allocation::Array(result.clone()));
                self.src.push_str(&format!(
                    "{result} = {allocation};\n"
                ));
                let i = self.locals.tmp("index");
                self.src.push_str(&format!("for ({op}, 0..) |{elem}, {i}| {{\n"));
                self.src.push_str(&format!(
                    "const {base} = @intCast(i32, @ptrToInt({result}.ptr)) + @intCast(i32, {i}) * {size};\n",
                ));
                self.src.push_str(&body);
                self.src.push_str("}\n");

                if realloc.is_none() {
                    // If an allocator isn't requested then we must clean up the
                    // allocation ourselves since our callee isn't taking
                    // ownership.
                    self.allocations.push(Allocation::Array(result));
                }

                results.push(format!("@intCast(i32, @ptrToInt({result}.ptr))"));
                results.push(format!("@intCast(i32, {op}.len)"));
            }

            Instruction::ListLift { element, .. } => {
                let (body, params) = self.blocks.pop().unwrap();
                let ptr = operands[0];
                let len = operands[1];

                let result = self.locals.tmp("lifted_array");
                let elem_type = self.interface_gen.type_def(element);
                let allocation = self.wrap_error(&format!(
                    "allocator.alloc({elem_type}, {len})"
                ));
                self.allocations.push(Allocation::Array(result.clone()));
                self.src.push_str(&format!(
                    // Note that since this is an allocation, we'll declare
                    // it at the top of the function, and assign it here.
                    "{result} = {allocation};\n"
                ));

                let elem = params.element.expect("No list element binding");
                let base = params.base.expect("No base pointer binding");
                let size = self.interface_gen.world_gen.sizes.size(element);

                let i = self.locals.tmp("index");
                self.src.push_str(&format!("for (0..{len}) |{i}| {{\n"));
                self.src.push_str(&format!(
                    "const {base} = {ptr} + {i} * {size};\n"
                ));
                self.src.push_str(&format!("{result}[{i}] = {body};\n"));
                self.src.push_str("}\n");
                results.push(result);

                self.src.push_str(&format!(
                    "allocator.free(@intToPtr([*]u8, @intCast(usize, {ptr}))[0..@intCast(usize, {len})]);\n"
                ));
            }

            Instruction::StringLower { realloc } => {
                let op = &operands[0];
                
                match self.interface_gen.world_gen.opts.string_encoding {
                    StringEncoding::CompactUTF16 => {
                        let result = self.locals.tmp("lowered_string");

                        let mask = self.locals.tmp("mask");
                        self.src.push_str(&format!("var {mask}: u32;\n"));
                        
                        if realloc.is_some() {
                            let s = self.locals.tmp("s");
                            self.allocations.push(Allocation::Array(result.clone()));
                            let allocation = self.wrap_error(&format!(
                                "allocator.realloc({s}, {s}.len)"
                            ));
                            let body = &format!("{result} = {allocation};\n");
                            self.src.push_str(&format!(
                                "switch ({op}) {{
                                    .latin1 => |{s}| {{
                                        {mask} = 0;
                                        {body}
                                    }},
                                    .utf16 => |{s}| {{
                                        {mask} = 1 << 31;
                                        {body}
                                    }},
                                }}\n"
                            ));
                        } else {
                            self.src.push_str(&format!("var {result};\n"));
                            self.src.push_str(&format!("{result} = {op};\n"));
                            self.src.push_str(&format!(
                                "{mask} = switch ({op}) {{
                                    .latin1 => |_| 0,
                                    .utf16 => |_| 1 << 31,
                                }};\n"
                            ));
                        }

                        results.push(format!("@intCast(i32, @ptrToInt({result}.ptr))"));
                        // Zig sees 1 << 31 as a positive comptime_int, and as such it's outside of
                        // the range of an i32, so we have to operate on u32's and @bitCast the result.
                        results.push(format!("@bitCast(i32, @intCast(u32, {result}.len) | {mask})"));
                    }
                    _ => {
                        let mut result;
                        if realloc.is_none() {
                            result = op;
                        } else {
                            result = &self.locals.tmp("lowered_string");
                            let allocation = self.wrap_error(&format!("allocator.realloc({op}, {op}.len)"));
                            self.allocations.push(Allocation::Array(result.clone()));
                            self.src.push_str(&format!("{result} = {allocation};\n"));
                        }
                        results.push(format!("@intCast(i32, @ptrToInt({result}.ptr))"));
                        results.push(format!("@intCast(i32, {result}.len)"));
                    }
                }
            }

            Instruction::StringLift => {
                let ptr = &operands[0];
                let mut len = &operands[1];
                let result = self.locals.tmp("tmp_str");
                
                let mut lift = |element_type: &str| {
                    format!(
                        // We cast the ptr from a i32 to a usize (since that's what @intToPtr requires),
                        // cast that to a many-item pointer, then convert the pointer to a slice using
                        // the length (after casting the length to a usize).
                        "@intToPtr([*]{element_type}, @intCast(usize, {ptr}))[0..@intCast(usize, {len})];"
                    )
                };

                let mut declare_result = |expr: &str| {
                    self.src.push_str(&format!(
                        "const {result} = {expr};\n"
                    ));
                };

                match self.interface_gen.world_gen.opts.string_encoding {
                    StringEncoding::UTF8 => declare_result(&lift("u8")),
                    StringEncoding::UTF16 => declare_result(&lift("u16")),
                    StringEncoding::CompactUTF16 => {
                        let mask = self.locals.tmp("mask");

                        // Since we're using a comptime expression, this should be a comptime_int
                        self.src.push_str(&format!("const {mask} = 1 << 31;\n"));

                        let packed_len = self.locals.tmp("packed_len");
                        self.src.push_str(&format!("const {packed_len} = @bitCast(u32, {len});\n"));

                        len = &format!("{packed_len} ^ {mask})");

                        let lifted_latin1 = lift("u8");
                        let lifted_utf16 = lift("u16");

                        let str_type = self.interface_gen.string_type_def();
                        self.src.push_str(&format!("var {result};\n"));
                        self.src.push_str(&format!(
                            "if ({packed_len} & {mask} == 0) {{
                                {result} = {str_type} {{
                                    .latin1 = {lifted_latin1}
                                }};
                            }} else {{
                                {result} = {str_type} {{
                                    .utf16 = {lifted_utf16}
                                }};
                            }}\n"
                        ));
                    }
                }

                results.push(result);
            }

            Instruction::CallWasm { name, sig } => {
                let func = self.extern_sig(
                    self.gen.wasm_import_module.unwrap(),
                    name,
                    sig,
                );

                // ... then call the function with all our operands
                if sig.results.len() > 0 {
                    self.push_str("let ret = ");
                    results.push("ret".to_string());
                }
                self.push_str(&func);
                self.push_str("(");
                self.push_str(&operands.join(", "));
                self.push_str(");\n");
            }
        }
    }
}