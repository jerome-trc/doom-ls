use std::{
	fs::File,
	io::{BufWriter, Write},
	path::Path,
	sync::OnceLock,
};

use regex::Regex;

fn main() -> UnitResult {
	let start_time = std::time::Instant::now();
	let data_dir = std::env::var("DOOMLS_DATA_DIR")?;
	println!(r#"cargo:rerun-if-changed={data_dir}"#);
	let data_dir = Path::new(&data_dir);

	let out_dir = std::env::var("OUT_DIR")?;
	let out_native_dir = Path::new(&out_dir).join("native");
	std::fs::create_dir_all(&out_native_dir)?;

	zscript(data_dir, &out_native_dir)?;

	eprintln!(
		"Native symbol definitions generated in {}ms.",
		start_time.elapsed().as_millis()
	);

	Ok(())
}

// ZScript /////////////////////////////////////////////////////////////////////

fn zscript(data_dir: &Path, out_native_dir: &Path) -> UnitResult {
	let out_zscript_file = out_native_dir.join("zscript.rs");
	let mut bw = BufWriter::new(std::fs::File::create(out_zscript_file)?);
	write_file_start(&mut bw)?;
	zscript_globals(&mut bw, &data_dir.join("global.toml"))?;

	write_array_start(&mut bw, "CLASSES", "RawNativeClass")?;
	zscript_class(&mut bw, data_dir, &data_dir.join("thinker.toml"))?;
	write_array_end(&mut bw)?;

	write_array_start(&mut bw, "ENUMS", "RawNativeEnum")?;
	write_array_end(&mut bw)?;

	write_array_start(&mut bw, "STRUCTS", "RawNativeStruct")?;
	zscript_struct(&mut bw, data_dir, &data_dir.join("cvar.toml"))?;
	write_array_end(&mut bw)?;

	write_array_start(&mut bw, "_VALUES", "RawNativeValue")?;
	write_array_end(&mut bw)?;

	Ok(())
}

fn zscript_globals(bw: &mut BufWriter<File>, file_path: &Path) -> UnitResult {
	let top_table = {
		let txt_global = std::fs::read_to_string(file_path)?;
		txt_global.parse::<toml::Table>()?
	};

	{
		write_array_start(bw, "GLOBAL_FUNCTIONS", "RawNativeFunction")?;

		let instance_methods = top_table
			.get("instance-methods")
			.unwrap()
			.as_table()
			.unwrap();

		category(instance_methods, bw, |bw, name, decl, doc| {
			write_zscript_function_datum(bw, name, decl, doc, true, false)?;
			Ok(())
		})?;

		write_array_end(bw)?;
	}

	{
		write_array_start(bw, "GLOBAL_VALUES", "RawNativeValue")?;

		let instance_members = top_table
			.get("instance-members")
			.unwrap()
			.as_table()
			.unwrap();

		category(instance_members, bw, |bw, name, decl, doc| {
			write_zscript_value_datum(bw, name, decl, doc, "Constant")?;
			Ok(())
		})?;

		let constants = top_table.get("constants").unwrap().as_table().unwrap();

		category(constants, bw, |bw, name, decl, doc| {
			write_zscript_value_datum(bw, name, decl, doc, "Constant")?;
			Ok(())
		})?;

		write_array_end(bw)?;
	}

	Ok(())
}

fn zscript_class(bw: &mut BufWriter<File>, data_dir: &Path, file_path: &Path) -> UnitResult {
	let top_table = {
		let txt_global = std::fs::read_to_string(file_path)?;
		txt_global.parse::<toml::Table>()?
	};

	let name = top_table.get("name").unwrap().as_str().unwrap();
	let decl = top_table.get("decl").unwrap().as_str().unwrap();
	let parent = top_table
		.get("parent")
		.map(|v| v.as_str().unwrap())
		.unwrap_or("");
	let doc = sanitize_doc(&top_table);

	write_zscript_class_datum(
		bw,
		name,
		decl,
		doc,
		parent,
		|bw| {
			let Some(struct_paths) = top_table.get("structs").map(|v| v.as_array().unwrap()) else {
				return Ok(());
			};

			for v in struct_paths {
				let path = Path::new(v.as_str().unwrap());
				zscript_struct(bw, data_dir, &data_dir.join(path))?;
			}

			Ok(())
		},
		|bw| zscript_process_enums(bw, &top_table, data_dir),
		|bw| zscript_process_values(bw, &top_table, data_dir),
		|bw| zscript_process_functions(bw, &top_table),
	)?;

	Ok(())
}

fn zscript_enum(bw: &mut BufWriter<File>, file_path: &Path) -> UnitResult {
	let top_table = {
		let txt_global = std::fs::read_to_string(file_path)?;
		txt_global.parse::<toml::Table>()?
	};

	let name = top_table.get("name").unwrap().as_str().unwrap();
	let decl = top_table.get("decl").unwrap().as_str().unwrap();
	let underlying = top_table
		.get("underlying")
		.map(|v| v.as_str().unwrap())
		.unwrap_or("int");
	let doc = sanitize_doc(&top_table);

	write_zscript_enum_datum(bw, name, decl, underlying, doc, |bw| {
		let variants = top_table.get("variants").unwrap().as_table().unwrap();

		if let Some(ungrouped) = variants.get("ungrouped").map(|v| v.as_table().unwrap()) {
			for (key, _) in ungrouped {
				write!(bw, r#""{key}","#)?;
				writeln!(bw)?;
			}
		}

		category(variants, bw, |bw, name, _decl, _doc| {
			write!(bw, r#""{name}","#)?;
			writeln!(bw)?;
			Ok(())
		})?;

		Ok(())
	})?;

	Ok(())
}

fn zscript_enum_variants(bw: &mut BufWriter<File>, file_path: &Path) -> UnitResult {
	let top_table = {
		let txt_global = std::fs::read_to_string(file_path)?;
		txt_global.parse::<toml::Table>()?
	};

	let variants = top_table.get("variants").unwrap().as_table().unwrap();

	if let Some(ungrouped) = variants.get("ungrouped").map(|v| v.as_table().unwrap()) {
		for (_, val) in ungrouped {
			let variant = val.as_table().unwrap();
			let doc = sanitize_doc(variant);
			let (decl, name) =
				extract_decl_and_name(variant.get("decl").unwrap().as_str().unwrap());
			write_zscript_value_datum(bw, name, &decl, doc, "EnumVariant")?;
		}
	}

	category(variants, bw, |bw, name, decl, doc| {
		write_zscript_value_datum(bw, name, decl, doc, "EnumVariant")?;
		Ok(())
	})?;

	Ok(())
}

fn zscript_struct(bw: &mut BufWriter<File>, data_dir: &Path, file_path: &Path) -> UnitResult {
	let top_table = {
		let txt_global = std::fs::read_to_string(file_path)?;
		txt_global.parse::<toml::Table>()?
	};

	let name = top_table.get("name").unwrap().as_str().unwrap();
	let decl = top_table.get("decl").unwrap().as_str().unwrap();
	let doc = sanitize_doc(&top_table);

	write_zscript_struct_datum(
		bw,
		name,
		decl,
		doc,
		|bw| zscript_process_enums(bw, &top_table, data_dir),
		|bw| zscript_process_values(bw, &top_table, data_dir),
		|bw| zscript_process_functions(bw, &top_table),
	)?;

	Ok(())
}

fn zscript_process_enums(
	bw: &mut BufWriter<File>,
	top_table: &TomlTable,
	data_dir: &Path,
) -> UnitResult {
	let Some(enum_paths) = top_table.get("enums").map(|v| v.as_array().unwrap()) else {
		return Ok(())
	};

	for v in enum_paths {
		let path = Path::new(v.as_str().unwrap());
		zscript_enum(bw, &data_dir.join(path))?;
	}

	Ok(())
}

fn zscript_process_functions(bw: &mut BufWriter<File>, top_table: &TomlTable) -> UnitResult {
	if let Some(class_methods) = top_table
		.get("class-methods")
		.map(|v| v.as_table().unwrap())
	{
		if let Some(ungrouped) = class_methods
			.get("ungrouped")
			.map(|v| v.as_table().unwrap())
		{
			for (_, val) in ungrouped {
				let field = val.as_table().unwrap();
				let doc = sanitize_doc(field);
				let (decl, name) =
					extract_decl_and_name(field.get("decl").unwrap().as_str().unwrap());
				write_zscript_function_datum(bw, name, &decl, doc, true, false)?;
			}
		}

		category(class_methods, bw, |bw, name, decl, doc| {
			write_zscript_function_datum(bw, name, decl, doc, true, decl.ends_with("const"))?;
			Ok(())
		})?;
	}

	if let Some(instance_methods) = top_table
		.get("instance-methods")
		.map(|v| v.as_table().unwrap())
	{
		if let Some(ungrouped) = instance_methods
			.get("ungrouped")
			.map(|v| v.as_table().unwrap())
		{
			for (_, val) in ungrouped {
				let field = val.as_table().unwrap();
				let doc = sanitize_doc(field);
				let (decl, name) =
					extract_decl_and_name(field.get("decl").unwrap().as_str().unwrap());
				write_zscript_function_datum(bw, name, &decl, doc, false, decl.ends_with("const"))?;
			}
		}

		category(instance_methods, bw, |bw, name, decl, doc| {
			write_zscript_function_datum(bw, name, decl, doc, false, decl.ends_with("const"))?;
			Ok(())
		})?;
	}

	Ok(())
}

fn zscript_process_values(
	bw: &mut BufWriter<File>,
	top_table: &TomlTable,
	data_dir: &Path,
) -> UnitResult {
	if let Some(enum_paths) = top_table.get("enums").map(|v| v.as_array().unwrap()) {
		for v in enum_paths {
			let path = Path::new(v.as_str().unwrap());
			zscript_enum_variants(bw, &data_dir.join(path))?;
		}
	};

	if let Some(instance_members) = top_table
		.get("instance-members")
		.map(|v| v.as_table().unwrap())
	{
		if let Some(ungrouped) = instance_members
			.get("ungrouped")
			.map(|v| v.as_table().unwrap())
		{
			for (_, val) in ungrouped {
				let field = val.as_table().unwrap();
				let doc = sanitize_doc(field);
				let (decl, name) =
					extract_decl_and_name(field.get("decl").unwrap().as_str().unwrap());
				write_zscript_value_datum(bw, name, &decl, doc, "Field")?;
			}
		}

		category(instance_members, bw, |bw, name, decl, doc| {
			write_zscript_value_datum(bw, name, decl, doc, "Field")?;
			Ok(())
		})?;
	}

	if let Some(constants) = top_table.get("constants").map(|v| v.as_table().unwrap()) {
		if let Some(ungrouped) = constants.get("ungrouped").map(|v| v.as_table().unwrap()) {
			for (_, val) in ungrouped {
				let field = val.as_table().unwrap();
				let doc = sanitize_doc(field);
				let (decl, name) =
					extract_decl_and_name(field.get("decl").unwrap().as_str().unwrap());
				write_zscript_value_datum(bw, name, &decl, doc, "Constant")?;
			}
		}

		category(constants, bw, |bw, name, decl, doc| {
			write_zscript_value_datum(bw, name, decl, doc, "Constant")?;
			Ok(())
		})?;
	}

	Ok(())
}

#[allow(clippy::too_many_arguments)]
fn write_zscript_class_datum(
	bw: &mut BufWriter<File>,
	name: &str,
	decl: &str,
	doc: &str,
	parent: &str,
	mut structs: impl FnMut(&mut BufWriter<File>) -> UnitResult,
	mut enums: impl FnMut(&mut BufWriter<File>) -> UnitResult,
	mut values: impl FnMut(&mut BufWriter<File>) -> UnitResult,
	mut functions: impl FnMut(&mut BufWriter<File>) -> UnitResult,
) -> UnitResult {
	write!(
		bw,
		r##"RawNativeClass {{
			name: r#"{name}"#,
			decl: r#"{decl}"#,
			doc: r#"{doc}"#,
			parent: r#"{parent}"#,
			structs: &["##
	)?;

	structs(bw)?;

	write!(
		bw,
		r##"],
		enums: &["##,
	)?;

	enums(bw)?;

	write!(
		bw,
		r##"],
		values: &["##,
	)?;

	values(bw)?;

	write!(
		bw,
		r##"],
		functions: &["##,
	)?;

	functions(bw)?;

	write!(
		bw,
		r##"],
	}},"##
	)?;

	Ok(())
}

fn write_zscript_enum_datum(
	bw: &mut BufWriter<File>,
	name: &str,
	decl: &str,
	underlying: &str,
	doc: &str,
	mut variants: impl FnMut(&mut BufWriter<File>) -> UnitResult,
) -> UnitResult {
	write!(
		bw,
		r##"RawNativeEnum {{
			name: r#"{name}"#,
			decl: r#"{decl}"#,
			doc: r#"{doc}"#,
			underlying: {u_t},
			variants: &["##,
		u_t = {
			match underlying {
				"sbyte" => "ast::EnumType::KwSByte",
				"byte" => "ast::EnumType::KwByte",
				"int8" => "ast::EnumType::KwInt8",
				"uint8" => "ast::EnumType::KwUInt8",
				"short" => "ast::EnumType::KwShort",
				"ushort" => "ast::EnumType::KwUShort",
				"int16" => "ast::EnumType::KwInt16",
				"uint16" => "ast::EnumType::KwUInt16",
				"int" => "ast::EnumType::KwInt",
				"uint" => "ast::EnumType::KwUInt",
				_ => unreachable!(),
			}
		}
	)?;

	variants(bw)?;

	write!(
		bw,
		r##"],
	}},"##
	)?;

	Ok(())
}

fn write_zscript_function_datum(
	bw: &mut BufWriter<File>,
	name: &str,
	decl: &str,
	doc: &str,
	is_static: bool,
	is_const: bool,
) -> UnitResult {
	write!(
		bw,
		r##"		RawNativeFunction {{
			name: r#"{name}"#,
			decl: r#"{decl}"#,
			doc: r#"{doc}"#,
			is_static: {is_static},
			is_const: {is_const}
		}},
"##
	)?;

	Ok(())
}

fn write_zscript_struct_datum(
	bw: &mut BufWriter<File>,
	name: &str,
	decl: &str,
	doc: &str,
	mut enums: impl FnMut(&mut BufWriter<File>) -> UnitResult,
	mut values: impl FnMut(&mut BufWriter<File>) -> UnitResult,
	mut functions: impl FnMut(&mut BufWriter<File>) -> UnitResult,
) -> UnitResult {
	write!(
		bw,
		r##"RawNativeStruct {{
			name: r#"{name}"#,
			decl: r#"{decl}"#,
			doc: r#"{doc}"#,
			enums: &["##
	)?;

	enums(bw)?;

	write!(
		bw,
		r##"],
		values: &["##,
	)?;

	values(bw)?;

	write!(
		bw,
		r##"],
		functions: &["##,
	)?;

	functions(bw)?;

	write!(
		bw,
		r##"],
	}},
"##
	)?;

	Ok(())
}

fn write_zscript_value_datum(
	bw: &mut BufWriter<File>,
	name: &str,
	decl: &str,
	doc: &str,
	kind: &'static str,
) -> UnitResult {
	write!(
		bw,
		r##"		RawNativeValue {{
			name: r#"{name}"#,
			decl: r#"{decl}"#,
			doc: r#"{doc}"#,
			kind: ValueKind::{kind}
		}},
"##
	)?;

	Ok(())
}

// Helpers /////////////////////////////////////////////////////////////////////

type UnitResult = Result<(), Box<dyn std::error::Error>>;
type TomlTable = toml::map::Map<String, toml::Value>;

/// A category is a section titled something like "Instance Methods", "Constants",
/// or "Variants". Each may or may not contain a table titled "groups" and another
/// titled "ungrouped".
///
/// `F`'s string slice parameters are `name`, `decl`, `doc`.
fn category<F>(category: &TomlTable, bw: &mut BufWriter<File>, mut callback: F) -> UnitResult
where
	F: FnMut(&mut BufWriter<File>, &str, &str, &str) -> UnitResult,
{
	let Some(groups) = category.get("groups").map(|v| v.as_table().unwrap()) else {
		return Ok(());
	};

	for group in groups.iter().map(|kvp| kvp.1.as_array().unwrap()) {
		for obj in group.iter().map(|value| value.as_table().unwrap()) {
			let doc = sanitize_doc(obj);
			let v_decl = obj.get("decl").unwrap();

			if let Some(s_decl) = v_decl.as_str() {
				let (decl, name) = extract_decl_and_name(s_decl);
				callback(bw, name, &decl, doc)?;
			} else if let Some(t_decl) = v_decl.as_array() {
				for raw_decl in t_decl.iter().map(|v| v.as_str().unwrap()) {
					let (decl, name) = extract_decl_and_name(raw_decl);
					callback(bw, name, &decl, doc)?;
				}
			} else {
				unreachable!()
			}
		}
	}

	Ok(())
}

/// Essentially a no-op now, but shortens other code and may be useful later.
#[must_use]
fn sanitize_doc(table: &TomlTable) -> &str {
	table.get("doc").map(|d| d.as_str().unwrap()).unwrap_or("")
}

#[must_use]
fn extract_decl_and_name(decl: &str) -> (String, &str) {
	static RGX_NAME: OnceLock<Regex> = OnceLock::new();
	let rgx_name = RGX_NAME.get_or_init(|| Regex::new(r"\{([A-Za-z0-9_]+)\}").unwrap());

	let name = rgx_name
		.captures(decl)
		.map(|cap| cap.get(1).unwrap().as_str())
		.or_else(|| panic!("Failed to extract name from declaration: `{decl}`"))
		.unwrap();

	let mut pre_ret = String::with_capacity(decl.len());
	let mut last_match = 0;

	static RGX: OnceLock<Regex> = OnceLock::new();

	let rgx = RGX.get_or_init(|| Regex::new(r#"(?x)(\\\[)|(\\\])|\[|\]|\{|\}|(\\<)"#).unwrap());

	for capset in rgx.captures_iter(decl) {
		let (range, replacement) = if let Some(m) = capset.get(1) {
			(m.range(), "\x1A")
		} else if let Some(m) = capset.get(2) {
			(m.range(), "\x1B")
		} else if let Some(m) = capset.get(3) {
			(m.range(), "<")
		} else {
			let r = capset.get(0).unwrap().range();
			(r.clone(), &decl[last_match..r.start])
		};

		pre_ret.push_str(replacement);
		last_match = range.end;
	}

	let mut ret = pre_ret.clone();

	for (idx, _) in pre_ret.match_indices(|c| c == '\x1A') {
		ret.replace_range(idx..(idx + 1), "[");
	}

	for (idx, _) in pre_ret.match_indices(|c| c == '\x1B') {
		ret.replace_range(idx..(idx + 1), "]");
	}

	ret.push_str(&decl[last_match..]);

	(ret, name)
}

// Common string writing functions /////////////////////////////////////////////

fn write_array_start(
	bw: &mut BufWriter<File>,
	name: &'static str,
	elem_t: &'static str,
) -> UnitResult {
	writeln!(bw, "\nconst {name}: &[{elem_t}] = &[")?;
	Ok(())
}

fn write_array_end(bw: &mut BufWriter<File>) -> UnitResult {
	writeln!(bw, "];")?;
	Ok(())
}

fn write_file_start(bw: &mut BufWriter<File>) -> UnitResult {
	writeln!(bw, r"// This file is auto-generated by a build script.")?;

	Ok(())
}
