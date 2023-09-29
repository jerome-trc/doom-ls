use doomfront::zdoom::zscript::{Syn, SyntaxToken};
use lsp_types::{HoverContents, LanguageString, MarkedString};

/// Documentation and code samples here are provided courtesy of zdoom-docs.
/// For licensing information, see the repository's README file.
#[must_use]
pub(super) fn hover_info_for(token: SyntaxToken) -> Option<HoverContents> {
	const DOCS_WHILE_UNTIL: &str = indoc::indoc! {
		"The `while` loop simply takes one expression for checking if the loop
		should break, equivalent to `for(; x;)`.

		The `until` loop is equivalent to `while(!x)`."
	};

	const DOCS_IF_ELSE: &str = indoc::indoc! {
		"A conditional statement will, conditionally,
		choose a statement (or none) to execute.
		They work the same as in C, C++, and ACS.

		```zscript
		// Simple conditional.
		if(a)
			B();

		// Simple conditional, with else statement and a block.
		if(a)
		{
			B();
			c = d;
		}
		else
			e = f;
		```"
	};

	let (kw, contents) = match token.kind() {
		Syn::KwAbstract => (
			"abstract",
			&["A class marked `abstract` cannot be instantiated with `new`."],
		),
		Syn::KwAction => (
			"action",
			&[indoc::indoc! {
				"ZScript includes an extra method type for descendents of `Actor`
				called *actions*, which are intended to be run from actor states and
				give extra information to the function. Action functions change the
				meaning of the `self` parameter and may pass in `invoker` and
				`stateinfo` parameters as well. `stateinfo` refers to the `State`
				which this action was called from."
			}],
		),
		Syn::KwAlignOf => (
			"alignOf",
			&[indoc::indoc! {
				"Can be used as the left side of a unary expression to get the
				memory alignment of that expression's output type in bytes."
			}],
		),
		Syn::KwBreak => (
			"break",
			&[indoc::indoc! {
				r#"`break` is available in loop statements and switch statements,
				and will break out of the containing statement early.

				```zscript
				// Use of "break."
				for(int i = 0; i < 50; i++)
				{
					// "break" when "i" is 25.
					if(i == 25)
						break;

					DoThing(i);
				}
				```"#
			}],
		),
		Syn::KwClass => (
			"class",
			&[indoc::indoc! {
				r#"A class defines an object type within ZScript, and is most
				of what you'll be creating within the language.

				```zscript
				// Basic class definition with a member variable and member function.
				class BasicClass
				{
					// "m_Thing" is attached to any "instance" of BasicClass.
					int m_Thing;

					// Changes "m_Thing" to 500 on an instance of BasicClass.
					void ChangeThing()
					{
						m_Thing = 500;
					}
				}
				```"#,
			}],
		),
		Syn::KwElse => ("else", &[DOCS_IF_ELSE]),
		Syn::KwEnum => (
			"enum",
			&[indoc::indoc! {
				"An enumeration is a list of named numbers.
				By default they decay to the type int,
				but the default decay type can be set manually.

				```zscript
				// Basic enumeration.
				enum MyCoolEnum
				{
					A_THING, // Has value int(0) ...
					BEES, // ... 1 ...
					CALCIUM, // ... 2 ...
					DEXTROSE, // ... and 3.
				}
				```

				Enumerators can either be incremental — from the last enumerator,
				or 0 if there is none — or explicitly set.

				```zscript
				// Less trivial example.
				enum MyCoolerEnum : int16
				{
					A = 500, // Has value int16(500),
					B, // 501,
					C = 200,
					D, // 201,
					E, // and 202.
				}
				```"
			}],
		),
		Syn::KwFinal => (
			"final",
			&[indoc::indoc! {
				"Used to mark overrides of abstract and virtual functions,
				preventing them from being overriden again."
			}],
		),
		Syn::KwFor => (
			"for",
			&[indoc::indoc! {
				"The for loop takes a limited statement and two optional expressions:
				the statement for when the loop begins (which is scoped to the loop),
				one expression for checking if the loop should break,
				and one which is executed every time the loop iterates."
			}],
		),
		Syn::KwIf => ("if", &[DOCS_IF_ELSE]),
		Syn::KwInclude => (
			"#include",
			&[indoc::indoc! {
				r#"Include directives include other files to be processed by the ZScript
				compiler, allowing you to organize and separate code into different
				files.

				Note that included files will conflict with other mods. If two mods
				have a file named `zscript/MyCoolClasses.zsc` and both include it,
				expecting to get different files, the engine will fail to load with a
				script error.

				To avoid this, it is suggested to place your ZScript code under a
				uniquely named sub-folder.

				```zscript
				#include "zscript/MyCoolMod/MyCoolClasses.zsc"
				```"#
			}],
		),
		Syn::KwInternal => (
			"internal",
			&[indoc::indoc! {
				"Marks a member field as only being writable from ZScript code in
				the base resource archive (generally `gzdoom.pk3`)."
			}],
		),
		Syn::KwLatent => (
			"latent",
			&[indoc::indoc! {
				"`latent` can be used to mark member fields,
				but it does nothing and its purpose is unknown."
			}],
		),
		Syn::KwLet => (
			"let",
			&[indoc::indoc! {
				r#"The `let` type automatically determines the type of a variable
				by its initializer.

				```zscript
				// Inferred to be an `int`.
				let i = 0;
				// Inferred to be a string.
				let s = "Hello World!";

				Object Function() {
					return null;
				}

				// Inferred to be an `Object`.
				let f = Function();
				```"#
			}],
		),
		Syn::KwNative => (
			"native",
			&[indoc::indoc! {
				"Marks a `class`, `struct`, member field or function as originating
				in the engine instead of being defined in ZScript.
				This qualifier is restricted from user code."
			}],
		),
		Syn::KwReturn => (
			"return",
			&[indoc::indoc! {
				r#"`return` is available in functions. If the function does not return
				any values, it may have no expressions, and will simply exit the
				function early. If the function does return values, it takes a
				comma-separated list for each expression returned.

				```zscript
				// Use of `return` in various contexts.

				void ReturnsNothing()
				{
					// Exit early if "m_Thing" isn't 50.
					if(m_Thing != 50)
						return;

					DoThing(m_Thing);
				}

				int ReturnsInt()
				{
					// "m_Thing" is 50, so return 50.
					if(m_Thing == 50)
						return 50;

					// Must have a return, eventually.
					return 0;
				}

				int, int ReturnsTwoInts()
				{
					// Returns 1 and 2.
					return 1, 2;
				}
				```"#
			}],
		),
		Syn::KwSizeOf => (
			"sizeOf",
			&[indoc::indoc! {
				"Can be used as the left side of a unary expression to get the
				size of that expression's output type in bytes."
			}],
		),
		Syn::KwStruct => (
			"struct",
			&[indoc::indoc! {
				"A structure is an object type that does not inherit from Object
				and is not always — though occasionally is — a reference type, unlike classes.

				```zscript
				// Simple structure.
				struct MyCoolStructure
				{
					int X;
					int Y;
					int Z;
				}
				```",
			}],
		),
		Syn::KwSwitch => (
			"switch",
			&[indoc::indoc! {
				r#"A switch statement takes an expression of integer or name type
					and selects a labeled statement to run.
					They work the same as in C, C++, and ACS.

					```zscript
					// A switch demonstrating fall-through and default cases.
					switch(a)
					{
					case 500:
						Console.PrintF("a is 500");
						break;
					case 501:
						Console.PrintF("a is 501");
						// Falls through to the next case.
					case 502:
						Console.PrintF("a is 501 or 502");
						break;
					default:
						Console.PrintF("not sure what a is!");
						// "break" is implied here.
					}
					```"#
			}],
		),
		Syn::KwTransient => (
			"transient",
			&[indoc::indoc! {
				"Member fields marked `transient` are not saved into save games.
				Required for unserializable objects and recommended for UI context objects."
			}],
		),
		Syn::KwUntil => ("until", &[DOCS_WHILE_UNTIL]),
		Syn::KwWhile => ("while", &[DOCS_WHILE_UNTIL]),
		// TODO: The rest!
		_ => return None,
	};

	let mut ret = vec![];

	ret.push(MarkedString::LanguageString(LanguageString {
		language: "zscript".to_string(),
		value: kw.to_string(),
	}));

	for c in contents {
		ret.push(MarkedString::String(c.to_string()));
	}

	#[cfg(debug_assertions)]
	tracing::debug!("Hover info hit: keyword");

	Some(HoverContents::Array(ret))
}
