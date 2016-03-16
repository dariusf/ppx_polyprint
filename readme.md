
# ppx_polyprint

A small library for convenient printf debugging and function tracing.

[Installation instructions](#installation).

## Inspecting Values

The API is exceedingly simple. Everything is contained in the module `PolyPrint`, which is suitable for opening at the top of a file.

The eponymous function `print` is polymorphic in its only argument and prints a string representation of it to stdout.

```ocaml
open PolyPrint

print 1
=> 1

print "true"
=> true
```

This may seem magical, but it's actually being replaced at compile-time with the appropriate monomorphic printer.

Also available are `to_string`, `string_of`, and `show` (aliases of each other), which return a string representation of a value:

```ocaml
print_endline (show 1)
=> 1
```

There's also `debug`, which is like `print`, but additionally outputs a representation of the expression it was passed.

```ocaml
let a = 1 in
print_endline (debug (a + a * 2))
=> a + a * 2: 3
```

## Function Tracing

This library is also helps generate the boilerplate for function-level tracing. Let-bound functions annotated with `[@@trace]` or `[@@tracerec]` will have their inputs and output printed to stdout.

```ocaml
let rec fact n =
  if n = 0 then 1
  else n * fact (n - 1)
  [@@tracerec]
```

Here's the default output of `fact 5` with `[@@tracerec]`, which tracks all recursive calls:

```
fact <- n = 5
fact <- n = 4
fact <- n = 3
fact <- n = 2
fact <- n = 1
fact <- n = 0
fact -> 1
fact -> 1
fact -> 2
fact -> 6
fact -> 24
fact -> 120
```

The magic functions from earlier will be used to automatically figure out printer types.

For some type `Module.t`, the printer that will be used is `Module.show_t`, following the conventions used by ppx_deriving. If this heuristic isn't sufficient, the printer name can be supplied manually (see below).

Let-bindings in expression and structure context are annotated differently:

```ocaml
(* structure item *)
let plus x y = x + y
  [@@trace]

(* expression *)
let [@trace] plus x y = x + y in ...
```

Apart from using `@@` instead of `@`, structure annotations apply only to one specific binding when there are several (as opposed to expression annotations, which apply to all bindings). In the following snippet, both `plus` and `minus` will be traced.

```ocaml
(* structure bindings may be annotated separately *)
let plus x y = x + y
  [@@trace]
and minus x y = x - y
  [@@trace]

(* expression bindings may not *)
let [@trace] plus x y = x + y
and minus x y = x - y
```

### Tweaks

Options can be passed to `trace` and `tracerec` to customise how tracing is performed. This forms a small DSL, which is made of a sequence of expressions, each of which corresponds to an item or feature below. Multiple features can be enabled by separating them with `;`. For example, `[@trace Custom; x, y]`.

#### Hooking into the tracing process

A module can be used to selectively override parts of the tracing process. Here's a typical way to use it.

```ocaml
open PolyPrint

module Custom : TraceConfig = struct
  include DefaultTraceConfig

  (* Your customisations here *)
end

let plus x y = x + y
  [@@trace Custom]
```

Refer to the signature of `TraceConfig` for API details and documentation on what may be tweaked. A high-level interface is available (for changing things like the printing format), but low-level control is also possible, allowing the customisation of details like where output goes, adding more information to recursive calls, etc.

If you are using a build tool which automatically discovers module dependencies (`ocamldep`/`ocamlbuild`) and the configuration module is in another file, it needs to be referenced from somewhere other than the `[@trace]` annotation for it to be picked up as a dependency. Here's an easy way to ensure this.

```ocaml
(* file: config.ml *)
open PolyPrint

module Custom : TraceConfig = struct
  include DefaultTraceConfig
end

(* wherever else Custom is passed to [@trace] *)
open Config

let plus x y = x + y
  [@@trace Custom]
```

#### Selectively tracing parameter values

Sometimes the values of only certain parameters are interesting. Which these are may be specified using a tuple of identifiers.

```ocaml
(* Only the first two arguments matter *)
let triple x y z = (x, y, 1)
  [@@trace x, y]
```

If the list is empty, all parameters will be included, but otherwise only the parameters listed will be.

Options can be associated with each identifier, for example, to override the printer to use. This is done by suffixing each identifier with a record.

```ocaml
let triple x y z = (x, y, z)
  [@@trace x {printer = string_of_int}, y]
```

## Installation

`ppx_polyprint` is distributed via an opam package. It is not yet on opam itself as the API is still not stable, but a local copy may be pinned via `make up`, and it should work as per normal.

As it is not on opam, the dependencies have to be installed manually.

```
opam install ppx_tools typpx alcotest

# optional
opam install ppx_deriving
```

The runtime library must first be included.

```
# ocamlbuild
true: package(ppx_polyprint)

# otherwise, ensure this flag is passed to ocamlfind
-package ppx_polyprint
```

In addition, the ppx preprocessor must be manually registered using `-ppx`. This is a temporary workaround for [a Merlin limitation](https://github.com/the-lambda-church/merlin/issues/483#issuecomment-182274832) and will be removed when that is fixed.

With `ocamlbuild`, one way is to add the following flags to `myocamlbuild.ml`,

```ocaml
open Ocamlbuild_plugin

let () = dispatch (
  function
  | After_rules ->
    flag ["ocaml"; "compile"; "polyprint_byte"] &
      S [A "-ppx"; A "$(ocamlfind query ppx_polyprint)/ppx_polyprint.byte"];
    flag ["ocaml"; "compile"; "polyprint_native"] &
      S [A "-ppx"; A "$(ocamlfind query ppx_polyprint)/ppx_polyprint.native"]
  | _ -> ())
```

then add the tag `polyprint_native` or `polyprint_byte` to your source files.

```
<*.{cmo,native,byte}>: polyprint_native
```

If you are not using `ocamlbuild`, pass the flag `-ppx $(ocamlfind query ppx_polyprint)/ppx_polyprint.native` manually to `ocamlfind`.

## Internals

This project implements a limited form of ad hoc polymorphism. A function with type `'a. 'a -> string` cannot be written without at least some constraints on what `'a` may be, or some hints as to the functionality it supports.

```ocaml
let stringify x =
  (* x has type 'a *)
  PolyPrint.to_string x
in
stringify 1
=> <polymorphic>
```

Here we default to some string because no more information is available. In general, typeclasses solve this problem better. This is a specialisation of typeclasses that happens to be sufficient for printf debugging, and avoids the complexities of passing dictionaries in OCaml. Hopefully, modular implicits will someday obviate the need for this.

This ppx preprocessor is built on [TyPPX](https://bitbucket.org/camlspotter/typpx) and inherits its caveats.

- It cannot be used in the toplevel, or in similar environments where program phrases are processed in isolation, as it requires access to the full typing environment of a program to insert printers.
- It is also sensitive to the order in which other ppx preprocessors are applied, as it requires a full picture of the typing environment. For example, it should be applied only after ppx_deriving, because references to ppx_deriving-generated functions would fail to typecheck if the latter hasn't already run.

