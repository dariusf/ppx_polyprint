
# ppx_polyprint [![Build Status](https://travis-ci.org/dariusf/ppx_polyprint.svg)](https://travis-ci.org/dariusf/ppx_polyprint)

A small library for convenient printf debugging and function tracing.

Check out the [installation instructions](#installation).

## Inspecting Values

The API is exceedingly simple. Everything is contained in the module `PolyPrint`, which is suitable for opening at the top of a file.

The eponymous function `print` is polymorphic in its only argument and prints a string representation of it to stdout.

```ocaml
open PolyPrint

print 1
=> 1

print "true"
=> true

print (false, 1)
=> "(false, 1)"
```

This may seem magical, but it's actually being replaced at compile-time with the appropriate monomorphic printer.

There's `show` (and its aliases `to_string` and `string_of`), which returns a string representation of a value:

```ocaml
print_endline (show 1)
=> 1
```

There's also `debug`, which is like `print`, but outputs a representation of the expression it was passed.

```ocaml
let a = 1 in
print_endline (debug (a + a * 2))
=> a + a * 2: 3
```

## Function Tracing

This library also helps generate the boilerplate for function-level tracing. Let-bound functions annotated with `[@@trace]` or `[@@tracerec]` will have their inputs and output printed to stdout.

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

### Configuration

Options may be passed to `trace` and `tracerec` to customise how tracing is performed. This forms a small DSL made of a sequence of expressions, each of which corresponding to an item or feature below. Multiple features can be enabled by separating them with `;`. For example, `[@trace Custom; x, y]`.

#### Selectively tracing parameter values

Sometimes the values of only certain parameters are interesting. Which these are may be specified using a tuple of identifiers.

```ocaml
(* Only the first two arguments matter *)
let triple x y z = (x, y, 1)
  [@@trace x, y]
```

If the list is empty, all parameters will be included, otherwise only the parameters listed will be.

Options can be associated with each identifier, for example, to override the printer to use. This is done by suffixing each identifier with a record.

```ocaml
let triple x y z = (x, y, z)
  [@@trace x {printer = string_of_int}, y]
```

#### Hooking into the tracing process

Parts of the tracing process may be selectively overridden by passing a module (containing an object, for open recursion) to `trace`. Here's a typical way to do this.

```ocaml
open PolyPrint

module Custom : TraceConfig = struct
  class api = object (self)
    inherit DefaultTraceConfig.api

    (* Your customisations here *)
  end
  let act = new api
end

let plus x y = x + y
  [@@trace Custom]
```

Refer to the signature of `TraceConfig` for API details and documentation on what may be tweaked. A high-level interface is available (for changing things like printing format), but lower-level control is also possible, allowing the customisation of details like where output goes, what information is added to recursive calls, etc.

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

#### The default annotation (experimental)

*This feature is unstable. See [this](#persistent-state) for caveats arising from its use.*

If we are using the same tracing configuration across a project, it may be useful to specify it in one place, instead of every time we trace a function. The default annotation helps with this:

```ocaml
[@@@polyprint Custom]
```

This will turn every `[@@trace]` annotation into `[@@trace Custom]`.

#### Call site information (experimental)

*This feature is unstable. See [this](#persistent-state) for caveats arising from its use.*

The `calln` functions in `TraceConfig` allow us to intercept calls to traced functions. We may use this to record call site information (for example, where traced functions are called from), or even to modify semantics temporarily for debugging.

Currently, function equality is determined by local name: if a function is applied and has the same name as a traced function, it will be wrapped with `calln`. This is a rather crude strategy, so it is advisable to use this only with unique function names pending a more sophisticated analysis.

#### Persistent state

As certain features of this library require state to be carried across ppx invocations (and ppx was designed to be invoked on compilation units modularly, independently of other compilation units), the configuration environment of the library is persisted to a file.

This does not introduce correctness problems when run sequentially (as module dependencies are still captured by `ocamldep`), but parallel compilation may clobber the file. For this reason it is best not to use this library when parallel builds are enabled.

Another issue is that of the state becoming stale. This may actually be useful when files are not rebuilt and processed during compilation, allowing their configuration to persist, but makes it difficult to remove annotations without deleting the file and doing a full rebuild.

Given all these caveats, the design of this portion of the library should be considered very unstable.

## Installation

`ppx_polyprint` is distributed as a findlib package. It is not yet on opam as it is still unstable, but it should build without much fuss if you want to try it out.

- Clone the repository
- Install dependencies (manually, for now)

```
opam install ppx_tools typpx alcotest ppx_deriving
```

- `make test` to check that everything works.
- `make up` to build and pin the package.
- Try building the examples with `make`.

### Usage

- Include the runtime library.

```
# ocamlbuild
true: package(ppx_polyprint)

# otherwise, ensure this flag is passed to ocamlfind
-package ppx_polyprint
```

- Register the ppx processor with `-ppx`. This is a temporary workaround for [a Merlin limitation](https://github.com/the-lambda-church/merlin/issues/483#issuecomment-182274832) and will be removed when the next version of Merlin is released.

If you using `ocamlbuild`, add the following flags to `myocamlbuild.ml`:

```ocaml
open Ocamlbuild_plugin

let () =
  dispatch (
    function
    | After_rules ->
      flag ["ocaml"; "compile"; "polyprint_native"] &
        S [A "-ppx"; A "$(ocamlfind query ppx_polyprint)/ppx_polyprint"]
    | _ -> ())
```

... then add the tag `polyprint_native` to your source files.

```
<*.{cmo,native,byte}>: polyprint_native
```

If you are not using `ocamlbuild`, pass the flag `-ppx $(ocamlfind query ppx_polyprint)/ppx_polyprint` to `ocamlfind`.

## Internals

This project implements a limited form of ad hoc polymorphism.

It's difficult to implement a function with type `'a. 'a -> string` meaningfully without constraints on what `'a` may be. Thus, we simply default to something reasonable in polymorphic contexts.

```ocaml
let stringify x =
  (* x has type 'a *)
  PolyPrint.to_string x

stringify 1
=> <polymorphic>
```

Typeclasses (see [ppx_implicits](https://bitbucket.org/camlspotter/ppx_implicits)) solve this problem in general. This is a specialisation of typeclasses that happens to be sufficient for printf debugging, and avoids the complexities of passing dictionaries in OCaml. Hopefully, modular implicits will someday obviate the need for this.

This library is built on [TyPPX](https://bitbucket.org/camlspotter/typpx) and inherits its caveats:

- It cannot be used in the toplevel, or in environments where program phrases are processed in isolation, as it requires access to the full typing environment of a program to insert printers.
- It is sensitive to the order in which other ppx preprocessors are applied, as it requires a full picture of the typing environment. For example, it should be applied only after something like ppx_deriving, because references to ppx_deriving-generated functions would fail to typecheck if the latter hasn't already run.

