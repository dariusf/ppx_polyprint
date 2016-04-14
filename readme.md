
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
=> (false, 1)
```

This may seem magical, but it's actually being replaced at compile-time with the appropriate monomorphic printer.

`show` returns a string representation of a value.

```ocaml
print_endline (show 1)
=> 1
```

`to_string` and `string_of` are aliases for `show`.

`debug` is like `print`, but outputs a representation of its argument in addition to its value.

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

Here's the default output of `fact 5` with `[@@tracerec]`, which tracks all recursive calls.

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

Let-bindings in expression and structure context are annotated differently.

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

`trace` and `tracerec` may be given parameters to customise how tracing is carried out. This forms a small DSL made of a sequence of expressions, each of which corresponding to an item or feature below. Multiple features can be enabled by separating them with `;`. For example, `[@trace Custom; x, y]`.

#### Selectively tracing parameter values

Sometimes the values of only certain parameters are interesting. Which these are may be specified using a tuple of identifiers.

```ocaml
(* Only the first two arguments matter *)
let triple x y z = x + y + 1
  [@@trace x, y]
```

If there no parameters specified, all parameters will be included, otherwise only the listed ones will be.

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

Refer to the signature of `TraceConfig` for API details and documentation on what may be tweaked. A high-level interface is available (for changing things like printing format), but lower-level control is also possible, allowing the customisation of details like where output goes, what information is added to recursive calls, etc. Examples of this may be found [here](examples/configs/src/config.ml).

If you are using a build tool which automatically discovers module dependencies (`ocamldep`/`ocamlbuild`) and the configuration module is in another file, it needs to be referenced from somewhere other than the `[@trace]` annotation for it to be picked up as a dependency. Here's an easy way to ensure this.

```ocaml
(* file: config.ml *)
open PolyPrint

module Custom : TraceConfig = struct
  include DefaultTraceConfig
end
```

```ocaml
(* wherever else Custom is passed to [@trace] *)
open Config

let plus x y = x + y
  [@@trace Custom]
```

#### The default annotation

If we are using the same configuration module across a file, it may be useful to specify it in one place, instead of in every annotation. The default annotation helps with this.

```ocaml
[@@@polyprint Custom]
```

This will turn every `[@@trace]` annotation within a compilation unit into `[@@trace Custom]`.

The default annotation supplies the module that call sites will use, if none is specified. Details [here](#call-site-information).

#### Call site information

The `call[n]` functions in `TraceConfig` allow us to intercept calls to traced functions. We may use this to record call site information (for example, where traced functions are called from), or even to modify semantics temporarily for debugging.

Call sites are identified via a type system extension. Annotated functions are given a special internal type, `Traced`, which is isomorphic to a function arrow; it serves only to track whether a function is being traced. At call sites, `Traced` functions will be wrapped with `call[n]`.

Traced functions require a configuration module to invoke `call[n]` on. The easiest way to specify this is via the default annotation.

```ocaml
let id x = x
  [@@traced]

[@@@polyprint Custom]

id x
```

A more granular alternative is to annotate the *application* of a traced function.

```ocaml
id x [@polyprint Custom]
```

Recursive, annotated functions invoke a `Traced` function as well (themselves!), and so their definitions may also be given a configuration module (only possible via the default annotation for now).

The `call[n]` functions are given the file and line number in/at which the application occurred, to provide more information in huge logs. In the case of partial application, this is the point at which the function was *first* applied.

**Caveat**: this feature currently does not interact well with Merlin, as Merlin's typechecker does not recognise `Traced` as a function type. The simplest way to get around this is to disable tracing for functions when it isn't needed (by commenting out the annotation).

## Installation

`ppx_polyprint` is distributed as a findlib package. It is not yet on opam as it is still unstable, but it should build without much fuss if you want to try it out.

- Clone the repository
- Install dependencies (manually, for now)

```
opam install ppx_tools typpx alcotest ppx_deriving
```

- `make test` to check that everything works
- `make up` to build and pin the package
- Try building the examples with `make`

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

This project implements a specialised form of ad hoc polymorphism.

It's difficult to implement a function with type `'a. 'a -> string` meaningfully without constraints on what `'a` may be. Thus, we simply default to something reasonable in polymorphic contexts.

```ocaml
let stringify x =
  (* x has type 'a *)
  PolyPrint.to_string x

stringify 1
=> <polymorphic>
```

A more sophisticated scheme could be used (abstracting over polymorphic functions and lifting them out, to be specialised at call sites), but is not implemented.

Typeclasses (see [ppx_implicits](https://bitbucket.org/camlspotter/ppx_implicits)) solve this problem in general. This is a specialisation of typeclasses that happens to be sufficient for printf debugging, and avoids the complexities of passing dictionaries in OCaml. Hopefully, modular implicits will someday obviate the need for this.

This library is built on [TyPPX](https://bitbucket.org/camlspotter/typpx) and inherits its caveats:

- It cannot be used in the toplevel, or in environments where program phrases are processed in isolation, as it requires access to the full typing environment of a program to insert printers.
- It is sensitive to the order in which other ppx preprocessors are applied, as it requires a full picture of the typing environment. For example, it should be applied only after something like ppx_deriving, because references to ppx_deriving-generated functions would fail to typecheck if the latter hasn't already run.

