# Debugging in {rtables} and Beyond

## Debugging

This is a short and non-comprehensive guide to debugging `rtables`.
Regardless, it is to be considered valid for personal use at your
discretion.

#### Coding in Practice

- It is easy to read and find problems
- It is not clever if it is impossible to debug

#### Some Definitions

- **Coding Error** - Code does not do what you intended -\> Bug in the
  punch card
- **Unexpected Input** - Defensive programming FAIL FAST FAIL LOUD
  (FFFL) -\> useful and not too time consuming
- **Bug in Dependency** -\> never use dependencies if you can!

#### Considerations About FFFL

Errors should be as close as possible to the source. For example, bad
inputs should be found very early. The worst possible example is a
software that is silently giving incorrect results. Common things that
we can catch early are missing values, column `length == 0`, or
`length > 1`.

#### General Suggestions

- Robust code base does not attempt doing possibly problematic
  operations.
- Read Error Messages
- `debugcall` you can add the signature (formals)
- `trace` is powerful because you can add the reaction
- `tracer` is very good and precise to find where it happens

`options(error = recover)` is one of the best tools to debug at it is a
core tool when developing that allows you to step into any point of the
function call sequence.

`dump.frames` and `debugger`: it saves it to a file or an object and
then you call debugger to step in it as you did recover.

#### `warn` Global Option

- `<0` ignored
- `0` top level function call
- `1` immediately as they occur
- `>=2` throws errors

`<<-` for `recover` or `debugger` gives it to the global environment

#### direct-modification techniques

- PRINT / CAT is always a low level debugging that can be used. It is
  helpful for server jobs where maybe only terminal or console output is
  available and no [`browser()`](https://rdrr.io/r/base/browser.html)
  can be used. For example, you can print the position or state of a
  function at a certain point until you find the break point.
- comment blocks -\> does not work with pipes (you can use
  [`identity()`](https://rdrr.io/r/base/identity.html) it is a step that
  does nothing but does not break the pipes)
- [`browser()`](https://rdrr.io/r/base/browser.html) bombing

#### Regression Tests

Almost every bug should become a regression test.

#### Debugging with Pipes

- Pipes are better to write code but horrible to debug
- T in pipe `%T>%` does print it midway
- [`debug_pipe()`](https://magrittr.tidyverse.org/reference/debug_pipe.html)
  -\> it is like the T pipe going into browser()

#### Shiny Debugging

More difficult due to reactivity.

#### General Suggestion

DO NOT BE CLEVER WITH CODE - ONLY IF YOU HAVE TO, CLEVER IS ALSO
SUBJECTIVE AND IT WILL CHANGE WITH TIME.

## Debugging in `rtables`

We invite the smart developer to use the provided examples as a way to
get an “interactive” and dynamic view of the internal algorithms as they
are routinely executed when constructing tables with `rtables`. This is
achieved by using [`browser()`](https://rdrr.io/r/base/browser.html) and
[`debugonce()`](https://rdrr.io/r/base/debug.html) on internal and
exported functions (`rtables:::` or `rtables::`), as we will see in a
moment. We invite you to continuously and autonomously explore the
multiple `S3` and `S4` objects that constitute the complexity and power
of `rtables`. To do so, we will use the following functions:

- `methods(generic_function)`: This function lists the methods that are
  available for a generic function. Specifically for `S4` generic
  functions, `showMethods(generic_function)` gives more detailed
  information about each method (e.g. inheritance).
- `class(object)`: This function returns the class of an object. If the
  class is not one of the built-in classes in R, you can use this
  information to search for its documentation and examples.
  [`help(class)`](https://rdrr.io/r/base/class.html) may be informative
  as it will call the documentation of the specific class. Similarly,
  the `?` operator will bring up the documentation page for different
  `S4` methods. For `S3` methods it is necessary to postfix the class
  name with a dot
  (e.g. [`?summary.lm`](https://rdrr.io/r/stats/summary.lm.html)).
- `getClass(class)`: This describes the type of class in a compact way,
  the slots that it has, and the relationships that it may have with the
  other classes that may inherit from or be inherited by it. With
  `getClass(object)` we can see to which values the slots of the object
  are assigned. It is possible to use `str(object, max.level = 2)` to
  see less formal and more compact descriptions of the slots, but it may
  be problematic when there are one or more objects in the class slots.
  Hence, the maximum number of levels should always be limited to 2 or 3
  (`max.level = 2`). Similarly,
  [`attributes()`](https://rdrr.io/r/base/attributes.html) can be used
  to retrieve some information, but we need to remember that storing
  important variables in this way is not encouraged. Information
  regarding the type of class can be retrieved with
  [`mode()`](https://rdrr.io/r/base/mode.html) and indirectly by
  [`summary()`](https://rdrr.io/r/base/summary.html) and `is.S4()`.
  \*`getAnywhere(function)` is very useful to get the source code of
  internal functions and specific generics. It works very well with `S3`
  methods, and will display the relevant namespace for each of the
  methods found. Similarly, `getMethod(S4_generic, S4_class)` can
  retrieve the source code of class-specific `S4` methods.
- `eval(debugcall(generic_function(obj)))`: this is a very useful way to
  browse a `S4` method, specifically for a defined object, without
  having to manually insert
  [`browser()`](https://rdrr.io/r/base/browser.html) into the code. It
  is also possible to do similarly with R \> 3.4.0 where `debug*()`
  calls can have the triggering signature (class) specified. Both of
  these are modern and simplified wrappers of the tracing function
  [`trace()`](https://rdrr.io/r/base/trace.html).
