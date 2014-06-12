# Scijors

A powerful and blazingly fast templating language and library for Clojure. With teeth.

![Templating language](https://upload.wikimedia.org/wikipedia/commons/4/43/Zackenschere.jpg "It has teeth")

 Your `project.clj` dependency:
```clojure
[scijors "0.1.4"]
```
or find it in [Clojars](https://clojars.org/scijors).

Also check out [Cornet](http://github.com/cosmi/cornet) - asset pipeline.

## Features

Scijors offers a well known templating paradigm, modelled on Python's Django's templating language and PHP's twig.
It is also inspired by earlier Clojure library, [Clabango](https://github.com/danlarkin/clabango) (which has been superseded by [Selmer](https://github.com/yogthos/Selmer) in the meantime).

Features:
* Complete expression syntax (eg. `f(x + 4, (v[4] + 1) * 2, #{1, 2, 3}`) - thanks to [Instaparse](https://github.com/Engelberg/instaparse) - an awesome parser generator for Clojure
* Blazingly fast - the templates are compiled into functions with closures.
* No macros or evals - all is done in functions, which a) saves your PermGen space, b) improves debugging capabilities and c) prepares `Scijors` to be ported to ClojureScript
* Has debug mode - in which the template will be recompiled when necessary (ie. when one of the files that the template depends on changes)
* Is extensible - you can define your own tags and filters and you can even define the grammar for them!
* You can call any function in Clojure from the template.
* Allows for template libraries (thanks to `load` tag)
* Has polymorphism (`multiblock`s inspired by Clojure's multimethods)
* (coming SOON: it will be easily [Turing Complete](https://en.wikipedia.org/wiki/Turing_completeness))

## Usage

### Quick start

In your project.clj dependencies add:

```clojure
[scijors "0.1.4"]
```

Put your template files on classpath in directory `templates`. In most cases that means `%PROJECT_PATH%/resources/templates`.

To use a template, load it first (`(scijors.core/load-template "example.html")` - assuming that your template exists in `%PROJECT_PATH%/resources/templates/example.html`). `load-template` will return a function that takes single argument - hashmap of arguments

To render just a single block, not the whole template, you can pass the block's name as second optional argument.

### Practical usage:

Most probably you will want to have some default data and other settings in your templates. To do so, create a file like this:
```clojure
(ns your-project-name.render
  (:require [scijors.core :as scijors]))


;; this will set the default mode - by default scijors will check for each dependency if it changed.
(scijors/set-reload-on-change! (System/getProperty "devmode"))

(defn enrich-input [input]
  (merge {:some-value "that you always want to inject into the template"
          :for-example "you might want to have the current user's data:"
          :user (get-current-user)}
          input))

(defn create-template [path]
  (let [template (scijors/create-template path :devmode DEVMODE)]
    (fn
      ([data]
         (template (enrich-data data)))
      ([data block]
         (template (enrich-data data) block)))))
         
(defmacro deftemplate [sym path]
  `(def ~sym (create-template ~path)))
```

### Files layout

By default, templates are on classpath in directory `templates`. You can change this by passing a different `loader` to `(scijors/create-template path :loader loader)`. Loader should be a function that for a path string starting with '/' returns java.io.Resource or java.io.File. For example:

```cloujure
(defn example-loader 
  [^String s]
  (or 
    (let [file (clojure.java.io/file (str "/filesystem/template/storage" s))]
      (when (.isFile file) file))
    (clojure.java.io/resource
     (str "templates" s))))
```
   
### Automatic template refresh:

By default, scijors template will check the files used by it every time it is generated. It is most of the time quite fast, but in production environment you might want to skip the check. To do so, either use `(scijors.core/set-reload-on-change! false)` or pass an extra argument to `load-template`: `(scijors.core/load-template "example.html" :mode :prod)`.


### Lazy template loading:

By default, scijors template will load the template in place. Sometimes it would be better to load it on demand, for example when you want to decrease the loading time of your app or to be able to access symbols from namespaces that haven't yet been required. To do so, pass an argument to `load-template`: `(scijors.core/load-template "example.html" :lazy true)`.


## Language syntax

The templates are build with three main things interleaved: text, tags and expressions. Text is just a text, Tags are parts wrapped in `{% %}`, expressions are wrapped with `{{ }}`. 

Simple example:
```
<html>
<head><title>Your webapp - {{item.name}}</title></head>
<body>
  {# This is a comment #}
    <p> Hello, {{user.name}}!</p>
    <h1>{{item.name}}</h1>
    <p style="color:{{item.description-color}}">{{item.description}}</p>
    <h2>Elements</h2>
  {% for element index i in item.elements %}
      <h3>Element no 
      {% if i % 15 == 0 %}Fizz Buzz
      {% else if i % 3 == 0 %}Fizz
      {% else if i % 5 == 0 %}Buzz
      {% else %}{{i + 1}}
      {% end %}
      </h3>
      <p>{{ element.description }}</p>
  {% end %}
</body>
</html>
```

### Expressions
Expressions are wrapped with `{{ }}`. The expression value will be pasted into the rendered code (if it is not string it will be converted by `clojure.core/str` function). For security reasons, all things that are not marked as safe, will be escaped (currently only HTML escaping is implemented). To mark expression as safe, use Filter (documented below)

#### Constants and variables
Scijors supports following constants:
```
true
false
nil or null
1
-2
1.5
-2.5
2/3
-3/4
:a
:some.namespace/a
"aaa\"aa"
clojure.string/trim
\clojure.core/+\ {# syntax for symbols using uncommon characters #}
/get-in {# equivalent to clojure.core/get-in #}
[1, 2, 3]
{:a : 1, :b : 2, 3 : 4}
#{1, 2, 3}
```

The variables scope is passed to the template as input hash-map. `x` is equivalent to (get input-scope :x). To get the whole input-scope, use `_global` special variable.

#### Operators

Expressions syntax is based on non-lisp contemporary languages:
#### Logical operators
```
x or y
x||y
x and y
x&&y
not y
!y
```
The spaces around the operators are _required_ when present in the snippet above.

Those operators are based on Clojure's - `or` will return the first true value (nil/false otherwise), `and` will return the last value if both are true or nil/false otherwise. `not` and `!` are equivalent.

You can construct ternary operator (`x?y:z` in Java or C++): `x and y or z`


#### Comparison operators
```
x == y
x != y
x === y
x !== y
x (< | <= | > | >=) y
```
The spaces around the operators are _recommended_.

The `==` and `!=` operators are just `clojure.core/=` and `clojure.core/not=`. 

The others are numerical operators, in particular `===` is `clojure.core/==` and `!==` is its negation.

#### Concatenation
```
x & "a"
```

This will concatenate the values by `clojure.core/str` - the non-string values will be first changed to strings.

#### Numerical operators
```
x+y 
x - y
x*y
x / y {# clojure.core//#}
x // y {# clojure.core/quot#}
x%y {#clojure.core/mod#}
-x
```

The spaces around the operators are _required_ when present in the snippet above and otherwise _recommended_.

#### Dot notation:
```
a.b.c
```
The above expression is equivalent to `(clojure.core/get-in a [:b :c]).

#### Special syntax:

```
@x - deref operator
```
This operator will just call `clojure.core/deref` on the value.

```
x(1, 2)
```
This will call treat x as clojure function and call with arguments in the parentheses

```
x[:a, :b]
```
This will call `(clojure.core/get-in x [:a :b]).

```
(x + y) * 2
```
Parentheses work as in most other non-lisp languages.


### Filters
Filters are function-like elements that will help with rendering the expression values

The most common is the `safe` filter. Used like this: `{{ expr | safe }}` will prevent escaping the value of `expr`. You can also use filters inside an expression, by wrapping it in parentheses `"username: " & (user.name | safe)` (of course this example doesn't make sense :) )

#### `safe` and `unsafe` filter
```
{{ expr | safe }}
( expr | safe)
```
Safe filter marks expression as safe and will prevent it from being escaped.

If for some reason you want to force something to be escaped, use `unsafe` filter:
``` 
{{expr | unsafe}}
```

#### `escape` filter
```
{{ expr | escape html }}
{{ expr | esc html }} {# alternative syntax#}
```
This will force expression to be html-escaped. As a side effect it marks the output as safe to prevent secondary 



#### `format` filter
```
{{ expr | format "value=%.4f" }}
```
This will call `(format "value=%.4f" expr)`. 

### Tags

Tags are wrapped with `{% %}`, but `{% }` is also supported. Usually the tag will end with`{%end%}`, but  `{%endtagname%}` is also supported. Spaces around `{% %}` are optional.

#### `if` tag
Examples:
```
{% if expr%}content{% end %}
{%if /count(items) == 1%}item{%else%}items{% end %}
```

#### `for` tag
```
{% for x index i in expr%}
{{i}}. {{x}}
{% interpose %}
<hr>
{% else %}
no content
{% end %}
```
`index i` is optional. When looping over map, `i` will get values of sucessive keys and x will hold values by that keys. In case of vector or list, `i` will be numerical index (starting from 0).

`interpose` part is optional. Its content will be rendered after each run of for loop, except the last.

`else` (equivalent: `empty`) part is optional and will be rendered if the looped over expression is empty.

#### `switch` tag
```
{% switch i.type case :image %}
Image
{% case :audio %}
Audio
{% else %}
Unknown
{% end %}
```
For convenience `{% switch i.type %} {%case :image %}` is also allowed as long as between and case tags there are only whitespace characters (they will be ignored).
`else` part is optional.

#### `with` tag

```
{% with [only] x = 7, y = 8 %}
do something
{% end %}
```
For compatibility reasons `with` keyword can be replaced by `let` (`{% let x = 7, y = 8 ....`).

Binds values to given expressions in the context surrounded by this tag. 
If the `only` keyword is not ommited, all the other variables are removed from the scope.

#### `id` tag

```{% id %}``` - renders number that is unique to file.

### Blocks and files

#### `extends` tag
```
{% extends "filename.html" %}
```
Current file is not rendered, instead "filename.html" is. The block defined in the current file will be registered and will override blocks in "filename.html".

#### `load` tag
```
{% load "mixin.html" %}
```
Loads blocks from "mixin.html".

#### `include` tag
```
{% include "subtemplate.html"%}
```
Renders "subtemplate.html" in an encapsulated context. The subtemplate will not see the current blocks. 


#### `callblock` and `defblock` tag
```
{% defblock content %}
Default content
{% end %}
{% callblock content %}
```

`defblock` registers a block, overriding an old one if it was registered already.
`callblock` renders a registered block. In this case it will render `Default content`, unless the `content` block was overriden somewhere else.

#### `block` tag
```
{% block content %}
Default content
{% end %}
```
is equivalent to:
```
{% defblock content %}
Default content
{% end %}
{% callblock content %}
```

#### `multiblock` and `defmultiblock` tag

```
{% defmultiblock content on content.type %}
Default content
{% end %}
{% defmultiblock content extends :image %}
Image: {{i.url}}
{% end %}
{% defmultiblock content extends :video %}
Video: {{i.url}}
{% end %}
{% callblock content %}
```

The `{% defmultiblock <name> on <expr> %}` syntax will register new multiblock and its default content.
Successive `{% defmultiblock <name> extends <const> %} will add new dispatch values and their content.
On `{% callblock <name> %}` the multiblock will be rendered: `<expr>` will be evaluated and if matches any of the registered dispatch values, its content will be rendered, otherwise the default one.

## FAQ

### "I need a feature not yet implemented. Can I contribute?"

Yes, please!

## License

Copyright © 2013- Marcin Skotniczny & [Software Mansion](http://swmansion.com)

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
