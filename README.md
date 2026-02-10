an actual programming language for ai2 since dragging blocks is a pile of dogshit

-----

# The language

> [!TIP]
> To get an idea of the language, read `sample.ail` in the project directory

## `aicc` command

> [!WARNING]
> WARNING: ERROR MESSAGES ARE HORRIBLE, DO NOT RELY ON THOSE TO FIX MISTAKES.

### Usage

```
./aicc [-p] srcfile outfile
```

Compiles source `srcfile` into xml or if `-p` is specified, output into a bunch of pngs.

### Notes

**You most likely want to use `-p` to use outputs for app inventor, it will become the default in a future version**
  
If `-p` is specified, `outfile` becomes a prefix for generated output pngs (`outfile0.png`, `outfile1.png`, ...)

## Basic reference:

### Literals
- boolean

    ```
    true
    false
    ```

- string

    ```
    "this is a string"
    "escape \" for double quotes"
    'escape \' for single quotes'
    ```

- number

    ```
    12
    -1
    0xFF
    0b10011010
    067
    ```

- color

    ```
    #FFFFFF
    #(0xFF, 255, 0b11111111)
    #(red, green, blue, alpha)
    ```

- array

    ```
    [1, "2", [3]]
    ```

- dictionary

    ```
    {"hello": "world", 12: 34}
    ```


### Variables

```
global global_var = "skibidi";
/*
 * to use global_var, always prefix it with "global "
 * eg. replace(global global_var, "skibidi", "gyatdula")
 */

let width = 1,
    height = 2 in {
    $Label:MyLabel.Width = width;
    $Label:MyLabel.Height = height;
};

$Label:MyLabel.Text = let text = "hello" in {
    replace(text, "lo", "p me") // <--- no semicolon, this is the return value
};
```

### Control flow
- `if` statement

    ```
    if cond1 {
        /* cond1 is true */
    } else if cond2 {
        /* cond2 is true */
    } else {
        /* otherwise, ... */
    }
    ```

- `while` loop

    ```
    while cond {
        /* runs while cond is true */
    }
    ```

- `for` loop
    - range-for loop

        ```
        for i from 1 to 5 by 2 {
            /* i goes from 1 to 5, incrementing by 2 every step */
        }
        ```

    - iterative loop

        ```
        for i in my_list {
            /* iterate over members of my_list */
        }
        ```

    - dictionary-based iterative loop

        ```
        for key, value in my_dict {
            /* iterate over keys and values of my_dict */
        }
        ```

### Event handler

> [!IMPORTANT]
> Unfortunately, you have to consult app inventor block editor for the names of arguments. Arguments will be documented in a future release.

```
when $Button:MyButton.Click {
    /* clicked */
}
```

### Functions

```
/* "func"tions must return a value */
func foo() {
    69 // <--- return value, no semicolon
}

/* "proc"edures don't return a value */
proc bar(baz) {
    do_stuff(baz + 1);
}
```

## Builtin functions

### Generic

- `proc append(obj1, obj2)`
- `func contains(haystack, needle)`
- `func copy(obj)`
- `func find(haystack, needle)`
- `func len(obj)`
- `proc remove(obj, prop)`
- `func reverse(obj)`

### Numerical

- `func abs(num)`
- `func arccos(num)`
- `func arcsin(num)`
- `func arctan(num)`
- `func atan2(y, x)`
- `func ceil(num)`
- `func cos(num)`
- `func exp(num)`
- `func floor(num)`
- `func log(num)`
- `func round(num)`
- `func sin(num)`
- `func sqrt(num)`
- `func tan(num)`

### Strings

- `func contains_all(text, substrings)`
- `func lexcmp(text1, [eq | ne | lt | gt], text2)`
- `func replace(text, search, replacement)`
- `func replace_multi(text, mappings, [max_munch | ordered])`
- `func segment(text, start, length)`
- `func split(text, separator)`
- `func split_once(text, separator)`
- `func to_lower(text)`
- `func to_upper(text)`
- `func trim(text)`

### Arrays

- `func filter(arr, var, func)`
- `proc insert(arr, idx, item)`
- `func join(arr, sep)`
- `func lookup_pairs(arr, key, default)`
- `func map(arr, var, func)`
- `func max(arr)`
- `func min(arr)`
- `func reduce(arr, init, acc, cur, func)`
- `func slice(arr, idx1, idx2)`
- `func sort(arr)` | `func sort(arr, item, func)` | `func sort(arr, item1, item2, func)`

### Dictionaries

- `func keys(dict)`
- `func path_all()`

> [!NOTE]
> NOTE: when using `path_get`, `path_set` or `path_walk`, ensure array indices are 1-indexed

- `func path_get(dict, path, default)`
- `proc path_set(obj, path, value)`
- `func path_walk(dict, path)`
- `func to_dict(arr)`
- `func to_pairs(dict)`
- `func values(dict)`
