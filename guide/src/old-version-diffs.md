# Differences from pewpew 0.5.x config

## Enums

Primarily, externally tagged enumerations now use the YAML `!type` syntax rather than simply being
a single-keyed map.

This applies to:

 - `load_pattern` segments
 - `providers` types
 - file provider `format`
 - other places as well

### Examples

#### Old:

```yaml
load_pattern:
  - linear:
    to: 100%
    over: 5m
  - linear:
    to: 75%
    over: 2h
```

#### New:

```yaml
load_pattern:
  - !linear
    to: 100%
    over: 5m
  - !linear
    to: 75%
    over: 2h
```

## CSV Parameters

For `!file` provider types, instead of a separate `csv` subtable that has no effect unless `format` is
set accordingly, these values are now subvalues of the `!csv` variant directly.

### Examples:

#### Old:

```yaml
path: "file.csv"
format: csv
csv:
  headers: true
```

```yaml
path: "file.txt"
format: line
csv:
  # does nothing
  delimiter: "^"
```

#### New:

```yaml
path: "file.csv"
format: !csv
  headers: true
```

```yaml
path: "file.txt"
format: !line
# no place to put csv parameters
```
