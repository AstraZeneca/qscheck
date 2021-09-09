# QSCheck


![Status](https://github.com/AstraZeneca/qscheck/actions/workflows/r.yml/badge.svg)
[![Maturity Level](https://img.shields.io/badge/Maturity%20Level-Under%20Development-orange)](https://img.shields.io/badge/Maturity%20Level-Under%20Development-orange)

QSCheck is a library to perform checks on values, data, and content. It's built on top of assertthat
and each routine can be used as assertions to stop execution. They can also be used to
perform decisions (e.g. in if conditions).

There is some overlap between qscheck and assertthat, as well as many similar
checker packages (e.g. checkr). At AstraZeneca, we needed an internally developed
solution for this requirement, so that we could have more flexibility on the conditions
we want to check and the error messages we will deliver to the user, without having
to rely on the development cycle of external packages.

## When to use it

qscheck is meant to be used to ensure that routine calls receive the correct arguments,
with the correct lengths, keys, or values, and fail with a properly descriptive error
if the argument does not comply with the expectation.

You should generally use qscheck for those packages that need to be used directly by
users or by other developers.

qscheck can also be used to evaluate data and perform decisions. It is not only limited
to assertions. You can use qscheck in if conditions and perform different actions according
to the passed data. All check routines can only return a single TRUE or FALSE, and are therefore
safe to use in if conditions.

## Conventions for routine and argument names

The following conventions exist for the naming of the functions:

- Routine names that check for a single value (and not an array)
  always end in _value (e.g. is_string_value will be satisfied by "foo",
  but not by c("foo", "bar")). This convention of course is only applied for
  atomic entities that can be vectors.

Arguments have the following conventions. They should always be called as
keyworded arguments, rather than using their position, as new ones may be
added.

- exact_* specifies that the entry must be satisfied _exactly_.
  e.g. exact_names means that the names must be exactly as specified, no
  more, no less.
- required_* specifies that the entity must be satisfied at a minimum.
  e.g. required_names means that the specified names must be present,
  but other names may exist as well.
- allow_(na|null) specifies that the test will pass not only for the actual
  request, but also if the value is NA or NULL.
- allow_na_values means that the vector is checked for presence of NAs,
  and if present, a decision to pass the test or not will be taken accordingly.

## Example

Check if an entity is a data frame with only the specified colnames "foo" and "bar", or a NULL value.

```
    assertthat::assert_that(
        qscheck::is_data_frame(
            my_parameter, exact_colnames = c("foo", "bar"), allow_null = TRUE
        )
    )
```

In this case, the assertion will produce a meaningful error message, specifying
exactly what kind of information was expected in the failed assertion.

The same check can also be used in an if condition to branch execution:

```
    if (qscheck::is_data_frame(
            my_parameter, exact_colnames = c("foo", "bar"), allow_null = TRUE
       )
    )
```

There are many different types of check function, each with additional parameters
to diversify the expected properties. There are also check functions to combine checks together,
or perform more advanced semantic integrity on the passed values.

# Future development

qscheck is still in 0.x release. Its API is therefore not fully defined, and maintaining
backward compatibility, while attempted as much as possible, is not guaranteed. We focus on
an API that is communicative of intent and uniform.

# Documentation

There is currently no generated documentation yet, but it will be added soon as github pages.
For now, the best action is to check the assertions.R file.

# Contacts

- Author: Stefano Borini <stefano.borini@astrazeneca.com>
- Maintainer: Stefano Borini <stefano.borini@astrazeneca.com>
