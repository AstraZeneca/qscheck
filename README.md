# QSCheck


![Status](https://github.com/AstraZeneca/qscheck/actions/workflows/r.yml/badge.svg)
[![Maturity Level](https://img.shields.io/badge/Maturity%20Level-ML--1-orange)](https://img.shields.io/badge/Maturity%20Level-ML--1-orange)

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

