# QSCheck


[![Maturity Level](https://img.shields.io/badge/Maturity%20Level-ML--1-orange)](https://img.shields.io/badge/Maturity%20Level-ML--1-orange)

QSCheck is a library to perform checks on values, data, and content. It's built on top of assertthat
and each routine can be used as assertions to stop execution. They can also be used to
perform decisions (e.g. in if conditions).

There is some overlap between qscheck and assertthat, as well as many similar
checker packages (e.g. checkr). The main difference is that we have full
control and therefore more flexibility on the conditions we want to check
without having to rely on external packages. Moreover, the syntax is meant to
be more readable and in line with AstraZeneca best practices.

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

# Contacts

- Author: Stefano Borini <stefano.borini@astrazeneca.com>
- Maintainer: Stefano Borini <stefano.borini@astrazeneca.com>

