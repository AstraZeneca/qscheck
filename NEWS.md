# qscheck 0.25.0

- #103 Added validate function to handle S4 behavior.
- #101, #100, #99, #96 Added is_gte_value, is_gt_value, is_lte_value, is_lt_value
- #95 Added general function is_satisfying_funcs
- #93 Added function message() to extract error message.

# qscheck 0.24.0

- #87, #88 fixed tests to match internal identifiers

# qscheck 0.23.0

- #85 Add consequential directive to check simultaneous presence or absence of parameters
- #84 Deprecate allow_degenerate in is_binary_vector in favour of allow_uniform
- #83 Update function analysis code against modern code
- #82 Added motivation for value check
- #81 Add motivation for string value and vector
- #80 added motivation failure to is_one_of
- #79 Added motivation failure for S4 checks
- #78 Added motivation failure for s3 checks
- #77 Add motivation failure for real entities.
- #76 Add is_r6_class/instance motivation failure output
- #75 Added motivation failure to is_logical_value
- #74 Added motivation failure for lists
- #73 Add square matrix checks
- #72 Added is_matrix
- #71, #69 Introduced motivation failure for integer
- #70 Introduced motivation failure for vector
- #68 Added motivation failure for factors
- #67 Added motivation failure report for data frame objects.
- #66 Refactor failure reasons in interval to introduce report object
- #65 Extract text snippets from message producing routines to ensure less duplication
- #63 Change allow_null handling to ensure that the value is returned immediately, and not left to a fallback

# qscheck 0.22.0

- #59 Added is_s4_instance

# qscheck 0.21.0

- #57 Ensure that the correct scope is passed through to assertthat::assert_that

# qscheck 0.20.0

- #53 Added is_positive_real_vector, is_non_negative_real_value, is_probability_vector, is_non_negative_real_vector

# qscheck 0.19.0

- Add assert function
- Added is_value as initial step towards passthrough of is_null / is_na
- Add default call getter (internal)
- #32 Added check for null to is_real_vector, is_integer_vector, is_string_vector

# qscheck 0.18.0

- #42 Fixes error in mutually_exclusive
- #41 Added Increasing/decreasing checks

# qscheck 0.17.0

- #38 Add allow_all_null for mutually exclusive entries
- #37 Added allow_null and allow_na to is_positive_real_value
- #36 Added allow_null to is_probability_value

# qscheck 0.16.0

- #29 Decomposed assertions.R into subfiles
- #28 Adds is_interval
- #27 Added number of arguments in function
- #26 Add is non negative integer value
- #25 Added is_positive_integer_vector

# qscheck 0.15.0

- #18 Improve error messages for allow_na_values
- #17 Allow or disallow for degenerate content in is_binary_vector

# qscheck 0.14.0

- #15 Added allow_na_values in is_factor
- #14 Fixed printout of any_satifisfied

# qscheck 0.13.0

- #9 Added is_factor

# qscheck 0.12.0

- First public release
