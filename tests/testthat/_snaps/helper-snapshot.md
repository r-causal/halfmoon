# expect_halfmoon_error asserts the class it is given

    Code
      abort("bad argument", error_class = "halfmoon_arg_error")
    Condition <halfmoon_arg_error>
      Error:
      ! bad argument

# expect_halfmoon_warning asserts the class it is given

    Code
      warn("odd data", warning_class = "halfmoon_data_warning")
    Condition <halfmoon_data_warning>
      Warning:
      odd data

# the snapshot helpers can see objects local to the test

    Code
      abort(local_message, error_class = "halfmoon_arg_error")
    Condition <halfmoon_arg_error>
      Error:
      ! locally defined message

