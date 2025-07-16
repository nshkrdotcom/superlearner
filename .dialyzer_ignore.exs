[
  # Ignore callback_info_missing for Mix.Task modules
  # These are false positives - Mix.Task behaviour info not in PLT
  {"lib/mix/tasks/cluster/test.ex", :callback_info_missing},
  {"lib/mix/tasks/test/config.ex", :callback_info_missing},
  {"lib/mix/tasks/test/distributed.ex", :callback_info_missing}
]
