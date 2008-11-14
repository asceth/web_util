%% This is the application resource file (.app file) for the web_util,
%% application.
{application, web_util,
  [{description, "Utility functions for web frameworks"},
   {vsn, "0.1.0"},
   {modules, [web_util]},
   {registered, []},
   {applications, [kernel, stdlib, adv_crypto]}
  ]}.
