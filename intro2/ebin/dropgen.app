{application, dropgen,
   [{description, "Dropping objects from towers"},
    {vsn, "0.0.1"},
    {modules, [dropgen, dropgen_sup, dropgen_app]},
    {registered, [dropgen, dropgen_sup]},
    {applications, [kernel, stdlib]},
    {mod, {dropgen_app, []} }]}.
