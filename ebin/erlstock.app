{application, erlstock,
 [{description, "Stock exchange server"},
  {vsn, "0.1.0"},
  {modules, [stockserver_app,
             stockserver_sup,
             stockserver]},
  {registered, [stockserver_sup]},
  {applications, [kernel, stdlib]},
  {mod, {stockserver_app, []}}
 ]}.