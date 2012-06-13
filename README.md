exodev_gpio
===========

sysfs GPIO port driver and erlang module.

Please ensure that the exodev_gpio repo is cloned to a local directory called "gpio":
git clone  git@github.com:Feuerlabs/exodev_gpio gpio

If this is not done, the code:priv_dir() will bomb out and gpio:open_pin() will fail.

application:start(gpio), gpio:open_pin(1, output, low), gpio:sequence(1, [ 2000, 2000, 2000 ]).
