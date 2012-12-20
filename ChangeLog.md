### v0.8.0

* Upgrade to `hosc 0.13` and `hsc3 0.13`
* Use `Sound.SC3.Server.Enum` data types for NRT file format
* Rename `outputHeaderFormat` to `outputSoundFileFormat`
* Add `runNRT` function and change type of `withNRT` and `NRTOptions`
* Add `Sound.SC3.Server.Process.Make`

### v0.7.0

* Split internal server module `Sound.SC3.Server.Process.Internal` into separate package `hsc3-process-internal`
* Module reorganisation

### v0.4.0

* Add support for **internal server transports** using [bindings-sc3](http://space.k-hornz.de/software/bindings-sc3)
* Overhaul [ConfigFile](http://hackage.haskell.org/package/ConfigFile) interface
