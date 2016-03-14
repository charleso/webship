Webship
=======

[![Build Status](https://travis-ci.org/charleso/webship.svg?branch=master)](https://travis-ci.org/charleso/webship)

Webship is a set of Haskell libraries for handling and serving HTTP requests in a RESTful fashion.

It is heavily inspired by [Airship](https://github.com/helium/airship) and
[Webcrank](https://github.com/webcrank/webcrank.hs)
which in turn were inspired by [Webmachine](https://github.com/basho/webmachine).

Both take a similar and fairly literal conversion of the `Webmachine` API, which
ends as a [single record](https://github.com/helium/airship/blob/master/src/Airship/Resource.hs)
with many functions, and a default value which can be modified.
Unfortunately this leads to the use of `StateT` for threading data between
function calls, which is clunky and inprecise.

This library is an attempt to implement a _simple_ but type-safe alternative.

## Example

See [Basic.hs](https://github.com/charleso/webship/blob/master/webship-example/src/Webship/Example/Basic.hs).

## HTTP

![decision](https://bytebucket.org/justin/webmachine/wiki/http-headers-status-v3.png)
