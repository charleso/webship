Webship
=======

[![Build Status](https://travis-ci.org/charleso/webship.svg?branch=master)](https://travis-ci.org/charleso/webship)

Webship is a set of Haskell libraries for handling and serving HTTP requests in a RESTful fashion.

Webship is a shameless fork of the excellent [Airship](https://github.com/helium/airship)
which was inspired by [Webmachine](https://github.com/basho/webmachine)
and [Webcrank](https://github.com/webcrank/webcrank.hs).

Both `Airship` and `Webcrank` take a similar and fairly literal conversion of the `Webmachine` API,
in the form of a [single record](https://github.com/helium/airship/blob/master/src/Airship/Resource.hs)
with many functions, and a default value which can be modified.
Unfortunately this leads to the use of `StateT` for threading data between
function calls, which is less than ideal.

This library is an attempt to capture the essence of `Webship` with a type-safe API.

## Example

See [Basic.hs](https://github.com/charleso/webship/blob/master/webship-example/src/Webship/Example/Basic.hs).

## HTTP

![decision](https://bytebucket.org/justin/webmachine/wiki/http-headers-status-v3.png)
