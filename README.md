![Github build](https://github.com/etorreborre/specs2-cats/actions/workflows/ci.yml/badge.svg)

Matchers for working with cats data types:

 - the `org.specs2.matcher.IOMatchers` trait provides synchronous matchers for checking `IO` values

 - the `org.specs2.matcher.IOExecution` trait allows to return any `IO[T]` in a specs2 example provided there is
   an implicit `org.specs2.execute.AsResult` instance in scope (this is the case for `Boolean`, `org.specs2.execute.Result` for example).
   This is the preferred way of executing `IO` values since they execute asynchronously when the example runs

 - the `org.specs2.matcher.ValidatedMatchers` trait provides matchers for `Validated[T]` values

 - the `org.specs2.matcher.NonEmptyMatchers` trait provides `haveSize` matchers for `NonEmpty(List, Chain, Vector)` values

Go to [specs2.org](http://specs2.org) to learn more about ***specs2***!
