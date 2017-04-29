## v1.2.0 (2017-04-29)

* Updated sangria to v1.2.0
* Add the `GlobalIdTypeAlias` for `IDType` (#19)

## v1.1.0 (2017-03-11)

* Updated sangria to v1.1.0

## v1.0.0 (2017-01-16)

* v1.0 Release
* Added `nodes` field to `NodeDefinition` (#18). It's a minor breaking change. Please note that because of this change the `resolve` function of `Node.definition` now should return `LeafAction` instead of `Action`. This slightly limits it's capabilities, but it was a necessary change to implement `nodes` field. 

## v1.0.0-RC5 (2016-11-28)

* Updated sangria to v1.0.0-RC5

## v1.0.0-RC4 (2016-11-20)

* Updated sangria to v1.0.0-RC4

## v1.0.0-RC3 (2016-11-05)

* Cross compile to scala 2.12 and scala 2.11
* Updated sangria to v1.0.0-RC3

## v1.0.0-RC2 (2016-10-10)

* Updated sangria to v1.0.0-RC2

## v1.0.0-RC1 (2016-10-08)

* Remove non-null restriction on clientMutationId field definitions (#16)
* First and last arguments must be positive integers (#15)
* `Identifier` type class now transparently supports `HasId` type-class
* Updated sangria to v1.0.0-RC1

## v0.7.3 (2016-08-26)

* Updated sangria to v0.7.3

## v0.7.2 (2016-08-01)

* Updated sangria to v0.7.2

## v0.7.1 (2016-07-02)

* Updated sangria to v0.7.1

## v0.7.0 (2016-06-12)

* Updated sangria to v0.7.0

## v0.6.3 (2016-05-01)

* Updated sangria to v0.6.3

## v0.6.2 (2016-04-10)

* Updated sangria to v0.6.2

## v0.6.1 (2016-04-02)

* Updated sangria to v0.6.1

## v0.6.0 (2016-03-19)

* Updated sangria to v0.6.0

## v0.5.2 (2016-02-28)

* `fieldWithClientMutationId` now allows to provide a field's description.
* Updated sangria to v0.5.2

## v0.5.1 (2016-01-23)

* The `typeName` argument of `Node.globalIdField` is now optional. If not provided, it would be inferred based on a parent type name.
  Since it's optional now, the signature of `Node.globalIdField` has changed, so this change is not backwards-compatible:
  
  ```scala
  // Old version 
  ObjectType("User", interfaces[Unit, User](nodeInterface),
    fields[Unit, User](
      Node.globalIdField("User"),
      ...
    )
  )
  
  // new version - if global ID type name is the same as parent type 
  // (like in example above)
  ObjectType("User", interfaces[Unit, User](nodeInterface),
    fields[Unit, User](
      Node.globalIdField,
      ...
    )
  )
     
  // new version - if global ID type name is different from the parent type name
  ObjectType("User", interfaces[Unit, User](nodeInterface),
    fields[Unit, User](
      Node.globalIdField(Some("CustomUser")),
      ...
    )
  )   
  ```
* Added `IdentifiableNode` type-class which provides more powerful alternative to `Identifiable` type-class. It allows you to extract ID not only based on
  the `Val`, but also based on the whole `Context[Ctx, Val]`.
* Updated sangria to v0.5.1
  
## 0.5.0 (2015-12-03)

* Updated sangria to v0.5.0

## 0.4.3 (2015-10-16)

* Updated sangria to v0.4.3

## 0.4.2 (2015-10-12)

* All functions that generate fields now also have `tags` and `complexity` arguments (#7, #8)
* Connection adapter for seq slices (#6)
* Updated sangria to v0.4.2

## 0.4.1 (2015-10-03)

* Updated sangria to v0.4.1

## 0.4.0 (2015-09-27)

* Updated sangria to v0.4.0

## 0.3.1 (2015-08-29)

* Updated sangria to v0.3.1
* #4 - Improved `GlobalID` parsing
* `mutateAndGetPayload` should return an `Action` now, which makes it much more flexible and removed necessity for `Future` variation of it

## 0.3.0 (2015-08-16)

Initial release
