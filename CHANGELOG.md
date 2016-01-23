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
