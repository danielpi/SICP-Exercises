import Cocoa
//: # Exercise 2.76
//: As a large system with generic operations evolves, new types of data objects or new operations may be needed. For each of the three strategies
//: - generic operations with explicit dispatch,
//: - data-directed style, and 
//: - message-passing style
//:
//: describe the changes that must be made to a system in order to add new types or new operations. 
//:
//: 1. Which organization would be most appropriate for a system in which new types must often be added? 
//: 2. Which would be most appropriate for a system in which new operations must often be added?

// For generic operations with explicit dispatch when you want to add a new type of data object you need a new set of constructors as well as modifying all selectors so that they can handle the new type. To add new operations you need to write it in such a way that it handles all of the possible types.

// For data directed style, If you add a new type you need create a new set of operations and selectors for that type and to register those functions with the generic system you are using. To add a new operation requires that you go and write a new version of the operation for each of the available types.

// For 3. If you add a new type you need to just write the new type def along with all of the operations it requires. To add a new operation you need to go back to all of your existing types and add the new operation to them.

// Message-passing or data directed handle new types with minimal fuss.

// Data-directed or explicit dispatch handle new operations with minimal fuss.