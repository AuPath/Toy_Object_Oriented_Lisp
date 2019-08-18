## Available Intructions


### def-class
**name** : def-class  

**arglist** : class-name, parents, slot-value 

Adds a class to the global hash table *classes-specs*
class-name must be a symbol
parents must be a list, can be empty
slot-value is a &rest arg, to add for example
a slot university syntax is 'university "Bicocca".

If not all slot-names have a corresponding slot-value error.

Can also define a method.

Return name of the class defined.

  
### new
**name** : new

**arglist** : class-name, slot-value

class-name is the class from which to create the instance.

slot-value is a &rest arg, can redefine attributes/methods from superclasses.

return created instance in form:

(:type 'oolinst :class-name class-name :fields (list of alists(slot value)


### getv
**name** : getv

arglist : instance, slot-name

gets value associated to slot-name from instance of one of it's superclasses.  


### getvx
**name** : getvx

**arglist** : instance, slot-name

slot-name is a list

return value from a class by following a chain of slot-names



### PS:
Se stai guardando questo progetto con l'intento di copiare evita perchè sicuro ti beccano.  
Instance specific methods are not implemented.
