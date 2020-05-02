module Smalltalk.UI.Base(Point(Point) Element(Element,AsyncElement)) where
    import Smalltalk.Cont
    data Point = Point Int Int
    data Element = Element {origin::Point} | AsyncElement (Cont Element)
    
