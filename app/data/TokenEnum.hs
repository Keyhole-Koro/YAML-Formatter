module Data.TokenEnum where
data Token = Scalar String | MappingStart | MappingEnd | SequenceStart | SequenceEnd | Colon | Dash | NewLine | Space | EOF deriving (Show, Eq)