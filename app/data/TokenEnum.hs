module Data.TokenEnum where
data Token = Scalar String | MappingStart | MappingEnd | SequenceStart | SequenceEnd | Colon | Dash | NewLine | EOF deriving (Show, Eq)