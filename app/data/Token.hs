module Data.Token where
data Token = SingleQuotedScalar String
           | DoubleQuotedScalar String
           | Scalar String
           | Key String
           | Value String
           | Item String
           | Comment String
           | MappingStart
           | MappingEnd
           | SequenceStart
           | SequenceEnd
           | Colon
           | Dash
           | NewLine
           | Space Int
           | Sharp
           | EOF
           deriving (Show, Eq)