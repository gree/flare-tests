-- author: Benjamin Surma <benjamin.surma@gree.net>

import Test.Framework

import FailoverTests (failoverTests)
import ProtocolTests (protocolTests)

main :: IO ()
main = defaultMain [ failoverTests
                   , protocolTests ]

