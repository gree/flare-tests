-- author: Benjamin Surma <benjamin.surma@gree.net>

import Test.Framework

import FailoverTests (failoverTests)
import ProtocolTests (protocolTests)
import Properties (properties)

main :: IO ()
main = defaultMain [ failoverTests
                   , protocolTests
                   , properties ]

