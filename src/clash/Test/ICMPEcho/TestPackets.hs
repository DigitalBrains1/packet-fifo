module Test.ICMPEcho.TestPackets where

import Clash.Prelude

echoReqPacket :: [Unsigned 8]
echoReqPacket
    = [ 0x52, 0x54, 0x00, 0xeb, 0x9b, 0xd0, 0x00, 0x50, 0xb6, 0x78, 0x57
      , 0xbe, 0x08, 0x00, 0x45, 0x00, 0x00, 0x54, 0xbc, 0x15, 0x40, 0x00
      , 0x40, 0x01, 0x39, 0xe8, 0xc0, 0xa8, 0x7a, 0x01, 0x0a, 0x00, 0x00
      , 0x02, 0x08, 0x00, 0x65, 0x14, 0x0f, 0xd4, 0x00, 0x01, 0xd7, 0xd8
      , 0xd7, 0x5d, 0x00, 0x00, 0x00, 0x00, 0x14, 0x0d, 0x01, 0x00, 0x00
      , 0x00, 0x00, 0x00, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17
      , 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22
      , 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d
      , 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37 ]

echoReplPacket :: [Unsigned 8]
echoReplPacket
    = [ 0x00, 0x50, 0xb6, 0x78, 0x57, 0xbe, 0x52, 0x54, 0x00, 0xeb, 0x9b
      , 0xd0, 0x08, 0x00, 0x45, 0x00, 0x00, 0x54, 0xbc, 0x15, 0x40, 0x00
      , 0x40, 0x01, 0x39, 0xe8, 0x0a, 0x00, 0x00, 0x02, 0xc0, 0xa8, 0x7a
      , 0x01, 0x00, 0x00, 0x6d, 0x14, 0x0f, 0xd4, 0x00, 0x01, 0xd7, 0xd8
      , 0xd7, 0x5d, 0x00, 0x00, 0x00, 0x00, 0x14, 0x0d, 0x01, 0x00, 0x00
      , 0x00, 0x00, 0x00, 0x10, 0x11, 0x12, 0x13, 0x14, 0x15, 0x16, 0x17
      , 0x18, 0x19, 0x1a, 0x1b, 0x1c, 0x1d, 0x1e, 0x1f, 0x20, 0x21, 0x22
      , 0x23, 0x24, 0x25, 0x26, 0x27, 0x28, 0x29, 0x2a, 0x2b, 0x2c, 0x2d
      , 0x2e, 0x2f, 0x30, 0x31, 0x32, 0x33, 0x34, 0x35, 0x36, 0x37 ]

tcpSynPacket :: [Unsigned 8]
tcpSynPacket
    = [ 0x52, 0x54, 0x00, 0xeb, 0x9b, 0xd0, 0x00, 0x50, 0xb6, 0x78, 0x57
      , 0xbe, 0x08, 0x00, 0x45, 0x10, 0x00, 0x3c, 0xb8, 0x34, 0x40, 0x00
      , 0x40, 0x06, 0x3d, 0xcc, 0xc0, 0xa8, 0x7a, 0x01, 0x0a, 0x00, 0x00
      , 0x02, 0xcc, 0x08, 0x00, 0x50, 0x19, 0x95, 0x72, 0xe6, 0x00, 0x00
      , 0x00, 0x00, 0xa0, 0x02, 0xfa, 0xf0, 0x44, 0x83, 0x00, 0x00, 0x02
      , 0x04, 0x05, 0xb4, 0x04, 0x02, 0x08, 0x0a, 0x21, 0x83, 0x49, 0x89
      , 0x00, 0x00, 0x00, 0x00, 0x01, 0x03, 0x03, 0x07 ]
