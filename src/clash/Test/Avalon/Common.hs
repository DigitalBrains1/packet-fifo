{-# LANGUAGE RankNTypes #-}
module Test.Avalon.Common where

import Clash.Prelude

import Avalon.Master
import Avalon.PacketReader
import Avalon.PacketWriter

{-
 - Top-level entity Synthesize annotation for synthesized testbenches.
 -
 - This structure is used both for the 'ICMPEcho' network communication PoC as
 - well as the testbenches here. See the top-level README in this repository
 - for how to use top entities with this "packet_fifo" name and structure.
 -}
packetFifoSynthesize =
    Synthesize
        { t_name   = "packet_fifo"
        , t_inputs =
              [ PortName "clk"
              , PortName "rst_n"
              , PortName "fpga_debounced_buttons"
              , avalonMasterExtInputNames "fifo_f2h_in_mm_"
              , avalonMasterExtInputNames "fifo_h2f_out_mm_"
              ]
        , t_output =
              PortProduct ""
                [ PortName "fpga_led_internal"
                , avalonMasterExtOutputNames "fifo_f2h_in_mm_"
                , avalonMasterExtOutputNames "fifo_h2f_out_mm_"
                ]
       }

type PacketFifoTopEntity =
    (    Clock System
     -> "rst_n" ::: Signal System Bool
     -- ^ Active-low asynchronous reset
     -> "fpga_debounced_buttons" ::: Signal System (BitVector 3)
     -> "fifo_f2h_in" ::: PacketWriterExtInput System
     -> "fifo_h2f_out" ::: PacketReaderExtInput System
     -> ( "fpga_led_internal" ::: Signal System (Unsigned 10)
        , "fifo_f2h_in" ::: PacketWriterExtOutput System
        , "fifo_h2f_out" ::: PacketReaderExtOutput System
        ))

type PacketFifoTopEntityImpl =
    (   SystemClockResetEnable
     => "fpga_debounced_buttons" ::: Signal System (BitVector 3)
     -> "fifo_f2h_in" ::: PacketWriterExtInput System
     -> "fifo_h2f_out" ::: PacketReaderExtInput System
     -> ( "fpga_led_internal" ::: Signal System (Unsigned 10)
        , "fifo_f2h_in" ::: PacketWriterExtOutput System
        , "fifo_h2f_out" ::: PacketReaderExtOutput System
        ))

{-
 - Top-level entity constructor for synthesized testbenches.
 -
 - This structure is used both for the 'ICMPEcho' network communication PoC as
 - well as the testbenches here. See the top-level README in this repository
 - for how to use top entities with this "packet_fifo" name and structure.
 -}
packetFifoTopEntity
    :: PacketFifoTopEntityImpl
    -> PacketFifoTopEntity
packetFifoTopEntity teImpl clk rst_n
    = withClockResetEnable clk rstS enableGen teImpl
    where
        rstS = resetSynchronizer clk (unsafeFromLowPolarity rst_n) enableGen
