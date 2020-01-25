module Test.Avalon.PacketEcho.OnlyFirst where

import Clash.Prelude

import Avalon.Master
import Avalon.PacketReader
import Avalon.PacketWriter
import Test.Avalon.PacketEcho.Common

{-# ANN avalonPacketEcho
    (Synthesize
        { t_name   = "packet_fifo"
        , t_inputs
            = [ PortName "clk"
              , PortName "rst_n"
              , PortName "fpga_debounced_buttons"
              , avalonMasterExtInputNames "fifo_f2h_in_mm_"
              , avalonMasterExtInputNames "fifo_h2f_out_mm_"
              ]
        , t_output
            = PortProduct ""
                [ PortName "fpga_led_internal"
                , avalonMasterExtOutputNames "fifo_f2h_in_mm_"
                , avalonMasterExtOutputNames "fifo_h2f_out_mm_"
                ]
        }) #-}
{-# NOINLINE avalonPacketEcho #-}
avalonPacketEcho clk rst_n
    = withClockResetEnable clk rstS enableGen
                           avalonPacketEchoXfm (.&. 0xff000000)
    where
        rstS = resetSynchronizer clk (unsafeFromLowPolarity rst_n) enableGen
