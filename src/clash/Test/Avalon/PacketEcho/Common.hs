{-
 - A design that echoes any packets written to the FPGA back to the HPS.
 -
 - An optional transformation enables experimenting with the layout of data.
 - Actual top entities are defined in different files for specific
 - transformations.h
 -
 - Packets pass through avalonMaster, packetReader and packetWriter, verifying
 - their basic functioning.
 -
 -}
module Test.Avalon.PacketEcho.Common where

import Clash.Prelude

import Avalon.Master
import Avalon.PacketReader
import Avalon.PacketWriter

avalonPacketEchoXfm
    :: SystemClockResetEnable
    => (Unsigned 32 -> Unsigned 32)
    -> Signal System (BitVector 3)
    -- ^ fpga_debounced_buttons
    -> ( Signal System (Unsigned 32)
       -- ^ fifo_f2h_in_mm_external_interface_read_data
       , Signal System Bool
       -- ^ fifo_f2h_in_mm_external_interface_acknowledge
       )
    -> ( Signal System (Unsigned 32)
       -- ^ fifo_h2f_out_mm_external_interface_read_data
       , Signal System Bool
       -- ^ fifo_h2f_out_mm_external_interface_acknowledge
       )
    -> ( Signal System (Unsigned 10)
       -- ^ fpga_led_internal
       , ( Signal System (Unsigned 3)
         -- ^ fifo_f2h_in_mm_external_interface_address
         , Signal System (Unsigned 32)
         -- ^ fifo_f2h_in_mm_external_interface_write_data
         , Signal System Bool
         -- ^ fifo_f2h_in_mm_external_interface_read
         , Signal System Bool
         -- ^ fifo_f2h_in_mm_external_interface_write
         , Signal System (BitVector 4)
         -- ^ fifo_f2h_in_mm_external_interface_byte_enable
         )
       , ( Signal System (Unsigned 6)
         -- ^ fifo_h2f_out_mm_external_interface_address
         , Signal System (Unsigned 32)
         -- ^ fifo_h2f_out_mm_external_interface_write_data
         , Signal System Bool
         -- ^ fifo_h2f_out_mm_external_interface_read
         , Signal System Bool
         -- ^ fifo_h2f_out_mm_external_interface_write
         , Signal System (BitVector 4)
         -- ^ fifo_h2f_out_mm_external_interface_byte_enable
         )
       )

avalonPacketEchoXfm xfm _ f2hIn h2fIn
    = (pure 0, f2hOut, h2fOut)
    where
        (f2hOut, f2hOpReady, f2hRes)
            = avalonMaster
                f2hIn f2hOp f2hResReady

        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster
                h2fIn h2fOp h2fResReady

        (h2fResReady, pOut, h2fOp)
            = packetReader h2fRes packetReady h2fOpReady

        (packetReady, f2hOp) = packetWriter pIn f2hOpReady
        f2hResReady = pure True

        (pOutValid, pOutData, pOutOther) = pOut
        pInData = xfm <$> pOutData
        pIn = (pOutValid, pInData, pOutOther)
