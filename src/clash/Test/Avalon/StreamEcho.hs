module Test.Avalon.StreamEcho where

import Clash.Prelude

import Avalon.Master
import Avalon.PacketReader
import Avalon.PacketWriter
import Avalon.PacketToByteStream
import Avalon.ByteStreamToPacket

{-# ANN avalonStreamEcho
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
avalonStreamEcho clk rst_n
    = withClockResetEnable clk rstS enableGen avalonStreamEcho'
    where
        rstS = resetSynchronizer clk (unsafeFromLowPolarity rst_n) enableGen
{-# NOINLINE avalonStreamEcho #-}

avalonStreamEcho'
    :: SystemClockResetEnable
    => Signal System (BitVector 3)
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

avalonStreamEcho' _ f2hIn h2fIn
    = (pure 0, f2hOut, h2fOut)
    where
        (f2hOut, f2hOpReady, f2hRes)
            = avalonMaster
                f2hIn f2hOp f2hResReady

        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster
                h2fIn h2fOp h2fResReady

        (h2fResReady, h2fPacket, h2fOp)
            = packetReader h2fRes h2fPacketReady h2fOpReady

        (f2hPacketReady, f2hOp) = packetWriter f2hPacket f2hOpReady
        f2hResReady = pure True

        (h2fPacketReady, stream)
            = packetToByteStream h2fPacket streamReady

        (streamReady, f2hPacket)
            = byteStreamToPacket stream f2hPacketReady
