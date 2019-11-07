module Test.Avalon.Master.Writer where

import Clash.Prelude
import qualified Data.Text.IO as TIO

import Avalon.Master
import Test.Avalon.Master.MockSlave

traceSignal1F = flip const
-- traceSignal1F = traceSignal1

{-# ANN avalonMasterWriter
    (Synthesize
        { t_name   = "avalon_master_test"
        , t_inputs
            = [ PortName "clk"
              , PortName "rst_n"
              , PortName "fpga_debounced_buttons"
              , PortName "fifo_f2h_in_mm_external_interface_read_data"
              , PortName "fifo_f2h_in_mm_external_interface_acknowledge"
              , PortName "fifo_h2f_out_mm_external_interface_read_data"
              , PortName "fifo_h2f_out_mm_external_interface_acknowledge"
              ]
        , t_output
            = PortProduct ""
                [ PortName "fpga_led_internal"
                , PortName "fifo_f2h_in_mm_external_interface_address"
                , PortName "fifo_f2h_in_mm_external_interface_write_data"
                , PortName "fifo_f2h_in_mm_external_interface_read"
                , PortName "fifo_f2h_in_mm_external_interface_write"
                , PortName "fifo_f2h_in_mm_external_interface_byte_enable"
                , PortName "fifo_h2f_out_mm_external_interface_address"
                , PortName "fifo_h2f_out_mm_external_interface_write_data"
                , PortName "fifo_h2f_out_mm_external_interface_read"
                , PortName "fifo_h2f_out_mm_external_interface_write"
                , PortName "fifo_h2f_out_mm_external_interface_byte_enable"
                ]
        }) #-}
avalonMasterWriter clk rst_n
    = exposeClockResetEnable avalonMasterWriter' clk rstS enableGen
    where
        rstS = resetSynchronizer clk (unsafeFromLowPolarity rst_n) enableGen
{-# NOINLINE avalonMasterWriter #-}

avalonMasterWriter'
    :: SystemClockResetEnable
    => Signal System (BitVector 3)
    -- ^ fpga_debounced_buttons
    -> Signal System (Unsigned 32)
    -- ^ fifo_f2h_in_mm_external_interface_read_data
    -> Signal System Bool
    -- ^ fifo_f2h_in_mm_external_interface_acknowledge
    -> Signal System (Unsigned 32)
    -- ^ fifo_h2f_out_mm_external_interface_read_data
    -> Signal System Bool
    -- ^ fifo_h2f_out_mm_external_interface_acknowledge
    -> ( Signal System (Unsigned 10)
       -- ^ fpga_led_internal
       , Signal System (Unsigned 3)
       -- ^ fifo_f2h_in_mm_external_interface_address
       , Signal System (Unsigned 32)
       -- ^ fifo_f2h_in_mm_external_interface_write_data
       , Signal System Bool
       -- ^ fifo_f2h_in_mm_external_interface_read
       , Signal System Bool
       -- ^ fifo_f2h_in_mm_external_interface_write
       , Signal System (BitVector 4)
       -- ^ fifo_f2h_in_mm_external_interface_byte_enable
       , Signal System (Unsigned 6)
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

avalonMasterWriter' _ f2hRData f2hAck h2fRData h2fAck
    = ( pure 0
      , traceSignal1F "f2hAddr" f2hAddr
      , traceSignal1F "f2hWData" f2hWData
      , traceSignal1F "f2hRead" f2hRead
      , traceSignal1F "f2hWrite" f2hWrite
      , traceSignal1F "f2hBE" f2hBE
      , h2fAddr
      , h2fWData
      , h2fRead
      , h2fWrite
      , h2fBE)
    where
        ( f2hAddr, f2hWData, f2hRead, f2hWrite, f2hBE, f2hResValid, f2hOpReady, _, _)
            = avalonMaster
                (traceSignal1F "f2hRData" f2hRData)
                (traceSignal1F "f2hAck" f2hAck)
                f2hOpValid (pure True) (pure ())
                (pure AvalonWrite) f2hOpAddr f2hOpData

        ( h2fAddr, h2fWData, h2fRead, h2fWrite, h2fBE, h2fResValid, h2fOpReady, _, h2fResData)
            = avalonMaster
                h2fRData h2fAck h2fOpValid h2fResReady (pure ())
                (pure AvalonRead) h2fOpAddr (pure undefined)

        (h2fOpValid, h2fResReady, h2fOpAddr)
            = (pure False, pure True, pure undefined)
            :: ( Signal System Bool, Signal System Bool
               , Signal System (Unsigned 6))


        (f2hOpValid, f2hOpAddr, f2hOpData)
            = writeDummyCounter f2hOpReady

writeDummyCounter
    :: HiddenClockResetEnable dom
    => Signal dom Bool
    -> ( Signal dom Bool
       , Signal dom (Unsigned 3)
       , Signal dom (Unsigned 32)
       )

writeDummyCounter opReady
    = mealyB writeDummyCounter' 0 opReady

writeDummyCounter' cnt opReady = (cnt', (opValid, 0, cnt))
    where
        maxCnt = 256
        opValid = cnt /= maxCnt
        cnt' | cnt == maxCnt = cnt
             | opReady       = cnt + 1
             | otherwise     = cnt

mockTopEntity
    :: Signal System Bool

mockTopEntity = f2hAck
    where
        (_, f2hAddr, f2hWData, f2hRead, f2hWrite, f2hBE, _, _, _, _, _)
            = avalonMasterWriter
                clockGen (pure True) (pure 0b111) f2hRData f2hAck
                (pure undefined) (pure False)
        (f2hRData, f2hAck)
            = withClockResetEnable
                clockGen resetGen enableGen
                mockAvalonSlave f2hAddr f2hWData f2hRead f2hWrite f2hBE

makeVCD
    = do
        Right vcd <-
            dumpVCD
                (0,2000)
                mockTopEntity
                [ "f2hAck", "f2hWrite", "f2hWData"
                ]
        TIO.writeFile "avm.vcd" vcd
