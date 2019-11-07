module Test.Avalon.Master.Reader where

import Clash.Prelude

import Avalon.Master

{-# ANN avalonMasterReader
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
avalonMasterReader clk rst_n
    = exposeClockResetEnable avalonMasterReader' clk rstS enableGen
    where
        rstS = resetSynchronizer clk (unsafeFromLowPolarity rst_n) enableGen
{-# NOINLINE avalonMasterReader #-}

avalonMasterReader'
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

avalonMasterReader' _ f2hRData f2hAck h2fRData h2fAck
    = ( leds
      , f2hAddr
      , f2hWData
      , f2hRead
      , f2hWrite
      , f2hBE
      , h2fAddr
      , h2fWData
      , h2fRead
      , h2fWrite
      , h2fBE)
    where
        ( f2hAddr, f2hWData, f2hRead, f2hWrite, f2hBE, f2hResValid, f2hOpReady, _, _)
            = avalonMaster
                (f2hRData)
                (f2hAck)
                f2hOpValid (pure True) (pure ())
                (pure AvalonWrite) f2hOpAddr f2hOpData

        ( h2fAddr, h2fWData, h2fRead, h2fWrite, h2fBE, h2fResValid, h2fOpReady, _, h2fResData)
            = avalonMaster
                h2fRData h2fAck h2fOpValid h2fResReady (pure ())
                (pure AvalonRead) h2fOpAddr (pure undefined)

        (leds, h2fOpValid, h2fResReady, h2fOpAddr)
            = ledFillLevel h2fOpReady h2fResValid h2fResData


        (f2hOpValid, f2hOpAddr, f2hOpData)
            = (pure False, pure undefined, pure undefined)
              :: ( Signal System Bool, Signal System (Unsigned 3)
                 , Signal System (Unsigned 32))

ledFillLevel
    :: KnownDomain dom
    => Signal dom Bool
    -> Signal dom Bool
    -> Signal dom (Unsigned 32)
    -> ( Signal dom (Unsigned 10)
       , Signal dom Bool
       , Signal dom Bool
       , Signal dom (Unsigned 6)
       )

ledFillLevel opReady resValid resData = (leds, opValid, resReady, opAddr)
    where
        opValid = pure True
        resReady = pure True
        opAddr = pure 0
        leds = truncateB <$> resData
