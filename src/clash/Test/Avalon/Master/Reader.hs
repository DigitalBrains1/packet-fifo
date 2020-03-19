{-
 - A design that tests the read functionality of avalonMaster
 -
 - It continually reads the "fill_level FIFO Status Register" and displays its
 - value in binary on the LEDs of the DE10 board. By writing to the FIFO from
 - the HPS, it can be verified that reading the fill_level register works as
 - the value on the LEDs increases.
 -}
module Test.Avalon.Master.Reader where

import Clash.Prelude

import Avalon.Master

{-# ANN avalonMasterReader
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
avalonMasterReader
    :: Clock System
    -> "rst_n" ::: Signal System Bool
    -- ^ Active-low asynchronous reset
    -> "fpga_debounced_buttons" ::: Signal System (BitVector 3)
    -> "fifo_f2h_in" :::
           ( Signal System (Unsigned 32)
           -- ^ fifo_f2h_in_mm_external_interface_read_data
           , Signal System Bool
           -- ^ fifo_f2h_in_mm_external_interface_acknowledge
           )
    -> "fifo_f2h_out" :::
           ( Signal System (Unsigned 32)
           -- ^ fifo_h2f_out_mm_external_interface_read_data
           , Signal System Bool
           -- ^ fifo_h2f_out_mm_external_interface_acknowledge
           )
    -> ( "fpga_led_internal" ::: Signal System (Unsigned 10)
       , "fifo_f2h_in" :::
             ( Signal System (Unsigned 3)
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
       , "fifo_f2h_out" :::
             ( Signal System (Unsigned 6)
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
avalonMasterReader clk rst_n
    = exposeClockResetEnable avalonMasterReader' clk rstS enableGen
    where
        rstS = resetSynchronizer clk (unsafeFromLowPolarity rst_n) enableGen
{-# NOINLINE avalonMasterReader #-}

avalonMasterReader'
    :: SystemClockResetEnable
    => "fpga_debounced_buttons" ::: Signal System (BitVector 3)
    -> "fifo_f2h_in" :::
           ( Signal System (Unsigned 32)
           -- ^ fifo_f2h_in_mm_external_interface_read_data
           , Signal System Bool
           -- ^ fifo_f2h_in_mm_external_interface_acknowledge
           )
    -> "fifo_f2h_out" :::
           ( Signal System (Unsigned 32)
           -- ^ fifo_h2f_out_mm_external_interface_read_data
           , Signal System Bool
           -- ^ fifo_h2f_out_mm_external_interface_acknowledge
           )
    -> ( "fpga_led_internal" ::: Signal System (Unsigned 10)
       , "fifo_f2h_in" :::
             ( Signal System (Unsigned 3)
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
       , "fifo_f2h_out" :::
             ( Signal System (Unsigned 6)
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
avalonMasterReader' _ f2hIn h2fIn
    = ( leds
      , f2hOut
      , h2fOut)
    where
        (f2hOut, f2hOpReady, f2hRes)
            = avalonMaster
                f2hIn f2hOp f2hResReady

        f2hResReady = pure True
        f2hOp = ( pure False, pure undefined, pure ()
                , pure (undefined :: Unsigned 3), pure undefined)

        (h2fOut, h2fOpReady, h2fRes)
            = avalonMaster
                h2fIn h2fOp h2fResReady

        (leds, h2fResReady, h2fOp)
            = ledFillLevel h2fOpReady h2fRes


        (f2hOpValid, f2hOpAddr, f2hOpData)
            = (pure False, pure undefined, pure undefined)
              :: ( Signal System Bool, Signal System (Unsigned 3)
                 , Signal System (Unsigned 32))

ledFillLevel
    :: KnownDomain dom
    => "opReady" ::: Signal dom Bool
    -> "res" :::
        ( "resValid" ::: Signal dom Bool
        -- ^ Result valid
        , "resTag" ::: Signal dom ()
        -- ^ Passthrough tag
        , "resData" ::: Signal dom (Unsigned 32)
        -- ^ Read data
        )
    -> ( "leds" ::: Signal dom (Unsigned 10)
       , "resReady" ::: Signal dom Bool
       -- ^ Result ready
       , "op" :::
           ( "opValid" ::: Signal dom Bool
           -- ^ Operation valid
           , "opCmd" ::: Signal dom AvalonCmd
           -- ^ Read or write
           , "opTag" ::: Signal dom ()
           -- ^ Passthrough tag for request matching
           , "opAddr" ::: Signal dom (Unsigned 6)
           -- ^ Address
           , "opData" ::: Signal dom (Unsigned 32)
           -- ^ Write data
           )
       )
ledFillLevel opReady res = (leds, resReady, op)
    where
        op = (pure True, pure AvalonRead, pure (), pure 0, pure undefined)
        opValid = pure True
        resReady = pure True
        (_, _, resData) = res
        leds = truncateB <$> resData
