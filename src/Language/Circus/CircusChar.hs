{- |
Module      : Language.ISOZ.CircusChar
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

Unicode chars used in Circus.

-}
module Language.Circus.CircusChar where

cChar_circrefines       = ['\x2291']        -- circrefines    
cChar_circsimulates     = ['\x227C']        -- circsimulates
cChar_circassertref     = "assert"          -- circassertref
cChar_prefixcolon       = ['\x2236']        -- prefixcolon
cChar_circdef           = ['\x2259']        -- circdef
cChar_circspot          = ['\x2219']        -- circspot
cChar_circindex         = ['\x2299']        -- circindex
cChar_circmu            = ['\x00B5']        -- circmu
cChar_circthen          = ['\x27FC']        -- circthen
cChar_circelse          = ['\x25AF']        -- circelse
cChar_then              = ['\x27F6']        -- then
cChar_circguard         = ['\x0026']        -- circguard
cChar_circseq           = ['\x037E']        -- circseq 
cChar_circinterrupt     = ['\x25B3']        -- circinterrupt
cChar_Semi              = ['\x2A1F']        -- Semi
cChar_interleave        = ['\x2980']        -- interleave
cChar_Parallel          = ['\x2225']        -- Parallel 
cChar_Interleave        = ['\x2AFC']        -- Interleave
cChar_circhide          = ['\x2AF5']        -- circhide 
cChar_extchoice         = ['\x25FB']        -- extchoice
cChar_Extchoice         = ['\x25A1']        -- Extchoice
cChar_intchoice         = ['\x2293']        -- intchoice
cChar_Intchoice         = ['\x25A1']        -- Intchoice
cChar_lchanset          = ['\x2983']        -- lchanset
cChar_rchanset          = ['\x2984']        -- rchanset
cChar_lcircindex        = ['\x230A']        -- lcircindex 
cChar_rcircindex        = ['\x230B']        -- rcircindex 
cChar_lcircguard        = ['\x3014']        -- lcircguard
cChar_rcircguard        = ['\x3015']        -- rcircguard 
cChar_lcircrename       = ['\x2768']        -- lcircrename 
cChar_rcircrename       = ['\x2769']        -- rcircrename 
cChar_lpar              = ['\x27E6']        -- lpar 
cChar_rpar              = ['\x27E7']        -- rpar 
cChar_linter            = ['\x301A']        -- linter 
cChar_rinter            = ['\x301B']        -- rinter 
cChar_circif            = "if"              -- circif
cChar_circfi            = "fi"              -- circfi
cChar_circdo            = "do"              -- circdo
cChar_circod            = "od"              -- circod
cChar_circcon           = "con"             -- circcon
cChar_circvar           = "var"             -- circvar
cChar_circval           = "val"             -- circval
cChar_circres           = "res"             -- circres
cChar_circvres          = "vres"            -- circvres
cChar_circchannel       = "channel"         -- circchannel
cChar_circchannelfrom   = "channelfrom"     -- circchannelfrom
cChar_circchannelset    = "channelset"      -- circchannelset
cChar_circnameset       = "nameset"         -- circnameset 
cChar_circprocess       = "process"         -- circprocess 
cChar_circbegin         = "begin"           -- circbegin 
cChar_circend           = "end"             -- circend 
cChar_circstate         = "circstate"       -- circstate
cChar_Skip              = "Skip"            -- Skip 
cChar_Stop              = "Stop"            -- Stop 
cChar_Chaos             = "Chaos"           -- Chaos 

-- | The unicode for the \begin{circus} environment
cChar_circchar          = ['\x257A']        -- 
-- | The unicode for the \begin{circusaction} environment
cChar_circactchar       = ['\x2576']        -- 

getCircusKeywords :: [String]
getCircusKeywords = [
    cChar_circrefines,
    cChar_circsimulates,
    cChar_circassertref,
    cChar_prefixcolon,
    cChar_circdef,
    cChar_circspot,
    cChar_circindex,
    cChar_circmu,
    cChar_circthen,
    cChar_circelse,
    cChar_then,
    cChar_circguard,
    cChar_circseq,
    cChar_circinterrupt,
    cChar_Semi,
    cChar_interleave,
    cChar_Parallel,
    cChar_Interleave,
    cChar_circhide,
    cChar_extchoice,
    cChar_Extchoice,
    cChar_intchoice,
    cChar_Intchoice,
--    cChar_lchanset,
--    cChar_rchanset,
--    cChar_lcircindex,
--    cChar_rcircindex,
--    cChar_lcircguard,
--    cChar_rcircguard,
--    cChar_lcircrename,
--    cChar_rcircrename,
--    cChar_lpar,
--    cChar_rpar,
--    cChar_linter,
--    cChar_rinter,
    cChar_circif,
    cChar_circfi,
    cChar_circdo,
    cChar_circod,
    cChar_circcon,
    cChar_circvar,
    cChar_circval,
    cChar_circres,
    cChar_circvres,
    cChar_circchannel,
    cChar_circchannelfrom,
    cChar_circchannelset,
    cChar_circnameset,
    cChar_circprocess,
    cChar_circbegin,
    cChar_circend,
    cChar_circstate,
    cChar_Skip,
    cChar_Stop,
    cChar_Chaos
    ]
