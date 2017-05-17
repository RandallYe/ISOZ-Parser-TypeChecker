{- |
Module      : Language.ISOZ.ZChar
Description : 
Copyright   : (C) Kangfeng Ye, 2017
License     : BSD3
Maintainer  : ye.randall@gmail.com 
Stability   : experimental

Unicode chars used in ISO Standard Z.

-}
module Language.ISOZ.ZChar where

zChar_delta      = ['\x0394']        -- delta
zChar_xi         = ['\x039E']        -- xi 
zChar_theta      = ['\x03B8']        -- theta
zChar_lambda     = ['\x03BB']        -- lambda 
zChar_mu         = ['\x03BC']        -- mu 
-- GREEK 
zChar_GREEK      = [zChar_delta, 
    zChar_xi,
    zChar_theta,
    zChar_lambda,
    zChar_mu]   -- greek 
-----------------------------------------
zChar_arithmos   = ['\x01D538']      -- arithmos
zChar_nat        = ['\x2115']        -- nat 
zChar_power      = ['\x2119']        -- power 
zChar_OTHERLETTER = [zChar_arithmos, 
    zChar_nat, 
    zChar_power]   -- otherletter
-----------------------------------------
-- set_toolkit
zChar_leftrightarrow     = ['\x2194']    -- <->
zChar_rightwardsarrow    = ['\x2192']    -- ->
zChar_notequalto         = ['\x2260']    -- not equal to
zChar_notelement         = ['\x2209']    -- not an element of
zChar_emptyset           = ['\x2205']    -- empty set 
zChar_subseteq           = ['\x2286']    -- subset of or equal to 
zChar_subset             = ['\x2282']    -- subset of 
zChar_union              = ['\x222A']    -- union 
zChar_inter              = ['\x2229']    -- intersection 
zChar_diff               = ['\\'] -- hide '\' 
zChar_revsolidus         = ['\x005C']    -- reverse solidus 
-- alternative char for \setminus from CZT
zChar_setminus           = ['\x2216']    -- set minus 
zChar_circminus          = ['\x2296']    -- circled minus 
zChar_naryunion          = ['\x22C3']    -- N-ary union 
zChar_naryinter          = ['\x22C2']    -- N-ary intersection 
zChar_doublef            = ['\x1D53D']   -- mathematical double-struck capital F 
--
zChar_SETTOOLKIT = [ 
    zChar_leftrightarrow,
    zChar_rightwardsarrow,
    zChar_notequalto,
    zChar_notelement,
    zChar_emptyset,
    zChar_subseteq,
    zChar_subset,
    zChar_union,
    zChar_inter,
    zChar_revsolidus,
    zChar_setminus,         
    zChar_circminus,
    zChar_naryunion,
    zChar_naryinter,
    zChar_doublef
    ]
-----------------------------------------
-- relation_toolkit
zChar_rightwardsarrowbar = ['\x21A6']    -- rightwards arrow from bar 
zChar_relcompose         = ['\x2A3E']    -- relational composition 
zChar_ringop             = ['\x2218']    -- ring operator 
zChar_lefttriangle       = ['\x25C1']    -- white left-pointing triangle 
zChar_righttriangle      = ['\x25B7']    -- white right-pointing triangle 
zChar_domantirest        = ['\x2A64']    -- domain anti-restriction 
zChar_ranantirest        = ['\x2A65']    -- range anti-restriction 
zChar_tildeop            = ['\x223C']    -- tilde operator 
zChar_leftimage          = ['\x2987']    -- left image bracket 
zChar_rightimage         = ['\x2988']    -- right image bracket 
zChar_circplus           = ['\x2295']    -- circled plus 
--
zChar_RELTOOLKIT = [
    zChar_rightwardsarrowbar,
    zChar_relcompose,
    zChar_ringop,
    zChar_lefttriangle,
    zChar_righttriangle,
    zChar_domantirest,
    zChar_ranantirest,
    zChar_tildeop,
    zChar_leftimage,
    zChar_rightimage,
    zChar_circplus
    ]
-----------------------------------------
-- function_toolkit
zChar_rarrowver          = ['\x21F8']    -- rightwards arrow with vertical stroke
zChar_rarrowtailver      = ['\x2914']    -- rightwards arrow with tail with vertical stroke
zChar_rarrowtail         = ['\x21A3']    -- rightwards arrow with tail
zChar_rtwoarrowver       = ['\x2900']    -- rightwards two-headed arrow with vertical stroke 
zChar_rtwoarrow          = ['\x21A0']    -- rightwards two-headed arrow 
zChar_rtwoarrowtail      = ['\x2916']    -- rightwards two-headed arrow with tail
zChar_rarrowdver         = ['\x21FB']    -- rightwards arrow with double vertical stroke 
zChar_rarrowtaildver     = ['\x2915']    -- rightwards arrow with tail with double vertical stroke 
-- 
zChar_FUNCTOOLKIT = [
    zChar_rarrowver,
    zChar_rarrowtailver,
    zChar_rarrowtail,
    zChar_rtwoarrowver,
    zChar_rtwoarrow,
    zChar_rtwoarrowtail,
    zChar_rarrowdver,
    zChar_rarrowtaildver
    ]

-----------------------------------------
-- number_toolkit
zChar_doublez            = ['\x2124']    -- double-struck capital Z
zChar_hypminus           = ['\x002D']    -- hyphen-minus 
zChar_minussign          = ['\x2212']    -- minus sign
zChar_lesseq             = ['\x2264']    -- less than or equal to 
zChar_less               = ['\x003C']    -- less than 
zChar_grteq              = ['\x2265']    -- greater than or equal to 
zChar_great              = ['\x003E']    -- greater than 
zChar_start              = ['\x002A']    -- asterisk 
--
zChar_NUMBERTOOLKIT  = [
    zChar_doublez,
    zChar_hypminus,
    zChar_minussign,
    zChar_lesseq,
    zChar_less,
    zChar_grteq,
    zChar_great,
    zChar_start
    ]
-----------------------------------------
-- sequence_toolkit
zChar_numsign            = ['\x0023']    -- number sign
zChar_leftangle          = ['\x27E8']    -- left angle bracket 
zChar_rightangle         = ['\x27E9']    -- right angle bracket 
zChar_chartie            = ['\x2040']    -- character tie 
zChar_upbarbleft         = ['\x21BF']    -- upwards harpoon with barb leftwards 
zChar_upbarbright        = ['\x21BE']    -- upwards harpoon with barb rightwards 
--
zChar_SEQTOOLKIT         = [
    zChar_numsign,
    zChar_leftangle,
    zChar_rightangle,
    zChar_chartie,
    zChar_upbarbleft,
    zChar_upbarbright
    ]
-----------------------------------------
zChar_mathletter = zChar_SETTOOLKIT ++ zChar_RELTOOLKIT ++ zChar_FUNCTOOLKIT ++ zChar_NUMBERTOOLKIT ++ zChar_SEQTOOLKIT 
-----------------------------------------
--zChar_letter     = [$LATIN $GREEK $OTHERLETTER $MATHLETTER] -- LETTER

-----------------------------------------
zChar_apostrophe = ['\x0027']   -- next in latex '
zChar_next       = ['\x2032']   -- next ʹ
zChar_in         = "?"          -- in 
zChar_out        = "!"           -- out
zChar_STROKECHAR = [
    zChar_next,
    zChar_in,
    zChar_out
    ] -- stroke 
-----------------------------------------
zChar_wordgluene = ['\x2197']        -- north east 
zChar_wordgluesw = ['\x2199']        -- south west 
zChar_wordgluese = ['\x2198']        -- south east 
zChar_wordgluenw = ['\x2196']        -- north west 
zChar_lowline    = ['_']           -- low line
zChar_WORDGLUE   = [
    zChar_wordgluene,
    zChar_wordgluesw,
    zChar_wordgluese,
    zChar_wordgluenw,
    zChar_lowline
    ] -- wordglue
-----------------------------------------
zChar_lparent    = "("          -- (
zChar_rparent    = ")"          -- )
zChar_lbracket   = "["
zChar_rbracket   = "]"
zChar_lbrace     = "{"
zChar_rbrace     = "}"
zChar_lblot      = ['\x2989']
zChar_rblot      = ['\x298A']
zChar_ldata      = ['\x27EA']
zChar_rdata      = ['\x27EB']
-- $BRACKET    = [\(\)\[\]\{\}\x2989 \x298A \x300A \x300B] 
zChar_BRACKET    = [
    zChar_lparent,
    zChar_rparent,
    zChar_lbracket,
    zChar_rbracket,
    zChar_lbrace,
    zChar_rbrace,
    zChar_lblot,
    zChar_rblot,
    zChar_ldata,
    zChar_rdata
    ]
-----------------------------------------
zChar_zedchar    = ['\x2500']        -- zedchar
zChar_axchar     = ['\x2577']        -- axchar 
zChar_schchar    = ['\x250C']        -- schchar 
zChar_conjchar   = ['\x250D']        -- conjchar 
zChar_genchar    = ['\x2550']        -- genchar 
zChar_endchar    = ['\x2029']        -- enchar 
zChar_BOXCHAR    = [
    zChar_zedchar,
    zChar_axchar,
    zChar_schchar,
    zChar_genchar,
    zChar_endchar
    ] -- boxchar
-----------------------------------------
zChar_nlchar     = ['\x2028']      -- newline
zChar_space      = ['\x20']        -- space
-----------------------------------------
--zChar_special    = [$STROKECHAR $WORDGLUE $BRACKET $BOXCHAR $NLCHAR $SPACE] -- special
-----------------------------------------
-- zChar_symbol     = [
--                 \|
--                 & 
--                 \x22A2      -- Right track (vdash)
--                 \x2227      -- land
--                 \x2228      -- lor
--                 \x21D2      -- implies 
--                 \x21D4      -- iff 
--                 \x00AC      -- lnot
--                 \x2200      -- forall 
--                 \x2203      -- exists 
--                 \x00D7      -- cross 
--                 \x002F      -- solidus 
--                 \x003D      -- equal
--                 \x2208      -- in 
--                 \x003A      -- colon :
--                 \x003B      -- semicolon ;
--                 \x002C      -- comma ,
--                 \x002E      -- full stop .
--                 \x2981      -- spot @
--                 \x29F9      -- hide
--                 \x2A21      -- schema project
--                 \x2A1F      -- schema composition
--                 \x2A20      -- schema piping
--                 \x002B      -- +  
--             ]   -- special
-----------------------------------------
-- Z Keywords
zChar_else              = "else"
zChar_false             = "false"
zChar_funcion           = "function"
zChar_generic           = "generic"
zChar_if                = "if"
zChar_leftassoc         = "leftassoc"
zChar_let               = "let"
zChar_powerset          = ['\x2119'] -- powerset
zChar_parents           = "parents" 
zChar_pre               = "pre"
zChar_relation          = "relation"
zChar_rightassoc        = "rightassoc"
zChar_section           = "section"
zChar_then              = "then"
zChar_true              = "true"
zChar_colon             = ":"
zChar_iff               = "=="
zChar_comma             = ","
zChar_freeequal         = "::="
zChar_bar               = "|"
zChar_andalso           = "&"
zChar_hide              = ['\x29F9'] -- hide '\' 
zChar_rename            = ['\x002F'] -- rename / 
zChar_dot               = "."
zChar_semicolon         = ";"
zChar_arg               = "_"
zChar_listarg           = ",,"
zChar_equal             = "="
zChar_conjecture        = ['\x22A2'] ++ ['?']    -- conjecture
zChar_forall            = ['\x2200']             -- forall
zChar_spot              = ['\x2981']             -- spot 
zChar_exists            = ['\x2203']             -- exists 
zChar_exists1           = ['\x2203'] ++ ['\x2198'] ++ "1" ++ ['\x2196']             -- exists1
zChar_equiv             = ['\x21D4']             -- equivalent 
zChar_implies           = ['\x21D2']             -- implies 
zChar_or                = ['\x2227']             -- or
zChar_and               = ['\x2228']             -- and
zChar_not               = ['\x00AC']             -- not
zChar_mem               = ['\x2208']             -- in/element of/member
zChar_project           = ['\x2A21']             -- project
zChar_cross             = ['\x00D7']             -- cross
--zChar_lambda            = ['\x03BB']             -- lambda
--zChar_mu                = ['\x03BC']             -- mu
--zChar_theta             = ['\x03B8']             -- theta
zChar_compose           = ['\x2A1F']             -- schema compose
zChar_pipe              = ['\x2A20']              -- schema pipe

getISOZKeywords :: [String]
getISOZKeywords = [
    zChar_else,
    zChar_false,
    zChar_funcion,
    zChar_generic,
    zChar_if,
    zChar_leftassoc,
    zChar_let,
    zChar_powerset,
    zChar_parents,
    zChar_pre,
    zChar_relation,
    zChar_rightassoc,
    zChar_section,
    zChar_then,
    zChar_true,
    zChar_colon,
    zChar_iff,
    zChar_comma,
    zChar_freeequal,
    zChar_bar,
    zChar_andalso,
    zChar_hide,
    zChar_rename,
    zChar_dot,
    zChar_semicolon,
    zChar_arg,
    zChar_listarg,
    zChar_equal,
    zChar_conjecture,
    zChar_forall,
    zChar_spot,
    zChar_exists,
    zChar_exists1,
    zChar_equiv,
    zChar_implies,
    zChar_or,
    zChar_and,
    zChar_not,
    zChar_mem,
    zChar_project,
    zChar_cross,
    zChar_lambda,
    zChar_mu,
    zChar_theta,
    zChar_compose,
    zChar_pipe
    ]
