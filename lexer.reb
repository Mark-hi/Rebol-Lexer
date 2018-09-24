multiline: has [r l] [r: l: "" until [r: join r [l "^/"] empty? l: input] do r] ; for when the contents of this file are pasted into an R3 console instead of being executed via DO
multiline ; to handle pasting the (upcoming) REBOL header -- this line and the line above are introductory lines and are ignored when given to DO, see the "intro-line:" rule below
REBOL [
    Title: "Rebol Lexer (PEG)" ; not scanner, because a scanner must provide numerical overflow handling (an implementation concern), and not parser, because that would be redundant, any PEG parses something and hence is a parser
    Description: "Intended to conform with as much as possible of^/Rebol 2, Rebol 3 Alpha, Red, Ren/C, and Pointillistic Ren,^/recognizing and partitioning textual content into (a series of)^/29 possible lexical items (lexemes)."
    Date: 26-Dec-2016
    Version: 1.1.2
    Authors: [[Mark Ingram]]
    Exports: [rebol? analyze html-after]
    Usage: [clear output if rebol? source: read %some-file.reb [analyze output write %some-file.html html-after source]]
    History: [
        10-Dec-2016 1.1.1 [Mark Ingram] {Initial revision.}
        26-Dec-2016 1.1.2 [Mark Ingram] {Added syntax highlighting.}
        24-Nov-2018 1.1.3 [Mark Ingram] {Improved documentation, debugging, and testing.}
    ]
]

; voluntary textual formatting of this ASCII file (or, equivalently, UTF-8 file with no multiple-byte encodings):
; a semi-colon always either starts or is inside a comment -- every literal semi-colon in this file is represented by an escape sequence value form "^(3B)"
; a caret, used only in literals or comments, always indicates the beginning of an escape sequence -- every literal caret in this file is represented by an escape sequence value form "^(5E)", never the character form (two carets)

; synopsis of major syntactical fixes:
; --------------------------------------------------------------------------------------------------------------------
; lit-word! get-word!                       permit only what would be permitted if bare, i.e., without ' : decorations
; arrow-words and slashes-words             permit more arrow-words, and permit set-word, get-word, and lit-word forms
; url!                                      can begin with < or > if any arrows and :/ follows, or a slashes-word if :
; tag!                                      cedes to bare-word, set-word, or url, except </ (closing-tag special case)
; issue! refinement!                        permit only regular-word trails and commas; tags separate, but not slashes
; path!                                     restrictions released (issue money percent tuple) and increased (file url)
; date!                                     T also separates time from date; Z also is zone zero (RFC 3339 date forms)
; --------------------------------------------------------------------------------------------------------------------

; lexeme separation:
; Rule 1 (Closing With a Delimiter): Lexemes will not need spaces after them if their final character is from ")]" or is a closing for a delimited string-like, specifically, "}" (from "{"), {"} (from itself), and "^/" (from ";").
; Rule 2 (Opening With a Delimiter): Lexemes will not need spaces after them if what is next begins with one of "([" or an opening for a delimited string-like, specifically, "{" (with "}"), {"} (with itself), and ";" (with "^/").
; Rule 3 (Tag Delimitation): "<" and ">" delimit the tag string-like, and Rule 1 applies exactly, but Rule 2 applies only to words, and not to either arrow-words (the "<" makes them longer) or set-words (the "<" makes them urls).

; low-level

; UTF-8 charsets and rules -- both raw and percent-encoded forms, only well-formed UTF-8 is permitted in either
octal: charset "01234567"
hexan: charset "0123456789ABCDEFabcdef"
utail: charset [#"^(80)" - #"^(BF)"]
ptail: ["%" ["8" | "9" | "A" | "a" | "B" | "b"] hexan]
u808f: charset [#"^(80)" - #"^(8F)"]
p808f: ["%8" hexan]
u809f: charset [#"^(80)" - #"^(9F)"]
p809f: ["%" ["8" | "9"] hexan]
u90bf: charset [#"^(90)" - #"^(BF)"]
p90bf: ["%" ["9" | "A" | "a" | "B" | "b"] hexan]
ua0bf: charset [#"^(A0)" - #"^(BF)"]
pa0bf: ["%" ["A" | "a" | "B" | "b"] hexan]
uc2df: charset [#"^(C2)" - #"^(DF)"]
pu-2f: charset "23456789ABCDEFabcdef"
pc2df: ["%" [["C" | "c"] pu-2f | ["D" | "d"] hexan]]
ue1ec: charset [#"^(E1)" - #"^(EC)"]
pu-1c: charset "123456789ABCabc"
pe1ec: ["%" ["E" | "e"] pu-1c]
ueeef: charset [#"^(EE)" - #"^(EF)"]
peeef: ["%" ["E" | "e"] ["E" | "e" | "F" | "f"]]
uf1f3: charset [#"^(F1)" - #"^(F3)"]
pf1f3: ["%" ["F" | "f"] ["1" | "2" | "3"]]
utf84: ["^(F0)" u90bf utail utail | uf1f3 utail utail utail | "^(F4)" u808f utail utail]
ptf84: ["%" ["F" | "f"] "0" p90bf ptail ptail | pf1f3 ptail ptail ptail | "%" ["F" | "f"] "4" p808f ptail ptail]
utf83: ["^(E0)" ua0bf utail | ue1ec utail utail | "^(ED)" u809f utail | ueeef utail utail]
ptf83: ["%" ["E" | "e"] "0" pa0bf ptail | pe1ec ptail ptail | "%" ["E" | "e"] ["D" | "d"] p809f ptail | peeef ptail ptail]
utf82: [uc2df utail]
ptf82: [pc2df ptail]
utf8*: [utf82 | utf83 | utf84]                                          ; all 1,111,936 non-ASCII non-surrogate Unicode codepoints, UTF-8-encoded -- all the multiple-code-unit sequences that are interpreted in binary source files
putf8: ["%" octal hexan | ptf82 | ptf83 | ptf84]                        ; all 1,112,064 non-surrogate Unicode codepoints, including all of ASCII, as sequences of percent-encoded UTF-8 code-units [Ed. -- not yet used in any rules]
hexal: charset "89ABCDEFabcdef"                                         ; used in non-ASCII UTF-8 percent-encoding -- if this is the first of two hexadecimal digits encoding a byte, then that byte has its most significant bit set
legac: charset [#"^(80)" - #"^(D7FF)" #"^(E000)" - #"^(FFFF)"]          ; the 63361 non-ASCII non-surrogate Unicode codepoints supported by R3 -- for parsing R3 string sources by character, never to be used on binary source files
upper: [marki: if (string? marki) legac | if (binary? marki) utf8*]     ; this rule can (obviously) be made simpler by making the target restricted in type -- but this PEG prioritizes inclusiveness and flexibility over efficiency

; characters charsets -- though the syntax-controlling characters are all ASCII, all UTF-8 (and in comments, any content) is permitted -- but a null codepoint (Unicode U+000000) always means end-of-file no matter where it appears
alpha: charset "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"   ;  52 ; majuscule and minuscule are not differentiated syntactically in caret escapes, exponential notation, month names, or hexadecimal binary lexical forms
digit: charset "0123456789"                                             ;  10 ; the only digits
runic: charset "!?*=&|~`_^(5E)"                                         ;  10 ; non-alphanumerics that act like letters when used anywhere in words -- note that this includes the caret, ruling out its usage in escaping word forms
specl: charset "@#$%'+-.,:<>\"                                          ;  13 ; lexeme determiners that cannot ("@#$%:,") or, in specific circumstances, can ("'+-.<>"), be in words, and backslash, an input form of slash for files
delim: charset {[](){}"/^(3B)}                                          ;   9 ; delimiters, including the semi-colon -- all can terminate the current lexeme, and some ("[({", double-quote, and semi-colon) also begin their new one
graph: union union union union alpha digit runic specl delim            ;  94 ; graphical characters -- the ASCII visible glyphs
cntrl: charset [0 - 31 127]                                             ;  33 ; control characters, including the end of line characters LF and CR
ascii: union union graph cntrl charset " "                              ; 128 ; every 7-bit character -- monobyte means entire single, and thus, holobyte utf8 [Ed. -- better than unibyte, but both are better than "not multibyte"]

; words charsets
arrow: charset "<-+|=~>"                                                ; the characters permitted in arrow-words
angle: charset "<>"                                                     ; the characters permitted in arrow-words but forbidden in regular-words -- not actually used in a rule yet, it is just to define what "angle" characters are
nangl: charset "-+|=~"                                                  ; the characters permitted in both arrow-words and regular-words [difference arrow angle]
nunic: charset "_!?*&`^(5E)"                                            ; the non-alphabetic characters that are able to start words but are forbidden everywhere in arrow-words [difference runic charset "|=~"]
marks: charset "+-.'"                                                   ; the characters permitted in (some) numerics and (at some places) in regular words
narks: charset ".'"                                                     ; the characters permitted in (some) numerics and (at some places) in regular-words but forbidden everywhere in arrow-words [difference marks charset "-+"]
wordw: union union union alpha digit runic marks                        ; the characters permitted in regular-words, including the five 'nangl characters permitted in arrow-words -- ASCII, but only used where 'upper also accepted
wordn: union union union alpha digit nunic narks                        ; the characters permitted in regular-words but forbidden in arrow-words [difference wordw nangl] -- ASCII, but only used where 'upper also accepted

; binaries charsets
bin-2: charset "01"                                                     ; only these two characters are permitted inside binary bitsets (in addition to whitespace)
bin64: union union alpha digit charset "+/"                             ; the permitted non-blank characters before the finalising 0 through 2 equals signs in base-64 binary lexemes, one of the few places that case is significant

; strings charsets
caret: charset "^(5E)"                                                  ; the caret introduces a character escape in strings, quoted files (fewer available), and bare files (very few available)
hatch: charset "@/-!~[\]_"                                              ; the non-alphabetic characters that have a "standard" meaning as character escapes

; numerics charsets
signc: charset "+-"                                                     ; one sign character
point: charset ".,"                                                     ; in some kinds of numerics, comma acts like a decimal point
digic: union digit charset "'"                                          ; in some kinds of numerics, after the first digit, ticks are allowed to separate digits into groups

; lexeme separation charsets
selim: difference delim charset "/"                                     ; all the delimiters except slash
nullc: charset "^@"
blank: union charset " " difference cntrl nullc                         ; all whitespace except null
endch: union union selim blank nullc                                    ; path termination, and lexeme termination when not in a path
lf|cr: charset "^/^M"                                                   ; end of line (not newline) charset
nulnl: union nullc lf|cr                                                ; null or end of line (used for comment terminator detection)
bland: difference blank lf|cr                                           ; all whitespace except line-ends and nulls

; none-value charsets
; CAVEAT (fix): "#<" is no longer incorrectly always an error -- it is implementation-dependently either a possibly-wired-off extension point or a none value followed by something that might be in error
pound-grabs: charset "^"{[("                                            ; all the opening delims except semi-colon (the characters that make a preceding # not interpretable as the none-value shorthand) -- "<" prepares for gorgobs

; mid-level

; codepoint charsets
ascii-nullc:                                difference ascii nullc
ascii-nullc-ditto:                          difference ascii-nullc charset {"}
ascii-nullc-ditto-rangl:                    difference ascii-nullc charset {">}
graph-caret-ditto-lf|cr:                    difference graph union lf|cr charset {^(5E)"}
graph-caret-lbrac-rbrac:                    difference graph charset "^(5E){}"
graph-caret-colon-semic-ditto-percy:        difference graph charset {^(5E):^(3B)"%}
graph-delim-percy:                          difference difference graph delim charset "%"
graph-delim-colon-percy:                    difference difference graph delim charset ":%"
graph-delim-whorl-percy:                    difference difference graph delim charset "@%"
graph-delim-whorl-colon-percy:              difference difference graph delim charset "@:%"
graph-delim-caret-colon-percy:              difference difference graph delim charset "^(5E):%"
graph-delim-digit-whorl-colon-percy:        difference difference difference graph delim digit charset "@:%"
graph-delim-whorl-cache-colon-percy:        difference difference graph delim charset "@$:%"
graph-delim-whorl-caret-colon-percy:        difference difference graph delim charset "@^(5E):%"
graph-delim-digit-whorl-cache-colon-percy:  difference difference difference graph delim digit charset "@$:%"

; codepoint rules
chars-nullc-ditto:                          [ascii-nullc-ditto | upper]
chars-nullc-ditto-rangl:                    [ascii-nullc-ditto-rangl | upper]
ghigh-delim-percy:                          [graph-delim-percy | upper]
ghigh-delim-colon-percy:                    [graph-delim-colon-percy | upper]
ghigh-delim-whorl-percy:                    [graph-delim-whorl-percy | upper]
ghigh-delim-whorl-colon-percy:              [graph-delim-whorl-colon-percy | upper]
ghigh-delim-digit-whorl-colon-percy:        [graph-delim-digit-whorl-colon-percy | upper]
ghigh-delim-whorl-cache-colon-percy:        [graph-delim-whorl-cache-colon-percy | upper]
ghigh-delim-digit-whorl-cache-colon-percy:  [graph-delim-digit-whorl-cache-colon-percy | upper]
ghigh-caret-ditto-lf|cr:                    [graph-caret-ditto-lf|cr | upper]
ghigh-caret-lbrac-rbrac:                    [graph-caret-lbrac-rbrac | upper]
ghigh-caret-colon-semic-ditto-percy:        [graph-caret-colon-semic-ditto-percy | upper]
ghigh-delim-caret-colon-percy:              [graph-delim-caret-colon-percy | upper]
ghigh-delim-whorl-caret-colon-percy:        [graph-delim-whorl-caret-colon-percy | upper]
hex-4:                                      [0 4 hexan]                                        ; legacy -- BMP, including surrogates
hex4n:                                      [not [["D" | "d"] hexal 2 hexan] hex-4]            ; legacy -- BMP non-surrogates -- as yet unused
hex-6:                                      ["10" 4 hexan | opt "0" 0 5 hexan]                 ; all 1,114,112 Unicode codepoints, including surrogates, in hex
hex6n:                                      [not [[0 2 "0" ["D" | "d"]] hexal 2 hexan] hex-6]  ; all 1,112,064 non-surrogate Unicode codepoints in hex -- as yet unused
perch:                                      ["%" hexan hexan]                                  ; all 256 bytes, only half of which are UTF8, percent-encoded -- when this legacy mode is removed, 'putf8 will replace this everywhere
ghigh-delim+perch:                          [ghigh-delim-percy | perch]
ghigh-delim-colon+perch:                    [ghigh-delim-colon-percy | perch]
ghigh-delim-whorl+perch:                    [ghigh-delim-whorl-percy | perch]
ghigh-delim-whorl-colon+perch:              [ghigh-delim-whorl-colon-percy | perch]
ghigh-delim-digit-whorl-colon+perch:        [ghigh-delim-digit-whorl-colon-percy | perch]
ghigh-delim-whorl-cache-colon+perch:        [ghigh-delim-whorl-cache-colon-percy | perch]
ghigh-delim-digit-whorl-cache-colon+perch:  [ghigh-delim-digit-whorl-cache-colon-percy | perch]
ghigh-delim-caret-colon+perch:              [ghigh-delim-caret-colon-percy | perch]
ghigh-delim-whorl-caret-colon+perch:        [ghigh-delim-whorl-caret-colon-percy | perch]

; rule generators -- in addition to 'charset, 'difference, and 'union
; this PEG is intended to match both strings and binaries, but 'parse can only do the latter case-sensitively -- hence explicit dual-casing is required, and these utilities help keep 'cname and the individual month rules readable
 case-free: func ["Return a string's case-free matching rule." s [string!] /local r][r: copy [] foreach c s [append/only r reduce [to string! uppercase c '| to string! lowercase c]] r]
make-case-free: func ["Replace every string in a rule with its case-free matching rule." rule "modified" [block!]][forall rule [if string? first rule [change/only rule case-free first rule]] head rule]

; character-escaping rules -- for strings -- strings are provided with three kinds of escaping, caret-char "^C", caret-name "^(name)", and caret-value "^(hex-value)", but not percent-encoding
; CAVEAT (fix): escaping no longer incorrectly permits carets to disappear if what follows is not a recognized caret escape sequence
; CAVEAT (fix): no longer incorrectly includes caret-null
  cname:                     make-case-free ["line" | "tab" | "page" | "escape" | "esc" | "back" | "del" | "null"]
 hat-char:                                  [caret [caret | alpha | hatch | "(" [cname | hex-4] ")"]]
qchar:                                      [blank | hat-char | ghigh-caret-ditto-lf|cr]
qchar-lbrac-rbrac:                          [blank | hat-char | ghigh-caret-lbrac-rbrac]

; character-escaping rules -- for files -- files are provided with two kinds of escaping, caret-char (those that work in strings work in quoted files, and a few others (colon, semi-colon) might be thrown in), and percent-encoding
; CAVEAT (fix): ^(00) and ^(7F) no longer incorrectly work in bare files (as ^@ and ^~ of course)
; CAVEAT (fix): the fact that if it comes before any slashes, ^@ appearing in a bare file makes it an email, no longer incorrectly pertains, because ^@ is no longer permitted (though @ will still do that, %40 avoids)
; CAVEAT (fix): ^- (^I), ^! (1E), ^\ (1C), and ^_ (1F), no longer incorrectly work in bare files, and so also the resultant file is no longer incorrectly truncated at the first one of them
fchar-colon-semic-ditto+perch:              [" " | hat-char | ghigh-caret-colon-semic-ditto-percy | perch] ; used only in quoted files -- all controls (including newlines) must be escaped, using either a caret or percent-encoding
 filch-coloned:                             [caret ":"]                        ; since colons must be escaped, this is the only way a bare file can end with a colon (and hence will not be interpreted as such at the end of a path)
 filch-uncoloned:                           [caret caret]                      ; the caret-char sequence that the legacy-only escaped colon (which the filch-coloned rule exists only to provide) causes to be required in bare files
filch:                                      [filch-coloned | filch-uncoloned]  ; the minimal set of caret-char escapes that bare files must come with, if colons must have caret-char escaping

; dates rules
month-jan:                   make-case-free ["January" | "Januar" | "Janua" | "Janu" | "Jan"]
month-feb:                   make-case-free ["February" | "Februar" | "Februa" | "Febru" | "Febr" | "Feb"]
month-mar:                   make-case-free ["March" | "Marc" | "Mar"]
month-apr:                   make-case-free ["April" | "Apri" | "Apr"]
month-may:                   make-case-free ["May"]
month-jun:                   make-case-free ["June" | "Jun"]
month-jul:                   make-case-free ["July" | "Jul"]
month-aug:                   make-case-free ["August" | "Augus" | "Augu" | "Aug"]
month-sep:                   make-case-free ["September" | "Septembe" | "Septemb" | "Septem" | "Septe" | "Sept" | "Sep"]
month-oct:                   make-case-free ["October" | "Octobe" | "Octob" | "Octo" | "Oct"]
month-nov:                   make-case-free ["November" | "Novembe" | "Novemb" | "Novem" | "Nove" | "Nov"]
month-dec:                   make-case-free ["December" | "Decembe" | "Decemb" | "Decem" | "Dece" | "Dec"]
month-winter:                               [month-jan | month-feb | month-mar]
month-spring:                               [month-apr | month-may | month-jun]
month-summer:                               [month-jul | month-aug | month-sep]
month-autumn:                               [month-oct | month-nov | month-dec]
month:                                      [opt "+" some digit | month-winter | month-spring | month-summer | month-autumn]
year:                                       [digit digit digit some digit]

; lexeme separation rules
path-end:                                   [endch | end]              ; paths that end in arrow-words, and normal set-word and arrow-word termination -- tags error here unless separated by whitespace
word-end:                                   [endch | end | tag-begin]  ; paths that end in regular words, and normal neither set-word nor arrow-word termination -- tags auto-separate in this case only
newln:                                      ["^M^/" | "^/^M" | lf|cr]  ; for newline detection and translation, as an example, for now used only inside the block-likes
endln:                                      [any [not nulnl skip]]     ; ensure what is parsed next is on a different line than the previous character (or the character containing the previous byte if a binary is being processed)

; top-level -- there are 29 possible top-level lexemes, but the rules aren't 1-1 (all four paths use the 'path rule), and some forms of some lexemes do not go everywhere in paths (all paths, the word "<", files and urls with "/")

; --------
; NUMERICS (number-likes, not delimited) - 8 - integer decimal pair tuple money percent time date
; --------

 intic:                    [digit any digic]
integer:                   [opt signc intic]
 undec:                    [[[intic opt [point any digic] | point intic] ["E" | "e"] opt signc some digit] | intic point any digic | point intic]
decimal:                   [opt signc undec]
 number:                   [decimal | integer]
pair:                      [number some [["X" | "x"] number]]
; CAVEAT (fix): tuples are no longer incorrectly permitted to have signs, nor to have elements that are only signs
 dotmd:                    ["." any digit]
tuple:                     [[some digit dotmd dotmd | "." some digit dotmd] any dotmd]
money:                     [["$" opt signc | signc "$"] [undec | intic]]
percent:                   [number "%"]
; CAVEAT (fix): :.1 is no longer incorrectly a get-word
; CAVEAT (fix): times are no longer incorrectly permitted to have any signs except one, that must be the first character of the lexeme if present
; sadly, the 'and clause is required, and why it is required is slightly complicated: additional colons at the end must not cause parsing to fail, for end-item reasons -- testcase: [parse "1:2:" [time ":"]] ; must be true
time:                      [opt signc any digit ":" opt [some digit ":" and [digit | point]] [some digit opt [point any digit] | point some digit]] ; also used in the zoned-time rule below
; CAVEAT (fix): dates no longer incorrectly permit a time separator to be followed by nothing
; CAVEAT (fix): date-times no longer incorrectly do not permit RFC 3339 ["T" | "t"] time separator
; CAVEAT (fix): date-times no longer incorrectly do not permit RFC 3339 ["Z" | "z"] as a zone
; CAVEAT (fix): date-times no longer incorrectly permit a trailing zone colon -- which may perhaps be a reason for making uncoloned zones of 15 or smaller default to the hour(s) offset, not the minute(s) offset as is the case now
; CAVEAT (fix): date-times no longer incorrectly permit signs in any zone field -- the zone separator *is* the sign of the zone
; sadly, the 'and clause is required, and why it is required is slightly complicated: additional colons at the end must not cause parsing to fail, for end-item reasons -- testcase: [parse ":1-2:" [zoned-time ":"]] ; must be true
  zoned-time:              [copy t time (t: join "time " [t "^/"]) opt [copy z  ["Z" | "z" | signc opt [any digit ":" and digit] some digit] (z: join "zone " [z "^/"])]] ; debugging note -- two auxiliary fields are captured here
 slash-date:               [[opt "+" digit opt digit "/" month "/" opt "+" some digit | opt "+" year any digit "/" month "/" opt "+" some digit] opt [["/" | "T" | "t"] zoned-time]]
 dash-date:                [opt "+" digit opt digit "-" month "-" opt "+" some digit | opt "+" year any digit "-" month "-" opt "+" some digit]
path-date:                 [dash-date opt [["T" | "t"] zoned-time]]
date:                      [slash-date | dash-date "/" zoned-time | path-date]

; ----------
; IDIOMATICS (word-likes, not delimited) - 6 - (def-words - 3 kinds: slashes, arrow, and regular, in 4 forms, 1 bare and 3 decorated) word set-word get-word lit-word (ref-words - 1 kind, in 2 prefix (flag) forms) issue refinement
; ----------

; CAVEAT (fix): +' and -' are no longer incorrectly unable to be, or to begin, words
; CAVEAT (fix): :+1 :-1 :+.1 and :-.1 are now correctly bad get-words, and :.1 is now correctly a time instead of a get-word
; CAVEAT (fix): def-word content model now correctly shared amongst words, set-words, get-words, and lit-words
  numbr:                   [opt ["-." | "+." | "+" | "-" | "."] digit] ; anything matching this begins a numeric, and therefore, in addition to "'" below, is forbidden from beginning def-words, and consequentially any url or path
 word-regular:             [not ["'" | numbr] any nangl [wordn | upper] any [wordw | upper]] ; all words with no angle characters and at least one (non-slash) non-arrow character
word:                      [word-regular | some arrow] ; all the def-words except slashes-words
; CAVEAT (fix): both issues (in this case including the none-value shorthand "#") and refinements no longer incorrectly permit an adjoining slash to auto-separate
; CAVEAT (fix): ref-word content model now correctly shared amongst issues and refinements -- def-words without slashes or angles are convertible between all six idiomatic forms, which is a very good idea for function refinements
issue:                     ["#" some [wordw | upper | ","]] ; permit leading ticks and numbers, forbid slashes and angles, permit commas, auto-separate from tags, and are permitted in paths
refinement:                ["/" some [wordw | upper | ","]] ; permit leading ticks and numbers, forbid slashes and angles, permit commas, auto-separate from tags, and are forbidden in paths

; ---------
; COHESIVES (string-likes, not delimited) - 2.5 - url email file (unquoted)
; ---------

; urls must begin with a word character -- if alpha, runic, dot, or upper, it must have a non-ending colon (or is (set-) word), and if (sign or) angle, it must have only ending colons succeeded by / (or is (time or) (set-) word))
  slash-trail:             ["/" | ghigh-delim+perch] ; widest freedom -- just two restrictions, one, a % always (percent-) encodes, and two, whitespace, nulls, and the (eight) non-slash delimiters ('selim) must be percent-encoded
; CAVEAT (fix): +' and -' are no longer incorrectly unable to begin sign-urls
  sign-scheme:             [signc opt [["." ghigh-delim-digit-whorl-cache-colon+perch | signc | "'" | alpha | runic | upper] any ghigh-delim-whorl-cache-colon+perch] some ":"]
 sign-url:                 [sign-scheme "/" any slash-trail] ; only used outside paths, because slash-free sign urls are not possible: if scheme-only, such are good or bad set-words, and otherwise, they are always bad times [-:a]
  dot-scheme:              ["." opt [ghigh-delim-digit-whorl-colon+perch any ghigh-delim-whorl-colon+perch] some ":"]
 dot-url-uncoloned:        [dot-scheme some [any ":" some ghigh-delim-whorl-colon+perch]]
 dot-url:                  [dot-scheme any ghigh-delim-whorl+perch] ; slash-free dot urls are possible: if scheme-only, such are good or bad set-words, and otherwise, they are always good urls [.:a]
 dotted-url:               [dot-scheme [some ghigh-delim-whorl+perch opt ["/" any slash-trail] | "/" any slash-trail]] ; only used outside paths
  word-scheme:             [[alpha | runic | upper] any ghigh-delim-colon+perch some ":"]
 word-url-uncoloned:       [word-scheme some [any ":" some ghigh-delim-colon+perch]]
 word-url-unslashed:       [word-scheme some ghigh-delim+perch] ; slash-free word urls are possible: if scheme-only, such are good or bad set-words, and otherwise, they are always good urls [a:a]
; CAVEAT (fix): urls are no longer incorrectly forbidden from beginning with slashes-words (outside paths only of course because of the slashes) or arrow-words (outside paths only of course, because such are urls only if slashed)
; the first character in whatever matches "some arrow" below will match one of < or > because the other five are already taken care of -- three by 'runic in 'word-scheme (|,~,=) and two by 'sign in 'sign-scheme in 'sign-url (+,-)
 word-url:                 [[word-scheme | some arrow some ":" and "/" | some "/" some ":"] some slash-trail] ; slash-free arrow urls are not possible: if scheme-only, such are good set-words, otherwise, they are always bad words
url:                       [dotted-url | sign-url | word-url] ; only used outside paths, and this is the only use of the word-url rule (so it too is only used outside paths)

; emails must have exactly one @ that isn't the first character -- and if it has a colon, it must not start with any word character (if <, it'd be a set-word, a url, or a tag, and otherwise, it'd be a set-word or a url)
  badword-email-uncoloned: [[digit | "#" | "$" | "'" | "," | ":" | "\" | perch] any ghigh-delim-whorl+perch "@" any [any ":" some ghigh-delim-whorl-colon+perch]] ; 17 ASCII graphics characters, including the percent (from 'perch)
  word-email:              [["." | signc | alpha | runic | upper | ">"] any ghigh-delim-whorl-colon+perch "@" any ghigh-delim-whorl-colon+perch] ; 66 ASCII graphics characters -- 11 never begin emails (@, <, and nine from 'delim)
 email-uncoloned:          [word-email | badword-email-uncoloned]
email:                     [word-email | badword-email-uncoloned any ghigh-delim-whorl+perch]

; files must begin with % -- and must not contain @ before the first / (or, perhaps slightly buggily, any unescaped colons (or, perhaps more buggily, for quoted files only, semi-colons) anywhere) or it is an email (or a bad file)
; CAVEAT (fix): a non-empty bare file followed by a quote-string is no longer incorrectly an error
 bare-file-uncoloned:      [not [{%"}] "%" any [any filch-coloned [ghigh-delim-whorl-caret-colon+perch | filch-uncoloned]]]
 bare-file-unslashed:      [not [{%"}] "%" any [ghigh-delim-whorl-caret-colon+perch | filch]]
bare-file:                 [bare-file-unslashed opt ["/" any ["/" | ghigh-delim-caret-colon+perch | filch]]]

; ---------
; LIMITIVES (string-likes, delimited) - 5.5 - file (quoted) char string binary tag comment
; ---------

 quote-file:               [{%"} any [fchar-colon-semic-ditto+perch | "^(5E):" | "^(5E)^(3B)" | {^(5E)"}] {"}] ; files are cohesives because all except the empty one are put out that way, but quote-file is delimited and goes here
char:                      [{#"} [qchar | {^(5E)"} | {"}] {"}]
 brace-string:             ["{" (depth: 1) any ["^(5E){" | "^(5E)}" | qchar-lbrac-rbrac | "{" (depth: depth + 1) | if (depth > 1) "}" (depth: depth - 1)] "}"] ; this is an example of how to handle a nesting lexeme non-recursively
; CAVEAT (fix): there is no longer any way to incorrectly escape literal carriage returns in ditto strings
 quote-string:             [{"} any [qchar | {^(5E)"}] {"}]
string:                    [quote-string | brace-string]
  one-bit:                 [bin-2 any blank]
 binary-base-2:            [any [one-bit one-bit one-bit one-bit  one-bit one-bit one-bit one-bit]] ; enforce number of one-bit characters to be multiple of eight
  four-bits:               [hexan any blank]
 binary-base-16:           [any [four-bits four-bits]] ; enforce number of (four-bit) hexadecimal characters to be even
  six-bits:                [bin64 any blank]
 binary-base-64:           [any [six-bits six-bits six-bits six-bits]] ; enforce number of six-bit characters to be multiple of four, producing three bytes of result with each such group
 fin64:                    [six-bits six-bits "=" any blank "=" any blank | six-bits six-bits six-bits "=" any blank] ; when result length is not a multiple of three -- last bits (four or two) of the final six-bits have no effect
binary:                    ["2#{" any blank binary-base-2 "}" | [opt "16"] "#{" any blank binary-base-16 "}" | "64#{" any blank binary-base-64 opt fin64 "}"]
; tags permitted to begin with arrow-words that start with "<", if it is as "</" or isn't otherwise a (set-) word, url, or path -- </> <:> are tags, but <> is word, <>/<> is path, <:/> is url, and <>:[]<> is set-word, block, word
 tag-begin:                ["<" not [not "/" any arrow any ":" ["/" | path-end]]] ; testcase: a/a<:/> should fail -- meaning <:/> should never match tag-begin (as well it should not, considering that <:/> is a perfectly good url)
; CAVEAT (fix): tags no longer incorrectly accept ill-formed utf8 -- testcase #{3CA03E} (binary) currently loads in R3 as a tag, but should fail to match anything in this PEG
tag:                       [tag-begin any arrow any [chars-nullc-ditto-rangl | {"} any chars-nullc-ditto {"}] ">"]
comment-rule:              ["^(3B)" endln] ; stops before the ending delimiter (the newline), leaving it behind for the calling rule to match

; ----------
; ECHELONICS (slash-separated lexeme containers, not delimited) - 4 - path set-path get-path lit-path (not coincidentally, formed just like the corresponding def-words, with the ' and : decorators applying to the path as a whole)
; ----------
; CAVEAT (fix): a path-ending colon can now correctly only indicate it is a set-path -- to be path-final, set-words must be wrapped in (), and a file, url, or email ending in a colon needs to percent-encode it or to be so wrapped
; CAVEAT (fix): files and urls are no longer incorrectly always the final item in paths -- in a path, / now only separates, so, like slash-dates (-) and date-times (T), files and urls substitute for / (they've a choice, \ or %2F)
; CAVEAT (yuk): lone # is shorthand for the none-value -- incorrectly, like lone % is still incorrectly a shorthand for %"" -- but, unlike issues, a following tag will not auto-separate (so as to permit the #<x> syntax extension)

also prin "" any [unset? :multiline multiline] ; for console pasting, empty line finishes
;
  end-item:                [ bare-file-uncoloned and [any ":" path-end]                               (type: 'bare-file-nocolon)                            ; : R3 change: set-paths can end with this item now
                           | dot-url-uncoloned and [any ":" path-end]                                 (type: 'dot-url-nocolon)                              ; :
                           | word-url-uncoloned and [any ":" path-end]                                (type: 'word-url-nocolon)                             ; :
                           | email-uncoloned and [any ":" path-end]                                   (type: 'email-nocolon)                                ; :
                           | copy x [any "'" any ":"] word and [some ":" path-end]                    (type: join to string! x 'word-not-slashes)           ; bare, get, lit, all arrows, all regulars, no slashes, coloned
                           | copy x [any "'" any ":"] some arrow and path-end                         (type: join to string! x 'arrow-word)                 ; bare, get, lit, all arrows, no regulars, no slashes, no colon
                           | copy x [any "'" any ":"] word-regular and word-end                       (type: join to string! x 'normal-word)                ; bare, get, lit, no arrows, all regulars, no slashes, no colon
                           | issue and [any ":" word-end]                                             (type: 'issue)                                        ; :
                           | "#" not pound-grabs and [any ":" word-end]                               (type: 'none-value)                                   ; :
                           | money and [any ":" path-end]                                             (type: 'money)                                        ; :
                           | path-date and [any ":" path-end]                                         (type: 'path-date emit t emit z t: copy z: copy "")   ; :
                           | time and [any ":" path-end]                                              (type: 'time)                                         ; :
                           | percent and [any ":" path-end]                                           (type: 'percent)                                      ; :
                           | tuple and [any ":" path-end]                                             (type: 'tuple)                                        ; :
                           | pair and [any ":" path-end]                                              (type: 'pair)                                         ; :
                           | decimal and [any ":" path-end]                                           (type: 'decimal)                                      ; :
                           | integer and [any ":" path-end]                                           (type: 'integer)                                      ;
                           ]
  mid-item:                [ not ["</"] copy x [any "'" any ":"] word copy c any ":" and "/"          (type: join to string! x ['mid-word to string! c])    ; bare, set, get, lit, no slashes; bare, also not lone <
                           | issue and "/"                                                            (type: 'issue)                                        ;
                           | "#" and "/"                                                              (type: 'none-value)                                   ;
                           | money and "/"                                                            (type: 'money)                                        ; / R3 change: all paths can continue on after this item now
                           | path-date and "/"                                                        (type: 'path-date emit t emit z t: copy z: copy "")   ;
                           | time and "/"                                                             (type: 'time)                                         ;
                           | percent and "/"                                                          (type: 'percent)                                      ; /
                           | tuple and "/"                                                            (type: 'tuple)                                        ;
                           | pair and "/"                                                             (type: 'pair)                                         ;
                           | decimal and "/"                                                          (type: 'decimal)                                      ; /
                           | integer and "/"                                                          (type: 'integer)                                      ;
                           | bare-file-unslashed and "/"                                              (type: 'bare-file-unslashed)                          ; /
                           | dot-url and "/"                                                          (type: 'dot-url)                                      ; /
                           | word-url-unslashed and "/"                                               (type: 'word-url-unslashed)                           ; /
                           | email and "/"                                                            (type: 'email)                                        ;
                           ]
;
; 'item is used by block-likes and is not directly used by echelonics, but it is put here after 'end-item and 'mid-item to make similarities and differences easier to spot and/or compare
;
  item:                    [ bare-file and path-end                                                   (type: 'bare-file)                                    ;
                           | url and path-end                                                         (type: 'url)                                          ;
                           | email and path-end                                                       (type: 'email)                                        ;
                           | copy x [any "'" any ":"] [word | some "/"] copy c some ":" and path-end  (type: join to string! x ['set-word to string! c])    ; set, all three kinds
                           | copy x [any "'" any ":"] some arrow and path-end                         (type: join to string! x 'arrow-word)                 ; bare, get, lit, arrows
                           | copy x [any "'" any ":"] some "/" and word-end                           (type: join to string! x 'slashes-word)               ; bare, get, lit, slashes
                           | copy x [any "'" any ":"] word-regular and word-end                       (type: join to string! x 'normal-word)                ; bare, get, lit, regulars
                           | issue and word-end                                                       (type: 'issue)                                        ;
                           | "#" not pound-grabs and word-end                                         (type: 'none-value)                                   ;
                           | refinement and word-end                                                  (type: 'refinement)                                   ;
                           | money and path-end                                                       (type: 'money)                                        ;
                           | date and path-end                                                        (type: 'date emit t emit z t: copy z: copy "")        ;
                           | time and path-end                                                        (type: 'time)                                         ;
                           | percent and path-end                                                     (type: 'percent)                                      ;
                           | tuple and path-end                                                       (type: 'tuple)                                        ;
                           | pair and path-end                                                        (type: 'pair)                                         ;
                           | decimal and path-end                                                     (type: 'decimal)                                      ;
                           | integer and path-end                                                     (type: 'integer)                                      ;
                           | comment-rule                                                             (type: 'comment)                                      ;
                           ]
;
; cascades (the block-likes) and unreparsed items (the delimited lexemes that are interpreted, that is, that are not comments) have the same form everywhere, either inside a cascade or as a (non-leading) component of an echelonic
;
  unreparsed-item:         [ quote-file                                                               (type: 'quote-file)                                   ; :
                           | char                                                                     (type: 'char)                                         ; :
                           | string                                                                   (type: 'string)                                       ; : /
                           | binary                                                                   (type: 'binary)                                       ; :
                           | tag                                                                      (type: 'tag)                                          ; :
                           ]
;
; cascades are not in 'item because they do not set 'type and, like the above, they will not be reparsed -- meaning delimited lexemes cannot be a prefix of any other lexeme, for example, <:/> must match url and must not match tag
;
  cascade:                 [ block                                                                                                                          ; : /
                           | eval-block                                                                                                                     ; :
                           | construction                                                                                                                   ; : /
;                           | gorgob                                                                                                                         ; ! [Ed. -- ! means gorgob-implementers delete the ; at their own peril]
                           ]

   path-close:             [copy pc any ":" (emit to string! first deco emit 'path-finish emitt to string! pc)] ; could adjust this to use show, to count paths in their different forms, similar to what happens with words
  unreparsed-mid-or-end:   [[cascade | copy s unreparsed-item (show s)] ["/" path-tail | not "/" path-close]] ; cascade before unreparsed-item (here and in the lexemes rule), otherwise constructs would match none-value then block
 path-tail:                [paint: [copy s (x: copy c: copy "") mid-item (show s) "/" path-tail | unreparsed-mid-or-end | copy s (x: copy c: copy "") end-item (show s) path-close]] ; order matters - mid before end - for reparsing
path:                      [copy px [any "'" any ":"] not "</" paint: copy s word "/" (type: 'path-word show/only s insert/only deco px emit to string! px emitt 'path-start) path-tail (remove deco)]

; --------
; CASCADES (whitespace-separated lexeme containers, delimited) - 3 - block eval-block construction
; --------

; with no header, source is merely a block without the outer brackets, so, a sequence of lexemes
  liner:                   [any [bland | newln (lined: lined + 1)] (if lined > 0 [emitt join "Next item lined (" [lined ")"] lined: 0])]
; gorgob:                   ["#<" (emitt 'gorgob-begin) thru ">" (emitt 'gorgob-end)] ; catch-all extension point ! -- watch the tests ... fail, a good thing twice, change extension point possibilities and require tests to change
 construction:             ["#[" (emitt 'construct-begin) lexemes "]" (emitt 'construct-end)]
 eval-block:               ["(" (emitt 'eval-block-begin) lexemes ")" (emitt 'eval-block-end)]
 block:                    ["[" (emitt 'block-begin) lexemes "]" (emitt 'block-end)]
lexemes:                   [any [liner [cascade | path | paint: copy s (x: c: copy "") [item | unreparsed-item] (show s) | ]] opt [nullc to end]]

; whether headed, embedded, or neither, LOAD will succeed on (and DO will try to evaluate) the string (or binary) if and only if it matches 'script
; CAVEAT (fix): left-bracket characters are no longer incorrectly not permitted in comments after the REBOL and before the left-bracket that begins the header block
    empty-line:            [any bland opt comment-rule [lf|cr | nullc to end]]
   rebol-word:             [["R" | "r"] ["E" | "e"] ["B" | "b"] ["O" | "o"] ["L" | "l"] any empty-line any bland]
  intro-line:              [empty-line | any bland [not ["[" | rebol-word] | [["[" any empty-line any bland [not rebol-word | rebol-word not "["]] | [rebol-word not "["]]] endln]
 rebol-embed:              [any intro-line any bland "[" any empty-line any bland rebol-word "[" (emitt 'Embedded.)]
 rebol-head:               [any intro-line any bland rebol-word "[" (emitt 'Headed.)]
 headed-contents:          [lexemes "]" (emitt 'Contents.) lexemes]
script:                    [(h: false) any intro-line any bland [rebol-embed (h: true) headed-contents "]" (emitt 'End.) to end | if (not h) rebol-head (h: true) headed-contents] | if (not h) lexemes]

; this function's name will be in the external context -- after the day this PEG gets turned into an object
set 'rebol? func ["Is it rebol?" candidate [string! binary!] /local result][paint-chart: none if result: parse candidate script [return paint-chart] result]

; debugging is currently hardwired into the PEG, but is easy to wire off (see below) and it is possible to remove (delete: from here on, all set-words and ()-code in rules except 'script and 'brace-string, and all COPYs in rules)
type: copy ""                ; holds a display form of the isolated, non-recursive, lexeme type that matched -- sometimes varying depending on the individual matching clause
deco: copy []                ; holds path decorations, which recurse, so, is a stack
s: copy x: copy c: copy ""   ; hold item match contents where recursion is guaranteed not to occur, but spans alternatives, so must be set (s) or reset (x c) before trying alternate matches
px: copy pc: copy ""         ; hold path (decoration) match contents, but only where recursion is guaranteed not to occur, and where each use has been recently set from the same rule
z: copy t: copy ""           ; hold 'zoned-time auxiliary captures, emitted and reset at the item level
lined: 0                     ; holds how many newlines were encountered, unnecessary but may catch something -- could otherwise be boolean, modifying the newline sanity testing slightly

emit: emitt: show: func [x /only] [] ; debugging and testing functions called by ()-code in the PEG -- the remainder of this file can now be skipped to safely wire off debugging and to slightly more safely not perform any testing

;===================
;  SYNTAX COMPLETE
;===================
;
; debugging
;            [Ed. -- apparently I'm multiline OK for all of the debugging etc. But I will not apologize for demanding blank lines in the PEG proper.]
also prin "" any [unset? :multiline multiline] ; for console pasting, empty line finishes
;
 output: copy "" ; debugging strings can be both stored and displayed -- see the two choices of definitions for 'emit below
emit: func [a][append output a] ; quiet debugging
emit: func [a][prin a append output a] ; noisy debugging
emitt: func [a][emit join a newline]
  int2hex: func [i [integer!]][back back tail form to-hex i]
 utf8-show: func [b [binary!] /local s][s: copy "#{ " until [either b/1 < 128 [append s int2hex b/1 b: next b][until [append s int2hex b/1 b: next b any [tail? b b/1 < 128 b/1 > 191]]] append s " " tail? b] append s "}"]
 paint: paint-chart: none
; 'show is affected by every match, via 'type (and 'c and 'x), but 'show itself is only called from 'lexemes, 'path, 'path-tail, and 'unreparsed-mid-or-end
show: func ["item detailer, final space instead of newline with /only" s [string! binary!] /only /local w][
    w: s if string? type [type: case [
            parse x [#"'" to end] ['lit-word]
            parse x [#":" to end] ['get-word]
            parse c [#":" to end] ['set-word]
            'else                 ['word]
        ]
        w: copy/part skip s length? x (length? s) - (length? x) - length? c paint: skip paint length? x
    ]
    if not paint-chart [paint-chart: copy []] if not find paint-chart type [append paint-chart reduce [type copy []]] append paint-chart/:type reduce [index? paint length? w]
    emit join "" [type " " to-string s] if type = 'string [emit join " " utf8-show to binary! s] emit either only [" "] ["^/"]
]
;
; syntax highlighting
;                      [Ed. -- a very unsophisticated example.]
html-header: {
    <!DOCTYPE html><html><head><style>
    .black {color: #000000;} .gray6 {color: #222222;} .gray5 {color: #444444;} .gray4 {color: #666666;} .gray3 {color: #888888;} .gray2 {color: #aaaaaa;} .gray1 {color: #ccccccc;} .white {color: #eeeeee;}
    .yellow {color: #ffff00;} .brown {color: #666600;} .red {color: #ff0000;} .magenta {color: #ff00ff;} .violet {color: #aa00aa;} .blue {color: #0000ff;} .cyan {color: #004488;} .green {color: #00aa00;}
    </style></head><body><pre>
}
html-trailer: {
    </pre></body></html>
}
paint-scheme: [%"a file" %file/2 an@email a:url 10/10/10 10-10-10 7 #an-issue /a-refinement # "a string" {another string} #{00000000} <a tag> 'a-lit-word :a-get-word a-set-word: a-word ;comment - nothing green or yellow please
    bare-file-nocolon    brown     bare-file-unslashed  brown     bare-file            brown
    dot-url-nocolon      brown     word-url-nocolon     brown     dot-url              brown     word-url-unslashed   brown     url                  brown
    email-nocolon        brown     email                brown
    path-date            red       date                 red
    time                 red
    money                red       percent              red       tuple                red       pair                 red       decimal              red       integer              red
    issue                gray5     refinement           gray5
    none-value           magenta
    quote-file           brown
    char                 red
    string               brown
    binary               violet
    tag                  violet
    comment              cyan
    lit-word             gray5
    get-word             black
    set-word             blue
    word                 black
]
set 'html-after func ["Return html-converted and syntax-highlit copy."
    s [string! binary!]
    /local sorted
][
    s: copy s sorted: copy []
    foreach [lexeme occurs] paint-chart [foreach [s f] occurs [append sorted reduce [s f lexeme]]]
    sort/reverse/skip sorted 3 s: tail s
    while [all [not head? s not tail? sorted]] [
        if sorted/1 + sorted/2 = index? s [insert s "</span>"]
        s: back s case [
            parse s [#"<" to end] [change/part s "&lt;" 1]
            parse s [#">" to end] [change/part s "&gt;" 1]
            parse s [#"&" to end] [change/part s "&amp;" 1]
        ]
        if sorted/1 = index? s [
            insert s join "<span class=" [paint-scheme/(sorted/3) ">"]
            sorted: skip sorted 3
        ]
    ]
    head insert append s html-trailer html-header
]
;
; analysis
;            Report lexical item counts -- determined via parsing the debugging output.
report-vars: [headed top-count container-count item-count path-count lp gp sp bp block-count eval-block-count construct-count ; sadly, something has to poison the outer context, otherwise it forces bind and/or duplication hell
    numerics n1 n2 n3 n4 n5 n6 n7 n8 idioms ref-words rw iw def-words lw gw sw bw coheres c1 c2 c3 limiteds l1 l2 l3 l4 l5 l6 nc]
stats: context append map-each v report-vars [to set-word! v] [0 bind report-vars self ; report-vars is deliberately modified
   def-word-types: ["lit-word" (++ lw) | "get-word" (++ gw) | "set-word" (++ sw) | "word" (++ bw)] ; the (bare) words that start paths are not counted here, but they could be
   ref-word-types: ["refinement" (++ rw) | "issue" (++ iw)]
  idiomatic-types: [def-word-types (++ def-words) | ref-word-types (++ ref-words)]
  numeric-types: ["tuple" (++ n1) | opt "path-" "date" (++ n2) | "time" (++ n3) | "pair" (++ n4) | "percent" (++ n5) | "money" (++ n6) | "decimal" (++ n7) | "integer" (++ n8)]
  cohesive-types: [["bare-file-unslashed" | "bare-file-nocolon" | "bare-file"] (++ c1) | ["email-nocolon" | "email"] (++ c2) | ["dot-url-nocolon" | "word-url-nocolon" | "dot-url" | "word-url-unslashed" | "url"] (++ c3)]
  limitive-types: ["comment" (++ l1) | "binary" (++ l2) | "tag" (++ l3) | "char" (++ l4) | "string" (++ l5) | "quote-file" (++ l6)]
 isolate-types: [numeric-types (++ numerics) | idiomatic-types (++ idioms) | cohesive-types (++ coheres) | limitive-types (++ limiteds) | "none-value" (++ nc)]
    x: c: "" ; for (path) get/set/lit decorations
   path-detail: [path-count: path-count + 1 if x/1 = #"'" [++ lp] if x/1 = #":" [++ gp] if all [x = "" c/1 = #":"] [++ sp] if all [x = "" c = ""][++ bp]]
  path-types: ["path-word" thru "path-start" newline some [all-types thru newline] (x: copy c: "") copy x [any "'" any ":"] "path-finish" copy c [any ":"] (do path-detail)]
   line-trail: [thru newline any ["Next item lined" thru newline]]
  block-types: ["block-begin" line-trail any [all-types line-trail] "block-end" (++ block-count)]
  eval-block-types: ["eval-block-begin" line-trail any [all-types line-trail] "eval-block-end" (++ eval-block-count)]
  construct-types: ["construct-begin" line-trail any [all-types line-trail] "construct-end" (++ construct-count)]
 container-types: [path-types | block-types | eval-block-types | construct-types]
all-types: [container-types (++ container-count) | isolate-types (++ item-count)]
lex: [any [all-types line-trail (++ top-count)]]
report: [
    join pick ["" "Embedded script.^/" "Header found.^/"] headed + 1
    "Top-level:" top-count newline
    "Total:" container-count + item-count newline
    "Containers:" container-count "--" "Paths:" path-count "-" "Lit paths" lp "Get paths" gp "Set paths" sp "Bare paths" bp "- Blocks:" block-count "- Eval blocks:" eval-block-count "- Constructs:" construct-count newline
    "Isolates total:" item-count newline
    "Numerics:" numerics "-" "Tuples:" n1 "-" "Dates:" n2 "-" "Times:" n3 "-" "Pairs:" n4 "-" "Percents:" n5 "-" "Moneys:" n6 "-" "Decimals:" n7 "-" "Integers:" n8 newline
    "Words:" idioms "--" "Referers:" ref-words "-" "Refinements" rw "Issues" iw "--" "Definers:" def-words "-" "Lit words" lw "Get words" gw "Set words" sw "Bare words" bw newline
    "Cohesives:" coheres "-" "Urls:" c3 "-" "Emails:" c2 "-" "Unquoted files:" c1 newline
    "Limitives:" limiteds "-" "Quoted files:" l6 "-" "Strings:" l5 "-" "Chars:" l4 "-" "Tags:" l3 "-" "Binaries:" l2 "-" "Comments:" l1 newline
    "None-values:" nc newline
]
analyze: func ["Report lexical statistics."
    output [string!] "Must be the output of a clean and successful REBOL? invocation."
][
    set report-vars 0
    if not parse output ["Embedded." line-trail lex "Contents." line-trail lex "End." to end (headed: 1) | "Headed." line-trail lex "Contents." line-trail lex (headed: 2) | lex] [return "Failed!"]
    print report
]
] ; end stats context
set 'analyze :stats/analyze
;
; testing
;
Suite: "Sanity"
Group: "breadth"
test-one: func [x [block!]] [print ["... Test" join x/1 ":"] true = try [all x]]
test-all: func [x [block!] /local r f] [r: true foreach t x [if not r: r and test-one t [f: t break]] print ["---^/" Suite Group "tests" join either r ["passed with flying colours"] [join "failed on test " mold f/1] ".^/--"]]
clear output
test-all breadth-tests: [
  [#1 false = parse "</>" path parse "</>" script parse "<</>" script parse "<>/>" script parse "a a: :a 'a #a /a" script]
  [#2 parse "-0" integer parse "-0.0" decimal parse "-0x0" pair parse "-0.0%" percent parse "-0:0" time]
  [#3 parse "" script parse "^/" script parse "^M" script parse "[]" script parse "[^/]" script parse "^/[]" script parse "[]^/" script parse "^/[]^/" script parse "^/[^/]^/" script]
  [#4 parse {[1 2% $3 4x4 5.5 6:06 7.7.7 8-8-8 8/Aug/8] [a1 a2: :a3 'a4 #a5 /a6] [a@1 a:2 %a3 %"" #"4" "5" {"5"} #{66} <7>] [a/1 a/2: :a/3 'a/4] [[a 1] (a 2) #[paren! [a 2]]] ^(3B) comment} script]
  [#5 parse to-binary {[1 2% $3 4x4 5.5 6:06 7.7.7 8-8-8 8/Aug/8] [a1 a2: :a3 'a4 #a5 /a6] [a@1 a:2 %a3 %"" #"4" "5" {"5"} #{66} <7>] [a/1 a/2: :a/3 'a/4] [[a 1] (a 2) #[paren! [a 2]]] ^(3B) comment} script] ; binary! test
  [#6 parse {X/a/(b:)/:c/'d/#e/(/f)/1/2x2/3:03/($4)/(5%)/6.0.6/(7.7)/8-8-8/(8/8/8)/1@/</a2>/(%3)/#"4"/#{A5}/(a:6)/({"7"})/(a/a1)/(a/a2:)/(:a/a3)/('a/</>/a4)/([])/(())/(1-Jan-2000/10:11:12.13+14:15)} script] ; R3 Alpha
  [#7 parse {'::X/''::a/::b::/:c/'d/#e/(/f)/1/1x2x3/3:03/$4/5%/1.2.3.4.5.6.7.8.9.10.11.12.13.14.15.16/7.7/8-8-8/><::/1@/</a2>/%3/#"4"/#{A5}/a:6/"7"/{7}/('a/'</'>/'a1)/[]/()/1-Jan-2000T10:11:12.13+14:15:::} script] ; R3 Plus
;  [#n #[unset!] = print "There is still a need for a kind of test that fails if set-words (or other things) are erroneously found, like when abutting paths (or other self-terminating things) perhaps -- needs more thinking"]
]
;
Group: "newline"
eachfor: func [wrds code vals][foreach :wrds vals code]
cr: #"^M" lf: #"^/" ; should be provided, but just in case
newline-tests: [] eachfor [name tests result] [append newline-tests reduce [name map-each x tests [rejoin x] result]]
[
    "Onesies" [[lf] [cr] [lf cr] [cr lf]] [4 "Next item lined (1)^/"]
    "Twosies" [[lf lf] [cr cr] [lf cr lf] [cr lf cr] [lf cr lf cr] [cr lf cr lf] [cr lf lf] [lf cr cr]] [8 "Next item lined (2)^/"]
]
newline-tests: head newline-tests
result: true
until [
    print join "... " [newline-tests/1 ": "]
    clear output
    foreach str newline-tests/2 [parse str script]
    result: result and parse output newline-tests/3
    print [pick [succeeded failed] result]
    newline-tests: skip newline-tests 3
    any [not result tail? newline-tests]
]
print ["--^/" Suite Group "tests" either result ["passed with flying colours."] [join {failed the "} [first skip newline-tests -3 {" test.}]] "^/--"]
;
Group: "date zone"
date-zone-tests: reduce [
    ; time and zone presence/absence
    "Should pass" ["1-1-1T1:0-1:15" "1-1-1T1:0-1:0" "1-1-1T1:0" "1-1-1" "1-1-1/1:0-1:15" "1-1-1/1:0-1:0" "1-1-1/1:0" "1/1/1T1:0-1:15" "1/1/1T1:0-1:0" "1/1/1T1:0" "1/1/1/1:0-1:15" "1/1/1/1:0-1:0" "1/1/1/1:0" "1/1/1"] true
    ; zones ending in colon
    "Shouldn't pass" ["1-1-1T1:0-1:" "1/1/1T1:0-1:"] false
    "Used to pass" ["1-1-1/1:0-1:" "1/1/1/1:0-1:"] false
]
date-zone-tests: head date-zone-tests
result: true
until [
    print join "... " [date-zone-tests/1 ": "]
    clear output
    foreach str date-zone-tests/2 [z: copy t: copy "" result: result and (date-zone-tests/3 = parse str script)]
    z: copy t: copy ""
    print [pick [succeeded failed] result]
    date-zone-tests: skip date-zone-tests 3
    any [not result tail? date-zone-tests]
]
print ["--^/" Suite Group "tests" either result ["passed with flying colours."] [join {failed the "} [first skip date-zone-tests -3 {" test.}]] "^/--"]
;
Group: "headers and embedding"
head-tests: compose/deep [ ; results blocks contain two things: what will [parse X script] return and what type will [try [load X]] be
;   Title of test                No leading text                             result block         Simple leading text              result block
    "Embedded"                   ["[^/^(3B)[^/REBOL ^(3B)^/ [] 7]xxx^/"      [(true) (integer!)]  "Normal^/[Rebol[]]xxx"           [(true) (block!)]]
    "Embedded, error in trailer" ["[^/^(3B)[^/REBOL ^(3B)^/ [] ,]xxx^/"      [(false) (error!)]   "Normal^/[Rebol[][xxx"           [(false) (error!)]]
    "Embedded, error in header"  ["[^/^(3B)^/REBOL ^(3B)[^/ [,]7]xxx^/"      [(false) (error!)]   "Normal^/[Rebol[)]xxx"           [(false) (error!)]]
    "Headed"                     ["REBOL^(3B)[^/[]7 xxx^/"                   [(true) (block!)]    "Normal^/Rebol [] xxx"           [(true) (word!)]]
    "Headed, error in trailer"   ["REBOL^(3B)[^/[]7 xx]^/"                   [(false) (error!)]   "Normal^/Rebol []]xxx"           [(false) (error!)]]
    "Headed, error in header"    ["REBOL^(3B)[^/[ 7xx ]^/"                   [(false) (error!)]   "Normal^/Rebol [[]xxx"           [(false) (error!)]]
    "Bug in header comments 1"   ["[^/^(3B)^/REBOL ^(3B)[^/ [] 7]xxx^/"      [(true) (error!)]    "Normal^/[Rebol^(3B)[^/[]]"      [(true) (error!)]]
    "Bug in header comments 2a"  ["[^/^(3B)^/REBOL ^(3B)[rebol^/ [] 7]xxx^/" [(true) (integer!)]  "Normal^/[Rebol^(3B)[rebol^/[]]" [(true) (block!)]]
    "Bug in header comments 2b"  ["[^/^(3B)^/REBOL ^(3B)[r bol^/ [] 7]xxx^/" [(true) (error!)]    "Normal^/[Rebol^(3B)[r bol^/[]]" [(true) (error!)]]
;    "Shouldn't Embed"            ["[^/^(3B)^/REBOL ^(3B)[^/x[] 7]xxx^/"      [(false) (error!)]   "Normal^/[Rebol()]xxx"           [(false) (error!)]] ; needs to be different test, needs to probe result
;    "Shouldn't Head"             ["^()waki^/REBDL^(3B)[^/[]7 xxx^/"          [(false) (error!)]   "Normal^/Rebcl [] xxx"           [(false) (error!)]] ; different tests for nullc's will obsolete this line
]
head-tests: head head-tests
result: reduce [true true]
until [
    print join "... " [head-tests/1 ": "]
    clear output
    foreach [str res] head-tests/2 [result/1: result/1 and (res/1 = parse str script) result/2: result/2 and (res/2 = type? try [load str])]
    print [pick [succeeded failed] result/1 and result/2]
    head-tests: skip head-tests 2
    any [not result/1 not result/2 tail? head-tests]
]
print ["--^/" Suite Group "tests" either result/1 and result/2 ["passed with flying colours."] [join {failed the "} [first skip head-tests -2 {" test.}]] "^/--"]
;
; UTF8 test groups, two of them, pass and fail, totally cribbed from Markus Kuhn's utf8 test file
;
; utf8 utility function, with its own test which is by default wired off
utf8: func ["UTF8 encode an integer." cp [integer!] /local cpb b2 b3 b4][
    b2: [   2#{11000000} or                   back tail to-binary shift cp -6
            2#{10000000} or and~ 2#{00111111} back tail cpb]
    b3: [   2#{11100000} or                   back tail to-binary shift cp -12
            2#{10000000} or and~ 2#{00111111} back tail to-binary shift cp -6
            2#{10000000} or and~ 2#{00111111} back tail cpb]
    b4: [   2#{11110000} or                   back tail to-binary shift cp -18
            2#{10000000} or and~ 2#{00111111} back tail to-binary shift cp -12
            2#{10000000} or and~ 2#{00111111} back tail to-binary shift cp -6
            2#{10000000} or and~ 2#{00111111} back tail cpb]
    if any [cp < 0 cp > to-integer #{10FFFF}] [ ; 0-1114111
        do make error! join "Codepoint " [cp " is not Unicode."]]
    if all [cp > to-integer #{D7FF} cp < to-integer #{E000}] [ ; 55296 - 57343
        do make error! join "Codepoint " [cp " is a surrogate."]]
    cpb: to-binary cp
    case [
        cp < to-integer #{80}     [back tail cpb]
        cp < to-integer #{0800}   [rejoin b2]
        cp < to-integer #{010000} [rejoin b3]
        'else                     [rejoin b4]
    ]
]
; to enable this exhaustive test of utf8 and to-string, remove the 'none and its following semi-colon from the next line
test-utf8: none ; for i 1 65535 1 [res: case [i = 13 10 (i > 55295) and (i < 57344) continue 'else i] bom: utf8 65279 if res <> try [to-integer first to-string join utf8 i bom] [break/return i]]
if any [test-utf8 not error? try [utf8 -1] not error? try [utf8 1114112] not error? try [utf8 55296] not error? try [utf8 57343]] [print "-- ERROR: integrated utf8 encoder has broken somehow" break]
;
Group: "good-utf8"
utf8-yes-tests: [
;   test name [length #-tested length-codepoints bin-result] -- length is number of input codepoints, and also the maximum length of the resultant converted string (if one shorter, it won't immediately trigger an error)
    "Example"                       [5 5 954 8057 963 956 949 #{CEBA E1BDB9 CF83 CEBC CEB5}] ; Greek word 'kosme'
    "BOM encoding"                  [2 1 65279 65279 #{EFBBBF EFBBBF}] ; two BOMs, only one checked, succeeds whether or not the BOM removal bug has been fixed
    "First and last of each length" [2 2 1 127 #{01 7F} 2 2 128 2047 #{C280 DFBF} 2 2 2048 65535 #{E0A080 EFBFBF} 2 2 65536 1114111 #{F0908080 F48FBFBF}]
    "Just outside surrogates"       [2 2 55295 57344 #{ED9FBF EE8080}]
    "Noncharacters"                 [2 2 65534 65535 #{EFBFBE EFBFBF} 2 2 64976 65007 #{EFB790 EFB7AF} 4 4 131070 131071 1048575 1114110 #{F09FBFBE F09FBFBF F3BFBFBF F48FBFBE} ] ; includes byte-reversed BOM
    "BOM removal bug"               [1 1 65279 65279 #{EFBBBF EFBBBF}] ; this test only succeeds if the BOM is incorrectly removed -- will have to be changed after the bug is fixed
]
utf8-yes-tests: head utf8-yes-tests
result: true
until [
    print join "... " [utf8-yes-tests/1 ": "]
    testset: utf8-yes-tests/2
    until [
        length: testset/1 test-length: testset/2 testset: next next testset
        encoded: copy #{} until [append encoded utf8 testset/1 testset: next testset binary? testset/1]
        res: testset/1 testset: next testset
        if any [encoded <> res length - 1 > length? to-str: to string! res length < length? to-str] [result: false break]
        clear output
        ; only testing against strings -- in another suite there will one day be tests for everywhere utf8 is permitted (inside everything but numerics and binaries, and especially as starter for words, urls, and emails)
        if any [not parse join #{22} [encoded #{22}] script not parse output [{string "} to end]] [result: false break]
        tail? testset
    ]
    print [pick [succeeded failed] result]
    utf8-yes-tests: skip utf8-yes-tests 2
    any [not result tail? utf8-yes-tests]
]
print ["--^/" Suite Group "tests" either result ["passed with flying colours."] [join {failed the "} [first skip utf8-yes-tests -2 {" test.}]] "^/--"]
;
Group: "bad-utf8"
utf8-no-tests: [
    "Outside Unicode"               [#{F4908080}]
    "First and last surrogate"      [#{EDA080} #{EDBFBF}]
    "Overlong"                      [#{C0AF} #{E080AF} #{F08080AF} #{C1BF} #{E09FBF} #{F08FBFBF} #{C080} #{E08080} #{F0808080}]
    "Unexpected continuation"       [#{80} #{BF} #{7F80} #{C280BF}]
    "Missing continuation"          [#{C2} #{C220} #{E0A0} #{E0A020} #{F09080} #{F0908020}]
    "Impossible bytes"              [#{FE} #{FF} #{FEFEFFFF}]
]
utf8-no-tests: head utf8-no-tests
result: true
until [
    print join "... " [utf8-no-tests/1 ": "]
    foreach bin testset utf8-no-tests/2 [
        ; only testing against strings -- in another suite there will one day be tests for everywhere bad utf8 should be detected (which is everywhere except inside comments)
        if parse join #{22} [bin #{22}] script [result: false break]
    ]
    print [pick [succeeded failed] result]
    utf8-no-tests: skip utf8-no-tests 2
    any [not result tail? utf8-no-tests]
]
print ["--^/" Suite Group "tests" either result ["passed with flying colours."] [join {failed the "} [first skip utf8-no-tests -2 {" test.}]] "^/--"]
;
; the following does not consider doubling, or tripling, and so on, or mixing amongst, the 4 blobs @#$% - it would be "too Perly" - and similarly the last 10 columns of this table are really for documentation only
; also not mentioned is the obvious path/ref-word extension, doubling (or tripling etc.) their slashes - which would equally obviously not be able to be varied within a single path for readability reasons
Group: "syntax extension points" ; narr - not an arrow word ; sentinel comes after, flag comes before ; T - available ; F - unavailable, i.e., already can be valid (if necessary, after appending what follows the F)
extents: { solo sentinel-word sentinel-number flagged-block flagged-eval-block flagged-brace-string flagged-ditto-string flagged-arrow-word flagged-tag flagged-word flagged-digit flagged-nonalnum-there-are-10
           T    F             F               T             T                  T                    T                    T                  T           T            T             T  T  T  T  T  T  T  T  T  T
           @    word@         0@              @[-]          @(-)               @{AA}                @"-"                 @<->               @<x>        @word        @0            @' @+ @- @. @, @: @< @> @\ @/
           F    T             T               F             T                  F                    F                    T                  F           F            F             F  F  F  F  F  T  T  T  T  T
           #    word#         0#              #[-]          #(-)               #{AA}                #"-"                 #<->               #<x>        #word        #0            #' #+ #- #. #, #: #< #> #\ #/
           T    T             T               T             T                  T                    T                    T                  T           T            F             T  F0 F0 F0 F0 T  T  T  T  T
           $    word$         0$              $[-]          $(-)               ${AA}                $"-"                 $<->               $<x>        $word        $0            $' $+ $- $. $, $: $< $> $\ $/
           F    T             F               F             F                  F                    F                    F                  F           F            F             F  F  F  F  F  T  F  F  F  F
           %    word%         0%              %[-]          %(-)               %{AA}                %"-"                 %<->               %<x>        %word        %0            %' %+ %- %. %, %: %< %> %\ %/
           T    F             F               T             T                  T                    T                    F                  T           F            T             T  F  F  F  T  T  F  F  T  F
           '    word'         0'              '[-]          '(-)               '{AA}                '"-"                 '<->               '<x>        'word        '0            '' '+ '- '. ', ': '< '> '\ '/
           F    F             T               F             F                  F                    F                    F                  T           F            F             F  F  F  F  T  F  F  F  T  Fa
           +    word+         0+              +[-]          +(-)               +{AA}                +"-"                 +<->               +<x>        +word        +0            +' ++ +- +. +, +: +< +> +\ +/
           F    F             F1-1            F             F                  F                    F                    F                  T           F            F             F  F  F  F  T  F  F  F  T  Fa
           -    word-         1-              -[-]          -(-)               -{AA}                -"-"                 -<->               -<x>        -word        -0            -' -+ -- -. -, -: -< -> -\ -/
           F    F             F               F             F                  F                    F                    T                  F           F            F             F  F  F  F  T  F  T  T  T  Fa
           .    word.         0.              .[-]          .(-)               .{AA}                ."-"                 .<->               .<x>        .word        .0            .' .+ .- .. ., .: .< .> .\ ./
           T    T             F               T             T                  T                    T                    T                  T           T            F             T  T  T  T  T  T  T  T  T  T
           ,    word,         0,              ,[-]          ,(-)               ,{AA}                ,"-"                 ,<->               ,<x>        ,word        ,0            ,' ,+ ,- ,. ,, ,: ,< ,> ,\ ,/
           T    F             F0              T             T                  T                    T                    F                  T           F            F             T  F  F  F  F0 T  F  F  T  F
           :    word:         0:              :[-]          :(-)               :{AA}                :"-"                 :<->               :<x>        :word        :0            :' :+ :- :. :, :: :< :> :\ :/
           F    T             T               F             F                  F                    F                    F                  F           F>           F>            F> F> F> F> F> F  F  F  F> F>
           <    narr<         0<              <[-]          <(-)               <{AA}                <"-"                 <<->               <<x>        <narr        <0            <' <+ <- <. <, <: << <> <\ </
           F    T             T               F             F                  F                    F                    F                  T           T            T             T  F  F  T  T  F  F  F  T  Fa
           >    narr>         0>              >[-]          >(-)               >{AA}                >"-"                 ><->               ><x>        >narr        >0            >' >+ >- >. >, >: <> >> >\ >/
           T    T             T               T             T                  T                    T                    T                  T           T            T             T  T  T  T  T  T  T  T  T  T
           \    word\         0\              \[-]          \(-)               \{AA}                \"-"                 \<->               \<x>        \word        \0            \' \+ \- \. \, \: \< \> \\ \/
           F    Fa            F1/1            F             F                  F                    F                    T                  F           F            F             F  F  F  F  F  F  T  T  T  F
           /    word/         1/              /[-]          /(-)               /{AA}                /"-"                 /<->               /<x>        /word        /0            /' /+ /- /. /, /: /< /> /\ //
}
result: true
interleave: func [a b /local r][r: copy a forall b [insert r: next r b/1 r: next r] head r]
foreach [expect trial] next head remove back tail split extents "^/" [
    expect: split expect [some " "] trial: split trial [some " "] foreach [expect trial] interleave expect trial [
        if 1 < length? expect [append trial next expect] if (expect/1 = #"F") <> parse trial script [result: trial break]
    ]
    if result <> true [break]
]
print ["--^/" Suite Group "tests" either true = result ["passed with flying colours."] [join {failed on "} [result {".}]] "^/--"]

;  = = = = = = = 
; Testing's over.
; = = = = = = = =

now/date
