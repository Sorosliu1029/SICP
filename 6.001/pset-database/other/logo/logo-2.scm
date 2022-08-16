From bh%anarres.Berkeley.EDU@berkeley.edu Mon Dec 11 15:29:38 1989
Return-Path: <bh%anarres.Berkeley.EDU@Berkeley.EDU>
Date: Mon, 11 Dec 89 11:47:48 PST
From: bh%anarres.Berkeley.EDU@berkeley.edu (Brian Harvey)
To: hal
Subject: logo/test.logo

to second :thing
op first bf :thing
end

to twice :thing
pr :thing
pr :thing
end

to pigl :word
if vowelp first :word [op word :word "ay]
op pigl word bf :word first :word
end

to vowelp :let
op memberp :let "aeiou
end

to memberp :thing :list
if emptyp :list [op "false]
if equalp :thing first :list [op "true]
op memberp :thing bf :list
end

to piglatin :sent
if emptyp :sent [op []]
op fput pigl first :sent piglatin bf :sent
end

to repeat :num :instr
if :num=0 [stop]
run :instr
repeat :num-1 :instr
end

to factorial :n
if :n=0 [output 1]
output :n * factorial :n-1
end


