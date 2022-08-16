@Comment(Copyright (c) 1990 Massachusetts Institute of Technology

This material was developed by the Scheme project at the Massachusetts
Institute of Technology, Department of Electrical Engineering and
Computer Science.  Permission to copy this material, to redistribute
it, and to use it for any non-commercial purpose is granted, subject
to the following restrictions and understandings.

1. Any copy made of this material must include this copyright notice
in full.

2. Users of this material agree to make their best efforts (a) to
return to the MIT Scheme project any improvements or extensions that
they make, so that these may be included in future releases; and (b)
to inform MIT of noteworthy uses of this material.

3. All materials developed as a consequence of the use of this
material shall duly acknowledge such use, in accordance with the usual
standards of acknowledging credit in academic research.

4. MIT has made no warrantee or representation that this material
(including the operation of software contained therein) will be
error-free, and MIT is under no obligation to provide any services, by
way of maintenance, update, or otherwise.

5. In conjunction with products arising from the use of this material,
there shall be no use of the name of the Massachusetts Institute of
Technology nor of any adaptation thereof in any advertising,
promotional, or sales literature without prior written consent from
MIT in each case. )
@Marker(Make,6001,Press)

@LibraryFile [Symbols]
@define(s, facecode s)
@define(m, facecode m)
@Modify [Itemize, Numbered <@@Bullet {} @,@@Diamond {} @,@@Star {}>,
  Spread 0.2lines]

@Define(BodyStyle,Font BodyFont,Spacing 1,Spread 0.2lines)
@Define(NoteStyle,Font SmallBodyFont,FaceCode R,Spacing 1)
@Font(6001)

@comment{Don't break verbatims}
@modify(verbatim, group)

@comment(programs are indented, never broken except at blank lines)
@modify(example, leftmargin + 1quad, blanklines hinge)

@comment(we also need unbroken examples)
@define(unbrokenexample, use example, blanklines kept)

@comment(Programexample env is like example, but has smaller font)
@define(programexample=example, font smallbodyfont)

@comment(A env is used for code in text)
@Define(A,Facecode A)

@comment(D env for highlighted code)
@Define(D,Facecode D)

@comment(in student version, D and A are same font, but keep them
         distinct, since they will be different in publisher's copy)

@string(runningheadnumber="")

@Enable(Outline,Index,Contents,Exerciselist)
@Send(Contents "@NewPage(0)@Set(Page=1)@Style(PageNumber <@i>)")
@Style(DoubleSided,BindingMargin=0.3inch)
@Send(Contents "@PrefaceSection(Table of Contents)")
@Send(ExerciseList "@PrefaceSection(List of Exercises)")
@send(#Index	"@UnNumbered(Index)",
      #Index    "@center(@b[Index complete only through Chapter 3])",
      #Index	"@Begin(IndexEnv)")
@SendEnd(#Index "@End(IndexEnv)")

@Define	(HDX,LeftMargin 0,Indent 0,Fill,Spaces compact,Above 1,Below 0,
	  break,need 4,Justification Off)
@Define	(Hd0,Use HdX,Font TitleFont5,FaceCode R,Above 1inch,Below 0.5inch)
@Define(Hd1,Use HdX,Font TitleFont5,FaceCode R,Above 1inch,Below
	   0.25inch, PageBreak UntilOdd,Spacing 1.2)
@Define(HD1A=HD1,Centered)
@Define(Hd2,Use HdX,Font TitleFont2,FaceCode R,Above 0.4inch,
        Below 0.1inch,Need 10lines)
@Define(Hd3,Use HdX,Font TitleFont1,FaceCode R,Above 0.4inch,
        Below 0.1inch,Need 5lines)
@Define(Hd4,Use HdX,Font TitleFont1,FaceCode R,Above 0.3inch, Need 4lines)
@Define(TcX,LeftMargin 5,Indent -5,RightMargin 5,Fill,Spaces compact,
	Need 4,
	Above 0,Spacing 1,Below 0,Break,Spread 0,Justification off)
@Define(Tc0=TcX,Font TitleFont3,FaceCode R)
@Define(Tc1=TcX,Font TitleFont1,FaceCode R,Above 0.1inch,
	Below 0.1inch,Need 1inch)
@Define(Tc2=TcX,LeftMargin 8,Font TitleFont0,FaceCode R)
@Define(Tc3=TcX,LeftMargin 12,Font TitleFont0,FaceCode R)
@Define(Tc4=TcX,LeftMargin 8,Font Smallbodyfont,FaceCode R)
@Counter(MajorPart,TitleEnv HD0,ContentsEnv tc0,Numbered [@I],
	  IncrementedBy Use,Announced)

@counter(chapterexercisehead, incrementedby use,
        table "exerciselist",
        titleform "",
        init 0,
        contentsform "@tc1(Chapter @parmref(chapter): @parm(title))")

@counter(chapter,titleenv hd1,contentsenv tc1,numbered [@1.],
	titleform "@chapterexercisehead(@parm(title))@~
         @begin(hd1)@=Chapter @parm(referenced)@*@=@parm(title)@end(hd1)@~
         @string(runningheadnumber=@parm(referenced))",
	  incrementedby use,referenced [@1],announced)

@Counter(Appendix,TitleEnv HD1,ContentsEnv tc1,Numbered [@I.],
	 ContentsForm "@Tc1(Appendix @parm(referenced). @rfstr(@parm(page))@parm(Title))",
	 TitleForm "@Hd1(@=Appendix @parm(referenced)@*@=@Parm(Title))@~
          @string(runningheadnumber=@parm(referenced)",
	  IncrementedBy,Referenced [@I],Announced,Alias Chapter)
@Counter(UnNumbered,TitleEnv HD1,ContentsEnv tc1,Announced,Alias Chapter)
@Counter(Section,Within Chapter,
	titleform
         "@begin(hd2)@parm(numbered) @parm(title)@end(hd2)@~
         @string(runningheadnumber=@parm(referenced))",
          ContentsEnv tc2,
	  Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,
          Announced)
@Counter(ExerciseSection,
        TitleForm "@Hd3(Exercises for Section @ref(Section))",
	 ContentsForm "@Tc3(Exercises for Section @parmref(Section) @rfstr(@parm(page)))")

@Counter(SummarySection,
        TitleForm "@Hd2(Summary of Chapter @ref(Chapter))",
	 ContentsForm "@Tc2(Summary of Chapter @parmref(Chapter) @rfstr(@parm(page)))")

@Counter(AppendixSection,Within Appendix,TitleEnv HD2,ContentsEnv tc2,
	  Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,Announced)
@Counter(SubSection,Within Section,
	titleform
         "@begin(hd3)@parm(numbered) @parm(title)@end(hd3)@~
         @string(runningheadnumber=@parm(referenced))",
         ContentsEnv tc3,
	  Numbered [@#@:.@1.],IncrementedBy Use,Referenced [@#@:.@1])
@Define(Paragraph=HD4)
@Counter(AppendixSubsec,Within AppendixSection,TitleEnv Hd3,ContentsEnv Tc3,
	Numbered [@#@:.@1.],IncrementedBy USe,Referenced [@#@:.@1])


@Counter(PrefaceSection,TitleEnv HD1A,Alias Chapter)
@Define(IndexEnv,Break,CRBreak,Fill,BlankLines Kept,Font SmallBodyFont,
	FaceCode R,Spread 0,Spacing 1,Spaces Kept,LeftMargin 18,Indent -8)

@Counter(Excounter, within chapter, numbered [Exercise @#@:-@1],
         referenced [@#@:-@1],
         init 0, incrementedby use,
        TitleForm "@b(@parm(numbered): )",
        Table "ExerciseList",
        ContentsForm "@tc4[@imbed(numbered,def '@parm(numbered)@ @ ')@parm(page)@~
        @imbed(title,def '@parm(title)')]")

@Define(Exercise, use insert, indent 0, below 1, above 1,
        blanklines break,counter excounter,Numberlocation LFL,
        Font SmallBodyFont, 
	initialize "@excounter()")

@define(exercises, use enumerate, leftmargin 0, indent 0,
        numberlocation lfr, spread 1)

@define(epigraph, use text, leftmargin +2.5inches, indent 0, spread
        0.2, below 1, facecode i)

@define(alphaenumerate,use itemize,numbered <@a. @,@i. >,
	referenced <@a@,@i>)

@libraryfile(figures)
@comment(scribe really sucks when it comes to figure placement)
@modify(figure, group, break around)


@comment{I don't understand why this works, whereas programexample
screws up for figure bodies, but it does.  Oh well.}
@define(smallfigurebody, use verbatim, group, blanklines kept, facecode t, 
font smallbodyfont)      

@comment{We may have long captions}
@define(Captionenv,Continue,Above 0.5line,Below 0.5line,Spacing 1,
        fill, font smallbodyfont,
	LeftMargin +5,RightMargin 5,indent 0,TabExport False,
	BlankLines kept,Initialize "@TabClear()")

@libraryfile(math)
@libraryfile(titlepage)

@modify(equationcounter,within chapter)
@modify(theoremcounter,within chapter)
@modify(footnotecounter, within chapter)
@modify(equation, facecode i)

@modify(hdg, fixed 0.75inch)

@equate(sec=section,subsec=subsection,chap=chapter,para=paragraph,
	subsubsec=paragraph,appendixsec=appendixsection)
@begin(text,indent 1quad,leftmargin 1.25inch,topmargin 1.25inch,
	bottommargin 1.25inch, 
	linewidth 6.0inches,spread 0.075inch,
	use bodystyle,justification,facecode r,spaces compact)
@set(page=0)

@style(date "8 march 1952")
@style(references=cacm)

@pageheading(even,
             left "@value(page)",
             right "@c[draft] -- @value(timestamp)")
@pageheading(odd,
             left "@title(chapter)",
             right "@value(page)")



@marker(make,6001)
@define(bodystyle,spacing 2)
@define(titlestyle,spacing 1)
@define(notestyle,spacing 1)

@comment(this should help some with undefined dover symbols)
@define(m, facecode r)
@define(i, facecode r)
@define(b, facecode r)
@define(t, facecode r)

@textform(leftarrow = "<--")
@textform(rightarrow = "-->")
@textform(dot = "*")
@textform(greaterequal = ">=")
@textform(lessequal = "<=")
@textform(approximately = "~")
@textform(pi = "pi")
@textform(phi = "phi")
@textform(psi = "psi")
@textform(radical = "sqrt ")

@string(runningheadnumber="")
@Enable(Outline,Index,Contents,Exerciselist)
@Send(Contents "@NewPage(0)@Set(Page=1)@Style(PageNumber <@i>)")
@Style(DoubleSided,BindingMargin=0.3inch)
@Send(Contents "@PrefaceSection(Table of Contents)")
@Send(ExerciseList "@PrefaceSection(List of Exercises)")
@send(#Index	"@UnNumbered(Index)",
      #Index    "@b[Index complete only through Chapter 3]",
      #Index	"@Begin(IndexEnv)")
@SendEnd(#Index "@End(IndexEnv)")

@define	(hdx,leftmargin 0,rightmargin 0,indent 0,fill,spaces compact,
	above 2,below 0,break,need 4,justification off)
@define	(hd0,use hdx,above 1inch,below 0.5inch,use b)
@define(hd1,use hdx,below 1,pagebreak untilodd,use b,capitalized)
@define(hd1a=hd1,capitalized off)
@define(hd2,use hdx,above 3,below 1,use b)
@define(hd3,use hdx,use b)
@define(hd4,use hdx,use b)
@define(tcx,leftmargin 5,indent -5,rightmargin 5,fill,spaces compact,
	above 0,spacing 1,below 0,break,spread 0,justification off)
@define(tc0=tcx,use b)
@define(tc1=tcx,above 1,below 1,use b,need 1inch)
@define(tc2=tcx,leftmargin 10)
@define(tc3=tcx,leftmargin 15)
@define(tc4=tcx,leftmargin 20)
@counter(majorpart,titleenv hd0,contentsenv tc0,numbered [@i],
	  incrementedby use,announced)

@counter(chapterexercisehead, incrementedby use,
        table "exerciselist",
        titleform "",
        init 0,
        contentsform "@tc1(Chapter @parmref(chapter): @parm(title))")

@counter(chapter,titleenv hd1,contentsenv tc1,numbered [@1.],
	titleform "@chapterexercisehead(@parm(title))@~
         @begin(hd1)@=Chapter @parm(referenced)@*@=@parm(title)@end(hd1)@~
         @string(runningheadnumber=@parm(referenced))",
	  incrementedby use,referenced [@1],announced)

@Counter(Appendix,TitleEnv HD1,ContentsEnv tc1,Numbered [@I.],
	 ContentsForm "@Tc1(Appendix @parm(referenced). @rfstr(@parm(page))@parm(Title))",
	 TitleForm "@Hd1(@=Appendix @parm(referenced)@*@=@Parm(Title))@~
          @string(runningheadnumber=@parm(referenced)",
	  IncrementedBy,Referenced [@I],Announced,Alias Chapter)

@counter(unnumbered,titleenv hd1,contentsenv tc1,announced,alias chapter)
@Counter(Section,Within Chapter,
	titleform
         "@begin(hd2)@parm(numbered) @parm(title)@end(hd2)@~
         @string(runningheadnumber=@parm(referenced))",
          ContentsEnv tc2,
	  Numbered [@#@:.@1.],Referenced [@#@:.@1],IncrementedBy Use,
          Announced)

@counter(appendixsection,within appendix,titleenv hd2,contentsenv tc2,
	  numbered [@#@:.@1.],referenced [@#@:.@1],incrementedby use,announced)
@Counter(SubSection,Within Section,
	titleform
         "@begin(hd3)@parm(numbered) @parm(title)@end(hd3)@~
         @string(runningheadnumber=@parm(referenced))",
         ContentsEnv tc3,
	  Numbered [@#@:.@1.],IncrementedBy Use,Referenced [@#@:.@1])

@define(paragraph=hd4)

@counter(prefacesection,titleenv hd1a,alias chapter)
@define(indexenv,break,crbreak,fill,blanklines kept,
	spread 0,spacing 1,spaces kept,leftmargin 18,indent -8)

@counter(exercisesection,
        titleform "@hd3(exercises for section @ref(section))",
	 contentsform "@tc3(exercises for section @parmref(section) @rfstr(@parm(page)))")

@Counter(Excounter, within chapter, numbered [Exercise @#@:-@1],
         referenced [@#@:-@1],
         init 0, incrementedby use,
        TitleForm "@b(@parm(numbered): )",
        Table "ExerciseList",
        ContentsForm "@tc4[@imbed(numbered,def '@parm(numbered)@ @ ')@parm(page)@~
        @imbed(title,def '@parm(title)')]")

@Define(Exercise, use insert, indent 0, below 1, above 1,
        blanklines break,counter excounter,Numberlocation LFL,
	initialize "@excounter()")

@Counter(SummarySection,
        TitleForm "@Hd2(Summary of Chapter @ref(Chapter))",
	 ContentsForm "@Tc2(Summary of Chapter @parmref(Chapter) @rfstr(@parm(page)))")

@Define(Exercises, Use Enumerate, Leftmargin 0, Indent 0,
        Numberlocation LFR, spread 1)

@Define(AlphaEnumerate,Use Itemize,Numbered <@A. @,@i. >,
	Referenced <@A@,@i>)

@Define(Epigraph, Use Text, Leftmargin +2.5inches, Indent 0, spread
        0.2, below 1)

@LibraryFile(Figures)

@comment(scribe really sucks when it comes to figure placement)
@modify(figure, group, break around)

@comment{I don't understand why this works, whereas programexample
screws up for figure bodies, but it does.  Oh well.}
@define(smallfigurebody, use verbatim, group, blanklines kept, facecode t)

@comment{We may have long captions}
@define(Captionenv,Continue,Above 0.5line,Below 0.5line,Spacing 1,
        fill,
	LeftMargin +5,RightMargin 5,indent 0,TabExport False,
	BlankLines kept,Initialize "@TabClear()")


@LibraryFile(Math)
@LibraryFile(TitlePage)

@Modify(EquationCounter,Within Chapter)
@Modify(TheoremCounter,Within Chapter)
@modify(footnotecounter, within chapter)

@Equate(Sec=Section,Subsec=SubSection,Chap=Chapter,Para=Paragraph,
	SubSubSec=Paragraph,AppendixSec=AppendixSection)
@Begin(Text,Indent 2,Spread 1,Use BodyStyle,LineWidth 7.5inches,
	Spaces Compact,
	Justification,Font CharDef,FaceCode R)
@set(page=0)


@Style(DoubleSided,BindingMargin=0.3inch)
@PageHeading(Even,Left "@value(Page)")
@PageHeading(Odd,Right "@value(Page)")

@modify(example, capitalized)
@modify(example, leftmargin + 1quad, blanklines hinge)

@comment(Programexample env is like example, but has smaller font)
@define(programexample=example)

@comment(D env for highlighted code)
@Define(D, Facecode R, capitalized)

@comment(A env for code in text)
@define(A, Facecode R, capitalized)


@Style(Date "8 March 1952")
@Style(References=CACM)

@PageHeading(even,
             left "@Value(Page)",
             right "@c[Draft] -- @Value(timestamp)")
@PageHeading(odd,
             Left "@title(Chapter)",
             right "@value(page)")

