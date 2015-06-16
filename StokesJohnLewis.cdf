(* Content-type: application/vnd.wolfram.cdf.text *)

(*** Wolfram CDF File ***)
(* http://www.wolfram.com/cdf *)

(* CreatedBy='Mathematica 9.0' *)

(*************************************************************************)
(*                                                                       *)
(*  The Mathematica License under which this file was created prohibits  *)
(*  restricting third parties in receipt of this file from republishing  *)
(*  or redistributing it by any means, including but not limited to      *)
(*  rights management or terms of use, without the express consent of    *)
(*  Wolfram Research, Inc. For additional information concerning CDF     *)
(*  licensing and redistribution see:                                    *)
(*                                                                       *)
(*        www.wolfram.com/cdf/adopting-cdf/licensing-options.html        *)
(*                                                                       *)
(*************************************************************************)

(*CacheID: 234*)
(* Internal cache information:
NotebookFileLineBreakTest
NotebookFileLineBreakTest
NotebookDataPosition[      1063,         20]
NotebookDataLength[      9583,        256]
NotebookOptionsPosition[     10000,        246]
NotebookOutlinePosition[     10513,        268]
CellTagsIndexPosition[     10470,        265]
WindowFrame->Normal*)

(* Beginning of Notebook Content *)
Notebook[{

Cell[CellGroupData[{
Cell[BoxData[{
 RowBox[{"Remove", "[", "\"\<Global`*\>\"", "]"}], "\[IndentingNewLine]", 
 RowBox[{
  RowBox[{"Manipulate", "[", "\[IndentingNewLine]", 
   RowBox[{
    RowBox[{
     RowBox[{"stokesI", "=", "1"}], ";", "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"Ex", "[", "t_", "]"}], ":=", 
      RowBox[{"\[ScriptCapitalE]", " ", 
       RowBox[{"(", 
        RowBox[{
         RowBox[{
          RowBox[{"Cos", "[", "\[Beta]", "]"}], 
          RowBox[{"Cos", "[", "\[Chi]", "]"}], 
          RowBox[{"Cos", "[", "t", "]"}]}], "+", 
         RowBox[{
          RowBox[{"Sin", "[", "\[Beta]", "]"}], 
          RowBox[{"Sin", "[", "\[Chi]", "]"}], 
          RowBox[{"Sin", "[", "t", "]"}]}]}], ")"}]}]}], ";", 
     "\[IndentingNewLine]", 
     RowBox[{
      RowBox[{"Ey", "[", "t_", "]"}], ":=", 
      RowBox[{"\[ScriptCapitalE]", " ", 
       RowBox[{"(", 
        RowBox[{
         RowBox[{
          RowBox[{"Cos", "[", "\[Beta]", "]"}], 
          RowBox[{"Sin", "[", "\[Chi]", "]"}], 
          RowBox[{"Cos", "[", "t", "]"}]}], "-", 
         RowBox[{
          RowBox[{"Sin", "[", "\[Beta]", "]"}], 
          RowBox[{"Cos", "[", "\[Chi]", "]"}], 
          RowBox[{"Sin", "[", "t", "]"}]}]}], ")"}]}]}], ";", 
     "\[IndentingNewLine]", 
     RowBox[{"\[Chi]", ":=", 
      RowBox[{
       RowBox[{"(", 
        RowBox[{"1", "/", "2"}], ")"}], 
       TagBox[GridBox[{
          {"\[Piecewise]", GridBox[{
             {
              RowBox[{"ArcTan", "[", 
               RowBox[{"Abs", "[", 
                RowBox[{
                 RowBox[{"Sqrt", "[", 
                  RowBox[{
                   SuperscriptBox["stokesI", "2"], "-", 
                   SuperscriptBox["V", "2"], "-", 
                   SuperscriptBox["Q", "2"]}], "]"}], "/", "Q"}], "]"}], 
               "]"}], 
              RowBox[{"Q", "\[NotEqual]", "0"}]},
             {
              RowBox[{"\[Pi]", "/", "2"}], 
              RowBox[{"Q", "\[Equal]", "0"}]}
            },
            AllowedDimensions->{2, Automatic},
            Editable->True,
            
            GridBoxAlignment->{
             "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
              "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
            
            GridBoxItemSize->{
             "Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, 
              "Rows" -> {{1.}}, "RowsIndexed" -> {}},
            GridBoxSpacings->{"Columns" -> {
                Offset[0.27999999999999997`], {
                 Offset[0.84]}, 
                Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
                Offset[0.2], {
                 Offset[0.4]}, 
                Offset[0.2]}, "RowsIndexed" -> {}},
            Selectable->True]}
         },
         GridBoxAlignment->{
          "Columns" -> {{Left}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{Baseline}}, "RowsIndexed" -> {}},
         GridBoxItemSize->{
          "Columns" -> {{Automatic}}, "ColumnsIndexed" -> {}, 
           "Rows" -> {{1.}}, "RowsIndexed" -> {}},
         GridBoxSpacings->{"Columns" -> {
             Offset[0.27999999999999997`], {
              Offset[0.35]}, 
             Offset[0.27999999999999997`]}, "ColumnsIndexed" -> {}, "Rows" -> {
             Offset[0.2], {
              Offset[0.4]}, 
             Offset[0.2]}, "RowsIndexed" -> {}}],
        "Piecewise",
        DeleteWithContents->True,
        Editable->False,
        SelectWithContents->True,
        Selectable->False]}]}], ";", "\[IndentingNewLine]", 
     RowBox[{"\[Beta]", ":=", 
      RowBox[{
       RowBox[{"(", 
        RowBox[{"1", "/", "2"}], ")"}], 
       RowBox[{"ArcSin", "[", 
        RowBox[{"Abs", "[", 
         RowBox[{"V", "/", "stokesI"}], "]"}], "]"}]}]}], ";", 
     "\[IndentingNewLine]", 
     RowBox[{"\[ScriptCapitalE]", ":=", 
      RowBox[{"Sqrt", "[", 
       RowBox[{"Sqrt", "[", 
        RowBox[{
         RowBox[{"Q", "^", "2"}], " ", "+", " ", 
         RowBox[{
          RowBox[{"Sqrt", "[", 
           RowBox[{
            SuperscriptBox["stokesI", "2"], "-", 
            SuperscriptBox["V", "2"], "-", 
            SuperscriptBox["Q", "2"]}], "]"}], "^", "2"}], " ", "+", " ", 
         RowBox[{"V", "^", "2"}]}], "]"}], "]"}]}], ";", 
     "\[IndentingNewLine]", 
     RowBox[{"ParametricPlot", "[", 
      RowBox[{
       RowBox[{"{", 
        RowBox[{"{", 
         RowBox[{
          RowBox[{"Ex", "[", "t", "]"}], ",", " ", 
          RowBox[{"Ey", "[", "t", "]"}]}], "}"}], "}"}], ",", 
       RowBox[{"{", 
        RowBox[{"t", ",", "0", ",", 
         RowBox[{"2", "\[Pi]"}]}], "}"}], ",", 
       RowBox[{"Axes", "\[Rule]", "None"}], ",", 
       RowBox[{"PlotRange", "\[Rule]", 
        RowBox[{"{", 
         RowBox[{
          RowBox[{"{", 
           RowBox[{
            RowBox[{"-", "1"}], ",", "1"}], "}"}], ",", 
          RowBox[{"{", 
           RowBox[{
            RowBox[{"-", "1"}], ",", "1"}], "}"}]}], "}"}]}], ",", 
       RowBox[{"Frame", "\[Rule]", "True"}], ",", 
       RowBox[{"PlotStyle", "\[Rule]", "Blue"}], ",", 
       RowBox[{"ImageSize", "\[Rule]", "Medium"}]}], "]"}]}], ",", 
    RowBox[{"{", 
     RowBox[{"V", ",", "0", ",", "1"}], "}"}], ",", 
    RowBox[{"{", 
     RowBox[{"Q", ",", "0", ",", "1"}], "}"}]}], "]"}], 
  "\[IndentingNewLine]"}], "\[IndentingNewLine]"}], "Input"],

Cell[BoxData[
 TagBox[
  StyleBox[
   DynamicModuleBox[{$CellContext`Q$$ = 
    0.19980090595781563`, $CellContext`V$$ = 0.20528042614459396`, 
    Typeset`show$$ = True, Typeset`bookmarkList$$ = {}, 
    Typeset`bookmarkMode$$ = "Menu", Typeset`animator$$, Typeset`animvar$$ = 
    1, Typeset`name$$ = "\"untitled\"", Typeset`specs$$ = {{
      Hold[$CellContext`V$$], 0, 1}, {
      Hold[$CellContext`Q$$], 0, 1}}, Typeset`size$$ = {360., {173., 177.}}, 
    Typeset`update$$ = 0, Typeset`initDone$$, Typeset`skipInitDone$$ = 
    True, $CellContext`V$146300$$ = 0, $CellContext`Q$146301$$ = 0}, 
    DynamicBox[Manipulate`ManipulateBoxes[
     1, StandardForm, 
      "Variables" :> {$CellContext`Q$$ = 0, $CellContext`V$$ = 0}, 
      "ControllerVariables" :> {
        Hold[$CellContext`V$$, $CellContext`V$146300$$, 0], 
        Hold[$CellContext`Q$$, $CellContext`Q$146301$$, 0]}, 
      "OtherVariables" :> {
       Typeset`show$$, Typeset`bookmarkList$$, Typeset`bookmarkMode$$, 
        Typeset`animator$$, Typeset`animvar$$, Typeset`name$$, 
        Typeset`specs$$, Typeset`size$$, Typeset`update$$, Typeset`initDone$$,
         Typeset`skipInitDone$$}, 
      "Body" :> ($CellContext`stokesI = 1; $CellContext`Ex[
          Pattern[$CellContext`t, 
           
           Blank[]]] := $CellContext`\[ScriptCapitalE] ((
            Cos[$CellContext`\[Beta]] Cos[$CellContext`\[Chi]]) 
           Cos[$CellContext`t] + (Sin[$CellContext`\[Beta]] 
            Sin[$CellContext`\[Chi]]) Sin[$CellContext`t]); $CellContext`Ey[
          Pattern[$CellContext`t, 
           
           Blank[]]] := $CellContext`\[ScriptCapitalE] ((
            Cos[$CellContext`\[Beta]] Sin[$CellContext`\[Chi]]) 
           Cos[$CellContext`t] - (Sin[$CellContext`\[Beta]] 
           Cos[$CellContext`\[Chi]]) 
          Sin[$CellContext`t]); $CellContext`\[Chi] := (1/2) Piecewise[{{
             ArcTan[
              Abs[
              Sqrt[$CellContext`stokesI^2 - $CellContext`V$$^2 - \
$CellContext`Q$$^2]/$CellContext`Q$$]], $CellContext`Q$$ != 0}, {
            Pi/2, $CellContext`Q$$ == 0}}]; $CellContext`\[Beta] := (1/2) 
         ArcSin[
           
           Abs[$CellContext`V$$/$CellContext`stokesI]]; $CellContext`\
\[ScriptCapitalE] := Sqrt[
          
          Sqrt[$CellContext`Q$$^2 + 
           Sqrt[$CellContext`stokesI^2 - $CellContext`V$$^2 - \
$CellContext`Q$$^2]^2 + $CellContext`V$$^2]]; ParametricPlot[{{
           $CellContext`Ex[$CellContext`t], 
           $CellContext`Ey[$CellContext`t]}}, {$CellContext`t, 0, 2 Pi}, Axes -> 
         None, PlotRange -> {{-1, 1}, {-1, 1}}, Frame -> True, PlotStyle -> 
         Blue, ImageSize -> Medium]), 
      "Specifications" :> {{$CellContext`V$$, 0, 1}, {$CellContext`Q$$, 0, 
         1}}, "Options" :> {}, "DefaultOptions" :> {}],
     ImageSizeCache->{405., {258., 263.}},
     SingleEvaluation->True],
    Deinitialization:>None,
    DynamicModuleValues:>{},
    SynchronousInitialization->True,
    UnsavedVariables:>{Typeset`initDone$$},
    UntrackedVariables:>{Typeset`size$$}], "Manipulate",
   Deployed->True,
   StripOnInput->False],
  Manipulate`InterpretManipulate[1]]], "Output"]
}, Open  ]]
},
WindowSize->{707, 546},
WindowMargins->{{341, Automatic}, {Automatic, 155}},
Visible->True,
ScrollingOptions->{"VerticalScrollRange"->Fit},
ShowCellBracket->False,
Deployed->True,
CellContext->Notebook,
TrackCellChangeTimes->False,
FrontEndVersion->"9.0 for Mac OS X x86 (32-bit, 64-bit Kernel) (January 25, \
2013)",
StyleDefinitions->"Default.nb"
]
(* End of Notebook Content *)

(* Internal cache information *)
(*CellTagsOutline
CellTagsIndex->{}
*)
(*CellTagsIndex
CellTagsIndex->{}
*)
(*NotebookFileOutline
Notebook[{
Cell[CellGroupData[{
Cell[1485, 35, 5332, 141, 290, "Input"],
Cell[6820, 178, 3164, 65, 538, "Output"]
}, Open  ]]
}
]
*)

(* End of internal cache information *)

(* NotebookSignature MwTHxEg@dTs7@BwVz1d6OwQt *)
