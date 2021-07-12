#!/usr/bin/env python3
import json
import sys
import re


problemNo = 125
segments = """
_91,11_90,9_89,10_88,15_87,16_86,18_92,12_93,6_94,3_95,1_96,4_97,0_0,2_1,5_2,7_3,8_4,13_5,17_6,14_7,20_8,25_9,19_10,24_11,40_12,45_67_96_124

_52,199_51,201_50,200_49,198_48,195_47,186_46,194_45,187_44,176
 
68,87_69,107_70,113_71,108_72,89_73,94_74,64_75,63_76,50_80,46_79,34_78,30_77,37
# 
_57,197_58,196_59,191_60,173_61,148_62,138_63,162_64,127_65,110
# 
_36,99_35,132_34,139_33,153_32,105_31,95_30,77_29,72_28,42_27,28_26,35_25,23_24,27
# 
31,95_21,85_20,68_19,48_18,55_17,44_16,60_15,81


22,52
23,32
"""
auto = [ 51,43,33, 180, 62, ]



problemNo = 122
segments = """
0,12_65,0_64,1_63,13_62,14_61,31_60,56_59,79_58,81_57,100_56,112_55,121_54,140_53,154_50,164_49,182_48,188_47,208_46,212_45,214_44,213_43,209_42,210_41,207
41,207_42,210_43,209_44,213_45,214_46,212_47,208_48,188_49,182_50,164_53,154_54,140_55,121_56,112_57,100_58,81_59,79_60,56_61,31_62,14_63,13_64,1_65,0_0,12


_29,114_28,95_26,73_25,90_24,91_23,67_22,50
_22,50_23,67_24,91_25,90_26,73_28,95_29,114

#_23,155_25,173_26,174_28,172_29,148_27,128_24,132
#                            _24,132_27,128_29,148_28,172_26,174_25,173_23,155

_15,24_14,37_13,21_12,11_11,5_10,3

# _33,10_31,4_35,6_37,18_38,27
_35,9_37,2_38,6_39,18_40,24

"""
# 10, 11 swap?


problemNo = 106
segments = """
_14,52_15,55_16,70_17,85_18,80_19,98_20,102_21,107_22,114_23,120_24,116_25,125_26,124_27,128_28,130_29,131_30,129_31,123_32,122_33,111_34,93
34,93_33,111_32,122_31,123_30,129_29,131_28,130_27,128_26,124_25,125_24,116_23,120_22,114_21,107_20,102_19,98_18,80_17,85_16,70_15,55_14,52

12,46_11,28_10,42_9,34_8,21_7,16_6,11_5,12_4,7_3,5_2,6_1,4_0,1_72,0_71,2_70,9_69,3_68,8_67,13_66,23_65,29

_65,29_66,23_67,13_68,8_69,3_70,9_71,2_72,0_0,1_1,4_2,6_3,5_4,7_5,12_6,11_7,16_8,21_9,34_10,42_11,28_12,46

_38,119_39,126_40,127_41,117_37,113_36,100_49,88_48,96_47,86_46,83_45,101_44,103_43,115
_42,121_41,117_40,127_39,126_38,119_37,113_36,100_49,88_48,96_47,86_44,103_45,101_46,83
_42,121_41,117_40,127_39,126_38,119_37,113_36,100_49,88_48,96_44,103_45,101_46,83_47,86
_42,121_41,117_40,127_39,126_38,119_37,113_36,100_49,88_48,96_44,103_47,86_46,83_45,101
_43,115_44,103_45,101_46,83_47,86_48,96_49,88_36,100_37,113_41,117_40,127_39,126_38,119
_45,101_46,83_47,86_44,103_48,96_49,88_36,100_37,113_38,119_39,126_40,127_41,117_42,121
_46,83_45,101_44,103_47,86_48,96_49,88_36,100_37,113_38,119_39,126_40,127_41,117_42,121
_47,86_46,83_45,101_44,103_48,96_49,88_36,100_37,113_38,119_39,126_40,127_41,117_42,121
_42,121_41,117_40,127_39,126_38,119_37,113_36,100_49,88_48,96_47,86_46,83_45,101_44,103_43,115
_43,115_44,103_45,101_46,83_47,86_48,96_49,88_36,100_37,113_38,119_39,126_40,127_41,117_42,121


_56,41_57,31_58,17_59,10_60,15_61,27
_56,41_57,31_58,17_59,10_60,15_61,27
_58,17_59,10_60,15_61,27_57,31_56,41_55,37_54,51
61,27_60,15_59,10_58,17_57,31_56,41_55,37_54,51
61,27_60,15_59,10_58,17_57,31_56,41_55,37_54,51
"""



segments = [i.strip(" ") for i in segments.split("\n")]
segments = [i for i in segments if i and not i.startswith("#")]

usedHole = set()
usedFig = set()
usedHoleFig = set()

problem = json.loads(open(f"problems/{problemNo}.json").read())
solution = problem["figure"]["vertices"]
solution = [(x//3-200, y//3) for (x, y) in solution]

#print(problem)
#problem["hole"]

for segment in segments:
    for i in segment.strip("_").split("_"):
        i = [int(i) for i in i.split(",")]
        if len(i) == 1:
            hole, fig = None, i[0]
        else:
            hole, fig = i

        if hole is not None and hole in usedHole and (hole, fig) not in usedHoleFig:
            print(f"Bad hole {hole}", file=sys.stderr)
        if fig in usedFig and (hole, fig) not in usedHoleFig:
            print(f"Bad fig {fig}", file=sys.stderr)

        usedHole.add(hole)
        usedFig.add(fig)
        usedHoleFig.add((hole, fig))

        if hole is not None:
            solution[fig] = problem["hole"][hole]

print(json.dumps({"vertices": solution}))


usedFig_ = ",".join(map(str, usedFig))

print(
    f"import org.codingteam.icfpc2021.Repl; import org.codingteam.icfpc2021.solver.PathSolver, scala.collection.immutable.BitSet; val p = Repl.load({problemNo}); "
    f"new PathSolver(p).solve(BitSet({usedFig_}), 1, 15)",
    file=sys.stderr,
)

