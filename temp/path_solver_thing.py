#!/usr/bin/env python3
import json
import sys
import re

problemNo = 114
segments = [
    "8:_40_50_17_18_51_49_69_100_99_70_83_74_118_156_160_168_134_126_88_66_58",
    "61:_207_216_213_193_172_158_153_170_125_107_63_43_76_113",
    "81:_32_37_30_15_14_8_10_12_26_24",
    "51:_211_221_223_219_212_189",
    "41:_192_208_215_222_220_217",
    "24:_131_95_136_159_174",
    "0:_3_7_5_4_13_19_22",
    "91:_54_80_65_47_23_39",
    "101:_16_20_11_6_2",
]

problemNo = 125
segments = [
    "86:_18_16_15_10_9_11_12_6_3_1_4_0_2_5_7_8_13_17_14_20_25_19_24_40_45",
    "24:_27_23_35_28_42_72_77_95_105_153_139_132_99_116_142_163_154_157",
]

used = set()

problem = json.loads(open(f"problems/{problemNo}.json").read())
solution = problem["figure"]["vertices"]
solution = [(x-100, y) for (x, y) in solution]

for segment in segments:
    start, l = segment.split(":")
    start = int(start)
    for pos, figVert in enumerate(re.findall(r"[_-][0-9]+", l)):
        if figVert[1:] in used:
            print("BAD", file=sys.stderr)
        used.add(figVert[1:])
        if figVert[0] == "_":
            solution[int(figVert[1:])] = problem["hole"][(start + pos) % len(problem["hole"])]

print(json.dumps({"vertices": solution}))
print(",".join(used), file=sys.stderr)
