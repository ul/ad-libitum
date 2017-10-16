# Quick & dirty superfast tangler for org mode files.

import os
import ospaths
import strutils
import nre
import tables

type ParserState = enum
  text, namedBlock, code

let blockNameRe = re"#\+NAME:\s*(.+)\s*"
let beginTangleBlock = re"#\+BEGIN_SRC.+:tangle\s+(\S+).*"
let endBlockRe = re"#\+END_SRC\s*"
let macroRe = re"(?<prefix>.*)<<(?<name>.+)>>(?<suffix>.*)"
let paddleRe = re"(\s*)"

let inputName = paramStr 1
let inputText = readFile inputName

var parserState = text
var blockName: string
var tanglePath: string
var counter = 0

var pathToName = initTable[string, string]()
var nameToCode = initTable[string, seq[string]]()

for line in inputText.splitLines:
  case parserState
  of text:
    let m = line.match blockNameRe
    if m.isSome:
      parserState = namedBlock
      blockName = m.get.captures[0]
    else:
      let m = line.match beginTangleBlock
      if m.isSome:
        parserState = code
        tanglePath = m.get.captures[0]
        blockName = "auto-" & counter.intToStr
        counter = counter + 1
        pathToName[tanglePath] = blockName
  of namedBlock:
    parserState = code
    let m = line.match beginTangleBlock
    if m.isSome:
      tanglePath = m.get.captures[0]
      pathToName[tanglePath] = blockName
  of code:
    if line.match(endBlockRe).isSome:
      parserState = text
    else:
      if not nameToCode.hasKey(blockName):
        nameToCode[blockName] = @[]
      nameToCode[blockName].add line

proc paddleLen(s: string): int =
  return s.match(paddleRe).get.captures[0].len

proc tangle(prefix: string, blockName: string, output: File) =
  let code = nameToCode[blockName]
  let paddle = code[0].paddleLen
  for rawLine in code:
      let line = rawLine.substr paddle
      let m = line.match macroRe
      if m.isSome:
        let m = m.get.captures
        tangle prefix & m["prefix"], m["name"], output
        output.writeLine m["suffix"]
      else:
        output.writeLine prefix & line

for tanglePath, blockName in pathToName.pairs:
  let path = tanglePath.splitPath[0]
  if path != "" and not path.dirExists:
    path.createDir
  let output = open(tanglePath, fmWrite)
  tangle "", blockName, output
  close output
